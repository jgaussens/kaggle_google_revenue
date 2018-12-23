
wd = "~/Google Drive/A5/Data Science/Apprentissage/Kaggle/kaggle_google_revenue/"

setwd(wd)
rm(wd)
require(data.table)
require(dplyr)
require(h2o)

glob = fread("../data/glob.csv", stringsAsFactors = T)


#Explore And Adjust ####
str(glob)
summary(glob)


#Part des NA
require(DataExplorer)
plot_missing(glob)


#Removing NA Over 95% (except transactionrevenue)
glob = glob[, !c("adContent", "page", "slot", "adNetworkType", "isVideoAd", "gclId")]


#Visualiser le nombre de modalités par colonne, et enlever celles à 1
sapply(glob, function(x) length(unique(x)))

todrop <- names(glob)[which(sapply(glob,uniqueN)<2)]
glob[, (todrop) := NULL]
rm(todrop)

plot_missing(glob) #Recheck des parts des missings

glob$date = ymd(glob$date)

glob$transactionRevenue[is.na(glob$transactionRevenue)] <- 0

#Ajout de la colonne log
glob$dollarLogTransactionRevenue = glob$transactionRevenue / 1000000 #Pour plot plus concrets
#Ajout de la colonne de la somme de la transaction revenue, par personne 
tmp = glob[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
glob = merge(glob, tmp, by="fullVisitorId", all.x = TRUE)

glob$logTransactionRevenue = log1p(glob$transactionRevenue)
glob$logSumTransactionRevenue = log1p(glob$sumTransactionRevenue)
#
backup = glob


tmp_non_buyer = glob[glob$logSumTransactionRevenue == 0]
tmp_buyer = glob[glob$logSumTransactionRevenue != 0]

set.seed(1234) #
#on en prend que 10% de la population des non acheteurs (à changer selon ce qu'on veut)
tmp_non_buyer = tmp_non_buyer[sample(NROW(tmp_non_buyer), NROW(tmp_non_buyer)*(1 - 0.90)),]


glob = rbind(tmp_non_buyer, tmp_buyer)

backup2 = glob
str(glob)




# Premier Jet de modèle, pour voir si il y a du jus avant de feature engineering####
h2o.init( max_mem_size = "6G")

glob <- glob %>% select(logSumTransactionRevenue,everything()) #pour placer flag pnf en premi?re column
glob=glob[, -c("dollarLogTransactionRevenue", "transactionRevenue", "sumTransactionRevenue","logTransactionRevenue", "datasplit_test", "datasplit_train")]
#Enlever les grosses modalités pour que h2o tienne le coup
glob = glob[, -c("fullVisitorId", "sessionId", "visitId", "visitStartTime", "networkDomain" )] #Ces variables seront surement à discrétiser pcq trop de modalités

glob$datasplit = as.character(glob$datasplit)

train = as.h2o(glob[glob$datasplit == "train"]) #Train
valid = as.h2o(glob[glob$datasplit == "test"]) #Test



drf_params1 <- list(max_depth = 200
                    ,ntrees = 100)
#,mtries = seq(100,400,100))


search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 100,seed = 1234)


# Train and validate a grid of GBMs
system.time(
  drf_grid1 <- h2o.grid("xgboost", x = c(2:ncol(glob)), y = 1,
                        grid_id = "drf_grid2"
                        ,training_frame = train
                        ,validation_frame = valid
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = drf_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="auto"
                        #,grow_policy="lossguide"
                        , learn_rate = 0.01)
  
)

 a# Get the grid results, sorted by AUC
drf_gridperf1 <- h2o.getGrid(grid_id = "drf_grid2", 
                             sort_by = "rmse", 
                             decreasing = TRUE)

print(drf_gridperf1) 
# Grab the model_id for the top GBM model, chosen by validation AUC
best_drf_model_id <- drf_gridperf1@model_ids[[1]]
best_drf <- h2o.getModel(best_drf_model_id)

# Now let's evaluate the model performance on a test set
# so we get an honest estimate of top model performance
best_drf_perf <- h2o.performance(model = best_drf, 
                                 newdata = test)

tt = glob[glob$datasplit == "test"]
tt = as.h2o(tt)
h2o.rmse(best_drf, valid = TRUE)
