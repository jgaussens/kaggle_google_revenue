# Init --------------------------------------------------------------------

#Kernel avec de bonnes idées: https://www.kaggle.com/erikbruin/google-analytics-eda-lightgbm-screenshots

wd = "D:/Users/u157201/Desktop/ESILV/A5/fun/"
wd = "C:/Users/gauss/Google Drive/A5/Data Science/Apprentissage/Kaggle/"

wd = "~/Google Drive/A5/Data Science/Apprentissage/Kaggle/kaggle_google_revenue/"

setwd(wd)
rm(wd)
require(jsonlite) 
require(data.table)
require(lubridate)
require(questionr) #Pour un accès visuel facile des proportions par variable
require(dummies)
require(dplyr)
require(ggplot2)
require(dummies)
require(h2o)
require(fastDummies)
#
# lecture des données ####
train<-read.csv("../data/train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("../data/test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 



# création d'une colonne indicatrice train test avant assemblage des deux tables ####
train$datasplit<-"train" ; test$datasplit<-"test"
# suppression d'une colonne visiblement inutile
train$campaignCode<-NULL ; test$campaignCode<-NULL
# identification des 4 colonnes au format json
json<-c("trafficSource","totals","geoNetwork","device")
tables<-c("train","test")
glob<-data.table() #table vide qui va récupérer les tableas transformées
# lecture et transformation successive train et test (suppression au passage de colonnes inutiles) 
for (t in tables) {
  partiel<-get(t)[,setdiff(names(get(t)),json)] # colonnes non json
  for (j in json) partiel<-cbind(partiel,fromJSON(paste("[", paste(get(t)[[j]], collapse = ","), "]")))
  temp<-partiel$adwordsClickInfo
  partiel$adwordsClickInfo<-NULL
  temp$targetingCriteria<-NULL
  result<-as.data.table(cbind(partiel,temp))
  if(t=="train") result$campaignCode<-NULL else result$transactionRevenue<-NA
  glob<-rbind(glob,result)
}









#Lecture des données (simplifié) ####
glob = fread("../data/glob.csv", stringsAsFactors = T)


## ### ## ## ### ## ### ## ### ## ### ## 
#Analyse de la donnée, des NA, Remove de certaines colonnes ####
## ### ## ## ### ## ### ## ### ## ### ##

#Explore
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


#Retraitement des typages de certaines colonnes 

glob$date = ymd(glob$date)
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

#Passage de variables en int
numVars <- c("hits", "bounces", "pageviews", "newVisits")
glob[, numVars] <- lapply(glob[, ..numVars], as.integer)
rm(numVars)




#Ajout des colonnes dollar, log, sum, etc ####

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



# Réduction drastique du dataset pour un premier test dummies avec toutes les variables sans prendre trop de place en mémoire####
tmp_non_buyer = glob[glob$logSumTransactionRevenue == 0]
tmp_buyer = glob[glob$logSumTransactionRevenue != 0]

set.seed(1234) #
#on en prend que 10% de la population des non acheteurs (à changer selon ce qu'on veut)
tmp_non_buyer = tmp_non_buyer[sample(NROW(tmp_non_buyer), NROW(tmp_non_buyer)*(1 - 0.90)),]


glob = rbind(tmp_non_buyer, tmp_buyer)

backup2 = glob


#Removing variables like "not available" ####

is_na_val <- function(x) x %in% c("not available in demo dataset", "(not provided)",
                                  "(not set)", "<NA>", "unknown.unknown",  "(none)")

glob <- glob %>% mutate_all(funs(ifelse(is_na_val(.), NA, .)))

glob = as.data.table(glob)

glob$date <- as.Date(glob$date, origin='1970-01-01')
tmp = glob$datasplit
#Test avec le mutate (remplace les categorielles en int), plutôt que le dummies
glob = glob %>% 
  mutate_if(is.character, factor) %>% 
  glimpse()

glob = glob %>% 
  mutate_if(is.factor, as.integer) %>% 
  glimpse()

glob = as.data.table(glob)

#
#Dummify dataset ####
str(glob)

#Auto Dummyfy variables with not to much levels
sapply(glob, function(x) length(unique(x)))


#Getting Categorical Variables with less than 800 levels
chrs <- sapply(glob, is.character)
chrCols <- names(glob[, ..chrs])
tmp = sapply(glob[, ..chrs], function(x) length(unique(x)))
tmp = tmp[tmp<800]
tmp = names(tmp)

tmp2 = glob[, ..tmp]

dummy_cols(tmp2)
#tmp2 = as.data.frame(tmp2)
tmp2 = as.data.table(tmp2)


# 
tmp2 = tmp2[, -..tmp] #Keeping only integer variables

glob = cbind(glob, tmp2)






#Tests de forets one by one (h2o) ####

h2o.init(nthreads = -1)



#https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R
# H2O ####
library(h2o)
h2o.init(nthreads = -1)

###### ### ### ### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### 
###                              RANDOM FOREST 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

glob <- glob %>% select(transactionRevenue,everything()) #pour placer flag pnf en premi?re column

#On enlève char et factors

#char_cols <- unlist(lapply(glob, is.character))
#glob = glob[, -..char_cols]

#date_cols <- unlist(lapply(glob, is.Date))
#glob = glob[, -..date_cols]



#Remove des ints inutiles pour la prédiction
glob=glob[, -c("dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue", "datasplit_test", "datasplit_train")]

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(glob[glob$datasplit == "train"]) 
                         ,ratios = c(0.6,0.2)  #partition data into 70%, 15%, 15% chunks
                         ,destination_frames = c("train","valid","test")
                         ,seed = 1234)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

train = as.h2o(glob[glob$datasplit == "2"]) #Train
valid = as.h2o(glob[glob$datasplit == "1"]) #Test



drf_params1 <- list(max_depth = 100
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
                        ,tree_method="hist"
                        ,grow_policy="lossguide")
  
)

# Get the grid results, sorted by AUC
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
 