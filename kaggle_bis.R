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
require(Metrics)
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

glob = fread("../data/glob.csv", stringsAsFactors = F)
glob = fread("../data/glob.csv", stringsAsFactors = T)

glob = glob[glob$datasplit == "train"]
#Bibliothèque de fonctions ####

#Harmonisation des NA du dataset
na_replacer <- function(data_set, characters_to_replace =  c("not available in demo dataset", "(not provided)",
                                                             "(not set)", "<NA>", "unknown.unknown",  "(none)", "")) {
  library(data.table)
  setDT(data_set)
  text_features <- names(data_set)[sapply(data_set, class) %in% c("character", "factor")]
  for (x in text_features) {
    foo <- data_set[, get(x)]
    data_set[, eval(x) := ifelse(foo %in% characters_to_replace, NA, foo)]
  }
  return(data_set)
}


#Fonction de plot frequency
plot_histo <- function(dt, col, top){ 
  
  t = table(dt[[col]])
  t = as.data.frame(t)
  tt  <- t[order(t[,2],decreasing=TRUE),]
  tt = tt[1:top,]
  
  
  print(ggplot(tt,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity"))
  
  return(tt)
}



#Plot les sessions en fonction de factors (vertical)
plotSessions <- function(dataframe, factorVariable, topN=12) {
  var_col <- enquo(factorVariable)
  dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>%
    ggplot(aes_(x=var_col, y=~n, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="number of sessions")+
    theme(legend.position="none")
}

#Plot les transactionRevenue en fonction de factors (vertical)
plotRevenue <- function(dataframe, factorVariable, topN=12) {
  var_col <- enquo(factorVariable)
  dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none")
}


#Plot les session en fonction de factors (horizontal)
plotSessionsFlip <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  x <- dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>% arrange(n)
  y <- x[[1]]
  x %>% ggplot(aes_(x=var_col, y=~n)) + coord_flip() +
    geom_bar(stat='identity', fill="orange")+
    scale_y_continuous(labels=comma)+
    labs(x="", y="number of sessions")+
    theme(legend.position="none") +
    scale_x_discrete(limits=y)
}

#Plot les transactionRevenue en fonction de factors (horizontal)
plotRevenueFlip <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  x <- dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% arrange(rev) %>% ungroup()
  y <- x[[1]]
  x %>% ggplot(aes_(x=var_col, y=~rev)) + coord_flip() +
    geom_bar(stat='identity', fill="orange")+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none") +
    scale_x_discrete(limits=y)
}


#Traitement de la cible TransactionRevenue, ajout des logs1p pour plus de clarté ####
glob$transactionRevenue[is.na(glob$transactionRevenue)] <- 0
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

#Ajout de la colonne log
glob$dollarLogTransactionRevenue = glob$transactionRevenue / 1000000 #Pour plot plus concrets
#Ajout de la colonne de la somme de la transaction revenue, par personne 
tmp = glob[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
glob = merge(glob, tmp, by="fullVisitorId", all.x = TRUE)

glob$logTransactionRevenue = log1p(glob$transactionRevenue)
glob$logSumTransactionRevenue = log1p(glob$sumTransactionRevenue)

#Colonne indiquant si achat par ligne
glob$isTransaction[glob$transactionRevenue != 0] = 1
glob$isTransaction[glob$transactionRevenue == 0] = 0


#Colonne indiquant au moins un achat par access id
glob$isOnceTransaction[glob$sumTransactionRevenue != 0] = 1
glob$isOnceTransaction[glob$sumTransactionRevenue == 0] = 0




## ### ## ## ### ## ### ## ### ## ### ## 
#Traitement et Remove des NA, Remove de certaines colonnes ####
## ### ## ## ### ## ### ## ### ## ### ##

#Exploration
str(glob)
summary(glob)


#Visualiser le nombre de modalités par colonne, et enlever celles uniques
sapply(glob, function(x) length(unique(x)))

todrop <- names(glob)[which(sapply(glob,uniqueN)<2)]
glob[, (todrop) := NULL]
rm(todrop)


#Traitement des NA
na_replacer(glob) #Fonction pour enlever les NA au global

require(DataExplorer)
plot_missing(glob)


#Removing NA Over 95% (except transactionrevenue)
# Tester SANS
#glob = glob[, !c("adContent", "page", "slot", "adNetworkType", "isVideoAd", "gclId", "campaign", "keyword")]

plot_missing(glob) #Recheck des parts des missings


#Retraitement des typages de dates 

glob$date = ymd(glob$date)
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

glob$visitStartTime = as.POSIXct(glob$visitStartTime, tz="UTC", origin='1970-01-01')
glob$hour = lubridate::hour(glob$visitStartTime)

#Passage de variables en int
numVars <- c("hits", "bounces", "pageviews", "newVisits")
glob[, numVars] <- lapply(glob[, ..numVars], as.integer)
rm(numVars)


## On remplace les NA avec leurs "vrais valeurs"

#glob$pageviews[is.na(glob$pageviews)] <- 1
glob$pageviews[is.na(glob$pageviews)] <- 0


glob$newVisits[is.na(glob$newVisits)] <- 0

glob$bounces[is.na(glob$bounces)] <- 0

glob$isTrueDirect[is.na(glob$isTrueDirect)] <- FALSE

globThumb = glob[glob$isTransaction == 1]
globThumb = as.data.table(globThumb)


# --- --- --- --- ---  FEATURE ENGINEERING --- --- --- --- # ####

#Frequences et discrétisations des variables QUALITATIVES - mode 2 ####
sapply(glob, function(x) length(unique(x)))


#networkDomain ###
plot_histo(glob, "networkDomain", 10)

tt = as.data.table(plot_histo(glob, "networkDomain", 10))
tmp = as.character(tt$Var1)

glob$networkDomain[!glob$networkDomain %in% tmp & !is.na(glob$networkDomain)] = "Autre"

#Country ###
plot_histo(glob, "country", 10)
plot_histo(glob, "country", 10)

tt = as.data.table(plot_histo(glob, "country", 10))
tmp = as.character(tt$Var1)

glob$country[!glob$country %in% tmp & !is.na(glob$country)] = "Autre"

# referralPath ###
plot_histo(glob, "referralPath", 10)
plot_histo(glob, "referralPath", 10)

tt = as.data.table(plot_histo(glob, "referralPath", 2))
tmp = as.character(tt$Var1)

glob$referralPath[!glob$referralPath %in% tmp & !is.na(glob$referralPath)] = "Autre"

#Region ###
plot_histo(glob, "region", 10)
plot_histo(glob, "region", 30)

tt = as.data.table(plot_histo(glob, "region", 15))
tmp = as.character(tt$Var1)

glob$region[!glob$region %in% tmp & !is.na(glob$region)] = "Autre"

#Source ###
plot_histo(glob, "source", 10)
plot_histo(glob, "source", 3)

tt = as.data.table(plot_histo(glob, "source", 3))
tmp = as.character(tt$Var1)

glob$source[!glob$source %in% tmp & !is.na(glob$source)] = "Autre"

#city ###
plot_histo(glob, "city", 10)
plot_histo(glob, "city", 40)

tt = as.data.table(plot_histo(glob, "city", 40))
tmp = as.character(tt$Var1)

glob$city[!glob$city %in% tmp & !is.na(glob$city)] = "Autre"

#keyword ###
plot_histo(glob, "keyword", 2)
plot_histo(glob, "keyword", 4)

tt = as.data.table(plot_histo(glob, "keyword", 4))
tmp = as.character(tt$Var1)

glob$keyword[!glob$keyword %in% tmp & !is.na(glob$keyword)] = "Autre"





#https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R

#Dates ####

# Cr?e une var pour les jours de la semaine 
glob$weekdayy <- weekdays(glob$date) 

# Cr?e une var pour les mois de l'a semaine l'ann?e
glob$month <- months(glob$date) 

# Cr?e une var pour les jours de la semaine 
glob$quarter <- quarter(glob$date) 

# Cr?ation des p?riode de solde au USA psk une grosses partie des clients viennent de la bas

black_friday <- seq(as.Date("2017/11/23"), as.Date("2017/11/27"),"days")
president_day <- seq(as.Date("2017/2/17"), as.Date("2017/2/20"),"days")
memorial_day <- seq(as.Date("2017/5/25"), as.Date("2017/5/29"),"days")
independence_day <- seq(as.Date("2017/6/30"), as.Date("2017/7/4"),"days")
back_to_schoo__labor_day <- seq(as.Date("2017/8/26"), as.Date("2017/9/4"),"days")
colombus_day <- seq(as.Date("2017/10/6"), as.Date("2017/10/9"),"days")
christmas_sales <- seq(as.Date("2017/12/1"), as.Date("2017/12/26"),"days")
# Id?e rajouter en plus les d?but/fin d'ann?e psk c'est des goodies qu'on offre aux ?tudiant et plus fin de graduation = solde et achats
# Aussi saint valentin + fete des meres toussa


tempSalesDate <- as.Date(c(black_friday, president_day ,memorial_day ,independence_day ,back_to_schoo__labor_day
                           ,colombus_day ,christmas_sales))

# Probl?me chaque ann?e certaines dates ne sont pas les meme donc pour l'instant on s'en fout on enleve la date mais du coup on aura pas 100% de accuracy 
# Les dates sont cal? sur l'ann?e 2017
salesDate<-format(tempSalesDate, format="%m-%d")

rm(tempSalesDate,black_friday, president_day ,memorial_day ,independence_day ,back_to_schoo__labor_day,colombus_day ,christmas_sales)

glob$date_without_year <- format(glob$date, format="%m-%d")

glob$isSalesPeriod <- 0
glob$isSalesPeriod[glob$date_without_year %in% salesDate] = 1

as.data.frame(table(glob$isSalesPeriod))

length(which(glob$month == "juillet" & glob$isTransaction == 1 ))

  
#Retraitement var jules
glob$quarter = as.factor(glob$quarter)
glob$isSalesPeriod = as.factor(glob$isSalesPeriod)

# --- --- --- --- MODÉLISATION --- --- --- --- #
# H2O ####

###### ### ### ### ### ### ### ### ### ### ###  ### ### ### ### ### ### ### ### ### ### ### 
###                              Xgboost - Classif
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###



h2o.init(nthreads = -1)

glob = fread("../data/glob_noDiscr.csv", na.strings = "", stringsAsFactors = T)
str(glob)
glob$isTransaction = as.factor(glob$isTransaction)
glob$isSalesPeriod = as.factor(glob$isSalesPeriod)
glob$quarter = as.factor(glob$quarter)
glob$date = as.Date(glob$date)


#glob$logSumTransactionRevenue = as.numeric(glob$logSumTransactionRevenue)

glob <- glob %>% select(isOnceTransaction,everything()) 
glob <- glob %>% select(fullVisitorId,everything()) 
glob <- glob %>% select(transactionRevenue,everything()) 
glob <- glob %>% select(isTransaction,everything()) 

globThumb = glob[glob$isOnceTransaction == 1]


#Remove des ints inutiles pour la prédiction
glob=glob[, -c("gclId", "date","visitStartTime", "datasplit", "visitId", "sessionId","dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue", "datasplit_test", "datasplit_train")]

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(glob) 
                         ,ratios = c(0.7,0.25)  #partition data into 60%, 20%, 20% chunks
                         ,destination_frames = c("train","valid","test")
                         ,seed = 1234)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]


#autre méthode de split
glob=glob[, -c("gclId","visitStartTime", "datasplit", "visitId", "sessionId","dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue", "datasplit_test", "datasplit_train")]

train = as.h2o(glob[glob$date < as.Date("2017-04-01")])
valid = as.h2o(glob[glob$date > as.Date("2017-04-01")])


#train = as.h2o(glob[glob$datasplit == "2"]) #Train
#valid = as.h2o(glob[glob$datasplit == "1"]) #Test



drf_params1 <- list(max_depth = 100
                  ,ntrees = 150
                  , learn_rate = 0.01
                  , stopping_rounds = 1000)
                  #,mtries = seq(100,400,100))


search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 100,seed = 1234)


# Train and validate a grid of GBMs
system.time(
  drf_grid1 <- h2o.grid("xgboost", x = c(5:ncol(glob)), y = 1,
                        grid_id = "drf_grid2"
                        ,training_frame = train
                        ,validation_frame = valid
                        #,nfolds = 5
                        #,keep_cross_validation_predictions = T
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = drf_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="hist"
                        ,grow_policy="lossguide"
                        )
)

# Get the grid results, sorted by AUC
drf_gridperf1 <- h2o.getGrid(grid_id = "drf_grid2", 
                             sort_by = "auc", 
                             decreasing = T)

print(drf_gridperf1) 
# Grab the model_id for the top GBM model, chosen by validation AUC
best_drf_model_id <- drf_gridperf1@model_ids[[1]]
best_drf <- h2o.getModel(best_drf_model_id)
best_drf

t =predict(best_drf, valid)
t = as.data.table(t)
t[, .N, by="predict"]

#test2 = cbind(test2, t$predict)



#Regression 
#pour tester direct sans la classif en amont, pas lancer sur train2 mais sur train et test



globThumb = as.data.table(train)
globThumb = globThumb[globThumb$isOnceTransaction == 1] #Pour train uniquement sur les gens ayant acheté
train2 = globThumb


#optionnel si on veut régression directe
glob <- glob %>% select(transactionRevenue,everything()) 


train2 <- train2 %>% select(transactionRevenue,everything()) 

test2 = as.data.table(valid)
test2
train2 <- train2 %>% select(transactionRevenue,everything()) 

train2 = train2[, !c("date")]
test2 = test2[, !c("date")]


train2 = as.h2o(train2)
test2 = as.h2o(test2)

system.time(
  drf_grid3 <- h2o.grid("xgboost", x = c(5:ncol(glob)), y = 1,
                        grid_id = "drf_grid3"
                        ,training_frame = train2
                        ,validation_frame = test2
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = drf_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="hist"
                        ,grow_policy="lossguide"
  )
)


drf_gridperf3 <- h2o.getGrid(grid_id = "drf_grid3", 
                             sort_by = "rmse", 
                             decreasing = F)

print(drf_gridperf3) 

#test2 = test #Pour tester sur autre chose que valid

best_drf_model_id <- drf_gridperf3@model_ids[[1]]
best_drf <- h2o.getModel(best_drf_model_id)
best_drf

t2 =predict(best_drf, test2)
t2 = as.data.table(t2)
t[, .N, by="predict"]

v = as.data.table(valid)
final = cbind(v, t$predict, t2$predict) #classif + régression

final

final$y_transactionRevenue = final$V3
final$y_transactionRevenue[final$V2 == 0] = 0


#ajout de logSumtransactionRevenue
tmp = final[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)

final$y_transactionRevenue[final$y_transactionRevenue < 0] = 0

final$logTransactionRevenue = log1p(final$transactionRevenue)
final$logSumTransactionRevenue = log1p(final$sumTransactionRevenue)

#Ajout de predictedLogSumTransactionRevenue
tmp = final[, sum(y_transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "y_sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)

final$y_logTransactionRevenue = log1p(final$y_transactionRevenue)
final$y_logSumTransactionRevenue = log1p(final$y_sumTransactionRevenue)

#Les Négatifs <= 0
#final$y_logSumTransactionRevenue[final$y_sumTransactionRevenue < 0] = 0

#refaire logSumTransactionRevenue
rmse(final$logSumTransactionRevenue, final$y_logSumTransactionRevenue)


#Passer à 0 les prédites en 0
#passer à predict les prédits en 1
#faire le rmse

#régression Directe ####

h2o.init(nthreads = -1)

glob = fread("../data/glob_noDiscr.csv", na.strings = "", stringsAsFactors = T)
str(glob)
glob$isTransaction = as.factor(glob$isTransaction)
glob$isSalesPeriod = as.factor(glob$isSalesPeriod)
glob$quarter = as.factor(glob$quarter)
glob$date = as.Date(glob$date)


#glob$logSumTransactionRevenue = as.numeric(glob$logSumTransactionRevenue)

glob <- glob %>% select(isOnceTransaction,everything()) 
glob <- glob %>% select(fullVisitorId,everything()) 
glob <- glob %>% select(isTransaction,everything()) 
glob <- glob %>% select(transactionRevenue,everything()) 

globThumb = glob[glob$isOnceTransaction == 1]


#Remove des ints inutiles pour la prédiction
glob=glob[, -c("gclId", "date","visitStartTime", "datasplit", "visitId", "sessionId","dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue", "datasplit_test", "datasplit_train")]

# Partition the data into training, validation and test sets
splits <- h2o.splitFrame(data = as.h2o(glob) 
                         ,ratios = c(0.7,0.25)  #partition data into 60%, 20%, 20% chunks
                         ,destination_frames = c("train","valid","test")
                         ,seed = 1234)  #setting a seed will guarantee reproducibility
train <- splits[[1]]
valid <- splits[[2]]
test <- splits[[3]]

#Split
glob=glob[, -c("gclId","visitStartTime", "datasplit", "visitId", "sessionId","dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue", "datasplit_test", "datasplit_train")]

train = glob[glob$date < as.Date("2017-04-01")]
valid = glob[glob$date > as.Date("2017-04-01")]

train = train[, -c("date")]
valid = valid[, -c("date")]
train = as.h2o(train)
valid = as.h2o(valid)


drf_params1 <- list(max_depth = 100
                    ,ntrees = 300
                    , learn_rate = 0.01
                    , stopping_rounds = 1000)
#,mtries = seq(100,400,100))


search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 100,seed = 1234)




system.time(
  drf_grid3 <- h2o.grid("xgboost", x = c(5:ncol(glob)), y = 1,
                        grid_id = "drf_grid3"
                        ,training_frame = train
                        ,validation_frame = valid
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = drf_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="hist"
                        ,grow_policy="lossguide"
  )
)

drf_gridperf3 <- h2o.getGrid(grid_id = "drf_grid3", 
                             sort_by = "rmse", 
                             decreasing = F)

print(drf_gridperf3) 

#test2 = test #Pour tester sur autre chose que valid

best_drf_model_id <- drf_gridperf3@model_ids[[1]]
best_drf <- h2o.getModel(best_drf_model_id)
best_drf


t2 =predict(best_drf, valid)
t2 = as.data.table(t2)
t2[, .N, by="predict"]

v = as.data.table(valid)
final = cbind(v, t2$predict) #régression


final

#ajout de logSumtransactionRevenue
tmp = final[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)

#final$y_transactionRevenue[final$y_transactionRevenue < 0] = 0

final$logTransactionRevenue = log1p(final$transactionRevenue)
final$logSumTransactionRevenue = log1p(final$sumTransactionRevenue)

#Ajout de predictedLogSumTransactionRevenue
tmp = final[, sum(V2), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "y_sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)

final$y_logTransactionRevenue = log1p(final$V2)
final$y_logSumTransactionRevenue = log1p(final$y_sumTransactionRevenue)

#Les Négatifs <= 0
final$y_logSumTransactionRevenue[final$y_sumTransactionRevenue < 0] = 0

#refaire logSumTransactionRevenue
rmse(final$logSumTransactionRevenue, final$y_logSumTransactionRevenue)



#







# Réduction drastique du dataset pour un premier test dummies avec toutes les variables sans prendre trop de place en mémoire####
tmp_non_buyer = glob[glob$logSumTransactionRevenue == 0]
tmp_buyer = glob[glob$logSumTransactionRevenue != 0]

set.seed(1234) #
#on en prend que 10% de la population des non acheteurs (à changer selon ce qu'on veut)
tmp_non_buyer = tmp_non_buyer[sample(NROW(tmp_non_buyer), NROW(tmp_non_buyer)*(1 - 0.90)),]


glob = rbind(tmp_non_buyer, tmp_buyer)

backup2 = glob






#Removing variables like "not available" ####






glob[]

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


# passage en factors auto ####
glob <- glob %>%
  select(-fullVisitorId, -visitId, -sessionId, -visitStartTime) %>% 
  mutate_if(is.character, factor)

#keeping vector of categorical features for LGB
categorical_feature <- names(Filter(is.factor, glob))


 


#Test group by ####

aggregateInteger <- function (df, nameC){
  
  columnName <- as.symbol(paste0(nameC))
  columnNameSum <- as.symbol(paste0(nameC,"Sum"))
  columnNameMean <- as.symbol(paste0(nameC,"Mean"))
  columnNameMedian <- as.symbol(paste0(nameC,"Median"))
  columnNameVar <- as.symbol(paste0(nameC,"Var"))
  columnNameSd <- as.symbol(paste0(nameC,"Sd"))
  
  #Creation des nouvelles colonnes aggrega
  etape1 <- merge(df, group_by(glob,fullVisitorId) %>% 
                    summarize(!!columnNameSum := sum(!!columnName),
                              !!columnNameMean := mean(!!columnName),
                              !!columnNameMedian := median(!!columnName),
                              !!columnNameVar := var(!!columnName),
                              !!columnNameSd := sd(!!columnName)
                    ))
  
  #suppression de la colonne qu'on vient d'aggreger
  resultat <- etape1 %>% select(-one_of(nameC))
  
  return(resultat)
  
}


aggregateCharacter <- function (df, nameC){
  
  listeValue <- unique(glob[[nameC]])
  
  for(x in listeValue){
    val <- as.symbol(paste0(x))
    columnName <- as.symbol(paste0(nameC))
    columnNamePresence <- as.symbol(paste0(nameC,x,"Presence"))
    columnNameCount <- as.symbol(paste0(nameC,x,"Count"))
    
    
    #Creation des nouvelles colonnes aggrega
    df <- merge(df, group_by(glob,fullVisitorId) %>% 
                  summarise(!!columnNameCount := sum(!!columnName == val)
                            #!!columnNamePresence := ceiling(sum(!!columnName == val)/(sum(!!columnName == val) + 1))
                  ))
  }
  
  resultat <- df %>% select(-one_of(nameC))
  
  return(resultat)
}



#Fonction qui ne garde que les 20 valeurs les plus frequentes pour une colonnes char, les autre valeurs sont transformees en "nomColonneDivers"
limitCharacter <- function(df, nameC){
  #print(paste("select ",nameC," ,count(*) as freq from df group by ",nameC," order by count(*) desc", sep = " "))
  vecChar<-sqldf(paste("select ",nameC," ,count(*) freq from df group by ",nameC," order by count(*) desc", sep = " "))
  toDelete<-tail(vecChar[[nameC]],n = length(vecChar[[nameC]]) - 20 )
  df[[nameC]][df[[nameC]] %in% toDelete] <- paste(nameC,"Divers")
  return(df)
}


glob = aggregateInteger(glob, "hits")




#Frequences et discrétisations des variables QUALITATIVES (old) ####
sapply(glob, function(x) length(unique(x)))


#networkDomain ###
plot_histo(glob, "networkDomain", 10)

tt = as.data.table(plot_histo(globThumb, "networkDomain", 10))
tmp = as.character(tt$Var1)

glob$networkDomain[!glob$networkDomain %in% tmp & !is.na(glob$networkDomain)] = "Autre"

#Country ###
plot_histo(glob, "country", 10)
plot_histo(globThumb, "country", 10)

tt = as.data.table(plot_histo(globThumb, "country", 10))
tmp = as.character(tt$Var1)

glob$country[!glob$country %in% tmp & !is.na(glob$country)] = "Autre"

# referralPath ###
plot_histo(glob, "referralPath", 10)
plot_histo(globThumb, "referralPath", 10)

tt = as.data.table(plot_histo(globThumb, "referralPath", 2))
tmp = as.character(tt$Var1)

glob$referralPath[!glob$referralPath %in% tmp & !is.na(glob$referralPath)] = "Autre"

#Region ###
plot_histo(glob, "region", 10)
plot_histo(globThumb, "region", 30)

tt = as.data.table(plot_histo(globThumb, "region", 15))
tmp = as.character(tt$Var1)

glob$region[!glob$region %in% tmp & !is.na(glob$region)] = "Autre"

#Source ###
plot_histo(glob, "source", 10)
plot_histo(globThumb, "source", 3)

tt = as.data.table(plot_histo(globThumb, "source", 3))
tmp = as.character(tt$Var1)

glob$source[!glob$source %in% tmp & !is.na(glob$source)] = "Autre"

#city ###
plot_histo(glob, "city", 10)
plot_histo(globThumb, "city", 40)

tt = as.data.table(plot_histo(globThumb, "city", 40))
tmp = as.character(tt$Var1)

glob$city[!glob$city %in% tmp & !is.na(glob$city)] = "Autre"

#keyword ###
plot_histo(glob, "keyword", 2)
plot_histo(globThumb, "keyword", 4)

tt = as.data.table(plot_histo(globThumb, "keyword", 4))
tmp = as.character(tt$Var1)

glob$keyword[!glob$keyword %in% tmp & !is.na(glob$keyword)] = "Autre"





#https://github.com/h2oai/h2o-tutorials/blob/master/tutorials/gbm-randomforest/GBM_RandomForest_Example.R

