wd = "C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Dev/kaggle_google_revenue"


setwd(wd)
rm(wd)

require(jsonlite) 
require(data.table)
require(lubridate)
require(questionr) #Pour un accÃ¨s visuel facile des proportions par variable
require(dummies)
require(dplyr)
require(ggplot2)

# lecture des donnÃ©es ####
train<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 



# crÃ©ation d'une colonne indicatrice train test avant assemblage des deux tables ####
train$datasplit<-"train" ; test$datasplit<-"test"
# suppression d'une colonne visiblement inutile
train$campaignCode<-NULL ; test$campaignCode<-NULL
# identification des 4 colonnes au format json
json<-c("trafficSource","totals","geoNetwork","device")
tables<-c("train","test")
glob<-data.table() #table vide qui va rÃ©cupÃ©rer les tableas transformÃ©es
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
rm(partiel, train, test) ; gc()



## ### ## ## ### ## ### ## ### ## ### ## 
#Analyse de la donnÃ©e, des NA, Remove de certaines colonnes ####
## ### ## ## ### ## ### ## ### ## ### ##

#Explore
str(glob)
summary(glob)


#Part des NA
require(DataExplorer)
plot_missing(glob)


#Removing NA Over 95% (except transactionrevenue)
glob = glob[, !c("adContent", "page", "slot", "adNetworkType", "isVideoAd", "gclId")]


#Visualiser le nombre de modalitÃ©s par colonne, et enlever celles Ã  1
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

#Peut Ãªtre pas, je pref garder tel quel en fait (posix), dÃ©commenter si voulu
#glob$visitStartTime <- as.POSIXct(glob$visitStartTime, tz="UTC", origin='1970-01-01')


#Analyse de sessionId, par journÃ©e, par heure, par navigateur fermÃ©? 
tmp = glob[, .N, by="sessionId"] 
tmp

nrow(glob) - nrow(tmp)
tmp[, .N, by="N"]

# 1724/2 personnes qui ont ouvert 2 sessions la mÃªme journÃ©e


#Ajouter le reste des analyses globales






#Traitement de la cible TransactionRevenue, ajout des logs1p, etc ####
glob$transactionRevenue[is.na(glob$transactionRevenue)] <- 0

#Ajout de la colonne log
glob$dollarLogTransactionRevenue = glob$transactionRevenue / 1000000 #Pour plot plus concrets
#Ajout de la colonne de la somme de la transaction revenue, par personne 
tmp = glob[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
glob = merge(glob, tmp, by="fullVisitorId", all.x = TRUE)

glob$logTransactionRevenue = log1p(glob$transactionRevenue)
glob$logSumTransactionRevenue = log1p(glob$sumTransactionRevenue)
