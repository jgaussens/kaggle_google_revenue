wd = "C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Dev/kaggle_google_revenue"
wd = "~/Google Drive/A5/Data Science/Apprentissage/Kaggle/kaggle_google_revenue/"


setwd(wd)
rm(wd)
getwd()

require(jsonlite) 
require(data.table)
require(lubridate)
require(questionr) #Pour un accès visuel facile des proportions par variable
require(dummies)
require(dplyr)
require(ggplot2)

# lecture des données ####
train<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 



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
rm(partiel, train, test) ; gc()

## ### ## ## ### ## ### ## ### ## ### ## 


#Lecture directe du fichier (plus rapide que la lecture totale puis le retraitement des JSONS) ####
glob = fread("../data/glob.csv", stringsAsFactors = T)
glob = fread("../data/glob.csv", stringsAsFactors = F)



#Traitement de la cible TransactionRevenue, ajout des logs1p pour plus de clarté ####
glob$transactionRevenue[is.na(glob$transactionRevenue)] <- 0

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



#Remove des NA, Remove et retypage de certaines colonnes ####
## ### ## ## ### ## ### ## ### ## ### ##

#Explore
str(glob)
summary(glob)

#Plot des NA
require(DataExplorer)
plot_missing(glob)

#On supprime les NA quand NA% > 70% (except transactionrevenue)
glob = glob[, !c("adContent", "page", "slot", "adNetworkType", "isVideoAd", "gclId")]

#Visualiser le nombre d'occurence d'une variable, si elle est fixe (toujours la meme variable) on supprime
sapply(glob, function(x) length(unique(x)))

todrop <- names(glob)[which(sapply(glob,uniqueN)<2)]
glob[, (todrop) := NULL]
rm(todrop)

plot_missing(glob) #Recheck des NA

#Retraitement des typages de certaines colonnes 

glob$date = ymd(glob$date)
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

#Passage de variables en int
numVars <- c("hits", "bounces", "pageviews", "newVisits")
glob[, numVars] <- lapply(glob[, ..numVars], as.integer)
rm(numVars)

#Que faire des 'not available in demo dataset'



# Analyses en tout genre en vue d'une future discrétisation####

#Ajouter le reste des analyses globales

#Analyse de sessionId, par journ?e, par heure, par navigateur ferm?e ? 
tmp = glob[, .N, by="sessionId"] 
tmp

nrow(glob) - nrow(tmp)
tmp[, .N, by="N"]



# Fonction d'histogramme 




# Discrétisation des variables nécessaires (pour réduire les grosses dimensionnalités) ####





# Test d'un premier Modèle Pour voir si il y a du jus ####




