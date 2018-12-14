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
rm(partiel, train, test) ; gc()
