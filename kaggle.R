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

#Peut être pas, je pref garder tel quel en fait (posix), décommenter si voulu
#glob$visitStartTime <- as.POSIXct(glob$visitStartTime, tz="UTC", origin='1970-01-01')


#Analyse de sessionId, par journée, par heure, par navigateur fermé? 
tmp = glob[, .N, by="sessionId"] 
tmp

nrow(glob) - nrow(tmp)
tmp[, .N, by="N"]

# 1724/2 personnes qui ont ouvert 2 sessions la même journée


#Ajouter le reste des analyses globales






#Traitement de la cible TransactionRevenue ####
glob$transactionRevenue[is.na(glob$transactionRevenue)] <- 0

#Ajout de la colonne log
glob$logTransactionRevenue = log(glob$transactionRevenue)
glob$dollarLogTransactionRevenue = glob$transactionRevenue / 1000000 #Pour plot plus concrets

#test dun truc mais ballec ####
#aggregate by fullvisitorid mais à faire avec log et pas mean: pour log ça me met toutes les lignes pcq 

glob$transactionRevenue[glob$fullVisitorId == "2805355288930230044"]


log(sum(glob$transactionRevenue[glob$fullVisitorId == "2805355288930230044"], na.rm = TRUE))
#log de sum marche, mais est-ce que c'est ça qu'on veut???
#Ici





# Analyse univariée ####


#TimeSeries

#day
glob$month_year = format(as.Date(glob$date), "%Y-%m") #Ajout de la colonne mois

#fonction
time_series <- function(dt, col, periode){ #peut être ajouter une option de type de graphique selon ce qu'on veut faire (timeseries, histo ou autre)
  
  dt[, `:=` (time_session = .N, var = dt[[col]]), by = periode] #Ajout d'une colonne count qui compte le nombre par mois (faire pareil par jour)
  
  ggplot(dt, aes(x=date, y=time_session)) + geom_line(col='blue') + geom_smooth(col='red') +
    labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
  
}

#spécifier glob, la variable qu'on veut plotter entre guillements, et "month_year" si on veut par mois, "date" si on veut par jour
time_series(glob, "sessionId", "date")





#Distribution dollartransactionrevenue ( = transactionRevenue passé en dollar)


#Check des proportions de transactionrevenue en fonction de différentes variables


#Test d'un premier modèle sans features engineering ####





#les variables à enlever: [dollarLogTransactionRevenue, ]

# Questions####
#Group by visitorid, pour log prediction: quelle ligne garder? celle avec la transaction?








#bib de fonctions ####


#Traitement des NA ####



  #Traitement des NA, si besoin:
  
  # Étant donné que 30 variables only, on peut regarder au cas par cas. On aurait pu faire un PCA mais je pense qu'on peut se permettre de regarder vu que 30 variables seulement
  

  #Exploration des données NA cas par cas et Analyse du nombre de modalités différentes avant choix de features engineerer ou non 
  
  unique(glob$isTrueDirect) #On peut considérer que si la ligne est à NA, elle est false, donc le nobmre de NA n'est pas très grave. Remplacer les NA par FALSE?
  
  unique(glob$referralPath) #urls, dans l'immédiat je vois pas comment traiter les NA, je laisse
  
  unique(glob$keyword)
  
  unique(glob$bounces) #1 ou NA
  
  unique(glob$newVisits) #1 ou NA
  

#https://www.kaggle.com/c/ga-customer-revenue-prediction#evaluation

#Analyse et exploration des données, histogrammes en tous genres etc ####
  

  
#foret rpart ####


#random forest ####


#régression ####
  
  
#gbm ####

#xgboost ####

