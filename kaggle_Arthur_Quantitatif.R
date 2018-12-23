wd = "C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Dev/kaggle_google_revenue"


setwd(wd)
rm(wd)
getwd()


require(jsonlite) 
require(data.table)
require(lubridate)
require(questionr) #Pour un accÃ¨s visuel facile des proportions par variable
require(dummies)
require(dplyr)
require(ggplot2)

library(scales)
library(gridExtra)

# lecture des donnÃ©es ####
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








#Lecture des données (simplifié) ####

glob = fread("../data/glob.csv", stringsAsFactors = F)
#glob = fread("../data/glob.csv", stringsAsFactors = T)


## ### ## ## ### ## ### ## ### ## ### ## 
#Analyse de la donnée, des NA, Remove de certaines colonnes ####
## ### ## ## ### ## ### ## ### ## ### ##

#Explore
str(glob)
summary(glob)

###################
#Feature engineering
###################

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


globThumb = glob[glob$isTransaction == 1]


#Frequences, discrétisation, etc ####



#Fonction de plot frequency
freq_col <- function(dt, col, top){ 
  
  t = table(dt[[col]])
  t = as.data.frame(t)
  tt  <- t[order(t[,2],decreasing=TRUE),]
  tt = tt[1:top,]
  
  ggplot(tt,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity")
  
}


#networkDomain ####
freq_col(glob, "networkDomain", 10)

glob


freq_col(globThumb, "networkDomain", 20)



# Réduction drastique du dataset pour un premier test dummies avec toutes les variables sans prendre trop de place en mémoire####
tmp_non_buyer = glob[glob$logSumTransactionRevenue == 0]
tmp_buyer = glob[glob$logSumTransactionRevenue != 0]

## Travail sur les dates

# Cr�e une var pour les jours de la semaine 
glob$weekdayy <- weekdays(glob$date) 

# Cr�e une var pour les mois de l'a semaine l'ann�e
glob$month <- months(glob$date) 

# Cr�e une var pour les jours de la semaine 
glob$quarter <- quarter(glob$date) 

#Plot les sessions en fonction de factors
plotSessions <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>%
    ggplot(aes_(x=var_col, y=~n, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="number of sessions")+
    theme(legend.position="none")
}

#Plot les transactionRevenue en fonction de factors
plotRevenue <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenues (USD)")+
    theme(legend.position="none")
}

## Plot en bar plot (Need package Scales et GridExtra)
options(repr.plot.height=4)
w1 <- plotSessions(glob, weekdayy)
w2 <- plotRevenue(glob, weekdayy)

w3 <- plotSessions(glob, month)
w4 <- plotRevenue(glob, month)

w5 <- plotSessions(glob, quarter)
w6 <- plotRevenue(glob, quarter)

grid.arrange(w1, w2, w3, w4, w5, w6)
