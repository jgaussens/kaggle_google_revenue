wd = "C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Dev/kaggle_google_revenue"


setwd(wd)
rm(wd)
getwd()


require(jsonlite) 
require(data.table)
require(lubridate)
require(questionr) #Pour un accÃƒÂ¨s visuel facile des proportions par variable
require(dummies)
require(dplyr)
require(ggplot2)

library(scales)
library(gridExtra)

# lecture des donnÃƒÂ©es ####
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








#Lecture des donnÃ©es (simplifiÃ©) ####

glob = fread("../data/glob.csv", stringsAsFactors = F)



## ### ## ## ### ## ### ## ### ## ### ## 
#Analyse de la donnÃ©e, des NA, Remove de certaines colonnes ####
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




#Traitement de la cible TransactionRevenue, ajout des logs1p pour plus de clartÃ© ####
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


#Frequences, discrÃ©tisation, etc ####



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



# RÃ©duction drastique du dataset pour un premier test dummies avec toutes les variables sans prendre trop de place en mÃ©moire####
tmp_non_buyer = glob[glob$logSumTransactionRevenue == 0]
tmp_buyer = glob[glob$logSumTransactionRevenue != 0]

########################
## Travail sur les dates
########################

# Crée une var pour les jours de la semaine 
glob$weekdayy <- weekdays(glob$date) 

# Crée une var pour les mois de l'a semaine l'année
glob$month <- months(glob$date) 

# Crée une var pour les jours de la semaine 
glob$quarter <- quarter(glob$date) 

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

## Plot en bar plot (Need package Scales et GridExtra)
options(repr.plot.height=4)
w1 <- plotSessions(glob, weekdayy)
w2 <- plotRevenue(glob, weekdayy)

w3 <- plotSessions(glob, month)
w4 <- plotRevenue(glob, month)

w5 <- plotSessions(glob, quarter)
w6 <- plotRevenue(glob, quarter)

grid.arrange(w1, w2, w3, w4, w5, w6)

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

c1 <- plotSessionsFlip(glob, country, 20)
c2 <- plotRevenueFlip(glob, country, 20)
grid.arrange(c1, c2, nrow=1)


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

plot_missing(glob)

glob <- na_replacer(glob)

# On remplace les NA avec leurs "vrais valeurs"

glob$pageviews[is.na(glob$pageviews)] <- 1

glob$newVisits[is.na(glob$newVisits)] <- 0

glob$bounces[is.na(glob$bounces)] <- 0

glob$isTrueDirect[is.na(glob$isTrueDirect)] <- FALSE

# A partir de la peut etre c'est mieux de laisser juste en NA

glob$country[is.na(glob$country)] <- "Unknown"

# Pour tous les NA qui dépendent des pays on pourrait remettre les pays sinon
glob$subContinent[is.na(glob$subContinent)] <- "Unknown"

glob$continent[is.na(glob$continent)] <- "Unknown"

glob$operatingSystem[is.na(glob$operatingSystem)] <- "Unknown"

# Askip : Direct: Source exactly matches direct AND Medium exactly matches (not set) OR Medium exactly matches (none)
# Donc est ce que les NA on met "Direct" ?
 # glob$medium[is.na(glob$medium)] <- "Direct" 

# Création des période de solde au USA psk une grosses partie des clients viennent de la bas

black_friday <- seq(as.Date("2017/11/23"), as.Date("2017/11/27"),"days")
president_day <- seq(as.Date("2017/2/17"), as.Date("2017/2/20"),"days")
memorial_day <- seq(as.Date("2017/5/25"), as.Date("2017/5/29"),"days")
independence_day <- seq(as.Date("2017/6/30"), as.Date("2017/7/4"),"days")
back_to_schoo__labor_day <- seq(as.Date("2017/8/26"), as.Date("2017/9/4"),"days")
colombus_day <- seq(as.Date("2017/10/6"), as.Date("2017/10/9"),"days")
christmas_sales <- seq(as.Date("2017/12/1"), as.Date("2017/12/26"),"days")
# Idée rajouter en plus les début/fin d'année psk c'est des goodies qu'on offre aux étudiant et plus fin de graduation = solde et achats
# Aussi saint valentin + fete des meres toussa


tempSalesDate <- as.Date(c(black_friday, president_day ,memorial_day ,independence_day ,back_to_schoo__labor_day
                           ,colombus_day ,christmas_sales))

# Problème chaque année certaines dates ne sont pas les meme donc pour l'instant on s'en fout on enleve la date mais du coup on aura pas 100% de accuracy 
# Les dates sont calé sur l'année 2017
salesDate<-format(tempSalesDate, format="%m-%d")

rm(tempSalesDate,black_friday, president_day ,memorial_day ,independence_day ,back_to_schoo__labor_day,colombus_day ,christmas_sales)

glob$date_without_year <- format(glob$date, format="%m-%d")

glob$isSalesPeriod <- 0
glob$isSalesPeriod[glob$date_without_year %in% salesDate] = 1

as.data.frame(table(glob$isSalesPeriod))

length(which(glob$month == "juillet" & glob$isTransaction == 1 ))
