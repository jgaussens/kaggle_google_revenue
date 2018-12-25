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


#Lecture directe du fichier (plus rapide que la lecture totale puis le retraitement des JSONS) ####
glob = fread("../data/glob.csv", stringsAsFactors = T)
glob = fread("../data/glob.csv", stringsAsFactors = F)

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
freq_col <- function(dt, col, top){ 
  
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

time_series <- function(dt, col, periode){ #peut être ajouter une option de type de graphique selon ce qu'on veut faire (timeseries, histo ou autre)
  
  dt[, `:=` (time_session = .N, var = dt[[col]]), by = periode] #Ajout d'une colonne count qui compte le nombre par mois (faire pareil par jour)
  
  ggplot(dt, aes(x=date, y=time_session)) + geom_line(col='blue') + geom_smooth(col='red') +
    labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
  
}



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

glob = glob[, !c("adContent", "page", "slot", "adNetworkType", "isVideoAd", "gclId", "campaign", "keyword")]

plot_missing(glob) #Recheck des parts des missings


#Retraitement des typages de certaines colonnes 

glob$date = ymd(glob$date)
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

#Passage de variables en int
numVars <- c("hits", "bounces", "pageviews", "newVisits")
glob[, numVars] <- lapply(glob[, ..numVars], as.integer)
rm(numVars)


globThumb = glob[glob$isTransaction == 1]
globThumb = as.data.table(globThumb)

plot_missing(globThumb) #Intéréssant de faire le parallèle entre la part des NA sur Glob et sur globTHumb



# Analyses univariées, de dates, Plots etc####

#TimeSeries
time_series(glob, "region", "date")

## Plot en bar plot (Need package Scales et GridExtra)
options(repr.plot.height=4)
w1 <- plotSessions(glob, weekdayy)
w2 <- plotRevenue(glob, weekdayy)

w3 <- plotSessions(glob, month)
w4 <- plotRevenue(glob, month)

w5 <- plotSessions(glob, quarter)
w6 <- plotRevenue(glob, quarter)

grid.arrange(w1, w2, w3, w4, w5, w6)


#Plot les transactionRevenue en fonction de factors (horizontal)
c1 <- plotSessionsFlip(glob, country, 20)
c2 <- plotRevenueFlip(glob, country, 20)
grid.arrange(c1, c2, nrow=1)


#Frequences et discrétisations des variables QUALITATIVES ####
sapply(glob, function(x) length(unique(x)))


#networkDomain ###
freq_col(glob, "networkDomain", 10)

tt = as.data.table(freq_col(globThumb, "networkDomain", 10))
tmp = as.character(tt$Var1)

glob$networkDomain[!glob$networkDomain %in% tmp] = "Autre"

#Country ###
freq_col(glob, "country", 10)
freq_col(globThumb, "country", 10)

tt = as.data.table(freq_col(globThumb, "country", 10))
tmp = as.character(tt$Var1)

glob$country[!glob$country %in% tmp] = "Autre"

# referralPath ###
freq_col(glob, "referralPath", 10)
freq_col(globThumb, "referralPath", 10)

tt = as.data.table(freq_col(globThumb, "referralPath", 2))
tmp = as.character(tt$Var1)

glob$referralPath[!glob$referralPath %in% tmp] = "Autre"

#Region ###
freq_col(glob, "region", 10)
freq_col(globThumb, "region", 30)

tt = as.data.table(freq_col(globThumb, "region", 15))
tmp = as.character(tt$Var1)

glob$region[!glob$region %in% tmp] = "Autre"

#Source ###
freq_col(glob, "source", 10)
freq_col(globThumb, "source", 3)

tt = as.data.table(freq_col(globThumb, "source", 3))
tmp = as.character(tt$Var1)

glob$source[!glob$source %in% tmp] = "Autre"

#city ###
freq_col(glob, "city", 10)
freq_col(globThumb, "city", 40)

tt = as.data.table(freq_col(globThumb, "city", 40))
tmp = as.character(tt$Var1)

glob$city[!glob$city %in% tmp] = "Autre"








#Feature engineering sur les Dates et périodes ####

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


# Modélisation ####