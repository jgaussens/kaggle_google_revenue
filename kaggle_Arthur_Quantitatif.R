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

require(scales)
require(gridExtra)

# lecture des donnÃƒÂ©es ####
train<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 

saveRDS(glob, file="glob_data.Rda")
glob <- readRDS(file="glob_data.Rda")

fwrite(glob, "glob.csv")

#Lecture directe du fichier (plus rapide que la lecture totale puis le retraitement des JSONS) ####
glob = fread("glob.csv", stringsAsFactors = T)


# crÃ©ation d'une colonne indicatrice train test avant assemblage des deux tables ####
train$datasplit<-"train"
# suppression d'une colonne visiblement inutile
train$campaignCode<-NULL ; 
# identification des 4 colonnes au format json
json<-c("trafficSource","totals","geoNetwork","device")
tables<-c("train")
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
rm(partiel, train, result, temp) ; gc()



###################
#Fonctions
###################

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
plotSessions <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% count(!!var_col) %>% top_n(topN, wt=n) %>%
    ggplot(aes_(x=var_col, y=~n, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Nombre de sessions")+
    theme(legend.position="none")
}

#Plot les transactionRevenue en fonction de factors (vertical)
plotRevenue <- function(dataframe, factorVariable, topN=10) {
  var_col <- enquo(factorVariable)
  dataframe %>% group_by(!!var_col) %>% summarize(rev=sum(transactionRevenue)) %>% filter(rev>0) %>% top_n(topN, wt=rev) %>% ungroup() %>%
    ggplot(aes_(x=var_col, y=~rev, fill=var_col)) +
    geom_bar(stat='identity')+
    scale_y_continuous(labels=comma)+
    labs(x="", y="Revenus (USD)")+
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
    labs(x="", y="Nombre de sessions")+
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
    labs(x="", y="Revenus (USD)")+
    theme(legend.position="none") +
    scale_x_discrete(limits=y)
}

time_series <- function(dt, col, periode){ #peut Ãªtre ajouter une option de type de graphique selon ce qu'on veut faire (timeseries, histo ou autre)
  
  dt[, `:=` (time_session = .N, var = dt[[col]]), by = periode] #Ajout d'une colonne count qui compte le nombre par mois (faire pareil par jour)
  
  ggplot(dt, aes(x=date, y=time_session)) + geom_line(col='blue') + geom_smooth(col='red') +
    labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
  
}


#Explore
str(glob)
summary(glob)

###################
#Feature engineering
###################

#Traitement de la cible TransactionRevenue, ajout des logs1p pour plus de clartÃ© ####
glob$transactionRevenue[is.na(glob$transactionRevenue)] <- 0
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

#Ajout de la colonne log
glob$dollarLogTransactionRevenue = glob$transactionRevenue / 1000000 #Pour plot plus concrets
#Ajout de la colonne de la somme de la transaction revenue, par personne 
tmp = glob[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
glob = merge(glob, tmp, by="fullVisitorId", all.x = TRUE)
rm(tmp)

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

#Visualiser le nombre de modalitÃ©s par colonne, et enlever celles uniques
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
glob = glob[, !c("keyword")]


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

plot_missing(globThumb) #IntÃ©rÃ©ssant de faire le parallÃ¨le entre la part des NA sur Glob et sur globTHumb




#Frequences et discrÃ©tisations des variables QUALITATIVES ####
sapply(glob, function(x) length(unique(x)))


#networkDomain ###
freq_col(glob, "networkDomain", 10)

tt = as.data.table(freq_col(globThumb, "networkDomain", 10))
tmp = as.character(tt$Var1)

glob$networkDomain[!glob$networkDomain %in% tmp & !is.na(glob$networkDomain)] = "Autre"

#Country ###
freq_col(glob, "country", 12)
freq_col(globThumb, "country", 10)

tt = as.data.table(freq_col(globThumb, "country", 10))
tmp = as.character(tt$Var1)

glob$country[!glob$country %in% tmp & !is.na(glob$country)] = "Autre"

# referralPath ###
freq_col(glob, "referralPath", 10)
freq_col(globThumb, "referralPath", 10)

tt = as.data.table(freq_col(globThumb, "referralPath", 2))
tmp = as.character(tt$Var1)

glob$referralPath[!glob$referralPath %in% tmp & !is.na(glob$referralPath)] = "Autre"

#Region ###
freq_col(glob, "region", 10)
freq_col(globThumb, "region", 30)

tt = as.data.table(freq_col(globThumb, "region", 15))
tmp = as.character(tt$Var1)

glob$region[!glob$region %in% tmp & !is.na(glob$region)] = "Autre"

#Source ###
freq_col(glob, "source", 10)
freq_col(globThumb, "source", 3)

tt = as.data.table(freq_col(globThumb, "source", 3))
tmp = as.character(tt$Var1)

glob$source[!glob$source %in% tmp & !is.na(glob$source)] = "Autre"

#city ###
freq_col(glob, "city", 10)
freq_col(globThumb, "city", 40)

tt = as.data.table(freq_col(globThumb, "city", 40))
tmp = as.character(tt$Var1)

glob$city[!glob$city %in% tmp & !is.na(glob$city)] = "Autre"

#keyword ###
freq_col(glob, "keyword", 2)
freq_col(globThumb, "keyword", 4)

tt = as.data.table(freq_col(globThumb, "keyword", 4))
tmp = as.character(tt$Var1)

glob$keyword[!glob$keyword %in% tmp & !is.na(glob$keyword)] = "Autre"

### Travail sur les dates ###

# Crée une var pour les jours de la semaine 
glob$weekdayy <- weekdays(glob$date) 

# Crée une var pour les mois de l'a semaine l'année
glob$month <- months(glob$date) 

# Crée une var pour les jours de la semaine 
glob$quarter <- quarter(glob$date) 

glob$visitStartTime = as.POSIXct(glob$visitStartTime, tz="UTC", origin='1970-01-01')
glob$hour = lubridate::hour(glob$visitStartTime)

## Plot en bar plot (Need package Scales et GridExtra)
options(repr.plot.height=4)
w1 <- plotSessions(glob, weekdayy)
w2 <- plotRevenue(glob, weekdayy)

w3 <- plotSessions(glob, month)
w4 <- plotRevenue(glob, month)

w5 <- plotSessions(glob, quarter)
w6 <- plotRevenue(glob, quarter)

w7 <- plotSessions(glob, ratioPageVisiteNumber)
w8 <- plotRevenue(glob, ratioPageVisiteNumber)

grid.arrange(w7, w8, w1, w1)

glob$ratioPageHits <-glob$pageviews/glob$hits


c1 <- plotSessionsFlip(glob, region, 20)
c2 <- plotRevenueFlip(glob, region, 20)
grid.arrange(c1, c2, nrow=1)


plot_missing(glob)

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

plot_missing(glob)

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

a <- length(which(glob$isTransaction == 1))
b <- length(which(glob$isSalesPeriod == 1 & glob$isTransaction == 1))
a <- length(glob$isSalesPeriod)
bb <- length(which(glob$isSalesPeriod == 1))
100*b/a


#Retraitement var jules
glob$quarter = as.factor(glob$quarter)
glob$hour = as.factor(glob$hour)
glob$isSalesPeriod = as.factor(glob$isSalesPeriod)


# Réduction drastique du dataset pour un premier test dummies avec toutes les variables sans prendre trop de place en mémoire####
tmp_non_buyer = glob[glob$logSumTransactionRevenue == 0]
tmp_buyer = glob[glob$logSumTransactionRevenue != 0]

set.seed(1234) #
#on en prend que 10% de la population des non acheteurs (à changer selon ce qu'on veut)
tmp_non_buyer = tmp_non_buyer[sample(NROW(tmp_non_buyer), NROW(tmp_non_buyer)*(1 - 0.90)),]


glob = rbind(tmp_non_buyer, tmp_buyer)

###############
#Modèlisation
###############

plot_missing(glob)


require(randomForest)
require(caTools)

set.seed(1)


fit <- randomForest(glob$isTransaction ~ ., data = glob)

print(fit)

sapply(glob, is.factor)
