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
require(dummies)
require(h2o)
require(fastDummies)
require(Metrics)
require(DataExplorer)
require(grid)
require(lattice)
require(gridExtra)
require(scales)

# lecture des données ####
train<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("C:/Users/arthu/OneDrive - De Vinci/ESILV/A5/Apprentissage/Kaggle_projet/Data/test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 

train<-read.csv("../data/train.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 
test<-read.csv("../data/test.csv",stringsAsFactors = FALSE,colClasses=c("character","integer","character","character","character","character","character","character","character","integer","integer","integer")) ; 



# création d'une colonne indicatrice train test avant assemblage des deux tables ###
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
glob = fread("../data/glob.csv", stringsAsFactors = F)

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
  print(ggplot(tt,aes(x= reorder(Var1,-Freq),Freq))+geom_bar(stat ="identity", fill="blue") + labs(x = col) )
  
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
    labs(x="", y="Revenues")+
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
    labs(x="", y="Revenues")+
    theme(legend.position="none") +
    scale_x_discrete(limits=y)
}

time_series <- function(dt, col, periode){ #peut être ajouter une option de type de graphique selon ce qu'on veut faire (timeseries, histo ou autre)
  
  dt[, `:=` (time_session = .N, var = dt[[col]]), by = periode] #Ajout d'une colonne count qui compte le nombre par mois (faire pareil par jour)
  
  ggplot(dt, aes(x=date, y=time_session)) + geom_line(col='blue') + geom_smooth(col='red') +
    labs(x="", y="Sessions per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
  
}

time_series_sum <- function(dt, col, periode){ #peut être ajouter une option de type de graphique selon ce qu'on veut faire (timeseries, histo ou autre)
  
  tmp = dt[,(sum(dt[[col]])), by = periode] #Ajout d'une colonne count qui compte le nombre par mois (faire pareil par jour)
  
  ggplot(tmp, aes(x=date, y=V1)) + geom_line(col='blue') + geom_smooth(col='red') +
    labs(x="", y="Revenue per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")
  
}



#####  Analyses des données et prétraitements ######

### --- --- --- Analyses globales et prétraitements --- --- --- ###

#Exploration
str(glob)
summary(glob)


#Visualiser le nombre de modalités par colonne, et enlever celles uniques
sapply(glob, function(x) length(unique(x)))
#Retrait des variables n'ayant qu'une seule modalité
todrop <- names(glob)[which(sapply(glob,uniqueN)<2)]
glob[, (todrop) := NULL]
rm(todrop)


#Traitement des NA
na_replacer(glob) #Fonction pour enlever les NA au global

#Retraitement des typages de dates 
glob$date = ymd(glob$date)
glob$transactionRevenue = as.numeric(glob$transactionRevenue)

glob$visitStartTime = as.POSIXct(glob$visitStartTime, tz="UTC", origin='1970-01-01')

#Passage de variables en int
numVars <- c("hits", "bounces", "pageviews", "newVisits")
glob[, numVars] <- lapply(glob[, ..numVars], as.integer)
rm(numVars)

#Traitement de la cible TransactionRevenue #
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


#Ajout de colonnes temporelles
#Heure
glob$hour = lubridate::hour(glob$visitStartTime)

# Cr?e une var pour les jours de la semaine 
glob$weekdayy <- weekdays(glob$date) 

# Cr?e une var pour les mois de la semaine l'ann?e
glob$month <- months(glob$date) 

# Cr?e une var pour les jours de la semaine 
glob$quarter <- quarter(glob$date) 


#Plot des NA
plot_missing(glob)


### --- --- --- Analyses Univariées --- --- --- ###

#TimeSeries
#Sessions
time_series(glob, "sessionId", "date")
glob = glob[, !c("time_session", "var")] #Variables induites par la fonction, plus nécessaires après

#Revenues
tmp = glob[, sum(transactionRevenue), by="date"]
ggplot(tmp, aes(x=date, y=V1)) + geom_line(col='blue') + geom_smooth(col='red') +
  labs(x="", y="Revenues per Day") + scale_x_date(date_breaks = "1 month", date_labels = "%b %d")


## Plot en bar plot (Need package Scales et GridExtra)
options(repr.plot.height=4)
w1 <- plotSessions(glob, weekdayy)
w2 <- plotRevenue(glob, weekdayy)

w3 <- plotSessions(glob, month)
w4 <- plotRevenue(glob, month)

w5 <- plotSessions(glob, quarter)
w6 <- plotRevenue(glob, quarter)

w7 <- plotSessions(glob, hour)
w8 <- plotRevenue(glob, hour)

grid.arrange(w1, w2, w3, w4)
grid.arrange(w5, w6, w7, w8)


#Plot les transactionRevenue en fonction de factors (horizontal)
c1 <- plotSessionsFlip(glob, country, 20)
c2 <- plotRevenueFlip(glob, country, 20)
grid.arrange(c1, c2, nrow=1)


#Histogrammes ###
sapply(glob, function(x) length(unique(x)))

plot_histo(glob, "networkDomain", 10)
plot_histo(glob, "country", 10)
plot_histo(glob, "referralPath", 10)
plot_histo(glob, "region", 10)
plot_histo(glob, "source", 10)
plot_histo(glob, "city", 10)
plot_histo(glob, "keyword", 10)



#Feature Engineering ####

#Ajout de colonne Soldes
# Création des période de solde au USA psk une grosses partie des clients viennent de la bas

black_friday <- seq(as.Date("2017/11/23"), as.Date("2017/11/27"),"days")
president_day <- seq(as.Date("2017/2/17"), as.Date("2017/2/20"),"days")
memorial_day <- seq(as.Date("2017/5/25"), as.Date("2017/5/29"),"days")
independence_day <- seq(as.Date("2017/6/30"), as.Date("2017/7/4"),"days")
back_to_schoo__labor_day <- seq(as.Date("2017/8/26"), as.Date("2017/9/4"),"days")
colombus_day <- seq(as.Date("2017/10/6"), as.Date("2017/10/9"),"days")
christmas_sales <- seq(as.Date("2017/12/1"), as.Date("2017/12/26"),"days")


tempSalesDate <- as.Date(c(black_friday, president_day ,memorial_day ,independence_day ,back_to_schoo__labor_day
                           ,colombus_day ,christmas_sales))

# Les dates sont calées sur l'année 2017
salesDate<-format(tempSalesDate, format="%m-%d")

rm(tempSalesDate,black_friday, president_day ,memorial_day ,independence_day ,back_to_schoo__labor_day,colombus_day ,christmas_sales)

glob$date_without_year <- format(glob$date, format="%m-%d")

glob$isSalesPeriod <- 0
glob$isSalesPeriod[glob$date_without_year %in% salesDate] = 1

as.data.frame(table(glob$isSalesPeriod))


#Ajout des sommes des variables numériques:
tmp1 = glob[, sum(hits), by="fullVisitorId"]
colnames(tmp1) = c("fullVisitorId","hitsSum")

tmp2 = glob[, sum(pageviews), by="fullVisitorId"]
colnames(tmp2) = c("fullVisitorId","pageViewsSum")

tmpFinal = Reduce(function(x, y) merge(x, y, all=TRUE), list(tmp1, tmp2))

glob = merge(glob, tmpFinal, by = "fullVisitorId", all.x = TRUE)


#Réduction des Modalités #
tt = as.data.table(plot_histo(glob, "networkDomain", 10))
tmp = as.character(tt$Var1)
glob$networkDomain[!glob$networkDomain %in% tmp & !is.na(glob$networkDomain)] = "Autre"

tt = as.data.table(plot_histo(glob, "country", 10))
tmp = as.character(tt$Var1)
glob$country[!glob$country %in% tmp & !is.na(glob$country)] = "Autre"

tt = as.data.table(plot_histo(glob, "referralPath", 10))
tmp = as.character(tt$Var1)
glob$referralPath[!glob$referralPath %in% tmp & !is.na(glob$referralPath)] = "Autre"

tt = as.data.table(plot_histo(glob, "region", 10))
tmp = as.character(tt$Var1)
glob$region[!glob$region %in% tmp & !is.na(glob$region)] = "Autre"

tt = as.data.table(plot_histo(glob, "source", 10))
tmp = as.character(tt$Var1)
glob$source[!glob$source %in% tmp & !is.na(glob$source)] = "Autre"

tt = as.data.table(plot_histo(glob, "city", 10))
tmp = as.character(tt$Var1)
glob$city[!glob$city %in% tmp & !is.na(glob$city)] = "Autre"

tt = as.data.table(plot_histo(glob, "keyword", 10))
tmp = as.character(tt$Var1)
glob$keyword[!glob$keyword %in% tmp & !is.na(glob$keyword)] = "Autre"



# Modélisation ####

#Section Régression directe sur transactionRevenue####

h2o.init(min_mem_size = "8G")

#glob = fread("../data/glob_final.csv") #Pour accès plus rapide aux données

#Passage en factors
glob <- glob %>%
  mutate_if(is.character, factor)

glob$isTransaction = as.factor(glob$isTransaction)
glob$isSalesPeriod = as.factor(glob$isSalesPeriod)
glob$quarter = as.factor(glob$quarter)
glob$hour = as.factor(glob$hour)

#Date
glob$date = as.Date(glob$date)


#Remove de certaines colonnes
glob = as.data.table(glob)
glob=glob[, -c("gclId","visitStartTime", "datasplit", "visitId", "sessionId","dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue")]


#glob$logSumTransactionRevenue = as.numeric(glob$logSumTransactionRevenue)

glob <- glob %>% select(isOnceTransaction,everything()) 
glob <- glob %>% select(fullVisitorId,everything()) 
glob <- glob %>% select(isTransaction,everything()) 
glob <- glob %>% select(transactionRevenue,everything()) 


train = glob[glob$date < as.Date("2017-04-01")]
valid = glob[glob$date > as.Date("2017-04-01")]

train = train[, -c("date")]
valid = valid[, -c("date")]
train = as.h2o(train)
valid = as.h2o(valid)


model_params1 <- list(max_depth = seq(50, 200, 50)
                      ,ntrees = 150
                      , learn_rate = seq(0.01, 0.1, 0.04)
                      ,mtries = seq(1,41,5))


search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 200,seed = 1234)




system.time(
  model_grid3 <- h2o.grid("xgboost", x = c(5:ncol(glob)), y = 1,
                        grid_id = "model_grid3"
                        ,training_frame = train
                        ,validation_frame = valid
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = model_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="hist"
                        ,grow_policy="lossguide"
  )
)

model_gridperf3 <- h2o.getGrid(grid_id = "model_grid3", 
                             sort_by = "rmse", 
                             decreasing = F)

print(model_gridperf3) 


best_model_model_id <- model_gridperf3@model_ids[[1]]
best_model <- h2o.getModel(best_model_model_id)
best_model

#Agrégat puis calcul du RMSE#

t2 =predict(best_model, valid)
t2 = as.data.table(t2)
t2[, .N, by="predict"]

v = as.data.table(valid)
final = cbind(v, t2$predict) #régression
final

#ajout de logSumtransactionRevenue
tmp = final[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)


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



#Section Classification de isTransaction puis Régression transactionRevenue ####
#glob = fread("../data/glob_final.csv") #Pour accès plus rapide aux données

#Typages des colonnes#
#Passage en factors
glob <- glob %>%
  mutate_if(is.character, factor)

glob$isTransaction = as.factor(glob$isTransaction)
glob$isSalesPeriod = as.factor(glob$isSalesPeriod)
glob$quarter = as.factor(glob$quarter)
glob$hour = as.factor(glob$hour)

#Date
glob$date = as.Date(glob$date)

#Reorder 
glob <- glob %>% select(isOnceTransaction,everything()) 
glob <- glob %>% select(fullVisitorId,everything()) 
glob <- glob %>% select(transactionRevenue,everything()) 
glob <- glob %>% select(isTransaction,everything()) 

#Remove de certaines colonnes
glob = as.data.table(glob)
glob=glob[, -c("gclId", "visitStartTime", "datasplit", "visitId", "sessionId","dollarLogTransactionRevenue", "logSumTransactionRevenue", "sumTransactionRevenue","logTransactionRevenue")]

#Free Memory
gc()

# H2O #
#Initialisation du serveur h2o
h2o.init(min_mem_size = "8G")


#Jeux de train et de test
train = as.h2o(glob[glob$date < as.Date("2017-04-01")])
valid = as.h2o(glob[glob$date > as.Date("2017-04-01")])


#Paramètres de la grid Search
model_params1 <- list(max_depth = seq(50, 200, 50)
                    ,ntrees = 150
                    , learn_rate = seq(0.01, 0.1, 0.03))
                    #, stopping_rounds = 1000)


search_criteria2 <- list(strategy = "RandomDiscrete", 
                         max_models = 100,seed = 1234)


# Train and validate a grid of GBMs
system.time(
  model_grid1 <- h2o.grid("xgboost", x = c(5:ncol(glob)), y = 1,
                        grid_id = "model_grid1"
                        ,training_frame = train
                        ,validation_frame = valid
                        #,nfolds = 5
                        #,keep_cross_validation_predictions = T
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = model_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="hist"
                        ,grow_policy="lossguide"
  )
)



# Résultats de la grid, ordonnés par AUC Décroissants
model_gridperf1 <- h2o.getGrid(grid_id = "model_grid1", 
                             sort_by = "auc", 
                             decreasing = T)

print(model_gridperf1) 


#Changer la valeur entre crochets pour parcourir les modèles et regarder leurs métriques. Ici, sélection arbitraire après parcours de chaque modèle
best_model_model_id <- model_gridperf1@model_ids[[1]] #Récupérer le modèle 1
best_model <- h2o.getModel(best_model_model_id)
best_model

#métriques
h2o.confusionMatrix(best_model, valid = T) 
h2o.gainsLift(best_model)
h2o.gainsLift(best_model, valid = T)

#best_model = h2o::h2o.loadModel("../data/h2o_best_model_classif/model_grid1_model_0")
#Récupération des prédictions, pour plus tard
t =predict(best_model, valid)
t = as.data.table(t)
t[, .N, by="predict"]

gc()

#Regression 
#Train et test
globThumb = as.data.table(train)
globThumb = globThumb[globThumb$isTransaction == 1] #Pour train uniquement sur les visites à achats
train2 = globThumb

test2 = as.data.table(valid)
test2

#On prédit maintenant la transactionRevenue
glob <- glob %>% select(transactionRevenue,everything()) 
train2 <- train2 %>% select(transactionRevenue,everything()) 
test2 <- test2 %>% select(transactionRevenue,everything()) 


#Posixc not supported
train2 = train2[, !c("date")]
test2 = test2[, !c("date")]


train2 = as.h2o(train2)
test2 = as.h2o(test2)

gc()
system.time(
  model_grid2 <- h2o.grid("xgboost", x = c(5:ncol(glob)), y = 1,
                        grid_id = "model_grid3"
                        ,training_frame = train2
                        ,validation_frame = test2
                        #,balance_classes = T
                        ,seed = 1234
                        ,hyper_params = model_params1
                        ,search_criteria = search_criteria2
                        ,tree_method="hist"
                        ,grow_policy="lossguide"
  )
)


model_gridperf3 <- h2o.getGrid(grid_id = "model_grid3", 
                             sort_by = "rmse", 
                             decreasing = F)

print(model_gridperf3) 


best_model_model_id <- model_gridperf3@model_ids[[1]]
best_model <- h2o.getModel(best_model_model_id)
best_model


#Agrégat + Calcul du RMSE#

t2 =predict(best_model, test2)
t2 = as.data.table(t2)
t[, .N, by="predict"]

v = as.data.table(valid)
final = cbind(v, t$predict, t2$predict) #classif + régression

final

final$y_transactionRevenue = final$V3
final$y_transactionRevenue[final$V2 == 0] = 0 #Les transactions classifiées en 0 voient leur revenu passé à 0


#ajout de logSumtransactionRevenue
tmp = final[, sum(transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)

final$y_transactionRevenue[final$y_transactionRevenue < 0] = 0 #Les revenus prédits en négatifs passent à 0

final$logTransactionRevenue = log1p(final$transactionRevenue)
final$logSumTransactionRevenue = log1p(final$sumTransactionRevenue)

#Ajout de predictedLogSumTransactionRevenue
tmp = final[, sum(y_transactionRevenue), by="fullVisitorId"]
colnames(tmp) = c("fullVisitorId", "y_sumTransactionRevenue")
final = merge(final, tmp, by="fullVisitorId", all.x = TRUE)

final$y_logTransactionRevenue = log1p(final$y_transactionRevenue)
final$y_logSumTransactionRevenue = log1p(final$y_sumTransactionRevenue)

#RMSE FINAL
rmse(final$logSumTransactionRevenue, final$y_logSumTransactionRevenue)



#Autres pistes explorées ####
#Fonction d'aggrégation d'integers en data table pour tout grouper par identifiant
aggregateInteger <- function (dt, col){
  
  tmp1 = dt[, sum(eval(col)), by="fullVisitorId"]
  colnames(tmp1) = c("fullVisitorId",paste0(col,"Sum"))
  
  tmp2 = dt[, mean(eval(col)), by="fullVisitorId"]
  colnames(tmp2) = c("fullVisitorId",paste0(col,"Mean"))
  
  tmp3 = dt[, median(eval(col)), by="fullVisitorId"]
  colnames(tmp3) = c("fullVisitorId",paste0(col,"Median"))
  
  tmp4 = dt[, var(eval(col)), by="fullVisitorId"]
  colnames(tmp4) = c("fullVisitorId",paste0(col,"Variance"))
  
  tmp5 = dt[, sd(eval(col)), by="fullVisitorId"]
  colnames(tmp5) = c("fullVisitorId",paste0(col,"Sd"))
  
  tmpFinal = Reduce(function(x, y) merge(x, y, all=TRUE), list(tmp1, tmp2, tmp3, tmp4, tmp5))
  
  dt = merge(dt, tmpFinal, by = "fullVisitorId", all.x = TRUE)
  
  return(dt)
  
}

aggregateInteger(glob, quote(hits)) #Ne marche pas !

