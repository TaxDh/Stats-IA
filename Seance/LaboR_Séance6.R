#######################################
# Laboratoire R - Séance 6
# 21 février 2024
# Foret aleatoire, importance de variable
# Librairie de données actuarielles
#######################################


# Librairie CASdatasets contient des données actuarielle, librairie UQAM (prof. Charpentier)
# you need xts and sp 
install.packages('xts')
install.packages('sp')
install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/", type="source")
library(CASdatasets)
data(freMPL2)
head(freMPL2)
source('Fonctions.r')


hist(freMPL2$ClaimAmount)
mean(freMPL2$ClaimAmount)

#Gardons seulement les ligne tel qu'une réclamation est faite
#On peut avori un vecteur TRUE/FALSE 
freMPL2$ClaimAmount > 0

#On garde les lignes avec TRUE
Data = freMPL2[freMPL2$ClaimAmount > 0,]#juste les lignes qui il y a eu une réclamation
hist(Data$ClaimAmount)
mean(Data$ClaimAmount)
#Honnettement, pas très bon pour une RF, drole de distribution de données.

head(Data)

#c'est un packages qui fait des forêts aléatoires
install.packages('randomForest')
library(randomForest)
?randomForest


allData <- train_test_split(0.8,Data)
ent_data <- allData[[1]]
tes_data <- allData[[2]]


Data$Gender
rf <- randomForest(ClaimAmount~LicAge+VehAge+Gender+MariStat+SocioCateg+VehUsage,data=ent_data)
pred <- predict(rf,tes_data)#?predict, tres mauvaise documentation sur la fonction predict
eqm <- EQM(pred,tes_data$ClaimAmount)
importance(rf,type=1)
importance(rf,type=2)
eqm

rf <- randomForest(ClaimAmount~LicAge+VehAge+Gender+MariStat+SocioCateg+VehUsage,data=ent_data, importance=TRUE)
importance(rf,type=1)
importance(rf,type=2)

plot(rf)


#Importer et préparer les données
Data <- read.table('wine.data')
head(Data)
dim(Data)

Data <- read.table('wine.data',sep=',')
colnames(Data) <-c('Cultivars','Alcohol','Malicacid','Ash','Alcalinity', 'Magnesium',
                   'Phenols','Flavanoids', 'Nonflavanoidphenols','Proanthocyanins',
                   'Colorintensity', 'Hue', 'ODtwo','Proline')

head(Data)
dim(Data)
class(Data$Cultivars)

#oooppsss changeons ça!
Data$Cultivars <- factor(Data$Cultivars)

allData <- train_test_split(0.8,Data)
ent_data <- allData[[1]]
tes_data <- allData[[2]]

p = ncol(ent_data)-1

?randomForest

rf <- randomForest(Cultivars~.,data=ent_data,importance=TRUE, ntree=1000,mtry=p, replace=FALSE)
pred <- predict(rf,tes_data)
Precision <- Prec(pred,tes_data$Cultivars)
# On va coder ça ensemble!

importance(rf,type=1)
importance(rf,type=2)

plot(rf)

# randomForestSRC est un peu le nouveau package qui remplace randomForest
# Ishwaran s'en occupe!
library(randomForestSRC)
rf <- rfsrc(Cultivars~.,data=ent_data)
VI <- holdout.vimp(Cultivars~.,data=ent_data, ntree=10000)
VI$importance

