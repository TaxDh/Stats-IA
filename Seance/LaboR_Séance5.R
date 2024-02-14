#######################################
# Laboratoire Séance #5
# 14 février 2024
# Arbre de décision
# Importer des données sur internet
#######################################

library("readxl")
#On se fait un repo et on y travail
#setwd("C")
#Parlons vin! https://archive.ics.uci.edu/ml/datasets/Wine
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

#oooppsss changeons ça! On ne veut que ce soit une variable catégorielle
Data$Cultivars <- factor(Data$Cultivars)


#Fonction pour diviser le data en training et test:
train_test_split <- function(prop,data) {
  id <- sample(nrow(data))
  nent <- floor(prop*nrow(data))
  ent_data <- data[id[1:nent],]
  tes_data <- data[id[(nent+1):nrow(data)],]
  
  return(list(ent_data,tes_data))
}

allData <- train_test_split(0.8,Data)
ent_data <- allData[[1]]
tes_data <- allData[[2]]

tes_data$Cultivars

library(tree) 
# Package employé les labos R du livre.
# Plus d'information: https://cran.r-project.org/web/packages/tree/tree.pdf
colnames(ent_data)
fit <- tree(Cultivars~.,ent_data)#Y en fonction de tout le reste (~.)

#Un tit peu d'information rapide
summary(fit)
#nous donnes l'arbre
fit

#imprimer l'arbre
plot(fit)
text(fit , pretty = 0)

#Prédiction
preds <- predict(fit,tes_data)

#regardons ça ressemble à quoi
preds

preds <- factor(apply(preds,1,which.max))#apply va prendre l'operation sur les lignes ou sur les colonnes
preds
#Précision
DTPrec = sum(tes_data$Cultivars == preds)/nrow(tes_data)
DTPrec

#Emmondage
cv <- cv.tree(fit , FUN = prune.misclass)

#size est le nombre de région (feuilles), dev nb d'obs mal classé
#k est le paramètre lambda
cv#7 dans mon abre le plus riche, 14 hétéreogénité, ...
par(mfrow = c(1, 2))
plot(cv$size , cv$dev, type = "b")
plot(cv$k, cv$dev, type = "b")

#Maintenant choisissons l'arbre déterminé par validation croisé
fit.prune <- prune.misclass(fit, best = 5)
par(mfrow = c(1, 1))
plot(fit.prune)
text(fit.prune , pretty = 0)


#Prédiction
preds <- predict(fit.prune,tes_data)
preds <- factor(apply(preds,1,which.max))
#Précision
PrunePrec = sum(tes_data$Cultivars == preds)/nrow(tes_data)

DTPrec
PrunePrec

#Pas vraiment de différence pour la régression, au prochain lab (RF) 
#on fra de la régression!


# Moi j'aime mieux rpart, voici commment tout refaire avec ce package:  
library(rpart)
# Rpart construit des arbres simples et interpretable.
# Plus d'information: https://cran.r-project.org/web/packages/rpart/rpart.pdf
# rpart est le package original de 1989 qui utilise la m?thode simple pr?sent? aujourd'hui: partitionnement binaire r?cursif.
# Le package nous permet de construire des arbres et des les visualiser.


fit <- rpart(Cultivars~., data=ent_data)
fit
plot(fit)
?rpart

fit <- rpart(Cultivars~., data=ent_data,minsplit=20,cp=0, method = "class")
fit
par(mfrow=c(1,2))
par(xpd = TRUE)
plot(fit, compress = TRUE)
text(fit, use.n = TRUE)
# Stable ?!?!?

#Prédiction
preds <- predict(fit,tes_data)
preds <- factor(apply(preds,1,which.max))
#Précision
sum(Test$Cultivars == preds)/nrow(tes_data)


# Un data set plus gros
Data <- read_excel('energy.xlsx')
head(Data)
dim(Data)

n <- nrow(Data)
n_train <- floor(n*0.8)
n_test <- n - n_train
ordre <- sample(n,n)
Train <- Data[ordre[1:n_train],]
Test <- Data[ordre[(n_train+1):n],]

xnam <- paste("X", 1:8, sep="")
fmla <- as.formula(paste("Y1 ~ ", paste(xnam, collapse= "+")))

fit <- rpart(fmla, data=Train,minsplit=5,cp=0,method='anova')
fit
par(mfrow=c(1,2))
par(xpd = TRUE)
plot(fit, compress = TRUE)
text(fit, use.n = TRUE)

prune(fit,cp=0.01)

#CV pour ?mondage
printcp(fit)
fit_2 = prune(fit,cp=3.0830e-05)
plot(fit_2)


#Pr?diction  emmond? vs non-?mmond?
preds <- predict(fit,Test)
#Pr?cision
sum(Test$Y1 - preds)^2/n_test


#Pr?diction
preds <- predict(fit_2,Test)
#Pr?cision
sum(Test$Y1 - preds)^2/n_test


library(cparty)
# Package plus moderne avec de nouveaux arbres fancy
# Plus d'information: https://cran.r-project.org/web/packages/party/party.pdf

