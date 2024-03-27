#######################################
# Laboratoire séance #2
# Introduction à R 
# Régression linéaire et logistique
#######################################

#On se fait un repo et on y travail
#setwd("C")

#install.packages("MASS")
#install.packages("ISLR2")
library(MASS)
library(ISLR2)
library(nnet)

#######################################
# Simple visualisation du jeu de donnée 
#######################################

head(Boston)#head est preferable a view
View(Boston)

#The ISLR2 library contains the Boston data set, which records medv (median
#house value) for 506 census tracts in Boston. We will seek to predict
#medv using 12 predictors such as rm (average number of rooms per house),
#age (average age of houses), and lstat (percent of households with low
#socioeconomic status).

nrow(Boston)
ncol(Boston)
dim(Boston)

# On google ça!
is.na(Boston)#est-ce que la variable est manquante, pas tres pratique car grosse matrice

which(is.na(Boston))

sum(is.na(Boston))

colnames(Boston)

Boston$medv

hist(Boston$medv)

class(Boston)#c'est quoi cette variable la => data.frame

# Data frame ça ressemble a une matrice mais c'est pas exactement pariel pour R
# C'est un type d'object et ça vient avec plusieurs fonction dédié au data frames.
# On va souvent vouloir travailler avec un data frame, on peut transformer des matrices
# de données en data frame.

class(Boston$medv)

class(Boston$chas)

class(Boston$lstat)


#######################################
# Régression linéaire
#######################################
?lm
#C'est l'outil le plus important, ça vous dit quoi googler/demander a chatgpt


lm(medv~lstat)#cause une erreur
#2 facons de regler le problem
fit1 <- lm(Boston$medv~Boston$lstat)
fit1
#ou
fit1 <- lm(medv~lstat, data=Boston)
fit1


summary(fit1)
#ça contient pas mal tout!


# On veut prédire des données tests!
id <- sample(nrow(Boston)) #pour mieux melanger et eviter qu'il y ait un ordre
prop <- 0.9 #a notre choix, facile a changer (proportion de données entrainement)
nent <- prop*nrow(Boston) # oooppps pas un nombre entier
nent <- floor(prop*nrow(Boston))

nrow(Boston) - nent

ent_Boston = Boston[id[1:nent],]
tes_Boston = Boston[id[(nent+1):nrow(Boston)],]

nrow(ent_Boston) 
nrow(tes_Boston)

fit2 <- lm(medv~lstat, data=ent_Boston)

ent_Boston[1,]
coef(fit2)

?predict#fonction trop vague

preds = predict(fit2)#pour les modele lineaire
preds
length(preds)
#ok ok ça donne la prédiction pour les données d'entrainement de fit2

preds2 = predict(fit2,tes_Boston)
preds2
length(preds)
#ok ok c'est ce que je veux.. je pense... regardons au long connaissant notre modèle

#beta0 + beta_1 * lstat
coef(fit2)[1]+coef(fit2)[2]*tes_Boston[1,]$lstat
# Ce genre de verification est l'essence de votre expertise
# tout le monde peut utiliser les fonction R et Python, mais de comprendre
# ces fonctions vous permet de les tester, les modifier etc...

plot(Boston$lstat , Boston$medv)
abline(fit2,color=red)
?abline
abline(fit2,col='red')

# EQM:

err = preds2-tes_Boston$medv
err
errq = err^2
errq^2
EQM = sum(errq)/nrow(tes_Boston)
EQM
#ou
EQM = sum((preds2-tes_Boston$medv)^2)/nrow(tes_Boston)#version plus courte
EQM

# Vous devez recreer le code de 3.6.2 pour plus de pratique!

#######################################
# Régression linéaire multiple
#######################################

fit3<- lm(medv~lstat+age, data=ent_Boston)
summary(fit3)

preds3 = predict(fit3,tes_Boston)
EQM(preds3,tes_Boston$medv)

fit4<- lm(medv~., data=ent_Boston)#le point apres le ~, c'est un moyen de Y~ TOUT
summary(fit4)#on peut faire toutes les variables en meme temps

#age n'est plus une variable importante ?!?!
#variable confondante, plutôt une question d'inférence!

preds4 = predict(fit4,tes_Boston)
EQM(preds4,tes_Boston$medv)
mean((preds4~ent_Boston$medv)^2)


fit5 <- lm(medv~.-age, data = tes_Boston)
summary(fit5)

preds5 = predict(fit5,tes_Boston)
EQM(preds5,tes_Boston$medv)

#Pour la prédiction, les variables inutiles sont nuisibles.
#La sélection de variables sera au coeur du prochain cours.

fit6 <- update(fit5,~.-indus)
summary(fit6)

preds6 = predict(fit6,tes_Boston)
EQM(preds6,tes_Boston$medv)

#######################################
# Intéractions, termes polynomiaux et variables qualitatives
#######################################

#lstat:age est le terme d'interaction

fit7 <- lm(medv~lstat+age+lstat:age, data = Boston)
summary(fit7)

#ou plus simplement lstat*age

fit7 <- lm(medv~lstat*age, data = Boston)
summary(fit7)

preds7 = predict(fit7,tes_Boston)
EQM = sum((preds7-tes_Boston$medv)^2)/nrow(tes_Boston)
EQM

fit8 <- lm(medv~lstat+I(lstat^2), data = Boston)
summary(fit8)

preds8 = predict(fit8,tes_Boston)
EQM = sum((preds8-tes_Boston$medv)^2)/nrow(tes_Boston)
EQM

# ou, pour de plus haut degrée

fit8 <- lm(medv~poly(lstat,2,raw=TRUE), data = Boston)#degree 2
summary(fit8)

fit8 <- lm(medv~poly(lstat,4,raw=TRUE), data = Boston)#degree 4
summary(fit8)

preds8 = predict(fit8,tes_Boston)
EQM = sum((preds8-tes_Boston$medv)^2)/nrow(tes_Boston)
EQM

fit <- lm(medv~poly(lstat,5,raw=TRUE), data = Boston)
summary(fit)

head(Carseats)

class(Carseats$ShelveLoc)#factor ca veut dire que c'est une categorie
Carseats$ShelveLoc
contrasts(Carseats$ShelveLoc)#cree la matrice ici

#Donc Bad c'est le par défaut, et on un terme additionnelle pour Good et Medium

fit1 <- lm(Sales ~ .,data=Carseats)
summary(fit1)

#######################################
# Régression logistique
#######################################
# À faire à la maison!

head(Smarket)
plot(Smarket$Volume)
plot(Smarket$Volume,type='l')

contrasts(Smarket$Direction)
lfit <- glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)#lag1 difference entre hier et aujourd'hui, lag2 avant-hier...
summary(lfit)

preds = predict(lfit)
# Essayer de comprendre ce que c'est.

prob = predict(lfit, type = 'response')

length(prob)

pred <- rep(0, 1250)
pred[prob > .5] = "Up"
pred[prob <= .5] = "Down"

pred

#######################################
# Régression logistique multiclasse
#######################################
# Donnée disponible ici: https://archive.ics.uci.edu/dataset/53/iris
Iris <- read.csv("iris.data")
head(Iris)

?read.csv

Iris <- read.csv("iris.data",header=FALSE)
head(Iris)
colnames(Iris) <- c('sepal.len','sepal.wid','petal.len','petal.wid','species')
head(Iris)


lfit <- glm(species ~., data=Iris, family=multinomial)

Iris$species <- as.factor(Iris$species)

class(Iris$species)
Iris$species

library(nnet)

lfit <- multinom(species ~., data=Iris, family=multinomial)
summary(lfit)