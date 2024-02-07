#######################################
# Laboratoire séance 4
# 31 janvier 2024
# Lasso, Elestic Net et données simulés
#######################################

#On se fait un repo et on y travail
setwd("~/Semaine 4")
source('fonctions.r')

#install.packages("MASS")
#install.packages("ISLR2")
library(ISLR2)
library(nnet)
library(glmnet)

########################################
# Simple visualisation du jeu de données
########################################
View(Hitters)
head(Hitters)

dim(Hitters)

sum(is.na(Hitters))

which(is.na(Hitters))

Hitters <- na.omit(Hitters)

dim(Hitters)


# On s'assure qu'on a bel et bien retiré les lignes avec valeurs manquantes
sum(is.na(Hitters))

colnames(Hitters)

hist(Hitters$Salary)

set.seed(1)
n = nrow(Hitters)
prop =0.8
ntrain = floor(nrow(Hitters)*prop)
ntest=n-ntrain

Hitters <- Hitters[sample(nrow(Hitters),nrow(Hitters)),]


Hitters_train <- Hitters[1:ntrain,]
x_train <- model.matrix(Salary~., Hitters_train)[,-1]
y_train <- Hitters_train$Salary
dim(x_train)

Hitters_test <- Hitters[(ntrain+1):n,]
x_test <- model.matrix(Salary~., Hitters_test)[,-1]
y_test <- Hitters_test$Salary
dim(x_test)

# À partir de maintenant, on met S_test de côté et on travail sur le reste.

cv.out <- cv.glmnet(x_train, y_train, alpha = 0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)


#Faisons un peu de prédiction avec notre meilleur lambda
ridgefit <- glmnet(x_train, y_train, alpha = 0, lambda = bestlam)
pred <- predict(ridgefit,s=bestlam,newx=x_test)
pred


#On rapporte la valeur suivante:
Ridge_EQM = EQM(pred,y_test)

#######################################
# Elastic Net
#######################################

?glmnet()

#le paramètre alpha créé un pénalité hybride entre lasso et ridge.
cv.out <- cv.glmnet(x_train, y_train, alpha = 0.5)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ENfit <- glmnet(x_train, y_train, alpha = 0.5)
coef(ENfit,s=bestlam)

pred <- predict(ENfit,s=bestlam,newx=x_test)

pred

#On rapporte la valeur suivante:
EN_EQM = EQM(pred,y_test)

#Comment choisir alpha ? par la validation

#Créons une fonction pour déterminer le meilleur alpha
#On va choisir le alpha avec le meilleur MSE (pour son meilleur lambda)
#sur un ensemble de validation

ntrain <- floor(nrow(Hitters_train)*0.8)
nval <- nrow(Hitters_train)-ntrain

Hitters_train <- Hitters[1:ntrain,]
x_train <- model.matrix(Salary~., Hitters_train)[,-1]
y_train <- Hitters_train$Salary
dim(x_train)

Hitters_val <- Hitters[(ntrain+1):(ntrain+nval),]
x_val <- model.matrix(Salary~., Hitters_val)[,-1]
y_val <- Hitters_val$Salary
dim(x_val)


alpha <- seq(0,1,0.05)

Select_alpha <- function(alpha, x_train, y_train, x_val, y_val){
  nalpha = length(alpha)
  MSE <- rep(0,nalpha)
  lambda <- rep(0,nalpha)
  for (i in 1:nalpha){
    
    
    cvfit <- cv.glmnet(x_train, y_train, alpha = alpha[i])
    
    
    lambda[i] <- cvfit$lambda.min
    pred <- predict(cvfit,s=bestlam,newx=x_val)
    
    
    MSE[i] <- EQM(pred,y_val)
  }
  
  return(list(alpha[which.min(MSE)],lambda[which.min(MSE)],MSE[which.min(MSE)],MSE))
}

sortie <- Select_alpha(alpha,x_train,y_train,x_val,y_val)

#le paramètre alpha créé un pénalité hybrid entre lasso et ridge.
cv.out <- cv.glmnet(x_train, y_train, alpha = sortie[[1]],lambda=grid)
plot(cv.out)
bestlam <- cv.out$lambda.min


ENfit <- glmnet(x_train, y_train, alpha = sortie[[1]],lambda=sortie[2])

pred <- predict(ENfit,s=bestlam,newx=x_test)

pred

#On rapporte la valeur suivante:
En_MSE = mean((pred-y_test)^2)

Ridge_MSE
En_MSE

#######################################
# Données simulés
#######################################

#Quand on sélectionne avec Lasso, sélectionne-t-on les bonnes variables?
#Pour des vraies problèmes on ne connait pas la réponse
#On ne connait pas quelle variables sont réellement utiles

#Dans ce contexte, on fais souvent une étude de simulation!

#On va générer 200 prédicteurs aléatoires, et seulement 5 d'entre eux seront utiles!
set.seed(123)
n = 500
p = 200
X <- matrix(data=rnorm(n*p,0,1),nrow=n)
Y <- 10 + 5*X[,1]+5*X[,2]+5*X[,3]+1*X[,4]+1*X[,5] + rnorm(n,0,1)

grid <- 10^seq(10, -2, length = 200)
cv.out <- cv.glmnet(X, Y, alpha = 1,lambda=grid)
bestlam <- cv.out$lambda.min
lassofit <- glmnet(X, Y, alpha = 1,lambda=grid)
coef(lassofit,s=bestlam)

#Ré-échantillonage
#Faisons 100 échantillon bootstrap, puis Lasso et conservons les coef qui sont toujours > 0

nbs = 1000
coefbs = matrix(data=rep(0,nbs*(p+1)),nrow=nbs)

for (i in 1:nbs) {
  id <- sample(nrow(X),nrow(X),replace=TRUE)
  Xtrain <- X[id,]
  Ytrain <- Y[id]
  
  grid <- 10^seq(10, -2, length = 200)
  cv.out <- cv.glmnet(Xtrain, Ytrain, alpha = 1,lambda=grid)
  bestlam <- cv.out$lambda.min
  lassofit <- glmnet(X, Y, alpha = 1,lambda=grid)
  coefbs[i,] = coef(lassofit,s=bestlam)[1:201]
  
}

coefbs

#Pour chaque prédicteur on regarde si Lasso retourne 0 sur au moins 1 échantillon BS
#si oui, on met a 0, sinon on prend la moyenne
coefficients <- rep(0,p)
for (j in 1:p) {
  if (sum(coefbs[,j] == 0)>0){
    coefficients[j] <- 0
  } else{
    coefficients[j] <- mean(coefbs[,j])
  }
  
}

coefficients


#Il y existe une garanties theorique comme quoi on peut laisse tomber un prédicteur 
#s'il est 0 pour au moins 1 échantillon bootstrap!

nbs = 10

#Bootstrap sans remise
# Lambda un peu plus restrictif
for (i in 1:nbs) {
  id <- sample(nrow(X),200,replace=FALSE)
  Xtrain <- X[id,]
  Ytrain <- Y[id]
  
  grid <- 10^seq(10, -2, length = 200)
  cv.out <- cv.glmnet(Xtrain, Ytrain, alpha = 1,lambda=grid)
  bestlam <- cv.out$lambda.1se
  lassofit <- glmnet(X, Y, alpha = 1,lambda=grid)
  coefbs[i,] = coef(lassofit,s=bestlam)[1:201]
  
}

#Pour chaque prédicteur on regarde si Lasso retourne 0 sur au moins 1 échantillon BS
#si oui, on met a 0, sinon on prend la moyenne
coefficients <- rep(0,p)
for (j in 1:p) {
  if (sum(coefbs[,j] == 0)>0){
    coefficients[j] <- 0
  } else{
    coefficients[j] <- mean(coefbs[,j])
  }
  
}

coefficients
# Ca marche super bien!

# Le ré-échantillonage, sert à résoudre toute sorte de problème. 
# On peut aussi estimé la variance de nos paramètres par exemple.
# C'est un outil de statistisque computational hyper flexible. 
