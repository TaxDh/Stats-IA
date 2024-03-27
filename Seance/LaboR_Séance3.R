#######################################
# Laboratoire séance 3
# 24 janvier 2024
# Ridge, Lasso et fonctions personalisées
#######################################

#On se fait un repo et on y travail
setwd("C")

#install.packages("MASS")
#install.packages("ISLR2")
library(ISLR2)
library(nnet)
library(glmnet)#librairy qui fait du lasso et du ridge

#######################################
# Simple visualisation du jeu de donnée 
#######################################
View(Hitters)
head(Hitters)

dim(Hitters)#combien de lignes et colonnes

sum(is.na(Hitters))#nombre de valeurs manquantes

which(is.na(Hitters))#lesquelles sont manquantes?

Hitters <- na.omit(Hitters)#elimine les lignes qui ont des valeurs manquantes

dim(Hitters)

# On s'assurer qu'on a bel et bien retirer les lignes avec valeurs manquantes
sum(is.na(Hitters))

colnames(Hitters)

hist(Hitters$Salary)


#######################################
# Ridge
#######################################
?glmnet
#On voit que x doit être une matrice et non un data frame... ça arrive

#beaucoup de paramètre, glmnet est une fonction très général

#une sequence de modèle (par le biais d'une séquence de lambda) est produit grâce à LARS
#on peut tester une certaine quantité de lambda déterminé par défaut avec nlambda
#on peut aussi donner une certaine séquence nous même par lambda

#alpha est un autre paramètre important, il fait le pont entre lasso et ridge
# https://en.wikipedia.org/wiki/Elastic_net_regularization


which(colnames(Hitters)=="Salary")#quel colonne est le salaire (la variable reponse)

x <- as.matrix(Hitters[,-19])#fais moi toutes les colonnes sans la 19e
x
#On remarque plusieurs "", les valeurs sont tous considérer comme étant des chaines 
#de charactère. C'est parce que plusieurs variables catégoriels sont des charactères.

x <- model.matrix(Salary~., Hitters)[,-1]#y = f(x) c'est y ~ f(x)
x
# model.matrix transforme les variables catégorielle en dummy

y <- Hitters$Salary #un vecteur maintenant

ridgefit <- glmnet(x, y, alpha = 0)#alpha = 1 => lasso, alpha =0 => ridge

dim(coef(ridgefit))
#20 coefficients, pour 100 valeurs de lambda

ridgefit$lambda[20]
coef(ridgefit)[,20]
sqrt(sum(coef(ridgefit)[,20]^2))


ridgefit$lambda[60]
coef(ridgefit)[,60]
sqrt(sum(coef(ridgefit)[,60]^2))

#hummmmmm bizarre heh ? ça ma pris un certain temps....
# C'est a cause de l'intercept!!!

ridgefit$lambda[20]
coef(ridgefit)[-1,20]
sqrt(sum(coef(ridgefit)[-1,20]^2))


ridgefit$lambda[60]
coef(ridgefit)[-1,60]
sqrt(sum(coef(ridgefit)[-1,60]^2))

ridgefit$lambda[92]
coef(ridgefit)[,92]

ridgefit$lambda[93]
coef(ridgefit)[,93]

predict(ridgefit , s = 50, type = "coefficients")[1:20,]#donne moi les coefficient pour une certaine valeur de lambda
#je crois que lamda = 50?

#Ok on comprend grosse modo comment ça marche, suivons une bonne procédure

set.seed(1)
n = nrow(x)
prop =0.8
ntrain = floor(nrow(x)*prop)
ntest=n-ntrain

train_id = sample(1:nrow(x),ntrain)
train_id

test = (-train_id)

#c'est quoi ça ?
test


#voici donc le numéro de ligne des ntrain elements du S_ent.
#Question: pourquoi pas prendre 1 à ntrain dans x ?


Hitters_train <- Hitters[train_id,]
x_train <- model.matrix(Salary~., Hitters_train)[,-1]
y_train <- Hitters_train$Salary
dim(x_train)
#...
#ou

x_train <- x[train_id,]
y_train <- y[train_id]
dim(x_train)

x_test <- x[test,]
y_test <- y[test]
dim(x_test)

#on peut aussi permuter aléatoire l'ordre des lignes et ensuite prendre les ntrain premières
#je préfère ça personellement
set.seed(1)
head(Hitters)
#permutation des lignes
Hitters <- Hitters[sample(nrow(x),nrow(x)),]
#on observe
head(Hitters)


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
plot(cv.out)#juste pour visualisé c'est quoi
bestlam <- cv.out$lambda.min
bestlam
log(bestlam)

#c'est quoi l'autre ligne pointillé
#cv.out$
# on chercher un lambda
log(cv.out$lambda.1se)
?cv.glmnet

#C'est donc le modèle le plus régularié qui est quand même correcte!

grid <- 10^seq(10, -2, length = 200)#fourni une grille de lambda
cv.out <- cv.glmnet(x_train, y_train, alpha = 0,lambda=grid)
plot(cv.out)
bestlam <- cv.out$lambda.min#meilleur lambda
bestlam


#Faisons un peu de prédiction avec notre meilleur lambda

pred <- predict(ridgefit,s=bestlam,newx=x_test)#salaire qu'on predit
pred


#On rapporte la valeur suivante:
Ridge_EQM = mean((pred-y_test)^2)

#######################################
# Fonctions personalisés
#######################################

EQM <- function(pred,true){
  return (mean((pred-true)^2))
}

EQM(pred,y_test)

#Une bonne pratique de travail est d'ajouter quelques commentaires pour décrire
#le comportement de la fonction

#EQM: retourne l'EQM entre deux vecteurs.
#pred: vecteurs de prédictions
#true: vecteur de vraies valeures
EQM <- function(pred,true){
  return (mean((pred-true)^2))
}

#À des fins de démonstration, regardons plusieurs valeurs différente de lambda 

lambda <- c(0.001,0.1,1,10,30,100,200,600,1000,2000,5000)
pred <- predict(ridgefit,s=lambda,newx=x_test)
dim(pred)

# on va regarder l'EQM pour nos lambda
test_EQM <- rep(0,length(lambda))

for (i in 1:length(lambda)){
  test_EQM[i] <- EQM(pred[,i],y_test)
}
plot(lambda,test_EQM)
plot(log(lambda),test_EQM)


#######################################
# Lasso
#######################################

#Pour Lasso, pas une grande différence, il suffit d'utiliser alpha=1
grid <- 10^seq(10, -2, length = 200)
cv.out <- cv.glmnet(x_train, y_train, alpha = 1,lambda=grid)#cv c'est pour validation croisee
plot(cv.out)#les nombres en haut du graphique c'est le nombre de variables
bestlam <- cv.out$lambda.min
bestlam

lassofit <- glmnet(x_train, y_train, alpha = 1,lambda=grid)
dim(coef(lassofit,s=bestlam))

#On observe les coefficient et ceux à 0!
coef(lassofit,s=bestlam)
predict(lassofit,type='coefficients',s=bestlam)#les valeurs "." veut dire que le coefficients a été amené à 0
#reduit les predicteur. par exemple walks est positivement correler avec le salaire

#On observe la différence principale entre lasso et ridge!

plot(lassofit)

plot(lassofit, "lambda", label = TRUE)

pred <- predict(lassofit,s=bestlam,newx=x_test)

pred

#On rapporte la valeur suivante:
Lasso_EQM = EQM(pred,y_test)


#On peut comparer avec la régression linéaire simple
lmfit <- lm(Salary~., data=Hitters_train)

predict(lassofit,s=0,exact=T,type="coefficients",x=x_train,y=y_train)
coef(lmfit)

pred <- predict(lmfit,newdata=Hitters_test)

Lm_EQM <- mean((pred-y_test)^2)

Ridge_EQM
Lasso_EQM
Lm_EQM




