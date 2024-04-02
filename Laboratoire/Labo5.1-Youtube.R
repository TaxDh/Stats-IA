#Labo 5.1 - Cross Validation - Youtube

install.packages("ISLR")
library(ISLR)

library(boot)

?cv.glm

plot(mpg ~ horsepower, data = Auto)

#LOOCV : Validation croisée à exclusion simple

glm.fit <- glm(mpg ~ horsepower, data = Auto)
cv.glm(Auto, glm.fit)$delta#forumule 5.2 du livre

#on créé une formule pour utiliser la formule 5.2 du livre
loocv <- function(fit) {
  h<-lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

#On l'essaie
loocv(glm.fit)

cv.error <- rep(0,5)
degree <- 1:5

for(d in degree){
  glm.fit <- glm(mpg ~ poly(horsepower,d), data = Auto)
  cv.error[d] <- loocv(glm.fit)
}

plot(degree, cv.error, type = "b")



# 10-fold CV

cv.error10 <- rep(0,5)
for(d in degree){
  glm.fit <- glm(mpg ~ poly(horsepower,d), data = Auto)
  cv.error10[d] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}

lines(degree, cv.error10, type = "b", col = "red")



