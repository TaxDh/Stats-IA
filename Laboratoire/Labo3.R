#on charge MASS et ISLR2
install.packages("ISLR2")

library(MASS)
library(ISLR2)

#avant le 3.6.5
head(Boston)
medv <- Boston$medv
lstat <- Boston$lstat
lm.fit <- lm(medv ~ lstat)

#3.6.5
#ici on a Y = beta0 + beta1 X1 + beta2 X2 ou kkchose dememe
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
# p-value: < 2.2e-16 signifie que le model a ete ameliorer
#utilisons anova pour comparé le modèle linéaire du quadratique
anova(lm.fit, lm.fit2)
#le résultat le modele lm.fit2 est  superieur

par(mfrow = c(2,2))
plot(lm.fit2)

lm.fit5 <- lm(medv ~ poly(lstat,5))
summary(lm.fit5)

#3.6.6
head(Carseats)
sales <- Carseats$Sales
price <- Carseats$Price
age <- Carseats$Age
Income <- Carseats$Income
Advertising <- Carseats$Advertising

lm.fita <- lm(sales ~ . + Income:Advertising + price:age, data = Carseats)
summary(lm.fita)













