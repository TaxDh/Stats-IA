#4.7.1
library(ISLR2)
names(Smarket)
dim(Smarket)
 summary(Smarket)
pairs(Smarket)
cor(Smarket[, -9])
#head(Smarket)
#head(Smarket[, -9])
#head(Smarket[, -3])
attach(Smarket)
plot(Volume)

#4.7.2
head(Smarket)
glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fits)

coef(glm.fits)
summary(glm.fits)$coef

glm.probs <- predict(glm.fits, type = "response")
glm.probs[1:10]
contrasts(Direction)
glm.probs

glm.pred <- rep (" Down ", 1250)
glm.pred[glm.probs > .5] = "Up"

table(glm.pred, Direction)
#trop long j'arrete