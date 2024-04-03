#######################################
# Laboratoire R - Scéance 10
# 3 avril 2024
# Mélange de modèle Gaussien
# Formation de groupes appliqués
#######################################

# On prépare notre directory ainsi que nos fonctions R.
#setwd()
source('Fonctions.R')


# Générons des données 3dimension à l'aide de notre générateur de GMM
n = 1000
prob = c(0.3,0.3,0.4)
centres = matrix(data=c(0,0,3,2,-2,0,5,5,-1),nrow=3,byrow=TRUE)
centres
data <- GenGroup(n,prob,centres)
#data <- out[[1]]
#assign <- out[[2]]
plot(data)

# Model clustering avec mclust
# https://cran.r-project.org/web/packages/mclust/mclust.pdf
library(mclust)

#permet de visualise toute les pairs de variable, permettant un peu d'observer les
#groupes dans chancune des dimensions.
clPairs(data)

# mclustBIC 
BIC <- mclustBIC(data)
plot(BIC)
summary(BIC)# pour EII c'est 3 groupes, VII 3 groupes, EEI 3 groupes (c'est le meilleurs). On va tjs prendre EII pour choisir dans le cours


?Mclust
# choix de l'hyperparamètre, x = BIC c'est comme s=bestlam; on dit a Mclust de choiris l'hyperpara par BIC fait pr...
gmm <- Mclust(data, x = BIC)
summary(gmm , parameters = TRUE)# variance proche de l'identité.
summary(gmm, parameters = T)[11]# moyenne

plot(gmm, what = "classification")

gmm$classification

plot(gmm, what = "uncertainty")

gmm$z# c'est notre pi, notre proba d'assignement


# amusons-nous a changer les centres, rajouter-enlever des dimensions un peu...

# more stuff here https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
# On peut comparer avec kmeans
?kmeans
kmeans(data,3)

# https://archive.ics.uci.edu/dataset/485/tarvel+review+ratings
review <- read.table('google_review_ratings.csv', sep=',',header=TRUE)

review <- cbind(review$Category.7,review$Category.2,review$Category.6,review$Category.20,review$Category.16,review$Category.22)
colnames(review) <- c('malls','resort','museums','bakeries','clubs','cafe')

sum(is.na(review))

review <- na.omit(review)

sum(is.na(review))

head(review)

clPairs(review)

#BIC <- mclustBIC(review,G=1:5,modelNames = "VII")
BIC <- mclustBIC(review,G=3,modelNames = "VII")

summary(BIC)

gmm <- Mclust(review, x = BIC)
summary(gmm , parameters = TRUE)

plot(gmm, what = "classification")





BIC <- mclustBIC(review,G=1:2,modelNames = "VII")

summary(BIC)

gmm <- Mclust(review, x = BIC)
summary(gmm , parameters = TRUE)

plot(gmm, what = "classification")

gmm$classification

plot(gmm, what = "uncertainty")


