# Régression Lasso
library(glmnet)

set.seed(2024)
donnees <- read.table("TUANDROMD.csv", sep = ",", header = TRUE)
donnees <- na.omit(donnees)
donnees$Label <- ifelse(donnees$Label == "goodware", 0, 1)

source("Fonctions.R")

# Diviser les données en ensembles d'entraînement et de test
list_data <- train_test_split(prop = 0.8, data = donnees)
training <- list_data[[1]]
test <- list_data[[2]]

#données d'entrainement et test
x_train <- as.matrix(training[,-ncol(training)]) # Tous sauf la colonne Label
y_train <- training[,ncol(training)]
x_test <- as.matrix(test[,-ncol(test)])
y_test <- test[,ncol(test)]

# On cherche le meilleur lamda pour lasso
cv.out <- cv.glmnet(x_train, y_train, alpha = 1, family = "binomial")
bestlam <- cv.out$lambda.min

# Entraîner le modèle Lasso avec le lambda optimal
lasso_model <- glmnet(x_train, y_train, alpha = 1, lambda = bestlam, family = "binomial")






# Générer des prédictions sur l'ensemble de test
probabilities <- predict(lasso_model, newx = x_test, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)

# Calculer la précision
precision <- Prec(predicted_classes, y_test)

# Afficher la précision
precision

#Graphique des OOB
plot(cv.out)
