set.seed(2024)
donnees <- read.table("TUANDROMD.csv", sep = ",", header = TRUE)
donnees <- na.omit(donnees)

donnees$Label <- ifelse(donnees$Label == "goodware", 0, 1)
donnees$Label <- factor(donnees$Label)

source("Fonctions.R")

# Préparation des données pour la validation croisée
donnees_ent <- donnees


library(randomForest)

# Fonction pour effectuer la validation croisée k-fold
#entree: les donnees, la quantité k, le nombre de prédicteur, le nombre d'arbre
validation_croisee <- function(donnees, k, mtry, ntree) {
  taille_fold <- floor(nrow(donnees) / k)
  precision_totale <- 0
  
  for (i in 1:k) {
    indices <- (((i - 1) * taille_fold + 1):(i * taille_fold))
    donnees_test <- donnees[indices, ]
    donnees_entr <- donnees[-indices, ]
    
    modele_rf <- randomForest(Label ~ ., data = donnees_entr, mtry = mtry, ntree = ntree)
    predictions <- predict(modele_rf, donnees_test)
    precision <- sum(predictions == donnees_test$Label) / nrow(donnees_test)
    precision_totale <- precision_totale + precision
  }
  
  return(precision_totale / k)
}

# Définition des hyperparamètres à tester
mtry_vals <- c(2, sqrt(ncol(donnees_ent) - 1))
ntree_vals <- c(500, 1000)

# Boucle pour tester différentes combinaisons d'hyperparamètres mtry et ntree
meilleure_precision <- 0
meilleurs_hyperparametres <- c(mtry = NA, ntree = NA)

for (m in mtry_vals) {
  for (n in ntree_vals) {
    precision <- validation_croisee(donnees_ent, 5, m, n)#on fait un 5-fold
    if (precision > meilleure_precision) {
      meilleure_precision <- precision
      meilleurs_hyperparametres <- c(mtry = m, ntree = n)
    }
  }
}

print(paste("Meilleure précision:", meilleure_precision))
print(paste("Meilleurs hyperparamètres: mtry =", meilleurs_hyperparametres["mtry"], ", ntree =", meilleurs_hyperparametres["ntree"]))


meilleure_foret <- randomForest(Label~., data=donnees_ent, mtry = 16, ntree= 500, importance=TRUE)

plot(meilleure_foret)
####################################################################################################################