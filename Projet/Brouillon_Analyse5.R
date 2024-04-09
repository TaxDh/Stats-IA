# Réseau de neurones
library(keras)
library(tensorflow)
library(dplyr)

set.seed(2024)
donnees <- read.table("TUANDROMD.csv", sep = ",", header = TRUE)
donnees <- na.omit(donnees)

donnees$Label <- ifelse(donnees$Label == "goodware", 0, 1)

source("Fonctions.R")

# Préparation des données pour la validation croisée
donnees_ent <- donnees

# Fonction de validation croisée pour un réseau de neurones
validation_croisee_nn <- function(donnees, k, units, activation_function) {
  taille_fold <- floor(nrow(donnees) / k)
  precision_totale <- numeric(k)
  
  for (i in 1:k) {
    indices <- (((i - 1) * taille_fold + 1):(i * taille_fold))
    donnees_test <- donnees[indices, ]
    donnees_entr <- donnees[-indices, ]
    
    # Préparation des données d'entrée pour Keras (sans la colonne 'Label')
    ent_x <- as.matrix(donnees_entr[, -which(names(donnees_entr) == "Label")])
    tes_x <- as.matrix(donnees_test[, -which(names(donnees_test) == "Label")])
    
    # Normalisation des caractéristiques avec scale()
    ent_x <- scale(ent_x)
    tes_x <- scale(tes_x)
    
    # Préparation des étiquettes pour l'entraînement et l'évaluation
    ent_y <- as.numeric(donnees_entr$Label)  # Des fois ça me fais des bogue si je ne met pas ça
    tes_y <- as.numeric(donnees_test$Label)
    
    # Construction et compilation du modèle
    modele_nn <- keras_model_sequential() %>%
      layer_dense(units = units, activation = activation_function, input_shape = c(ncol(ent_x))) %>%
      layer_dense(units = 1, activation = 'sigmoid')  # sigmoide car c'est une classification binaire
    
    modele_nn %>% compile(
      optimizer = 'adam',
      loss = 'binary_crossentropy',
      metrics = c('accuracy')
    )
    
    # Entraînement du modèle
    modele_nn %>% fit(ent_x, ent_y, epochs = 100, batch_size = 32, verbose = 0)
    
    # Évaluation du modèle
    scores <- modele_nn %>% evaluate(tes_x, tes_y, verbose = 0)
    precision_totale[i] <- scores[[2]]  # Utilisation de crochets pour extraire la précision
  }
  
  # Calcul de la précision moyenne sur tous les folds
  precision_moyenne <- mean(precision_totale)
  return(precision_moyenne)
}

# Exemple d'utilisation de la fonction avec des hyperparamètres spécifiques
units <- c(10, 50)  # Exemple : Tester avec 10 et 50 neurones
activation_functions <- c('relu', 'sigmoid')  # Exemple : Tester avec relu et sigmoid comme fonctions d'activation

meilleure_precision <- 0
meilleurs_hyperparametres <- list(units = NA, activation_function = NA)

# Début du comptage du temps
debut <- Sys.time()

# Nombre total d'itérations
total_iterations <- length(units) * length(activation_functions)
iteration_actuelle <- 0

for (u in units) {
  for (a in activation_functions) {
    iteration_actuelle <- iteration_actuelle + 1
    precision <- validation_croisee_nn(donnees, 5, u, a)
    if (precision > meilleure_precision) {
      meilleure_precision <- precision
      meilleurs_hyperparametres$units <- u
      meilleurs_hyperparametres$activation_function <- a
    }
    # Imprime le statut actuel et estime le temps restant
    temps_actuel <- Sys.time()
    temps_passe <- temps_actuel - debut
    temps_moyen_par_iteration <- temps_passe / iteration_actuelle
    temps_restant_estime <- (total_iterations - iteration_actuelle) * temps_moyen_par_iteration
    message(sprintf("Progression : %d/%d, Temps restant estimé : %s", iteration_actuelle, total_iterations, format(temps_restant_estime, "%H:%M:%S")))
  }
}

print(paste("Meilleure précision:", meilleure_precision))
print(paste("Meilleurs hyperparamètres: units =", meilleurs_hyperparametres$units, ", activation_function =", meilleurs_hyperparametres$activation_function))
