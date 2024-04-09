#Régression Lasso
set.seed(2024)
donnees <- read.table("TUANDROMD.csv", sep = ",", header = TRUE)
donnees <- na.omit(donnees)

donnees$Label <- ifelse(donnees$Label == "goodware", 0, 1)

source("Fonctions.R")

