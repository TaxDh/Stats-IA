EQM <- function(pred,true){
  return (mean((pred-true)^2))
}

#Precision: retourne la proportion de reponse categorielle bien classé
#pred: vecteurs de prédictions
#true: vecteurs de vraies valeurs
Precision <- function(pred,true){
  return (mean((pred-true)^2))
}