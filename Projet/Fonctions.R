#Fonction pour diviser le data en training et test
#prop: proportion de point d'entrainement
#data: les données à subdiviser
train_test_split <- function(prop,data) {
  id <- sample(nrow(data))
  nent <- floor(prop*nrow(data))
  ent_data <- data[id[1:nent],]
  tes_data <- data[id[(nent+1):nrow(data)],]
  
  return(list(ent_data,tes_data))
}


#EQM: retourne l'EQM entre deux vecteurs.
#pred: vecteurs de prédictions
#true: vecteur de vraies valeures
EQM <- function(pred,true){
  return (mean((pred-true)^2))
}

#Prec: retourne la proportion de terme faux dans les deux vecteurs
#pred: vecteurs de prédictions
#true: vecteur de vraies valeures
Prec2 <- function(pred,true){
  comp <- pred==true
  comptage <- sum(comp)
  prec <- comptage/length(true)
  return(prec)
}

#Prec: retourne la proportion de terme faux dans les deux vecteurs
#pred: vecteurs de prédictions
#true: vecteur de vraies valeures
Prec <- function(pred,true){
  return(sum(pred==true)/length(true))
}


#GenGroup: Genere des points de plusieurs groupes
#n: nombre d'observations
#prob: probabilité de groupe 
#centres: centre des groupes, de taille c X d
GenGroup <- function(n,prob,centres){
  
 #Dimension des données  
 d <- ncol(centres)
 
 #nombres de groupes
 c <- nrow(centres)
 data <- matrix(data=rep(0,n*d),nrow=n)
 
 #number of each class
 nc <- rmultinom(1, n, prob)
 start=0
 
 #En i on fait le tour de nos groupes et en j le tours de nos dimensions
 for (i in 1:c){
   for (j in 1:d){
    data[(start+1):(start+nc[i]),j] <- rnorm(nc[i],mean=centres[i,j])
   }
   start=start+nc[i]
 }
 
 return(data)
}

#Entrée: une foret, les données, la variable qu'on veut évaluer et la variable Y.
#Sortie, la nouvelle performance de la variable qu'on veut évaluer
indice_de_permutation <- function(foret, data, variable, Y) {
  # Calcul de la précision originale
  original_accuracy <- mean(predict(model, data, type = "class") == data[[Y]])
  
  # Préparation des données avec permutation de la variable spécifiée
  data_permuted <- data
  data_permuted[[variable]] <- sample(data[[variable]])
  
  # Calcul de la précision après permutation
  permuted_accuracy <- mean(predict(foret, data_permuted, type = "class") == data_permuted[[Y]])
  
  # Calcul et retour de la différence de précision
  accuracy_decrease <- original_accuracy - permuted_accuracy
  
  return(accuracy_decrease)
}
