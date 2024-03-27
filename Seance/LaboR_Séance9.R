#######################################
# Laboratoire R - Scéance 9
# 27 Mars 2024
# Partitionnement en k-moyennes
# Programmation d'algorithme
#######################################

# On prépare notre directory ainsi que nos fonctions R.
#setwd()
source('Fonctions.R')


#GenGroup: Genere des points de plusieurs groupes
#n: nombre d'observations
#prob: probabilité de groupe 
#centres: centre des groupes, de taille c X d (nombre de centre par dimension)
GenGroup <- function(n,prob,centres){
  
  #Dimension des données  
  d <- ncol(centres)
  
  #nombres de groupes
  c <- nrow(centres)
  data <- matrix(data=rep(0,n*d),nrow=n)
  
  #number of each class
  nc <- rmultinom(1, n, prob)
  start=0
  assign <- rep(0,n)
  
  for (j in 1:c){
    assign[(start+1):(start+nc[j])] <-j
    start=start+nc[j]
  }
  
  start=0
  #En i on fait le tour de nos groupes et en j le tours de nos dimensions
  for (i in 1:c){
    for (j in 1:d){
      data[(start+1):(start+nc[i]),j] <- rnorm(nc[i],mean=centres[i,j])
    }
    start=start+nc[i]
  }
  
  return(list(data,assign))
}
#La fonction devrait aller dans fonction.R (ce qui est le cas), bonne pratique de programmation


# Allons modifier Utility.R ensemble pour y ajouter un générateur de données
n = 1000
prob = c(0.3,0.3,0.4)
centres = matrix(data=c(0,3,3,-3,5,5),nrow=3,byrow=TRUE)
centres
out <- GenGroup(n,prob,centres)
data <- out[[1]]
assign <- out[[2]]
plot(data, col=assign)



# Essayons de programmer ensemble k-means clustering
# On commence par quelque chose de très haut niveau/général
# sans entrer dans les détails calculatoires.

# Disons:
#kmeans: procède au partitionnement en k groupes
# data: ensemble de données
# k: nombre de groupe
# ite: nombre d'itération
kmean <- function(data,k,iter) {
  
  #n: nb d'obersvation
  n <- nrow(data)
  
  #assignement aléatoire
  assignements <- sample(1:k, n, replace=TRUE)#assignement au hasard
  
  for (j in 1:iter){#il y avait une erreur dans le code original, c'était k au lieu de iter.
    centres <- Calcul_Centre(data,k,assignements)
    assignements <- Calcul_Assign(data,k,centres)
  }
  
  return(list(centres,assignements))
}








#37 min environ
Calcul_Centre <- function(data,k,assignements){
  
  d <- ncol(data)
  centres <- matrix(data=rep(0,d*k),nrow=k)
  
  for (j in 1:k){
    centres[j,] <- colSums(data[assignements==j,])/sum(assignements==j)#on aurait pu faire colMeans(data[assignements==j,])
  }
  
  
  return(centres)
}




#44 min
Calcul_Assign <- function(data,k,centres){
  
  assignements <- rep(0,nrow(data))
  distance <- matrix(rep(0,nrow(data)*k),nrow=nrow(data))
  
  for (i in 1:nrow(data)){
    for (j in 1:k){
     distance[i,j] <- sqrt(sum((data[i,]-centres[j,])^2))
    }
    assignements[i] <- which.min(distance[i,])
  }
  return(assignements)
}

Calcul_Assign2 <- function(data,k,centres){#autre calcul, tentative d'amélioration de calcul_assign, peut etre que ça ne fonctionne pas selon cédrik
  
  assignements <- rep(1,nrow(data))
  
  for (i in 1:nrow(data)){
    distance <- sqrt(sum((data[i,]-centres[1,])^2))
    for (j in 2:k){
      if (sqrt(sum((data[i,]-centres[j,])^2))<distance){
        distance <- sqrt(sum((data[i,]-centres[j,])^2))
        assignements[i] <- j
      }
      
    }
  }
  return(assignements)
}

#Une bonne procédure en analyse de données serait de mettre ces fonctions
#personalisé et algo dans un autre document; disons Kmeans.R

Returns <- kmean(data,3,10^3)

# On peut comparer avec la fonction r
?kmeans
returns <- kmeans(data,3)

plot(data,col=Returns[[2]])
plot(data,col=returns$cluster)




# Problème plus compliqué
n = 2000
prob = c(0.3,0.3,0.4)
centres = matrix(data=c(3,0,3,-3,4,4),nrow=3,byrow=TRUE)
centres
out <- GenGroup(n,prob,centres)
data <- out[[1]]
assign <- out[[2]]
plot(data, col=assign)


Returns <- kmean(data,3,10^3)
returns <- kmeans(data,3)

plot(data,col=Returns[[2]])
plot(data,col=returns$cluster)

# Problème plus compliqué
n = 2000
prob = c(0.2,0.2,0.2,0.2,0.2)
centres = matrix(data=c(3,0,3,-3,4,4,2,2,1,4),nrow=5,byrow=TRUE)
centres
out <- GenGroup(n,prob,centres)
data <- out[[1]]
assign <- out[[2]]
plot(data, col=assign)


Returns <- kmean(data,3,10^3)
returns <-kmeans(data,3)




#D'autres version de l'algorithme :
kmean <- function(data,k,iter) {
  n <- nrow(x)
  d <- ncol(x)
  r <- rep(0,n)
  dist <- matrix(ncol=k,nrow=n)
  means <- matrix(ncol=d,nrow=k)
  
  #Initializing the k means
  for ( i in 1:d) {
    means[,i] <- runif(k,range(x[,i])[1],range(x[,i])[2])
  }
  
  #Running the Algorithm
  for (i in 1:iter) {
    
    #Computing the assignements
    for (i in 1:n) {
      for (j in 1:k) {
        dist[i,j] <- l2norm(x[i,],means[j,])
      }
    }
    
    for (i in 1:n){
      r[i] <- argmin(dist[i,])
    }
    
    
    #Computing the new means
    for (j in 1:k) {
      sum <- rep(0,d)
      N <-rep(0,d)
      for (l in 1:d) {
        for ( i in 1:n) {
          sum[l] <- sum[l] + x[i,l]*(r[i]==j)
          N[l] <- N[l] + (r[i]==j)*1
        }
      }
      means[j,] <- sum/N
    }
    
  }
  
  return(means)
}



#ChatGPT !
# Define a function for k-means clustering
kmeans_custom <- function(data, k, max_iter = 100) {
  # Randomly initialize cluster centroids
  set.seed(123)  # Set a seed for reproducibility
  centroids <- data[sample(1:nrow(data), k), ]
  
  for (iter in 1:max_iter) {
    # Assign each data point to the nearest centroid
    distances <- sapply(1:k, function(i) {
      rowSums((data - centroids[i, ])^2)
    })
    cluster_assignments <- apply(distances, 1, which.min)
    
    # Update centroids
    new_centroids <- t(sapply(1:k, function(i) {
      if (sum(cluster_assignments == i) > 0) {
        colMeans(data[cluster_assignments == i, ])
      } else {
        centroids[i, ]  # Keep the centroid unchanged if no points assigned
      }
    }))
    
    # Check for convergence
    if (identical(centroids, new_centroids)) {
      break
    }
    
    centroids <- new_centroids
  }
  
  # Return the final cluster assignments and centroids
  result <- list(cluster_assignments = cluster_assignments, centroids = centroids)
  return(result)
}


# Problème plus compliqué
n = 2000
prob = c(0.3,0.3,0.4)
centres = matrix(data=c(3,0,3,-3,4,4),nrow=3,byrow=TRUE)
centres
out <- GenGroup(n,prob,centres)
data <- out[[1]]
assign <- out[[2]]
plot(data, col=assign)


Returns <- kmean(data,3,10^3)
returns <-kmeans(data,3)
Returns_gpt <-kmeans_custom(data,3,10^3)

plot(data,col=Returns[[2]])
plot(data,col=returns$cluster)
plot(data,col=Returns_gpt$cluster_assignments)
