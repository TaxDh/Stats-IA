#######################################
# Laboratoire R - Scéance 11
# 10 avril 2024
# Analyse en composante principale
# Vecteur propres et valeurs propres
# Interprétation
#######################################

# On prépare notre directory ainsi que nos fonctions R.
setwd()


# library FactoMineR super populaire pour ce genre de problème
# on s'en sert surtout pour obtenir des données, 
library(FactoMineR)

data(decathlon)

deca <- decathlon[14:41,1:10]

head(deca)

# Préparer les données
# On peut centré-réduire à main ou bien utiliser:
deca.scaled <- scale(x = deca, 
                     center = TRUE, 
                     scale = TRUE)

#les colones (prédicteurs) devrais avoir moyenne=0 et var=1 
deca.scaled
mean(deca.scaled[,1])

# la dimension est très élèvé, c'est dure de faire la visualisation
pairs(deca.scaled)

# on peut réduire la dimension parce que les bons athlètes sont bon
# dans plusieurs disciplines!

# On va faire l'ACP à main. En calculant S et ensuite les vecteurs propres
S <- cor(deca.scaled)

#on voit ici, avec des jambes fortes on court vite et on saute loin!
S


e <- eigen(S)

e$vectors[,1]%*%e$vectors[,1]

U <- e$vectors[,1:2]

projection <- deca.scaled%*%U

projection

# prcomp est la fonction R qui fait tout pour nous.
pca <- prcomp(deca.scaled)

# Matrice de projection 2 dimension de nos points
U <- pca$rotation[,1:2]

dim(U)

dim(deca.scaled)

projection <- deca.scaled%*%U

# une autre manière de faire du feature-engineering ou de la construciton de prédicteurs informatifs
projection

decathlon[14:41,]$Rank

# visualiser notre projection, on observer que ces 2 dimensions suffirait a prédire le rang!
rbPal <- colorRampPalette(c('red','blue'))

decacolors <- rbPal(10)[as.numeric(cut(decathlon[14:41,]$Rank,breaks = 10))]

#sans avoir inclut le rang, celui-ci apparait natuerellement dans les 2 axes de PCA
plot(projection, col=decacolors, pch=19)

# permet de visualisé les axes originaux aussi
biplot(pca,scale=0)
abline(h=0)
abline(v=0)

pca$rotation[,1]

# permet de visualisé les axes originaux aussi
biplot(pca,choices = c(1,3),scale=0)
abline(h=0)
abline(v=0)


# FactoMineR possède aussi sa propre fonction PCA
# Je vous invite a explore FactoMineR par vous même.
res.pca <- PCA(deca.scaled,graph=TRUE)

summary(res.pca)

plot(res.pca)


