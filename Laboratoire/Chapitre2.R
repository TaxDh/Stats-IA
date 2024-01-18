#Section 2.3 du livre ISLRv2
#2.3.1 Basic Commands

#creation de vecteur
x <- c(1,3,2,5)
x

#fonction help, mettre ? devant la fonction
?c()

#on peut utiliser = au lieu de <-, mais par experience ca peut causer des erreurs
x = c(1,6,2)
y = c(1,4,3)

#la fonction length() pour vérifier la longueur d'un vecteur
length(x)
length(y)

#maintenant qu'on a verifier que x et y on la meme longueur, on peut les additionner
x+y

#la fonction ls() permet de voir toute la liste des objects
ls()

#la fonction rm() permet d'effacer tous les objets à l'intérieur du ()
rm(x,y)
ls()
#on peut tous les effacer d'un coup:
rm(list = ls())
ls()

#On introduit la fonction matrix()
a <- matrix(data = c(1,2,3,4), nrow = 2, ncol =2)
a

#on peut ecrire une matrice sans argument
b <- matrix(c(1,2,3,4), 2, 2)
b

#Par defaut, matrix se remplit par colonne, ici on le fait par ligne
c <- matrix(c(1,2,3,4),2,2, byrow = TRUE)
c  

#Les fonction sqrt() et ^2 fonctionne aussi sur les matrice
sqrt(a)
a^2  

#La fonction rnorm() genere un vecteur aleatoire normal
x <- rnorm(50)
x

y <- x + rnorm(50, mean = 50, sd = 0.1)
y  

#On utilise maintenant la fonction cor() sur x et y pour voir la correlation
cor(x,y)

#Pour avoir toujours le meme vecteur aleatoire, on utilise set.seed()
set.seed(1303)
rnorm(50)  

#Voici quelque fonction usuelle en statistique en R
set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sqrt(var(y))
sd(y)

#2.3.2 Graphics

