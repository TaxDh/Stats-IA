#######################################
# Laboratoire R - Scéance 8
# 13 mars 2024
# Réseaux de neurones de convolution
# Keras, TensorFlow et couche dropout
#######################################


# La librairie keras est une librairir pour faire de l'apprentissage de réseau de neurone
# Les librairies ont été conçu sur Python, keras appelle TensorFlow à partir de R 
# et fait l'optimisation sur Python!

# Donc pour installer kera, il faut avoir Python et TensorFlow sur son ordinateur.
# https://tensorflow.rstudio.com/install/

# Pour installer keras
install.packages("keras")
reticulate::install_python()
library(keras)
install_keras()
library(tensorflow)

tf$constant("Hello TensorFlow!")

# On va entrainer comparé une NN standard (composé uniquement de couches denses)
# avec un réseaux de neurones de convolution

# On prépare notre directory ainsi que nos fonctions R.
#setwd("C:/Users/beaulac_Ce/Dropbox/UQAM/A2023/STT3030-ACT6100/Mon Cours/Lecture7")
source('Fonctions.R')

# Commencons avec MNSIT que nous avons vu la semaine passé
mnist <- dataset_mnist ()

# Pour MNIST, les données sont déjà divisé entre test et train
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train)

dim(x_test)

# Copier-coller du tutoriel keras pour visualiser une image
library(tidyr)
library(ggplot2)

image_1  <- as.data.frame(x_train[1, , ])#[a, , ] a est l'image d'entrainement, on peut changer pour 150
colnames(image_1) <- seq_len(ncol(image_1))
image_1$y <- seq_len(nrow(image_1))
image_1 <- gather(image_1, "x", "value", -y)
image_1$x <- as.integer(image_1$x)

ggplot(image_1, aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "black", na.value = NA) +
  scale_y_reverse() +
  theme_minimal() +
  theme(panel.grid = element_blank())   +
  theme(aspect.ratio = 1) +
  xlab("") +
  ylab("")

# Les réseaux de neurones vue en classe prennent un vecteur en entré
# On va vectorisé les prédicteurs
x_train <- array_reshape(x_train , c(nrow(x_train), 784))
x_test <- array_reshape(x_test , c(nrow(x_test), 784))
y_train <- to_categorical(y_train , 10)
y_test <- to_categorical(y_test , 10)

dim(x_train)

y_test

# ici on normalise un peu différement, on veux mettre les prédicteurs en 0 et 1
x_train <- x_train / 255
x_test <- x_test / 255

NN_MNIST <- keras_model_sequential ()
NN_MNIST %>%
   layer_dense(units = 256, activation = "relu",
                input_shape = c(784)) %>%
   layer_dropout(rate = 0.4) %>%
   layer_dense(units = 128, activation = "relu") %>%
   layer_dropout(rate = 0.3) %>%
   layer_dense(units = 10, activation = "softmax")

# C'est quoi le dropout ? Parlons-en un peu.
# https://tensorflow.rstudio.com/reference/keras/layer_dropout.html


NN_MNIST %>% compile(loss = "categorical_crossentropy",
                      optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

system.time(hist <- NN_MNIST %>% fit(x_train , y_train , epochs = 30, batch_size = 128,
          validation_split = 0.2))
plot(hist , smooth = FALSE)

y_test <- mnist$test$y
y_test

NN_MNIST %>% predict(x_test) 
pred = as.array(NN_MNIST %>% predict(x_test) %>% k_argmax())
NNPrec <- Prec(pred,y_test)

library(randomForest)
# Foret aléatoire a beosin de sa propose
Y <- as.factor(mnist$train$y)
ent_data <- data.frame(cbind(x_train,Y))
ent_data$Y <- as.factor(ent_data$Y)


Y <- as.factor(mnist$test$y)
tes_data <- data.frame(cbind(x_test,Y))
tes_data$Y <- as.factor(tes_data$Y)


#VERY SLOW
rf <- randomForest(Y~.,data=ent_data,ntree=20,nodesize=1000)
pred <- predict(rf,tes_data)
RFPrec <- Prec(pred,tes_data$Y)
RFPrec# =0.8674, c'est pas bon

# CNN
CNN_MNIST <- keras_model_sequential() %>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu', input_shape = c(28,28,1)) %>% #input_shape=c(dim_x, dim_y, noir/blanc/couleur)
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = 'softmax')

CNN_MNIST
NN_MNIST#ancien model beaucoup plus de parametre, environ 10 fois plus de paramètres

(28-3)/1 + 1

# On a besoin des images pas vectoris
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train[1,,]# on peut voir l'image à partir de la matrice

y_train <- to_categorical(y_train , 10)
y_test <- to_categorical(y_test , 10)

CNN_MNIST %>% compile(loss = "categorical_crossentropy",
                     optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

hist <- CNN_MNIST %>% fit(x_train , y_train , epochs = 30, batch_size = 128,
                                     validation_split = 0.2)
plot(hist , smooth = FALSE)

y_test <- mnist$test$y
y_test


pred = as.array(CNN_MNIST %>% predict(x_test) %>% k_argmax())
CNNPrec <- Prec(pred,y_test)

#comparaison
CNNPrec
NNPrec
RFPrec

## Exemple sur CIFAR-10
cifar10 <- dataset_cifar10()

?dataset_cifar10

x_train <- cifar10$train$x/255
x_test <- cifar10$test$x/255
y_train <- cifar10$train$y
y_test <- cifar10$test$y

dim(x_train)

y_train <- to_categorical(y_train , 10)
y_test <- to_categorical(y_test , 10)


# Initialize sequential model
CNN_CIFAR <- keras_model_sequential()


CNN_CIFAR <- CNN_CIFAR %>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu', input_shape = c(32,32,3)) %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_conv_2d(filters = 16, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 10, activation = 'softmax')

CNN_CIFAR

# Pourquoi tout ces choix là ? 
# Difficile à justifier, je recommande partir d'un exemple et l'adapter à vos besoin

CNN_CIFAR %>% compile(loss = "categorical_crossentropy",
                      optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

hist <- CNN_CIFAR %>% fit(x_train , y_train , epochs = 50, batch_size = 256,
                          validation_split = 0.1)
                         
CNN_CIFAR %>% evaluate(x_test, y_test)

#exercice faire une foret aléatoire avec cifar et comparé avec un réseau de neurones de convolution
