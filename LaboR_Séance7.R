#######################################
# Laboratoire R - Scéance 7
# 6 Mars 2024
# Réseau de neuronne simple pour la régression
# Réseau de neuronne simple pour la classification
# Keras, python et TensorFlow
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

# On va entrainer un NN dans un *mauvais* scénario; Hitters

# On prépare notre directory ainsi que nos fonctions R.
setwd("C:/Users/beaulac_Ce/Dropbox/UQAM/A2023/STT3030-ACT6100/Mon Cours/Lecture7")
source('Utility.R')

library(ISLR2)

Data <- na.omit(Hitters)
n <- nrow(Data)

allData <- train_test_split(0.8,Data)
ent_data <- allData[[1]]
tes_data <- allData[[2]]


lfit <- lm(Salary~., data = ent_data)
pred <- predict(lfit , tes_data)
lmEQM <- EQM(pred,tes_data$Salary)

# dans le lab il ne prend l'erreur quadratique mais bien l'erreur absolue
mean(sqrt((pred-tes_data$Salary)^2))

# Random forest
library(randomForest)

lfit <- randomForest(Salary~., data = ent_data)
pred <- predict(lfit , tes_data)
rfEQM <- EQM(pred,tes_data$Salary)




# il est standard de normaliser les prédicteurs, c'est ce que fait scale ici
# le -1 c'est pour enlever la colonne de 0 créé par model.matrix
ent_x <- scale(model.matrix(Salary~.-1, data = ent_data))
tes_x <- scale(model.matrix(Salary~.-1, data = tes_data))

#install.packages("dplyr")    # alternative installation of the %>%
library(dplyr)  

# Définition de l'architecture du réseau de neurone:

# on initialise le réseau de neurone, un layer_dense crée donc les paramètre à apprendre 
NN <- keras_model_sequential()
NN %>%
  layer_dense(units = 50, activation = 'relu',input_shape=ncol(ent_x)) %>%
  layer_dense(units = 1)

# Observons et tentons de comprend
NN
ncol(tes_x)*50 #Sont ou les 50 paramètres de plus ?

# On détermine la fonction objective et l'algorithme d'optimisation
NN %>% compile(loss = "mse",optimizer = 'adam')
hist <- NN %>% fit(ent_x,ent_data$Salary,epochs=1000)

# On peut ajouter des metriques additionnels et un ensemble test!
NN <- keras_model_sequential()
NN %>%
  layer_dense(units = 50, activation = 'relu',input_shape=ncol(ent_x)) %>%
  layer_dense(units = 1)
NN %>% compile(loss = "mse",optimizer = 'adam',metrics = list("mean_absolute_error"))
hist <- NN %>% fit(ent_x,ent_data$Salary,epochs=1500,batch_size=64,
                   validation_data=list(tes_x,tes_data$Salary))

# On ajoute du dropout pour prévenir le surapprentissage
NN <- keras_model_sequential()
NN %>%
  layer_dense(units = 50, activation = 'relu',input_shape=ncol(ent_x)) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)
NN %>% compile(loss = "mse",optimizer = 'adam',metrics = list("mean_absolute_error"))
hist <- NN %>% fit(ent_x,ent_data$Salary,epochs=1500,batch_size=64,
                   validation_data=list(tes_x,tes_data$Salary))

plot(hist)
pred <- predict(NN, tes_x)
NNEQM <- EQM(pred,tes_data$Salary)

lmEQM
rfEQM
NNEQM

# Combien de couche caché ? Combien de unit ? ...oooofff

# Okay, sur des données simples, il y a peu d'intérêt a utiliser
# les réseaux de neurones. 
# Allons-y sur des données plus complexes
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

image_1  <- as.data.frame(x_train[1, , ])
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
# On remarque l'activation 'softmax' pour la classification multi-catégorielle
# On utilise aussi 10 units, 1 par classe (et non 10-1=9): pourquoi ?

NN_MNIST %>% compile(loss = "categorical_crossentropy",
                      optimizer = optimizer_rmsprop (), metrics = c("accuracy"))

system.time(hist <- NN_MNIST %>% fit(x_train , y_train , epochs = 30, batch_size = 128,
          validation_split = 0.2))
plot(hist , smooth = FALSE)

y_test <- mnist$test$y
y_test

NN_MNIST %>% predict_classes(x_test) 
pred = as.array(NN_MNIST %>% predict(x_test) %>% k_argmax())
NNPrec <- Prec(pred,y_test)


# Pour MNIST, les données sont déjà divisé entre test et train
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

NNPrec
RFPrec
