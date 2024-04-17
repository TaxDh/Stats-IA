#######################################
# Laboratoire R - Scéance 12
# 17 avril 2024
# Retour sur keras
# Autoencodeur et visualisation 
#######################################

# On prépare notre directory ainsi que nos fonctions R.
#setwd()
library(keras)
library(tidyr)
library(ggplot2)
library(tensorflow)#keras est souvent insuffisant, il vaut mieux appeler tensorflow egalement
tf$constant("Hello Tensorflow!")


# Load the data
data <- dataset_mnist()

# Prepare the data
x_train <- data$train$x
x_test <- data$test$x
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
x_train <- x_train / 255
x_test <- x_test / 255





# 2 réseaux de neurones séparés
#a1 tablette
# on s'en sert surtout pour obtenir des données, 
# Define the encoder
encoder <- keras_model_sequential() %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>%
  layer_dense(units = 64, activation = 'relu')%>%
  layer_dense(units = 2)

#a2 tablette
# Define the decoder
decoder <- keras_model_sequential() %>%
  layer_dense(units = 64, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 256, activation = 'relu')%>%
  layer_dense(units = 784, activation = 'sigmoid')

# Connect them to create the autoencoder
autoencoder <- keras_model(inputs = encoder$input, outputs = decoder(encoder$output))


# Compile the model
autoencoder %>% compile(optimizer = 'adam', loss = 'MSE')# adam c'est une descente du gradient "fancy"



# Train the model
autoencoder %>% fit(x_train, x_train, epochs = 50, batch_size = 256, validation_data = list(x_test, x_test))


z = encoder(x_train); dim(z)
plot(x=z[,1],y=z[,2],col=data$train$y)

image_1  <- as.data.frame(matrix(data=x_train[2, ],nrow=28,byrow=TRUE))#si l'on change x_train[2, ] pour x_train[3, ] c'est le nb 4
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




#code de la première image une fois qu'elle a été compressée
z[1:10,]

#reconstruction
xtilde = autoencoder(x_train[1:10, ])
#OU
xtilde = decoder(z[1:10,])

dim(xtilde)

image_1  <- as.data.frame(matrix(data=xtilde[2, ],nrow=28,byrow=TRUE))
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

# On se souvient
plot(x=z[,1],y=z[,2],col=data$train$y)

# Essayons de voir le grpahique suivant mais sur x et non z:

par( mfrow= c(10,10),mai = c(0.01, 0.01, 0.01, 0.01) )
grid<- seq(-20,20,length.out=10)# était -30, 30 et là c'est -20,20
zs <- expand.grid(grid,grid)
xtilde = decoder(as_tensor(zs))


for (idx in 1:100) { 
  im <- matrix(data=xtilde[idx, ],nrow=28,byrow=T)
  im <- t(apply(im, 2, rev)) 
  image(1:28, 1:28, im, col=gray((0:255)/255),xaxt='n',yaxt='n')
}
