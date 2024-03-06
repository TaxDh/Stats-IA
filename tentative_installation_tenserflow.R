install.packages("remotes")
remotes::install_github("rstudio/tensorflow")

reticulate::install_python()

################### cette partie ne marche pas
#library(tensorflow)
#install_tensorflow(envname = "r-tensorflow")
###########################


#bogue ici
install.packages("keras")
library(keras)
install_keras()
#bogue ici