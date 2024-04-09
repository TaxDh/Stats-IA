#Brouillon analyse 2

set.seed(2024)
data <- read.table("TUANDROMD.csv", sep = ",", header = T)
dim(data)
table(data$Label)

#je ne comprends pas vraiment pourquoi, au moment de ma foret je me retrouve avec des données manquantes... mais ça règle mon problème ici
#data <- na.omit(data)

#899/(3565+899)

head(data)
colnames(data)

source("Fonctions.R")

#Pour simplfier mes analyses, je vais transformer la variable Label qui est un string en 0 et 1

# Recoder Label en 0 pour "goodware" et 1 pour "malware"
data$Label <- ifelse(data$Label == "goodware", 0, 1)#goodware ==0 et malware ==1

# Vérifier que le recodage s'est bien passé
table(data$Label)


data$Label <- factor(data$Label)
class(data$Label)

#On sépare les goodwares des malwares
goodware_data <- data[data$Label == 0,]
malware_data <- data[data$Label == 1,]

#on split les données séparé par goodware/malware
all_goodware <- train_test_split(0.8,goodware_data)
all_malware <- train_test_split(0.8,malware_data)

ent_goodware_data <- all_goodware[[1]]
tes_goodware_data <- all_goodware[[2]]

ent_malware_data <- all_malware[[1]]
tes_malware_data <- all_malware[[2]]

#On les merge
train_data <- rbind(ent_goodware_data, ent_malware_data)
test_data <- rbind(tes_goodware_data, tes_malware_data)

#install.packages("randomForestSRC")
library(randomForestSRC)
rf <- rfsrc(Label~.,data=train_data)
VI <- holdout.vimp(Label~.,data=train_data, ntree=10000)
#VI$importance

#plot(rf)


##################################################
pred_labels <- predict(rf, test_data)$predicted
# Assurez-vous que les niveaux de pred_labels correspondent à ceux de test_data$Label
pred_labels <- factor(pred_labels, levels = levels(test_data$Label))

# Calculez ensuite la précision
Precision <- Prec(pred_labels, test_data$Label)

# Affichez la précision
print(Precision)

##################################################



# Faire des prédictions sur l'ensemble de test
rfsrc_pred <- predict(rf, test_data)

# L'erreur de classification est directement fournie dans l'objet de prédiction
classification_error <- rfsrc_pred$err.rate

# La précision est 1 moins l'erreur de classification pour la classe d'intérêt (dans votre cas, probablement la deuxième classe)
precision <- 1 - classification_error[1, "OOB"]

# Affichage de la précision
print(precision)









pred <- predict(rf,test_data)
Precision <- Prec(pred,test_data$Label)
