set.seed(2024)
data <- read.table("TUANDROMD.csv", sep = ",", header = T)
#dim(data)
#table(data$Label)

#je ne comprends pas vraiment pourquoi, au moment de ma foret je me retrouve avec des données manquantes... mais ça règle mon problème ici
data <- na.omit(data)


source("Fonctions.R")

#Pour simplfier mes analyses, je vais transformer la variable Label qui est un string en 0 et 1

# Recoder Label en 0 pour "goodware" et 1 pour "malware"
data$Label <- ifelse(data$Label == "goodware", 0, 1)#goodware ==0 et malware ==1

# Vérifier que le recodage s'est bien passé
table(data$Label)


data$Label <- factor(data$Label)
class(data$Label)


####################################donnees_ent <- donnees


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


#Faisons notre foret aléatoire

#install.packages('randomForest')
library(randomForest)

p <- ncol(train_data)-1

#il y avait un problème dans mon train_data et mon test_data
#train_data$Label <- droplevels(train_data$Label)
#table(train_data$Label)
#test_data$Label <- droplevels(test_data$Label)
#table(test_data$Label)


rf <- randomForest(Label~.,data=train_data,importance=TRUE, ntree=1000,mtry=p, replace=FALSE)

#On vérifie la précision
pred <- predict(rf,test_data)
Precision <- Prec(pred,test_data$Label)

Precision#une assez bonne prédiction de 0.982

importance(rf,type=1)#plus c'est haut, plus c'est important
importance(rf,type=2)

#on va les trier:
type1 <- importance(rf, type = 1)
ordre_type1 <- type1[order(-type1), , drop = FALSE] # tri décroissant
print(ordre_type1)

#Etrangeté
#MODIFY_PHONE_STATE -1.679849


type2 <- importance(rf, type = 2)
ordre_type2 <- type2[order(-type2), , drop = FALSE] # tri décroissant
print(ordre_type2)

plot(rf)#vert-r



