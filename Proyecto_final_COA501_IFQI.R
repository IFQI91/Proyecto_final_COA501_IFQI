##Proyecto final
##COA-501 Herramientas de cómputo para investigadores (R y Python)
## Alumno: Ivan Fermin Quiroz Ibañez

#borrar objetos
rm(list = ls())

#directorios
dir()
dir.create(file.path("Figuras","subcarpeta"), recursive = T)
dir.create(file.path("Bases","subcarpeta"), recursive = T)



base <- read.csv("ABCPE_MIXTO_B1_R_coordenadas.csv")
summary(base)
str(base)

#matriz de diagramas de dispersion
pairs(base[,c(5,6,7,9,10,12,18,19,22,24,28:30,32)])

library(GGally)
ggpairs(base[,c(5,6,7,18,19,22,24,28:30)], 
        title="correlograma con ggpairs") 

psych::pairs.panels(base[,c(5,6,7,18,19,22,24,28:30)])

base2<- na.omit(base[,c(14,2,5,6,7,18,19,22,24,28:30)])

#Análisis de componentes principales 
pca <- prcomp(base2[,-c(1,2)], scale = T, center=T)
summary(pca)

par(mfrow = c(1,3))
plot(pca)
biplot(pca, main="PCA biplot")

colores <- function(vec){
  # la funci?n rainbow() devuelve un vector que contiene el n?mero de colores distintos
  col <- rainbow(length(unique(vec)))
  return(col[as.numeric(as.factor(vec))])
}

plot(pca$x[,c(1, 2)], col = colores(base$Bloque),
     pch = 19, 
     xlab = "PC1", 
     ylab = "PC2", main="PCA por Transparencia")

dev.off()

#biplot
library(ggplot2)
plt <- ggplot(base2, aes(x = pca$x[,2], y = pca$x[,1], colour = as.factor(base2$Anio))) +
  geom_point(size=3) +
  ggtitle("PCA por Edad del follaje")
plt


  ###########################Random Forest################################

library(randomForest)
library(caret)

#Getting Data
str(base)

base_rf <- na.omit(base[,c(2,5,6,7,18,19,22,24,28:30)])

base_rf$Bloque <- as.factor(base_rf$Bloque)
table(base_rf$Bloque)




#Data Partition
set.seed(123)
ind <- sample(2, nrow(base_rf), replace = TRUE, prob = c(0.7, 0.3))
train <- base_rf[ind==1,]
test <- base_rf[ind==2,]

#Random Forest in R
rf <- randomForest(Bloque~., data=train, proximity=TRUE) 
rf

#Confusion Matrix and Statistics
p1 <- predict(rf, train)
confusionMatrix(p1, train$Bloque)
(tab1 <- table(p1, train$Bloque))
1 - sum(diag(tab1)) / sum(tab1)
#error del 0%


p2 <- predict(rf, test)
confusionMatrix(p2, test$Bloque)
(tab2 <- table(p2, test$Bloque))
1 - sum(diag(tab2)) / sum(tab2)
#error del 0%



#Error rate of Random Forest
plot(rf)

#Tune mtry (Número de variables aleatorias utilizadas en cada árbol)
t <- tuneRF(train[,-1], train[,1],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 5,
            trace = TRUE,
            improve = 0.05)
#mtry=6

#No. of nodes for the trees
hist(treesize(rf),
     main = "No. of Nodes for the Trees",
     col = "blue")
#media de 60 árboles

#Variable Importance
varImpPlot(rf,
           sort = T,
           n.var = 10,
           main = "Top 10 - Importancia de Variables")
importance(rf)

#Partial Dependence Plot
partialPlot(rf, train, Y_UTM, "Alta")



###############################Naive Bayes#################################

#clasificacion en datos poco correlacionados
#https://www.r-bloggers.com/2021/04/naive-bayes-classification-in-r/
#Para segmentar imagenes (severidad)
#https://plantcv.readthedocs.io/en/latest/tutorials/machine_learning_tutorial/
library(naivebayes)
library(dplyr)
library(ggplot2)


base_nb <- na.omit(base[,c(2,5,6,7,18,19,22,24,28:30)])

#Dplyr
base_nb$Bloque <- as.factor(base_nb$Bloque)
table(base_nb$Bloque)

base_nb %>%
  ggplot(aes(x=base_nb$Bloque
             , y=base_nb$Sevmed, fill = base_nb$Bloque)) +
  geom_boxplot() +theme_bw()+stat_summary(fun="mean")+
  ggtitle("Box Plot: Severidad por Transparencia")


#particion de datos
set.seed(1234)
ind <- sample(2, nrow(base_nb), replace = T, prob = c(0.7, 0.3))
train_nb <- base_nb[ind == 1,]
test_nb <- base_nb[ind == 2,]



nb <- naive_bayes(Bloque ~ ., data = train_nb, usekernel = T) 
nb 

plot(nb) 

#Prediccion
p <- predict(nb, train_nb, type = 'prob')
head(cbind(p, train_nb))

#Confusion Matrix – train data
p1 <- predict(nb, train_nb)
(tab1 <- table(p1, train_nb$Bloque))

1 - sum(diag(tab1)) / sum(tab1) #error del 35%

#Confusion Matrix – test data
p2 <- predict(nb, test_nb)
(tab2 <- table(p2, test_nb$Bloque))
p2

1 - sum(diag(tab2)) / sum(tab2) #error del 51%

library(caret)
confusionMatrix(p2,test_nb$Bloque)

   ##############################K-NN##################################

#https://rpubs.com/JairoAyala/601703

library(kknn)

base_knn <- na.omit(base[,c(2,5,6,7,9,10,12,18,19,22,24,28:30,32)])

base_knn$Bloque <- as.factor(base_knn$Bloque)
table(base_knn$Bloque)


set.seed(2020)
muestra <- sample(1:624, 437)
train_knn <- base_knn[muestra,]#70%
test_knn<- base_knn[-muestra,]#30%
dim(train_knn)[1]
dim(test_knn)[1]

knn <- train.kknn(Bloque~ ., data = train_knn, kmax = 9)
knn

entre <- predict(knn, train_knn[,-1])
tt  <- table(train_knn[,1],entre)
tt

precision <- (sum(diag(tt)))/sum(tt)
precision

#precisión del 100 % en datos de entrenamiento


#Precisión test de prueba
pred    <- predict(knn, test_knn[,-1])
table   <- table(test_knn[,1],pred)
table


clas    <- (sum(diag(table)))/sum(table)
clas

#Precisión del 53% de datos de prueba

#matriz de confusion con la prueba

library(caret)
confusionMatrix(pred,test_knn$Bloque)




         ######################Curvas ROC#########################

par(mfrow = c(1,3))

#Random forest

# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf,test[,-1],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
test$Bloque <- as.factor(test$Bloque)
classes <- levels(test$Bloque)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(test[,1]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve-Random Forest",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve- Random Forest",col=pretty_colours[i],add=TRUE)
    abline (a = 0, b = 1, lty="dotted", lwd=2) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}


#Naive Bayes (ROC)

# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(nb,test_nb[,-1],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
test_nb$Bloque <- as.factor(test_nb$Bloque)
classes <- levels(test_nb$Bloque)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(test_nb[,1]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve-Naive Bayes",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve-Naive Bayes",col=pretty_colours[i],add=TRUE)
    abline (a = 0, b = 1, lty="dotted", lwd=2) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}



#KNN (ROC)

# Validation set assessment #2: ROC curves and AUC
# Needs to import ROCR package for ROC curve plotting:
library(ROCR)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(knn,test_knn[,-1],type="prob")
# Use pretty colours:
pretty_colours <- c("#F8766D","#00BA38","#619CFF")
# Specify the different classes 
test_knn$Bloque <- as.factor(test_knn$Bloque)
classes <- levels(test_knn$Bloque)
# For each class
for (i in 1:3)
{
  # Define which observations belong to class[i]
  true_values <- ifelse(test_knn[,1]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve - KNN",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve -KNN",col=pretty_colours[i],add=TRUE)
    abline (a = 0, b = 1, lty="dotted", lwd=2) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}

dev.off()



#guardar espacio de trabajo
save.image("Proyecto_final_COA501_IFQI.RData")







