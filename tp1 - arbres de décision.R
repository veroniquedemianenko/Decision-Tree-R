rm(list=ls())

library(tseries)
library(zoo)
library(forecast)

library(caret)
library(e1071)
library(rpart)

View(read.csv("~/Desktop/4 wine.csv"))
wine <- read.csv("~/Desktop/4 wine.csv", sep = ";", dec= ".")

View(wine)

#1) Analyse exploratoire descriptive

## a) calcul des différentes moyennes

moy <- colMeans(wine, na.rm=TRUE)
moy

ecart_type <- sapply(wine, function(col) sd(col, na.rm= TRUE))
ecart_type

for (col in colnames(wine)) {
  boxplot(wine[[col]], main = paste("Boxplot de", col))
}

# voir commentaire dans le compte-rendu

ptm <- proc.time()



# decoupage partie train et partie test
set.seed(1234)
index <- sample(1:nrow(wine),round(0.70*nrow(wine)))
train <- wine[index,]
test <- wine[-index,]
nrow(train)
nrow(test)

# On sélectionne la colonne "class" comme étant la variable dépendante, toutes les autres sont indépendantes.

# Construction de l'arbre


#fulltree<-rpart(class~Alcohol+Malicacid+Ash+Alcalinity_of_ash+Magnesium+Total_phenols+Flavanoids+Nonflavanoid_phenols+Proanthocyanins+Color_intensity+Hue+X0D280_0D315_of_diluted_wines+Proline, data=wine, method="class")

fulltree<-rpart(class~Alcohol+Malicacid+Ash+Alcalinity_of_ash+Magnesium+Total_phenols+Flavanoids+Nonflavanoid_phenols+Proanthocyanins+Color_intensity+Hue+X0D280_0D315_of_diluted_wines+Proline, data=train, method="class")
rpart.plot::rpart.plot(fulltree)


min_ind <- which.min(fulltree$cptable[, "xerror"])
min_cp <- fulltree$cptable[min_ind, "CP"]
pruned_fit <- rpart::prune(fulltree, cp = min_cp)
rpart.plot::rpart.plot(fulltree, main = "cv")
rpart.plot::rpart.plot(pruned_fit, main = "élagué")


summary(fulltree)
printcp(fulltree) # Affiche les points de coupe optimaux de l'arbre de décision.
varImp(fulltree) # Calcule et affiche l'importance des variables dans l'arbre de décision.



# Classifications :

## Prédiction

predicted <- predict(pruned_fit, test, type="class")
error1=sum(test$class != predicted)/length(predicted) # taux d'erreur 
predicted <- ordered(predicted, levels = c(2, 1))
actual<- ordered(test$class, levels = c(2, 1))
mc=table(predicted,actual, dnn=c( "Predicted","reelle")) #matrice de confusion
mc
error2=1-((mc[1,1]+mc[2,2])/sum(mc)) #mesure de l'exactitude globale du modèle
error2
error1

# les deux erreurs sont nulles, le modèle est donc bien optimal
# de plus, on observe dans la matrice de confusion que toutes les valeurs (32) de test ont été attribuées dans les cases True (positive et negative).
# les échantillons ont donc tous été correctement classés dans le modèle.

sensitivity(mc)
specificity(mc)
accuracy_Test <- sum(diag(mc)) / sum(mc)
accuracy_Test

# on obtient 1 pour les 3 tests, ce qui montre une performance parfaite sur l'ensemble de test.


## ROC


####Predictions as probability
Predprob <- predict(fulltree, newdata = test,type = "prob")
Predprob = as.data.frame(Predprob)
# taking the cut-off probability 50%
pred.DT <- ifelse(Predprob$"1" > 0.5, 1, 0)
# saving predicted vector as factor 
Pred <- as.factor(pred.DT)
# ordering the vectors
Pred <- ordered(Pred, levels = c(2, 1))
Actual <- ordered(test$class,levels = c(2, 1))
# making confusion matrix
cm1 <-confusionMatrix(table(Pred,Actual))
cm1

# loading the package for ROC curve and AUC
library(ROCR)
Prediction <- prediction(Predprob[2],test$class)
performance <- performance(Prediction, "tpr","fpr")
# plotting ROC curve



plot(performance,main = "ROC Curve",col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")

# area under curve
aucDT <- performance(Prediction, measure = "auc")
aucDT <- aucDT@y.values[[1]]
aucDT



