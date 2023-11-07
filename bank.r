#install.packages("")
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(ROCR)
#####library(tidyverse)
#####library(e1071)
#####library(rattle)
#####library(c50)
#####library(pROC)


# wczytanie danych
bank_data <- read.csv("bank-full.csv",sep=';')
bank_data$y <- as.factor(bank_data$y)

#info nt zmiennych w zbiorze danych
str(bank_data)

#podzial zbioru na trening i test
set.seed(1717)
inTrain <- createDataPartition(y=bank_data$y ,p=0.75,list=FALSE)
bank_train <- bank_data[inTrain,]
bank_test <- bank_data[-inTrain,]

# udzial klas zmiennej objasnianej y
prop.table(table(bank_train$y))
prop.table(table(bank_test$y))


################## ANALIZA DANYCH ######################

# rozklady zm

ggplot(bank_train) + 
  geom_bar( aes(x = y), fill = "steelblue")

ggplot(bank_train) + 
  geom_bar( aes(x = housing), fill = "steelblue")

# boxplot dla zm ciaglej
ggplot(bank_train) + 
  geom_boxplot( aes(x = balance, y=y), fill = "steelblue")

# wykres pokazujacy udzial danej klasy y
ggplot(bank_train, aes(x = poutcome, fill = y)) +
  geom_bar(position = "fill") +
  theme_classic()

# mozna tez tak
ggplot(bank_train, aes(x = age, fill = contact)) +
  geom_bar(position = "fill") +
  theme_classic()

# age
ggplot(bank_train) + 
  geom_bar( aes(x = age), fill = "steelblue")
ggplot(bank_train, aes(x = age, fill = y)) +
  geom_bar(position = "fill") +
  theme_classic()
# wsrod osob starszych klasa pozytywna jest czesciej, ale jest ich bd malo w probie


#macierz korelacji (zmiennych ilosciowych)
cor(bank_train[,c(1,6,10,12,13,14,15)])


################## MODELE ######################

#DRZEWO

#szukam najlepszej wart hiperparametru maxdepth
# for(i in 1:10) {
#   tree <- rpart(y ~.,
#                 data = bank_train,
#                 method = "class",
#                 control = list(maxdepth = i)) 
#   predicted <- factor(predict(tree, new = bank_test, type = "class"))
#   expected <- factor(bank_test$y)
#   example <- confusionMatrix(data=predicted, reference = expected, positive='yes')
#   print(example)
# }

# maxdepth = 6 wartosc optymalna


# ... i oto pierwsze drzewo:
tree1 <- rpart(y ~.,
                    data=bank_train,
                    method="class",
                    control = list(maxdepth = 6))
rpart.plot(tree1)
rpart.plot(tree1, under=FALSE, tweak=1.3, fallen.leaves = TRUE)


predicted_tree1 <- factor(predict(tree1, new = bank_test, type = "class"))
expected_tree1 <- factor(bank_test$y)
evaluate_tree1 <- confusionMatrix(data=predicted_tree1, reference = expected_tree1, positive='yes')
evaluate_tree1

# faworyzuje kl negatywna

#ROC
prognoza_ciagla_tree1 <- as.vector(predict(tree1, newdata = bank_test)[, 2])
plot(performance(prediction(prognoza_ciagla_tree1, bank_test$y), "tpr", "fpr"), lwd = 2, colorize = F) 
#AUC
(auc_tree1 <- performance(prediction(prognoza_ciagla_tree1, bank_test$y), "auc")@y.values[[1]])


### MODELE ZE ZBILANSOWANA PROBA W TRENINGOWYM

# zbilansowac probe usuwajac losowo obserwacje z klasa 'no' (downsampling)
set.seed(1717)
balanced <- downSample(select(bank_train, age:poutcome),bank_train$y)


# LAS LOSOWY

#balanced_dummy <- dummy_cols(
#   balanced,
#   select_columns = NULL,
#   remove_first_dummy = FALSE,
#   remove_most_frequent_dummy = FALSE,
#   ignore_na = FALSE,
#   split = NULL,
#   remove_selected_columns = FALSE
# )

rf <- randomForest(Class ~., data=balanced)

#istotnosc zm
varImpPlot(rf) # najbardziej istotne sa duration i month...
?predict

rf_predicted <- factor(predict(rf, new = bank_test, type = "class"))
rf_expected <- factor(bank_test$y)
rf_evaluated <- confusionMatrix(data=rf_predicted, reference = rf_expected, positive='yes')
rf_evaluated
# dzieki zbilansowanej mamy wysoka czulosc kosztem precyzji
# specyficznosc wciaz nie jest niska

#ROC
prognoza_ciagla_rf <- as.vector(predict(rf, newdata = bank_test, type = "prob")[, 2])
plot(performance(prediction(prognoza_ciagla_rf, bank_test$y), "tpr", "fpr"), lwd = 2, colorize = F)

#AUC
performance(prediction(prognoza_ciagla_rf, bank_test$y), "auc")@y.values[[1]]
# wyzsze AUC - ponad 90%

#Krzywa LIFT opadajaca
forecast <- predict(rf, newdata = bank_test, type = "prob")[,2]
plottingData <- prediction(forecast, bank_test$y)
lift2 <- performance(plottingData, 'lift','rpp')
plot(lift2)

# #ROC
# prognoza_ciagla_rf2 <- as.vector(predict(rf2, newdata = bank_test, type = "prob")[, 2])
# plot(performance(prediction(prognoza_ciagla_rf2, bank_test$y), "tpr", "fpr"), lwd = 2, colorize = F)
# #AUC
# (auc_tree <- performance(prediction(prognoza_ciagla_rf2, bank_test$y), "auc")@y.values[[1]])
# forecast <- predict(rf, newdata = bank_test, type = "prob")[,2]
# plottingData <- prediction(forecast, bank_test$y)
# # krzywa ROC - potrzebuje "ciaglej" prognozy
# plot(performance(plottingData,"tpr","fpr"),lwd=2)


# DRUGIE DRZEWO

# na zbilansowanej probie
# + wykluczamy month, day , poutcome i pdays z modelu

klon_palmowy <- rpart(Class ~duration+housing+contact+previous,
              data = balanced,
              method = "class",
)
rpart.plot(klon_palmowy)

klon_predicted <- factor(predict(klon_palmowy, new = bank_test, type = "class"))
klon_expected <- factor(bank_test$y)
klon_evaluated <- confusionMatrix(data=klon_predicted, reference = klon_expected, positive='yes')
klon_evaluated
# duzo wyzsza czulosc kosztem precyzji w porownaniu z pierwszym drzewem
# pytanie co jakie sa cele biznesowe tego modelu

# ROC
prognoza_ciagla_klon <- as.vector(predict(klon_palmowy, newdata = bank_test)[, 2])
plot(performance(prediction(prognoza_ciagla_klon, bank_test$y), "tpr", "fpr"), lwd = 2, colorize = F) 
# AUC
auc_klon <- performance(prediction(prognoza_ciagla_klon, bank_test$y), "auc")@y.values[[1]]
auc_klon
