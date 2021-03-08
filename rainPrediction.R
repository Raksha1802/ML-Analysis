library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(readr)
library(tidyr)
library(MVN)
library(googleVis)
library(gridExtra)
library(lm.beta)
library(leaps)
library(olsrr)

# Loading data from the csv file

aus_path = 'myDataSets/weather_data.csv'
weather <- read.csv(aus_path)

head(weather)

#Inspect the data structure
str(weather)

#Scan for missing values
sum(is.na(weather))

colSums(is.na(weather))

weather$Date <- as.Date(as.character(weather$Date))

weatherML <- weather %>% dplyr::select(-RISK_MM) %>% mutate(Date = month.abb[lubridate::month(Date)], RainTomorrow = factor(RainTomorrow, levels = c("Yes", "No")))

weatherML <- weatherML %>% na.omit()

# Dummy  Variables

dummy <- dummyVars(" ~ .", weatherML[,1:22])


weatherMLdum <- data.frame(predict(dummy, weatherML[,1:22]))

weatherMLfinal <- cbind(weatherMLdum[,1:127], RainTomorrow = weatherML$RainTomorrow)

#Now I randomly remove one dummy variable from each categorical predictor.

weatherMLfinal %>% select(-Location.Albury, -WindGustDir.WNW, -WindDir3pm.E, -WindDir9am.NNE, -RainToday.No, -DateJan)

#Check for correlated predictors
library(tidyverse)
library(caret)
library(magrittr)
library(corrplot)
library(caTools)
library(pROC)
library(ggthemes)
library(cowplot)

numeric <- map_lgl(weatherML, is.numeric)

correlations <- cor(weatherML[,numeric])

diag(correlations) <- 0

high <- apply(abs(correlations) >= 0.8, 2, any)

corrplot(correlations[high, high], method = "number")


weatherMLfinal %<>% select(-MinTemp, -MaxTemp, -Temp9am, -Pressure9am)

#Remove zero variance predictors
#Finally, I'll remove the predictors with little variance.

zero_var <- nearZeroVar(weatherMLfinal)

colnames(weatherMLfinal)[zero_var]

weatherMLfinal <- weatherMLfinal[, -zero_var]

#Logistic regression

#Prepare train and test sets
set.seed(123)

rows <- createDataPartition(weatherMLfinal$RainTomorrow, p =0.7, list = F)

ausTrain <- weatherMLfinal[rows, ]

ausTest <- weatherMLfinal[-rows, ]

#Run the model

logControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1, summaryFunction = twoClassSummary, classProbs = T, verboseIter = F, savePredictions = T)

ausLOG <- train(RainTomorrow ~ ., data = ausTrain, preProc = c("BoxCox", "center", "scale"), method = "glm", metric = "ROC", trControl = logControl)

ausLOG

#Confusion Matrix 
caret::confusionMatrix(ausLOG)

confusionMatrix(ausLOG)

#ROC Curve

predLOG <- predict(ausLOG, ausTest, type = "prob")

result.roc <- roc(ausTest$RainTomorrow, predLOG$Yes)

plot(result.roc, print.thres = "best", print.thres.best.method = "closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)

#Model prediction with new threshold and specificity from ROC Curve

rain_tomorrow <- predLOG$Yes

rain_thresh <- factor(ifelse(rain_tomorrow < 0.202, "No", "Yes"))

#Confusion Matrix
confusionMatrix(rain_thresh, ausTest$RainTomorrow)

#DEcision Tree

library(rpart)
library(rpart.plot)
library(gplots)
library(ROCR)
library(lattice)
library(ggplot2)
library(caret)
        
model<-rpart(RainTomorrow~., data= ausTrain)
prp(model, type = 2, extra = 4)

pred_class<-predict(model, ausTrain, type = "class")

#Confusion Matric Values for Decision Tree
confusionMatrix(pred_class, ausTrain$RainTomorrow, positive = "Yes")

#As we can see the accuraccy level is 83.66% and is higher than no information rate. On the other hand, our Sensitivity is too low

pred_prob<-predict(model, ausTest)

P_Test<- prediction(pred_prob[,2], ausTest$RainTomorrow)
perf<- performance(P_Test, "tpr", "fpr" )
plot(perf)

#ROC AUC Value for Decision Tree Method
performance(P_Test, "auc")@y.values

#KNN Regression
library(class)
ausTrain$RainTomorrow <- as.numeric(as.factor(ausTrain$RainTomorrow))
ausTest$RainTomorrow <- as.numeric(as.factor(ausTest$RainTomorrow))
y_predknn = knn(train = ausTrain[, -12],
                test = ausTest[, -12],
                cl = ausTrain[, 12],
                k = 5,
                prob = TRUE)

y_predknn

# Confusion Matrix Values for KNN
cm_knn = table(ausTest[, 12], y_predknn)
cm_knn
confusionMatrix(cm_knn)


