library(ggplot2)
library(dplyr)
library(caret)
library(ranger)
library(rpart)

# Loading data from the csv file

mel_file_path = 'myDataSets/housing_data.csv'
mel <- read.csv(mel_file_path)

head(mel)
         
str(mel)       

# removing rows with NA
colSums(is.na(mel))
mel <- na.omit(mel)

# removing identifying columns
mel <- mel %>%
  select(-c(Suburb, Address, Method, SellerG, Date, Postcode,Lattitude, Longtitude, Propertycount))

head(mel)

mel.onlyNumeric <- mel[,unlist(lapply(mel, is.numeric))]

# Plot the correlation matrix pairs, to determine the degrees of correlation
cor(mel.onlyNumeric[complete.cases(mel.onlyNumeric),])

# Plot of Price Distribution of Home type
ggplot(mel, aes(Type, Price)) +
  geom_boxplot(outlier.colour = "black") + 
  scale_x_discrete(labels = c('Houses','Townhouses','Units')) +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  xlab("Type of Home") +
  ylab("Price") +
  ggtitle("Price Distribution of Home Type")

# Plotting the average prices of homes
mel %>%
  filter(YearBuilt != 1196) %>%
  select(Price, Type, YearBuilt) %>%
  group_by(YearBuilt, Type) %>%
  summarise(Mean = mean(Price)) %>%
  ggplot(aes(x=YearBuilt, y=Mean, color=Type)) +
  geom_smooth(method = 'loess') +
  geom_line() +
  scale_color_discrete(name = "Type of Home", 
                       labels=c("House", "Townhouse", "Unit")) +
  xlab("Year") +
  ylab("Price") +
  ggtitle("Average Price of Homes")
# From the above plot we can infer that there has been a decrease in average prices of houses, with houses increasing in price around 2000.
# Averages prices of townhouses and units have stayed relatively stable.

ggplot(mel, aes(Regionname, Price)) +
  geom_boxplot(outlier.colour = "black") +
  scale_y_continuous(breaks=seq(0,10000000,1250000)) +
  theme(legend.position = "none") +
  xlab("Region") +
  ylab("Price") +
  ggtitle("Region Price Distributions") +
  coord_flip()
# From the plot we can infer that homes in the Southern Metropolitan region has the highest average price.

# Plot of Council Area Frequencies of house types
mel %>%
  group_by(CouncilArea) %>%
  summarise(Count = n()) %>%
  ggplot(aes(reorder(CouncilArea, Count), Count)) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  coord_flip() +
  xlab("Council Area") +
  ylab("Count") +
  ggtitle("Council Area Frequencies")
# From the plot we can infer that few council areas have larger number of homes than the others.

# To reduce the no of levels for this variable we can convert levels with less than 100 counts to others

mel$BuildingAge <- 2020 - mel$YearBuilt
mel$BuildingAge <- as.integer(mel$BuildingAge)
head(mel)

#Remove outliers from the above feature

mel <- mel %>%
  filter(BuildingAge != 824, Price != 9000000, BuildingArea < 3000)

str(mel$Price)
# Reducing Category Levels
temp <- mel %>%
  select(CouncilArea) %>%
  group_by(CouncilArea) %>%
  count(CouncilArea) %>%
  arrange(desc(n)) %>%
  filter(n <= 103)

mel$CouncilArea <- as.character(mel$CouncilArea)

mel <- mel %>%
  mutate(CouncilArea = replace(CouncilArea, CouncilArea %in% temp[1]$CouncilArea, 
                               "Other"))

mel$CouncilArea <- as.factor((mel$CouncilArea))

# Creating Dummy Variables

dummy_obj <- dummyVars(~ Type + Regionname + CouncilArea, data = mel, 
                       sep = ".", levelsOnly = TRUE)
dummies <- predict(dummy_obj, mel)

mel <- cbind(mel, dummies)
mel <- mel %>%
  select(-c(Type, Regionname, CouncilArea))

# Dividing the dataset into Train and Test data
set.seed(444)

data <- mel %>%
  select(-c(Bedroom2, YearBuilt))
data$Price <- log(data$Price)
colnames(data) <- make.names(colnames(data))

train_ind <- createDataPartition(y = data$Price, p = 0.8, list = FALSE)

training <- data[train_ind,]
testing <- data[-train_ind,]

# Regression Tree

dt_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
fitted_dt <- train(Price~., data = training,
                   trControl = dt_ctrl,
                   tuneLength = 10,
                   method = "rpart",
                   metric = "Rsquared")
fitted_dt

#Plot of Variable Importance
plot(varImp(fitted_dt))

#Plot the Model
plot(fitted_dt$finalModel)
text(fitted_dt$finalModel)

#Prediction on test data
predict_dt <- predict(fitted_dt, testing)
RMSE_test <- postResample(testing$Price, predict_dt)
#RMSE AND R SQUARED FOR TEST SET
RMSE_test



predict_dt <- predict(fitted_dt, training)
RMSE_train <- postResample(training$Price, predict_dt)
#RMSE AND R SQUARED FOR TRAIN SET
RMSE_train

# Both training(0.6539110) and testing(0.6426019) RSquared values are relatively close, which infers a good fitted model.

## RANDOM FOREST MODEL

rf <- trainControl(method = "cv", number = 5)
fitted_rf <- train(Price~., data = training,
                   trControl = rf,
                   tuneLength = 10,
                   method = "ranger",
                   importance = "permutation",
                   metric = "Rsquared",
                   verbose = TRUE)
fitted_rf
print(fitted_rf$finalModel)

#Plot of Variable Importance
plot(varImp(fitted_rf))
plot(fitted_rf)

fitted_rf$pred

ggplot(data=training,aes(x=log(Landsize), y=log(Price)) ) + 
  geom_point(color = "steelblue")

#Prediction on test dataset
predict_rf <- predict(fitted_rf, testing)
RMSE_rf <- postResample(testing$Price, predict_rf)
#RMSE AND R SQUARED FOR TEST SET
RMSE_rf


predict_rf <- predict(fitted_rf, training)
RMSE_rf_train <- postResample(training$Price, predict_rf)
#RMSE AND R SQUARED FOR TRAIN SET
RMSE_rf_train

Gsummary(sqrt(training$Price - predict_rf)^2)

## RSquared values for training and testing data are realtively similar.

## And considering both decision tree(0.6426019) and random forest(0.8825461) model, Random Forest
# produces better model for the given data.

rf1 <- randomForest::randomForest(Price~., ntree=500, data=training)
plot(rf1)
print(rf1)

# Variable Importance
randomForest::varImpPlot(rf1,  
           sort = T,
           n.var=15,
           main="Top 10 - Variable Importance")
# BuildingYear is by the most important variable. Weatern and Northerm Metropolitan has zero impact on the results.

#Variable Importance
var.imp = data.frame(importance(rf1,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDeczzreaseGini,decreasing = T),])

