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
library(car)
library(naniar)
library(tidyverse)
# Loading data from the csv file

suicide_path = 'myDataSets/suicide_data.csv'
suicide <- read.csv(suicide_path)

head(suicide)

#Inspect the data structure
str(suicide)

#Scan for missing values
sum(is.na(suicide))

colSums(is.na(suicide))

str(suicide)
vis_miss(suicide)


#Remove NA's in the data
suicide <- na.omit(suicide)
str(suicide)

colnames(suicide)[1] = "country"
colnames(suicide)[9] = "HDI.for.year"
colnames(suicide)[10] = "gdp_for_year"
colnames(suicide)[11] = "gdp_per_capita"

# Scan for Outliers
suicide %>% boxplot(suicides_no ~ year, data = ., main="Boxplot of SuicidesRate", ylab = "Suicide Rat per 100,000", xlab="Year", col="lightblue")
grid()

suicide1 <- suicide

outliers1 <- boxplot(suicide$gdp_for_year)$out
suicide <- suicide[-which(suicide$gdp_for_year %in% outliers1),]
boxplot(suicide$gdp_for_year)
outliers2 <- boxplot(suicide$gdp_per_capita)$out
suicide <- suicide[-which(suicide$gdp_per_capita %in% outliers2),]
boxplot(suicide$gdp_per_capita)

# Transforming the data
par(mfrow = c(2,2))
hist(suicide$gdp_per_capita, main = "GDP per Capita", col = "red")
hist(log10(suicide$gdp_per_capita), main = "GDP per capita Normal", col = "lightblue")

qqnorm(suicide$gdp_per_capita, main = "QQ Plot of GDP per Capita")
qqnorm(log10(suicide$gdp_per_capita), main = "GDP per Capita Normal")

# Visualisation 1

suicide_gdp_year <- suicide %>% select(year, suicides_no, gdp_per_capita)
suicide_gdp_year1 <- aggregate(.~`year`, data = suicide_gdp_year, sum)
plot(suicide_gdp_year1, col = "darkblue")

ggplot(suicide, aes(x = log10(gdp_per_capita), y = suicides_no)) + geom_jitter(alpha = 0.3) + geom_smooth(lwd = 0.85, alpha = 0.15) + ggtitle("Scatter Plot of Suicide Rate and GDP per Capita")

ggplot(suicide, aes(x = log10(gdp_per_capita), y = suicides_no, colour = factor(year))) + geom_line(alpha=0.5) + ggtitle("Plot of Yearly Suicide Rate and GDP per Capital")

# STATISTICAL CONCLUSION

summary(suicide$suicides_no)
summary(log10(suicide$gdp_per_capita))
summary(suicide$gdp_per_capita)

fit <- lm(log10(suicide$gdp_per_capita) ~ suicide$suicides_no,data=suicide)
  
summary(lm(log10(suicide$gdp_per_capita) ~ suicide$suicides_no,data=suicide))
plot(fit)

coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table
vcov(fit) # covariance matrix for model parameters
influence(fit) # regression diagnostics

# Co-relation test
cor.test(log10(suicide$gdp_per_capita), suicide$suicides_no, method = "pearson")


# Simple Linear Regression using individual dependents

mod1 <- lm(data = suicide, suicides_no ~ suicide$HDI.for.year)
summary(mod1)
plot(mod1)
plot(hatvalues (mod1))
which.max(hatvalues (mod1))
# Turns out, HDI for year has significantly contributed to be the cause of suicide rate.
# But in a very small due to the Adjusted R-squared is only 0.02298. And the values 10753 and 3277 has highest leverage.

mod11 <- lm(data = suicide, suicides_no ~ suicide$gdp_per_capita)
summary(mod11)
plot(mod11)
plot(hatvalues (mod11))
which.max(hatvalues (mod11))
# Turns out, GDP per capita has significantly contributed to be the cause of suicide rate.
# But in a very small due to the Adjusted R-squared is only 0.01095. And the values 15655 and 4861 has highest leverage.

mod12 <- lm(data = suicide, suicides_no ~ suicide$gdp_for_year)
summary(mod12)
plot(mod12)
plot(hatvalues (mod12))
which.max(hatvalues (mod12))
# Turns out, GDP for year has significantly contributed to be the cause of suicide rate.
# But in a very small due to the Adjusted R-squared is only 0.3686. And the values 27197 and 8161 has highest leverage.

# Regression Analysis.
# mmod1 using all the Available Variable in the data frame.

mmod1 <- lm(data = suicide, suicides_no ~ (suicide$year + suicide$sex + suicide$population + suicide$suicides.100k.pop
                                           + suicide$HDI.for.year + suicide$gdp_per_capita + suicide$gdp_for_year))

summary(mmod1)
vif(mmod1)

mod2 <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year)
summary(mod2)
#When we add another variable(gdp_for_year_log), we observe the increase of Adjusted R-squared from 0.0228 to 0.3698. 
#It telling us that gdp_for_year is playing an important role in variation of suicide rate.
#It's contributed a lot. It's a massive jump.

mod3 <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year + suicide$sex)
summary(mod3)
# After adding sex variable there was no major incraese in the Adjusted R Squared value.

mod4 <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year + suicide$age)
summary(mod4)
#Difference in age also drive the number of suicide rate. From 0.3948 to 0.4103

mod5 <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year + suicide$age + suicide$generation)
summary(mod5)
# Adition of generation didint contribute much to the suicides_no, so will exclude that in the next step.

mod6 <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year + suicide$age + suicide$gdp_per_capita)
summary(mod6)

mod7 <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year + suicide$age + suicide$gdp_per_capita + suicide$suicides.100k.pop)
summary(mod7)
# There is a huge difference in Adjusted-R Sqaured value to 0.4547 on additin of suicides.100k.pop.
# We can say that those set of variables can be represent the cause of suicide rate at 45,47% variation.

#To evaluate the model whether it's doing a great job or not. One of the assumption we can check is the residual distribution. 
#We expected to be normally distributed.

ggplot(data = as.data.frame(mod7$residuals), aes(x = mod7$residuals))+
  geom_histogram(alpha = .60 , fill = "blue")
# And finally we will order the variables contributing to suicides rate from highest to lowest.


summary(lm.beta(mod7))
# we can see that gdp_for_year is the most valuable variable. we can say that as gdp per year for a country increases
# suicide rates increases too. The other two variables are suicides.100k.pop and gdp_per_capita.


#Model and accuracy check
predict.lm <- predict(mod7)
#Check Mape on train
(Mape <- mean(abs(suicide$suicides_no - predict.lm)/suicide$suicides_no))

anova(mod1, mod7)


# Stepwise Regression
model <- lm(data = suicide , suicides_no ~ suicide$HDI.for.year + suicide$gdp_for_year + suicide$age + suicide$gdp_per_capita + suicide$suicides.100k.pop)
ols_step_both_p(model)
plot(ols_step_both_p(model))

ols_step_both_p(model, details = TRUE)

# Predict suicide nos using the above variables for a year
library(caret)
# Split data into train and test
index <- createDataPartition(suicide$suicides_no , p = .70, list = FALSE)
train <- suicide[index, ]
test <- suicide[-index, ]

dim(train)

#Multiple Linear Regression
# Taining model
lmModel <- lm(suicides_no ~ train$gdp_per_capita + train$age +train$suicides.100k.pop, data = train)
# Printing the model object
print(lmModel)
summary(lmModel)
p_suicides_no1 <- predict(lmModel,train)
lm.rmse1 <- postResample(train$suicides_no, p_suicides_no1)
# R Squared Value of Train Set for Multiple Linear Regression
lm.rmse1

#Training set produces 
library(Metrics)
rmse(actual = train$suicides_no, predicted = lmModel$fitted.values)

# Predicting Suicides_no in test dataset
lmModel1 <- lm(suicides_no ~  test$gdp_for_year+ test$gdp_per_capita +test$age +test$suicides.100k.pop, data = test)

p_suicides_no <- predict(lmModel1, test)

lm.rmse <- postResample(test$suicides_no, p_suicides_no)
# R Squared Value of Test Set for Multiple Linear Regression
lm.rmse

actual <- test$suicides_no
preds <- p_suicides_no
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq <- 1 - rss/tss
#Accuracy of Multiple Linear Regression
rsq

#Test set produces an accuracy of 0.3513605


library(pls)

## PLS Regression
model <- pcr(suicides_no ~ train$gdp_per_capita +train$age, 
               data = train
)
# Plot model RMSE vs different values of components
plot(model)
# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune
summary(model)

# Make predictions
predictions <- model %>% predict(train)

p.rmse <- postResample(train$suicides_no, predictions)

#RMSE for PLS Regression
p.rmse


# Model performance metrics
actual <- train$suicides_no
preds <- predictions
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq1 <- 1 - rss/tss
# Accuracy for PLS REgression
rsq1

#Random forest regression

rf1 <- randomForest::randomForest(suicides_no ~ train$gdp_per_capita + train$age + train$suicides.100k.pop, # formula 
              data = train, # data
              ntree = 500)
print(rf1)
rf.prediction <- predict(rf1, newdata = train)
str(rf.prediction)

rf.firstSSE <- mean((rf.prediction - train$suicides_no)^2)
mean(train$suicides_no)
print("Initial Prediction")
plot(rf.prediction, train$suicides_no, xlab = "predicted", ylab = "actual", xact="n")
abline(a=0,b=1)
rf.prediction

eval <- caret::confusionMatrix(factor(rf.prediction, levels = 1:10), factor(train$suicides_no, 1:10))
eval

rf.rmse <- postResample(train$suicides_no, rf.prediction)

#R Sqaured Value for TrainSet of Random Forest
rf.rmse

plot(rf.prediction)
table_rf <- table(train$suicides_no[1:10], rf.prediction[1:10])
table_rf


rf1 <- randomForest::randomForest(suicides_no ~ test$gdp_per_capita + test$age + test$suicides.100k.pop, # formula 
                                  data = test, # data
                                  ntree = 500)

rf.prediction1 <- predict(rf1, newdata = test)
rf.rmse <- postResample(test$suicides_no, rf.prediction1)
#R Squared Value for Test Set of Random Forest
rf.rmse



# Model performance metrics
actual <- train$suicides_no
preds <- rf.prediction
rss <- sum((preds - actual) ^ 2)
tss <- sum((actual - mean(actual)) ^ 2)
rsq2 <- 1 - rss/tss
# Accuracy for Random Forest
rsq2


