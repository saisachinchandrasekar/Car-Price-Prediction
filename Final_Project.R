print("Final Project")
packages <- install.packages(c("corrplot","RColorBrewer","car",
                               "dplyr","ggplot2","MASS","tidyverse",
                               "psych","mice","Hmisc","car","leaps"))
install.packages("Metrics")
install.packages("outliers")
library(outliers) #for outliers
library(ggplot2)   #for ggplot
library(dplyr)    # for mutate, select, filter, summarize, arrange
library(mice)    
library(corrplot)
library(ISLR)
library(psych)
library(pROC)
library(gridExtra)
library(MASS)
library(tidyverse)
library(glmnet)
library(caret)
library(Metrics)
library(RColorBrewer)  # for color
library(stats) # for correlation
library(car) # for vif
library(leaps) # for regsubsets
lapply(packages,library, character.only = TRUE)

#1. Loading the dataset
dataset <- read.csv("C:\\Users\\krish\\Desktop\\NEU\\Course\\ALY6015\\module6\\car_price.csv",row.names=NULL)

#2. Exploratory statistics
View(dataset)
names(dataset)
dim(dataset)
summary(dataset)
str(dataset)
#describe(dataset)
library(stringr)  #for string operations
sapply(dataset,class)

#identify missing values
sapply(dataset, function(x) sum(is.na(x)))

attach(dataset)


#cleaning the dataset
#removing X column
dataset <- dataset[,-1]
names(dataset)

dataset$car_prices_in_rupee <- str_replace_all(car_prices_in_rupee,",","")
dataset$kms_driven<-str_replace_all(kms_driven,",","")
x <- dataset$kms_driven
x <- str_replace_all(x," kms","")
dataset$kms_driven<-x
dataset$kms_driven

Value_unity <- ifelse(str_detect(dataset$car_prices_in_rupee, ' Lakh'), 1e5, ifelse(str_detect(dataset$car_prices_in_rupee, ' Crore'), 1e7, 1))
Value_new <- Value_unity * as.numeric(str_remove(dataset$car_prices_in_rupee, ' Lakh| Crore')) 
Value_new
dataset$car_prices_in_rupee<-Value_new

dataset$Seats <- str_remove_all(Seats," Seats")
dataset$engine <- str_remove_all(engine," cc")

dataset$ownership<-str_remove_all(ownership,"[^(-?(\\d*\\.)?\\d+)]")


#converting dataset to numeric values

dataset$engine <- as.numeric(dataset$engine)
dataset$ownership <- as.numeric(dataset$ownership)
dataset$manufacture <- as.numeric(dataset$manufacture)
dataset$Seats <- as.numeric(dataset$Seats)
dataset$kms_driven <-as.numeric(dataset$kms_driven)

#remove duplicated values
dataset[!duplicated(dataset$car_name), ]
dataset<- subset(dataset, engine>1000)


dataset <- within(dataset, {
  fuel_type[fuel_type == "Petrol"] <- 1
  fuel_type[fuel_type == "Diesel"] <- 2
  fuel_type[fuel_type == "Cng"] <- 3
  fuel_type[fuel_type== "Electric"]<-4
  fuel_type[fuel_type=="Lpg"]<-5
})
dataset$transmission <- ifelse(dataset$transmission == "Automatic", 1, 0)

dataset$transmission <-as.numeric(dataset$transmission)
dataset$fuel_type <-as.numeric(dataset$fuel_type)

#install.packages("tibble")
library(tibble)

rownames(dataset) <- make.names(dataset[,1], unique = TRUE)
dataset <- dataset[,-1]


View(dataset)


#pair <- subset(final_numeric_clean,select=c(""))
#corrplot(cor(pair),method="circle", col=brewer.pal(n=20,name="RdYlBu"))

class(car_prices_in_rupee)
#finding outliers
# Q <- quantile(dataset$car_prices_in_rupee, probs=c(.25, .75), na.rm = FALSE)
# Q[1]
# iqr <- IQR(dataset$car_prices_in_rupee)
# iqr
# up <-  Q[2]+1.5*iqr # Upper Range  
# up
# low<- Q[1]-1.5*iqr # Lower Range
# low
# eliminated<- subset(dataset, dataset$car_prices_in_rupee > 325000 )
# View(eliminated)
# boxplot(car_prices_in_rupee)

boxplot(dataset$car_prices_in_rupee, plot=FALSE)$out
outliers_car_price <- boxplot(dataset$car_prices_in_rupee, plot=FALSE)$out
x<-dataset 
x<- x[-which(x$car_prices_in_rupee %in% outliers_car_price),]

outliers_kms_driven<-boxplot(dataset$kms_driven,plot=FALSE)$out
x<-x[-which(x$kms_driven %in% outliers_kms_driven),]

outliers_engine<-boxplot(dataset$engine,plot=FALSE)$out
x<-x[-which(x$engine %in% outliers_engine),]

outliers_manufacture<-boxplot(dataset$manufacture,plot=FALSE)$out
x<-x[-which(x$manufacture %in% outliers_manufacture),]
boxplot(dataset$manufacture)

#filtering the required numerical values for correlation plot
numeric <- x %>% dplyr::select(where(is.numeric))
View(numeric)
names(numeric)

#plotting the correlation matrix
cor(numeric,use="everything")
corrplot(cor(numeric),method="circle",type="upper")

#checking for linear regression
fit <- lm(car_prices_in_rupee~kms_driven+fuel_type+transmission+ownership+manufacture+engine+Seats,data=dataset)
summary(fit)
#avoiding this fit as its not significant, removing the seats, ownership as both are above 0.05 scope

fit2<- regsubsets(car_prices_in_rupee~fuel_type+transmission+manufacture+engine+kms_driven,data=dataset)
summary.out<- summary(fit2)
summary.out

fit_lm <- lm(car_prices_in_rupee~fuel_type+transmission+manufacture+engine+kms_driven,data=numeric)
summary(fit_lm)

# model2 is more significant as due to less aic value, not much but can be considered

#Plot the regression model
par(mfrow=c(2,2))
plot(fit_lm)
dev.off()

#checking for multicollinearity
vif(fit_lm)
outlierTest(model = fit_lm)

plot(fit_lm,scale="adjr2",main="Plot of RegressSubsets of variables")


stepAIC(fit,direction="forward")
stepAIC(fit_lm,direction="forward")


ggplot(dataset, aes( x=manufacture  ,y=car_prices_in_rupee,color=transmission)) +
  geom_point(fill = "#0c4c8a")


qplot(data=dataset,x=kms_driven,y=car_prices_in_rupee,color=manufacture,main = "Car prices vs Kms driven based on manufacture year ")


df <- numeric

#1. Split the data
set.seed(123)
trainIndex <- sample(x= nrow(df), size=nrow(df)*0.6)
train <- df[trainIndex,]
test <- df[-trainIndex,]
train_x <- model.matrix(car_prices_in_rupee~.,train)[,-1]  
test_x <- model.matrix(car_prices_in_rupee~., test)[,-1]
train_y <- train$car_prices_in_rupee
test_y <- test$car_prices_in_rupee
head(train_x,5)
head(test_x,5)
# using car price in rupees as response variable in which removes the variables in Grd rate and 
#adds the categorical variable in training dataset

# RIDGE Regression
#Find the best lambda using cross-validation
set.seed(123)
cv.ridge <- cv.glmnet(train_x, train_y,alpha=0, nfolds=10)
cv.ridge

#Plot the results
dev.off()
plot(cv.ridge)

#lambda min (minimizes out of sample box)
lambda.min <- cv.ridge$lambda.min
lambda.min
log(lambda.min)

#lambda.1se (largest value of lambda within 1 standard error of lambda min)
lambda.1se <- cv.ridge$lambda.1se
lambda.1se
log(lambda.1se)

#Fit Ridge regression model against the training set
model.ridge.min <- glmnet(train_x, train_y, alpha = 0, lambda =cv.ridge$lambda.min) #lambda min 
model.ridge.min
coef(model.ridge.min)

#Regularization
model.ridge.1se <- glmnet(train_x, train_y, alpha = 0, lambda =cv.ridge$lambda.1se) #lambda 1se
model.ridge.1se
coef(model.ridge.1se)

#Train set prediction of RIDGE model by calculating RMSE
#lamda min - RIDGE
pred.ridge.min <- predict(model.ridge.min, newx = train_x)
train.ridge.rmse.min <- rmse(train_y, pred.ridge.min)
train.ridge.rmse.min

#lambda 1se - RIDGE
pred.ridge.1se <- predict(model.ridge.1se, newx = train_x)
train.ridge.rmse.1se <- rmse(train_y, pred.ridge.1se)
train.ridge.rmse.1se

#Test set prediction of ridge model using RMSE
#lambda min - RMSE RIDGE
pred.ridge.min.test <- predict(model.ridge.min, newx = test_x)
test.ridge.rmse.min <- rmse(test_y, pred.ridge.min.test)
test.ridge.rmse.min

#lambda 1se  - RMSE RIDGE
pred.ridge.1se.test <- predict(model.ridge.1se, newx = test_x)
test.ridge.rmse.1se <- rmse(test_y, pred.ridge.1se.test)
test.ridge.rmse.1se

#display coefficient of ols model with no regularization
ols <- lm(car_prices_in_rupee ~., data = train)
coef(ols)

#view RMSE of full model
preds.ols <- predict(ols, new = test)
rmse(test$car_prices_in_rupee, preds.ols)

# LASSO Regression
#Find best values of lambda
cv.lasso <- cv.glmnet(train_x, train_y, nfolds=10)
cv.lasso
View(numeric)
#lambda min
cv.lasso$lambda.min
#lambda.1se
cv.lasso$lambda.1se

#Plot the results
plot(cv.lasso)

#fit lasso regression model against the training set
model.lasso.min <- glmnet(train_x, train_y, alpha=1, lambda =cv.lasso$lambda.min)#lambda min
model.lasso.min
coef(model.lasso.min)

model.lasso.1se <- glmnet(train_x, train_y, alpha = 1, lambda =cv.lasso$lambda.1se)#lambda 1se
model.lasso.1se
coef(model.lasso.1se)

#Train set prediction of LASSO model by calculating RMSE
#lambda min
pred.lasso.min <- predict(model.lasso.min, newx = train_x)
train.lasso.rmse <- rmse(train_y, pred.lasso.min)
train.lasso.rmse

#lambda 1se
pred.lasso.1se <- predict(model.lasso.1se, newx = train_x)
train.lasso.rmse.1se <- rmse(train_y, pred.lasso.1se)
train.lasso.rmse.1se

#Test set prediction of lasso model using RMSE
#lambda min
pred.lasso.min.test <- predict(model.lasso.min, newx = test_x)
test.lasso.rmse.min <- rmse(test_y, pred.lasso.min.test)
test.lasso.rmse.min

#lambda 1se
pre.lasso.1se.test <- predict(model.lasso.1se, newx = test_x)
test.lasso.rmse.1se <- rmse(test_y, pre.lasso.1se.test)
test.lasso.rmse.1se

#Stepwise selection and regularization
#forward selection
step(lm(car_prices_in_rupee ~ 1, data = train), direction = 'forward', scope = ~kms_driven+ownership+manufacture+Seats+engine)
#backward selection
step(lm(car_prices_in_rupee ~ ., data = train), direction = 'backward')
#comparring regression models
fit1 <- lm(formula = car_prices_in_rupee ~ kms_driven+ownership+manufacture+Seats+engine, data = train) 
fit2 <- lm(formula = car_prices_in_rupee ~ kms_driven+ownership+manufacture+Seats+engine, data = train) 
fit3 <- lm(formula = car_prices_in_rupee ~ kms_driven+ownership+manufacture+Seats+engine, data = train)
AIC(fit1, fit2, fit3)
BIC(fit1,fit2, fit3)

#ROC curve
#plotting

qplot(x=fuel_type, y=car_prices_in_rupee, color=transmission, geom = "point") +scale_shape(solid=FALSE)+theme(text = element_text(size=10))
qplot(x=engine, y=manufacture, color=transmission, geom = "point") +scale_shape(solid=FALSE)+theme(text = element_text(size=10))


#Split data into train and test set
logdata <- numeric
logdata<- logdata[,-3]
names(logdata)
View(logdata)
attach(logdata)
logdata$transmission<- as.factor(logdata$transmission)

set.seed(123)
trainIndex <- createDataPartition(logdata$transmission, p=0.7, list = FALSE)
train <- logdata[trainIndex,]
test <- logdata[-trainIndex,]
head(train)
#Logistic regression model
model1 <- glm(transmission~., data=train, family = binomial(link = "logit"))
summary(model1)
#2185.1
model2 <- glm(transmission~car_prices_in_rupee+Seats+manufacture+ownership+kms_driven, data=train, family = binomial(link = "logit"))
#View(model2)
summary(model2)

probabilities.train <- predict(model2, newdata = train, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.train >= 0.5,
                                          "1", "0"))


#Confusion Matrix / Model Accuracy
confusionMatrix(predicted.classes.min,train$transmission)

ROC1 <- roc(train$transmission, probabilities.train)
plot(ROC1, col="blue", ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

#AUC
auc <-auc(ROC1)
auc



