library(data.table) 
library(dplyr)
library(randomForest)
library(readr)
library(igraph)
library(dplyr)
library(ggraph)
library(party)
library(MLmetrics)
library(Metrics)
setwd("C:/Users/Admin/Downloads")

store <- read_csv("C:/Users/Admin/Downloads/store.csv")
test <- read_csv("C:/Users/Admin/Downloads/test.csv")
train <- read_csv("C:/Users/Admin/Downloads/train.csv")

set.seed(435)
smp_size <- floor(0.70 * nrow(train))
splitdata <- sample(seq_len(nrow(train)), size = smp_size)

train <- train[splitdata, ]
Validation <- train[-splitdata, ]

train <- merge(train,store,by="Store")
test <- merge(test,store,by="Store")
Validation <- merge(Validation,store,by="Store")

#Setting Na values to 0 in both training and test data
train[is.na(train)] <- 0
test[is.na(test)] <- 0
Validation[is.na(Validation)] <- 0

str(train)
summary(train)


str(test)
summary(test)

str(Validation)
summary(Validation)

#Considering data for which the stores were open and has sales data.
#We are doing this because some stores for closed for a period of 6 months because of renovation purposes
train <- train[ which(train$Open=='1'),]
Validation <- Validation[ which(Validation$Open=='1'),]

#Converting the date to Date format in both train and test set
train$Date <- as.Date(train$Date,"%m/%d/%Y")
Validation$Date <- as.Date(Validation$Date,"%m/%d/%Y")
test$Date <- as.Date(test$Date)

#Extracting induvidual date components 
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))

Validation$month <- as.integer(format(Validation$Date, "%m"))
Validation$year <- as.integer(format(Validation$Date, "%y"))
Validation$day <- as.integer(format(Validation$Date, "%d"))

#Removing orginal date because we have exteacted the date components
train <- train[,-c(3,8)]
Validation <- Validation[,-c(3,8)]

#Extracting induvidual date components to test set
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

#Removing orginal date because we have exteacted the date components
test <- test[,-c(4,7)]


#Combing the predictor variables into one object

feature <- names(train)[c(1,2,6,8:12,14:19)]
feature

feature.validation <- names(Validation[c(1,2,6,8:12,14:19)])
feature.validation

#Converting to factors
for (f in feature) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

for (f in feature.validation) {
  if (class(Validation[[f]])=="character") {
    levels <- unique(c(Validation[[f]], test[[f]]))
    Validation[[f]] <- as.integer(factor(Validation[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

str(Validation)
str(train)

#Checking if some variables are missing after converting to factors

length(unique(train$Store))
length(unique(Validation$Store))


#Random Forest Model with 50 Trees and 100000 sample size
model1 <- randomForest(train[,feature], 
                    log(train$Sales+1),
                    mtry=5,
                    ntree=50,
                    sampsize=100000,
                    do.trace=TRUE)
model1$mse


getTree(model1, 1, labelVar=TRUE)

plot(model1,type="simple")

####importance
importance(model1,type=2)
varImpPlot(model1)

#Model Summary
print(model1)

summary(model1)
#Error vs tree plot
plot(model1)

#Using Model to predict Sales
pred <- exp(predict(model1, train)) -1
TrainPrediction <- data.frame(store=train$Store, Sales=pred)
write_csv(TrainPrediction, "TrainPrediction.csv")
head(TrainPrediction,20)

pred <- exp(predict(model1, Validation)) -1
ValidationPrediction <- data.frame(store=Validation$Store, Sales=pred)
write_csv(ValidationPrediction, "ValidationPrediction.csv")
head(ValidationPrediction,20)

pred <- exp(predict(model1, test)) -1
RossPrediction <- data.frame(store=test$Id, Sales=pred)
write_csv(RossPrediction, "RossPrediction.csv")
head(RossPrediction,20)
View(RossPrediction)
View(TrainPrediction)
View(ValidationPrediction)


RMSPE(train$Sales,TrainPrediction$Sales)
RMSE(train$Sales,TrainPrediction$Sales)
mase(train$Sales,TrainPrediction$Sales)
rmsle(train$Sales,TrainPrediction$Sales)
rrse(train$Sales,TrainPrediction$Sales)
rse(train$Sales,TrainPrediction$Sales)

RMSPE(Validation$Sales,ValidationPrediction$Sales)
RMSE(Validation$Sales,ValidationPrediction$Sales)
mase(Validation$Sales,ValidationPrediction$Sales)
rmsle(Validation$Sales,ValidationPrediction$Sales)
rrse(Validation$Sales,ValidationPrediction$Sales)
rse(Validation$Sales,ValidationPrediction$Sales)

#######XGBOOST#######

#Converting to numerical
for (f in feature) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.numeric(numeric(train[[f]], levels=levels))
    test[[f]]  <- as.numeric(numeric(test[[f]],  levels=levels))
  }
}

install.packages("xgboost")
str(test)
library(xgboost) 
library(tidyverse)

head(train)
View(train)

library(xgboost)
library(readr)
library(stringr)
library(caret)

#converting into a matrix
x1 <- scale(train)
train_matrix <- model.matrix(~Sales-1,train)
valid_matrix <- model.matrix(~Sales-1,Validation)
test_matrix <- model.matrix(~Id-1,test)

length(trainlabels)
length(train_matrix)
#dmatrix conversion
dtrain <- xgb.DMatrix(data = train_matrix, label= train$Sales)
dtest <- xgb.DMatrix(data = test_matrix, label= test$Id)
dvalid <- xgb.DMatrix(data= valid_matrix, label= Validation$Sales)

#xgmodel
default_param<-list(
  objective = "reg:linear",
  booster = "gbtree",
  eta=0.1, #default = 0.3
  gamma=10,
  max_depth=6,
  min_child_weight=1,
  max_delta_step=0,
  subsample=0.1,
  colsample_bytree=1,
  seed= 2018
)
xgb_mod <- xgb.train(data = dtrain, params=default_param, nrounds = 247)

plot(xgb_mod,type="simple")
#cross validation
xgbcv <- xgb.cv( params = default_param, data = dtrain, nrounds = 500, nfold = 5, showsd = T,stratified = T,
                 print_every_n = 40, early_stopping_rounds = 10, maximize = F)



xgb_val <- xgb.train(data = dvalid, params=default_param, nrounds = 115)

plot(as.data.frame(xgb_mod),type="simple")

XGBpred <- predict(xgb_mod, dtrain)
XGBpredv <- predict(xgb_val,dvalid)
XGBpred1 <- predict(xgb_mod, dtest)
write_csv(XGBpred1, "ValidationPredictionXG.csv")
#predictions_XGB <- exp(XGBpred1) #need to reverse the log to the real values
head(predictions_XGB)
View(XGBpred)

anova(xgb_mod)
XGBpred <- as.data.frame(XGBpred)
length(XGBpred)
length(dtrain$sales)

RMSE(Validation$Sales, XGBpredv)
RMSE(train$Sales,XGBpred)
RMSPE(Validation$Sales, XGBpredv)
RMSPE(train$Sales,XGBpred)
######BOOST#####


###linear model

model12 <- lm(train$Sales~train$Customers+train$Store+train$Customers+train$Open+train$Promo+
                train$SchoolHoliday+train$CompetitionDistance+train$CompetitionOpenSinceMonth+
                train$CompetitionOpenSinceYear+train$Promo2+train$Promo2SinceWeek+train$Promo2SinceYear)

summary(model12)

pred <- predict(model12, train)
FinalPrediction12 <- data.frame(store=train$Store, Sales=pred)
RMSPE(train$Sales,FinalPrediction12$Sales)


