## Import relavent libraries 
library(dplyr)
library(tidytext)
library(ggplot2)
library(data.table)
library(tidyr)
library(xgboost)
library(caret)

#setting working directory
setwd("C:/Users/oo-he/Documents/GitHub/McKinsey_Hack_4/Dataset")

## read in training and testing dataset 
train <- read.csv("train_hack_4.csv")
test <- read.csv("test_hack_4.csv")


## prelim visual analysis of training dataset 

## prelim feature engineering 

#combining test and training set

test$renewal <- -1
combined_data <- rbind(test, train)

#convert feature sourcing channel to individual binary features (one hot coding)

channel <- levels(combined_data$sourcing_channel)
channel_filler <- c(1:length(channel))
channel_data <- data.frame(as.list(channel_filler))
colnames(channel_data) <- channel
combined_data_V2 <- cbind(combined_data, channel_data)

for(i in 14:length(combined_data_V2)) {
  combined_data_V2[,i] <- 0
  }

coordinate <- list()

for (i in 1:length(combined_data_V2$sourcing_channel)) {
  channel_value <- as.character(combined_data_V2$sourcing_channel[i])
  ifelse(combined_data_V2$sourcing_channel[i] == channel, ifelse(sum(channel_value == colnames(combined_data_V2)) == 1, 
                                                                 combined_data_V2[i,channel_value] <- 1, coordinate <- c(channel_value,coordinate)),
                                                                 print("this is working"))
}



## create cross validation folder and convert dataset into xgb,DMatrix 

cross_validation_folds_list <- createFolds(train, k = 5,
                                           list = TRUE, returnTrain = FALSE)
train_label <- as.numeric(train$renewal)
train_matrix <- data.matrix(train)
test_matrix <- data.matrix(test)

str(train_matrix)
str(train)


dtrain <- xgb.DMatrix( data = train_matrix, label = train_label)
dtest  <- xgb.DMatrix( data = test_matrix)

## cross validation parameters with ROC AUC evaluation for part A 

param_1 <- list(booster = "gbtree",
                objective = "binary:logistic",
                eval_metric = "auc",
                eta = .03333,
                max_depth = 5,
                min_child_weight = 1,
                subsample = .7,
                colsample_bytree = .7)

## cross validation using pre-defined parameters 

XGB_cross_validation_1 <- xgb.cv(data = train,
                                 params = param_1,
                                 nrounds = 150,
                                 maximize = TRUE,
                                 prediction = TRUE,
                                 folds = cross_validation_folds_list,
                                 print_every_n = 25,
                                 early_stop_round = 20)
