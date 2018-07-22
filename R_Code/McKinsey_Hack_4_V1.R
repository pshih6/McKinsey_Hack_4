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

## create cross validation folder and convert dataset into xgb,DMatrix 

cross_validation_folds_list <- createFolds(train, k = 5,
                                           list = TRUE, returnTrain = FALSE)
train_label <- train$renewal
train_matrix <- as.matrix(train)
test_matrix <- as.matrix(test)


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
