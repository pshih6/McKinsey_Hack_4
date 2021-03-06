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

ggplot(data = train, aes(x = as.factor(renewal))) + geom_bar()

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

# convert feature residential area type to individual binary features (one hot coding)

resident <- levels(combined_data_V2$residence_area_type)
resident_filler <- c(1:length(resident))
resident_data <- data.frame(as.list(resident_filler))
colnames(resident_data) <- resident
combined_data_V3 <- cbind(combined_data_V2, resident_data)

for(i in 19:length(combined_data_V3)) {
  combined_data_V3[,i] <- 0
}

for (i in 1:length(combined_data_V3$residence_area_type)) {
  resident_value <- as.character(combined_data_V3$residence_area_type[i])
  ifelse(combined_data_V3$residence_area_type[i] == resident, ifelse(sum(resident_value == colnames(combined_data_V3)) == 1,
                                                                     combined_data_V3[i,resident_value] <- 1, coordinate <- c(resident_value, coordinate)),
                                                                     print("this is still working"))
}

# remove feature sourcing channel and residential area type 

remove <- c("residence_area_type","sourcing_channel")
combined_data_V4 <- combined_data_V3[, !(names(combined_data_V3) %in% remove)]

## set seed
set.seed(2018)

## create cross validation folder

cross_validation_folds_list <- createFolds(combined_data_V4[combined_data_V4$renewal != -1,], k = 3,
                                           list = TRUE, returnTrain = FALSE)
##convert dataset into xgb,DMatrix for training and testing dataset 

non_predictors <- setdiff(colnames(combined_data_V4), c("id","renewal"))

train_matrix <- data.matrix(combined_data_V4[combined_data_V4$renewal != -1, non_predictors])
test_matrix <- data.matrix(combined_data_V4[combined_data_V4$renewal == -1, non_predictors])

train_label <- combined_data_V4$renewal[combined_data_V4$renewal != -1] 
test_id <- combined_data_V4$id[combined_data_V4$renewal == -1]
dtrain <- xgb.DMatrix( data = train_matrix, label = train_label)
dtest  <- xgb.DMatrix( data = test_matrix)

## cross validation parameters with ROC AUC evaluation for part A 

param_1 <- list(booster = "gbtree",
                objective = "binary:logistic",
                eta = .03333,
                max_depth = 5,
                eval_metric = "auc",
                min_child_weight = 1,
                subsample = .7,
                colsample_bytree = .7)

## cross validation using pre-defined parameters 

XGB_cross_validation_1 <- xgb.cv(data = dtrain,
                                 params = param_1,
                                 nrounds = 300,
                                 maximize = TRUE,
                                 prediction = TRUE,
                                 folds = cross_validation_folds_list,
                                 print_every_n = 25,
                                 early_stop_round = 20)
## train the model 

xgb_model <- xgb.train(data = dtrain,
                         params = param_1,
                         watchlist = list(train = dtrain),
                         nrounds = 300,
                         verbose = 1,
                         print_every_n = 10)

## view how the model weighs the various features 

names <- dimnames(train_matrix)[[2]]
importance_matrix <- xgb.importance(names,model = xgb_model)
xgb.plot.importance(importance_matrix[1:18])

## make prediction on testing dataset
preds <- as.data.table(t(matrix(predict(xgb_model, dtest), nrow = 1, ncol = nrow(dtest))))

### part b algorithm 

## create dataframe to view relationships provided by the competition
incentive <- seq(0,10000, 0.1)
effort <- 10*(1-exp((-1*incentive)/400))
percent_improved <- 20*(1-exp((-1*effort)/5))
variables <- data.frame(incentive, effort, percent_improved)
ggplot(data = variables,aes(x = effort, y = percent_improved)) +
  geom_line() 

## assign max percent inproved and max effort(hr)
max_percentage_improved <- max(variables$percent_improved)
max_effort <- max(variables$effort)
max_incentive <- 67224.5

##calculate the incentive based on predicted likelihood of renewal
incentive_preds <- data.frame(incentive_preds = 0)
preds_v2 <- cbind(preds, incentive_preds)

for (i in 1:length(preds_v2$V1)) {
  ifelse(preds_v2$V1[i] + (max_percentage_improved/100) <= 1.0, preds_v2$incentive_preds[i] <- max_incentive, 
         400*(log((-10)/((5*log(-20/((((max_percentage_improved/100)-(preds_v2$V1[i] + (max_percentage_improved/100)-1))*100)-20)))-10)))
  )
  
}


effort <- (5*log(-20/((((max_percentage_improved/100)-(preds_v2$V1[i] + (max_percentage_improved/100)-1))*100)-20)))

400*(log((-10)/((5*log(-20/((((max_percentage_improved/100)-(preds_v2$V1[362] + (max_percentage_improved/100)-1))*100)-20)))-10)))
400*(log((-10)/((5*log(-20/((((max_percentage_improved/100)-(preds_v2$V1[217] + (max_percentage_improved/100)-1))*100)-20)))-10)))
