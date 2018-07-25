## Import relavent libraries 
library(dplyr)
library(tidytext)
library(ggplot2)
library(data.table)
library(tidyr)
library(xgboost)
library(caret)
library(e1071)


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

# Create is_na_underwriting for NA values in application_underwriting_score
combined_data_V4$is_na_underwriting <- 0

for(i in 1:length(combined_data_V4$application_underwriting_score)) {
  ifelse(is.na(combined_data_V4$application_underwriting_score[i]), combined_data_V4$is_na_underwriting[i] <- 1, print("0"))
}

## set seed
set.seed(2018)

## make a predictive model using svm 

#separate for training and testing dataset

predictor <- setdiff(colnames(combined_data_V4), c("id","renewal"))
train_V2 <- combined_data_V4[combined_data_V4$renewal != -1,]
test_V2 <- combined_data_V4[combined_data_V4$renewal == -1,]
test_id <- test_V2$id
train_na_omit <- na.omit(train)
predictions <- train_na_omit$renewal

# make the predictive model 
model <- svm(train_V2$renewal~ train_V2$Urban, probability = TRUE)

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
         preds_v2$incentive_preds[i] <-400*(log((-10)/((5*log(-20/((((max_percentage_improved/100)-(preds_v2$V1[i] + (max_percentage_improved/100)-1))*100)-20)))-10))))
}

## Format solution submission 
colnames(preds_v2) <- c("renewal","incentives")

## write solution submission into a CSV file 
write.table(data.table(id = test_id, preds_v2), "submission_v5.csv", sep = ",", dec = ".", quote = FALSE, row.names = FALSE)

