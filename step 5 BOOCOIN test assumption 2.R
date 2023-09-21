# Birds p m-sqr b _||_ Water consumption | Feed mix, Month

library(dagitty)
library(tidyverse)
library(caret)
library(ipred)
library(dplyr)
library(forcats)
library(stringr)
library(randomForest)
library(xgboost)
library(boot)
rm(list = ls())

setwd("C:/Users/christian.thorjussen/Project Nortura/")
source("C:/broiler_acites/ascites_case/step 1 merging dataframe.R")
source("C:/broiler_acites/ascites_case/step 3 data cleaning.R")
source("C:/broiler_acites/ascites_case/step 4a modeling growth curves.R")
source("C:/broiler_acites/ascites_case/step 4c data management.R")

load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")
# Assuming your data frame is named wide_data
wide_data <- subset(wide_data, hybrid == "Ross 308")





#Create a feed_group variable instead of feed type since it has so many levels
levels = 7  # Set the number of levels other than other
wide_data$feed_group = fct_lump_n(wide_data$feed_name, n = levels, other_level = "other")

wide_data$feed_group = as.factor(wide_data$feed_group)
wide_data$frequent_month = as.factor(wide_data$frequent_month)

##########################################
# Birds p m-sqr b _||_ Water consumption | Feed mix, Month
##########################################
randomf <- list()
bag <- list()
XGboost <- list()

for (i in 1:200){
  data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group', 'frequent_month'))
  data <- na.omit(data)
  
  #Partitioning data into training and test
  inTraining <- createDataPartition(data$feed_group, p = 0.8, list = FALSE)
  training <- data[inTraining,]
  testing  <- data[-inTraining,]
  
  #Random Forest
  rf <- randomForest(average_food ~  birds_m_sqr + feed_group + frequent_month, 
                     data=training,
                     ntree=500)
  test.features <- subset(testing, select=c(birds_m_sqr, feed_group, frequent_month))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(rf, newdata = test.features)
  rf_R2 <- cor(test.target, predictions) ^ 2
  
  #Bagged tree
  bagged <- bagging(
    average_food ~  birds_m_sqr + feed_group + frequent_month,
    data = training,
    coob = TRUE,
  )
  test.features <- subset(testing, select=c(birds_m_sqr, feed_group, frequent_month))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(bagged, newdata = test.features)
  bagged_R2 <- cor(test.target, predictions) ^ 2
  
  #XGBoost
  #Create a data frame with the month variable
  food <- data.frame(food = data$feed_group)
  month <- data.frame(month = data$frequent_month)
  #Create dummy variables for feed variable
  dummy_vars <- model.matrix(~ factor(food), food)
  dummy_vars2 <- model.matrix(~ factor(month), month)
  
  # Remove the intercept column
  dummy_vars <- dummy_vars[, -1]
  dummy_vars2 <- dummy_vars2[, -1]
  
  # Bind the dummy variables to the original data frame
  data <- cbind(data, dummy_vars, dummy_vars2)
  columns_to_exclude <- c("feed_group", "frequent_month")
  
  data <- data[, !names(data) %in% columns_to_exclude]

  colnames(data) <- c('average_food', 'birds_m_sqr', 
                      'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7', 
                      'month2', 'month3', 'month4', 'month5', 'month6', 'month7', 'month8', 'month9', 'month10', 'month11', 'month12')
  
  
  # Split the data into training and testing sets
  inTraining <- createDataPartition(data$average_food, p = 0.8, list = FALSE)
  training <- data[inTraining,]
  testing  <- data[-inTraining,]
  
  label <- training$average_food
  training <- as.matrix(training[, !(names(training) %in% c("average_food"))])
  
  # Training XGBoost model
  
  xgb_model <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 20,
    eta = 0.3,                      
    max_depth = 7)
  
  # Make predictions on the test set
  test.target <- testing$average_food
  testing <- as.matrix(testing[, !(names(testing) %in% c("average_food"))])
  predictions <- predict(xgb_model, as.matrix(testing))
  
  #  Calculate r-squared
  XGB_R2 <- cor(test.target, predictions) ^ 2
  
  randomf <- append(randomf, rf_R2)
  bag <- append(bag, bagged_R2)
  XGboost <- append(XGboost, XGB_R2)
  
  print(paste0('Loop ',i,''))
}

rf <- unlist(randomf)
bag <- unlist(bag)
XGB <- unlist(XGboost)
summary(rf)
summary(bag)
summary(XGB)

write.csv(rf,file='C:/broiler_acites/ascites_case/Results/rf_test2 Birds Food Feed.csv', row.names = FALSE)
write.csv(bag,file='C:/broiler_acites/ascites_case/Results/bag_test2 Birds Food Feed.csv', row.names = FALSE)
write.csv(XGB,file='C:/broiler_acites/ascites_case/Results/XGB_test2 Birds Food Feed.csv', row.names = FALSE)


#BOOCOIN testing
#Creating the test function based on model training above
#Again load the data
data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
data <- na.omit(data)
data$feed_group <- as.factor(data$feed_group)

boocoin <- function(data, index, p){
  resample <- data[index, ]
  # resample <- data[sample(nrow(data),nrow(data),replace=TRUE),] #Uncomment for testing
  # p = 0.8 #Uncomment for testing
  
  #Create the shuffled variable
  resample$shuffled_bird = sample(resample$birds_m_sqr)
  
  #XGBoost
  #Create a data frame with the month variable
  food <- data.frame(food = resample$feed_group)
  
  #Create dummy variables for feed variable
  dummy_vars <- model.matrix(~ factor(food), food)
  
  # Remove the intercept column
  dummy_vars <- dummy_vars[, -1]
  
  # Bind the dummy variables to the original data frame
  resample <- cbind(resample, dummy_vars)
  data_1 <- subset(resample, select = -c(feed_group, shuffled_bird))
  colnames(data_1) <- c('average_food', 'birds_m_sqr', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7' )
  
  #Data Partition
  #Partitioning data into training and test
  inTraining <- createDataPartition(data_1$average_food, p = p, list = FALSE)
  training <- data_1[inTraining,]
  testing  <- data_1[-inTraining,]
  
  # Splitting training data into dependent and features
  label <- training$average_food
  training <- as.matrix(training[, !(names(training) %in% c("average_food"))])
  
  xgb_model_1 <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 20,
    eta = 0.3,                      
    max_depth = 6)
  
  # Make predictions on the test set
  test.target <- testing$average_food
  testing <- as.matrix(testing[, !(names(testing) %in% c("average_food"))])
  predictions <- predict(xgb_model_1, as.matrix(testing))
  
  #  Calculate r-squared
  XGB_R2_1 <- cor(test.target, predictions) ^ 2
  
  data_2 <- subset(resample, select = -c(feed_group, birds_m_sqr))
  colnames(data_2) <- c('average_food', 'shuffled_bird', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7')
  
  #Data Partition
  #Partitioning data into training and test
  inTraining <- createDataPartition(data_2$average_food, p = p, list = FALSE)
  training <- data_2[inTraining,]
  testing  <- data_2[-inTraining,]
  
  # Splitting training data into dependent and features
  label <- training$average_food
  training <- as.matrix(training[, !(names(training) %in% c("average_food"))])
  
  xgb_model_2 <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 20,
    eta = 0.3,                      
    max_depth = 6)
  
  # Make predictions on the test set
  test.target <- testing$average_food
  testing <- as.matrix(testing[, !(names(testing) %in% c("average_food"))])
  predictions <- predict(xgb_model_2, as.matrix(testing))
  
  #  Calculate r-squared
  XGB_R2_2 <- cor(test.target, predictions) ^ 2
  
  
  c(XGB_R2_1, XGB_R2_2 ,XGB_R2_1-XGB_R2_2)
  
  return(c(XGB_R2_1, XGB_R2_2 ,XGB_R2_1-XGB_R2_2))
}

N <- nrow(data) #number of observations in data set
R = 10000 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) #Creating a matrix of dirichlet weights 
dirichlet_w <- dirichlet / rowSums(dirichlet)

bayesian_boot <- boot(data=data, statistic = boocoin, weights = dirichlet_w, R=rep(1,R), p=0.85) #Bootstrapping 


plot1 <- hist(bayesian_boot$t[,3], xlim = range(0, 1), breaks = 60,  freq = FALSE, main = " ", xlab = "Difference in R-Squared", col = "lightblue")

png("C:/broiler_acites/ascites_case/Results/plot1_test_food_birds_feed.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot1, col = "lightblue", main = " ", xlab = "Difference in R-Squared", xlim = range(0, 1))
dev.off()
