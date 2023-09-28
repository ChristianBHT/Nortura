library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(dagitty)
library(caret)
library(ipred)
library(parallel)
library(boot)
library(xgboost)
library(mlbench)
library(MLmetrics)
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "aa")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "o")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")
wide_data <- subset(wide_data, hybrid == "Ross 308")
data <- wide_data

data$aceties_prev <- 1000*data$aceties/data$n_of_chicken

# Create a panel of scatter plots
pairs_data <- subset(data, select = c('aceties_prev', 
                                      'start_weight', 
                                      'indoor_mean_maxhumidity', 
                                      'growth',
                                      'sqr_growth',
                                      'indoor_mean_maxtemp',
                                      'climate_mean_temp',
                                      'climate_mean_hum',
                                      'average_food'))
                                      
pairs(pairs_data, pch = 19, cex = 0.05, col = "black")

levels = 3  # Set the number of feed types for analysis
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
table(data$feed_group)


data$feed_group = as.factor(data$feed_group)
data$prod_type <- as.factor(data$prod_type)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$frequent_month <- as.factor(data$frequent_month)
data$aceties_prev <- 1000*data$aceties/data$n_of_chicken
table(data$feed_group)

data <- subset(data, select = c('feed_group', 
                                'aceties_prev', 
                                'prod_type', 
                                'intercept', 
                                'growth', 
                                'sqr_growth',
                                'indoor_mean_maxtemp',
                                'frequent_month', 
                                'climate_mean_temp',
                                'id_slaughterhouse', 
                                'average_food', 
                                'average_water',
                                'water_slope', 
                                'water_slope2', 
                                'birds_m_sqr',
                                'bird_slope', 
                                'bird_slope2',
                                'n_of_chicken')) 

data <- na.omit(data)
dummy_vars <- model.matrix(~ 0 + prod_type + frequent_month + id_slaughterhouse, data = data)
data <- cbind(data, dummy_vars)

data$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)


boxplot(data$aceties_prev ~ feed_group,
        data = data, 
        xlab = "Growth feed type", 
        ylab = "Prevalens of ascites per 1000",
        names = c("Feed 1", "Feed 2", "Feed 3", "Other"))

## Normalization of data (not used)
# normalize <- data
# numeric_vars <- sapply(normalize, is.numeric)
# normalize[numeric_vars] <- lapply(normalize[numeric_vars], scale)
# Treatment define treatment
# resample <- data; cv_folds <- 5  #Uncomment to test
# resample$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0) #Uncomment to test
# The function below performs GCOM (Generalized Causal Outcomes Model) estimation
# Convert categorical variables to dummy variables


S_learner <- function(data, 
                      index, 
                      Y, 
                      adjust_set, 
                      p = 0.9, 
                      nrounds = 10, 
                      objective = "reg:squarederror",
                      max_depth = 4,
                      eta = 0.01,
                      lambda = 5,
                      colsample_bytree = 0.6){ 
  
  resample <- data[index, ]
  
  # The control parameters for cross-validation
  
  inTraining <- createDataPartition(with(resample, get(Y)), p = p, list = FALSE)
  training <- resample[inTraining,]
  testing  <- resample[-inTraining,]
  # Train the random forest model with cross-validation
  # Prepare the data
  X_train <- as.matrix(training[, adjust_set])
  y_train <- with(training, get(Y))
  X_test <- as.matrix(testing[, adjust_set])
  y_test <- with(testing, get(Y))
  
  dtrain <- xgb.DMatrix(label = y_train, data = X_train)
  dtest <- xgb.DMatrix(label = y_test, data = X_test)
  
  # Retrieve the best hyperparameters
  
  model <- xgboost(data = dtrain, 
                   nrounds = nrounds,
                   objective = objective,
                   max_depth = max_depth,
                   eta = eta,
                   lambda = lambda,
                   colsample_bytree = colsample_bytree,
                   verbose = 0)
  
  train_r2 <-  cor(subset(training, select=get(Y))[,1], predict(model, dtrain))^2
  test_r2 <-  cor(subset(testing, select=get(Y))[,1], predict(model, dtest))^2
  
  # Access the cross-validation results
  full_y <- with(resample, get(Y))
  
  resample$treatment = 1
  full_X <- as.matrix(resample[, adjust_set])
  full_data_mu1 <- xgb.DMatrix(label = full_y, data = full_X)
  u_1 <- predict(model, full_data_mu1) 
  
  resample$treatment = 0
  full_X <- as.matrix(resample[, adjust_set])
  full_data_mu0 <- xgb.DMatrix(label = full_y, data = full_X)
  u_0 <- predict(model, full_data_mu0) 
  
  CATE <- mean(u_1 - u_0) # Mean of individual treatment effects
  
  return(c(CATE, train_r2, test_r2))
}

