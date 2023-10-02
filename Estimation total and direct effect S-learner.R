library(tidyr)
library(dplyr)
library(ggplot2)
library(dagitty)
library(caret)
library(ipred)
library(boot)
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "aa")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "o")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")
wide_data <- subset(wide_data, hybrid == "Ross 308")
data <- wide_data

data$ascites_prev <- 1000*data$ascites/data$n_of_chicken

formula <- ascites + 
  # Create a trainControl object for cross-validation
  ctrl <- trainControl(
    method = "cv",           # Cross-validation method (e.g., "cv" for k-fold)
    number = 5,              # Number of folds for cross-validation
    verboseIter = TRUE       # Print progress during tuning
  )

# Define the parameter grid to search over
param_grid <- expand.grid(
  mtry = c(2, 3),          # Number of variables randomly sampled at each split
  ntree = c(100, 200)      # Number of trees in the forest
)

# Train and tune the random forest model using cross-validation
set.seed(123)  # Set a random seed for reproducibility
rf_model <- train(
  x = df[, predictor_vars],  # Predictor variables
  y = df[, outcome_var],     # Outcome variable
  method = "rf",            # Specify the method ("rf" for random forest)
  trControl = ctrl,         # Use the trainControl object
  tuneGrid = param_grid     # Specify the parameter grid
)


N <- nrow(data)

R = 50 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) #Creating a matrix of dirichlet weights 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_total_effect <- boot(data=data, 
                       statistic = S_learner, 
                       formula = ascites_prev ~  frequent_month + treatment, 
                       weights = dirichlet_w, 
                       R=rep(1,R)) 