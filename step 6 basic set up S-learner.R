library(dplyr)
library(xgboost)
# Some basic data managment for testing 
load("wide_data_for_analysis.Rda")
data <- wide_data
data$feed_name <- str_replace(data$feed_name, "�", "aa")
data$feed_name <- str_replace(data$feed_name, "�", "o")
data$feed_name <- str_replace(data$feed_name, "o?=", "aa")
data <- subset(data, hybrid == "Ross 308")
levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

data$frequent_month <- as.factor(data$frequent_month)
data$ascites_prev <- 1000*data$ascites/data$n_of_chicken
data$prod_type <- as.factor(data$prod_type)
data$feed_name <- as.factor(data$feed_name)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)

formula <- ascites_prev ~ treatment + prod_type + frequent_month + id_slaughterhouse

# The S-learner function takes a formula (like Y ~ X + Z) and data as user inputs (pluss hyperparameters). 
# Then uses the XGBoost algorithm to estimate R^2 and ATE
S_learner <- function(data, index, formula, nrounds=100, eta = 0.1, max_depth = 3){ 
  
  resample <- data[index, ]
  
  # resample <- data (for testing)

  label <- subset(resample, select = c( as.character(update(formula, . ~ .)[[2]])))
  independent_vars <- all.vars(formula)[-1]
  features <- resample %>% select(independent_vars)
  
  #look for features factor variables 
  factor_variables <- names(features)[sapply(features, is.factor)]
  
  #Create dummy variables from factor variables
  dummy_matrix <- model.matrix(~ . - 1, data = resample[, factor_variables])
  # Bind with features data
  features <- cbind(features, dummy_matrix)
  # Remove the original factor variables
  features <- features %>%
    select(-any_of(factor_variables))
  # Create the data matrix for the XGBoost function
  data_matrix <- xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))
  
  # Hyperparameters
  params <- list(
    objective = "reg:squarederror", 
    eta = eta,                      
    max_depth = max_depth
  )
  
  # Estimation of function
  model <- xgboost(data = data_matrix, params = params, nrounds = nrounds, verbose=0)
  
  # Calculate R^2
  r2 <-  cor(subset(resample, select=ascites_prev)[,1], predict(model, as.matrix(features)))^2
  
  # Calculate contrafactual outcomes
  features$treatment = 1
  u_1 <- predict(model, as.matrix(features))
  
  features$treatment = 0
  u_0 <- predict(model, as.matrix(features)) 
  
  # Calculate mean difference
  ATE <- mean(u_1 - u_0) # Mean of individual treatment effects
  
  # Return results
  return(c(ATE, r2))
}




