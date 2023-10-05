
setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

#We only need the following variables 

load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "aa")
wide_data$feed_name <- str_replace(wide_data$feed_name, "�", "o")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")
wide_data <- subset(wide_data, hybrid == "Ross 308")
data <- wide_data

data$feed_name <- as.factor(data$feed_name)
data$prod_type <- as.factor(data$prod_type)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$leverandoer_nr <- data$leverandoer_nr
data$ascites_prev <- 1000*data$ascites/data$n_of_chicken
data$frequent_month <- as.factor(data$frequent_month)
levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)


formula <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse

# Creating a function to calculate bootstrapped statistics 
T_learner <- function(data, index, formula, nrounds_t=30, nrounds_c=30, eta_t = 0.1, eta_c = 0.1, max_depth_t = 3, max_depth_c = 3){ 
  resample <- data[index, ]
  # resample <- data # (for testing)
  data_t <- subset(resample, treatment == 1)
  data_c <- subset(resample, treatment == 0) 
  independent_vars <- all.vars(formula)[-1]
  
  label_t <- subset(data_t, select = c( as.character(update(formula, . ~ .)[[2]])))
  label_c <- subset(data_c, select = c( as.character(update(formula, . ~ .)[[2]])))

  features_t <- data_t %>% select(independent_vars)
  features_c <- data_c %>% select(independent_vars)
  full_data <- resample %>% select(independent_vars)
  #look for features factor variables 
  factor_variables <- names(features_t)[sapply(features_t, is.factor)]
  #Create dummy variables from factor variables
  dummy_matrix_t <- model.matrix(~ . - 1, data = data_t[, factor_variables])
  dummy_matrix_c <- model.matrix(~ . - 1, data = data_c[, factor_variables])
  dummy_matrix <- model.matrix(~ . - 1, data = full_data[, factor_variables])
  # Bind with features data
  features_t <- cbind(features_t, dummy_matrix_t)
  features_c <- cbind(features_c, dummy_matrix_c)
  full_data <- cbind(full_data, dummy_matrix)
  # Remove the original factor variables
  features_t <- features_t %>%
    select(-any_of(factor_variables))
  features_c <- features_c %>%
    select(-any_of(factor_variables))
  full_data <- full_data %>%
    select(-any_of(factor_variables))
  # Create the data matrix for the XGBoost function
  data_matrix_t <- xgb.DMatrix(data = as.matrix(features_t), label = as.matrix(label_t))
  data_matrix_c <- xgb.DMatrix(data = as.matrix(features_c), label = as.matrix(label_c))
  full_data <- xgb.DMatrix(data = as.matrix(full_data))
  # Hyperparameters
  params_t <- list(
    objective = "reg:squarederror", 
    eta = eta_t,                      
    max_depth = max_depth_t
  )
  params_c <- list(
    objective = "reg:squarederror", 
    eta = eta_c,                      
    max_depth = max_depth_c
  )
  # Estimation of functions
  model_t <- xgboost(data = data_matrix_t, params = params_t, nrounds = nrounds_t, verbose=0)
  model_c <- xgboost(data = data_matrix_c, params = params_c, nrounds = nrounds_c, verbose=0)
  # Calculate R^2
  r2_t <-  cor(subset(resample, select=ascites_prev)[,1], predict(model_t, full_data))^2
  r2_c <-  cor(subset(resample, select=ascites_prev)[,1], predict(model_c, full_data))^2
  
  y_1 <- predict(model_t, newdata = full_data)
  y_0 <- predict(model_c, newdata = full_data) 
  
  ITE <- y_1-y_0
  CATE <- mean(ITE)
  mu_0 <- mean(y_0)
  mu_1 <- mean(y_1)
  
  return(c(CATE, mu_0, mu_1, r2_t, r2_c))
}


