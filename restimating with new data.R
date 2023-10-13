library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(ipred)
library(forcats)
library(boot)
library(xgboost)

setwd("C:/Users/christian.thorjussen/Project Nortura/Nytt datauttrekk")
rm(list = ls())

load("wide_newdata_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed, "�", "aa")
wide_data$feed_name <- str_replace(wide_data$feed, "�", "o")
wide_data$feed_name <- str_replace(wide_data$feed, "o?=", "aa")
wide_data <- subset(wide_data, hybrid == "Ross 308")
data <- wide_data
names(data)[names(data) == 'aceties'] <- 'ascites'
data$feed_name <- as.factor(data$feed_name)
data$prod_type <- as.factor(data$prod_type)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$ascites_prev <- data$ascites/data$N_of_chicken

data$frequent_month <- as.factor(data$frequent_month)

#-----------------------------------------------------------------------------
# Total effect where W union X is prod_type, frequent_month, id_slaughterhouse
#-----------------------------------------------------------------------------

# Treatment feed 1
names(data)[names(data) == 'feed'] <- 'feed_group'

levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
table(data$feed_group)

data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)

#--------------------------------------------------------------------------------
# The first part of the script is to tune the base models and get hyperparameters
#--------------------------------------------------------------------------------
# Formula total effect
formula_t <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse
# Formula direct effect
formula_d <- ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight

formula <- formula_t

data_t <- subset(data, treatment == 1)
data_c <- subset(data, treatment == 0) 
independent_vars <- all.vars(formula)[-1]
independent_vars
label_t <- subset(data_t, select = c( as.character(update(formula, . ~ .)[[2]])))
label_c <- subset(data_c, select = c( as.character(update(formula, . ~ .)[[2]])))
label_c
features_t <- data_t %>% select(all_of(independent_vars))
features_c <- data_c %>% select(all_of(independent_vars))

#look for features factor variables 
factor_variables <- names(features_c)[sapply(features_c, is.factor)]
#Create dummy variables from factor variables
dummy_matrix_t <- model.matrix(~ . - 1, data = data_t[, factor_variables])
dummy_matrix_c <- model.matrix(~ . - 1, data = data_c[, factor_variables])
# Bind with features data
features_t <- cbind(features_t, dummy_matrix_t)
features_c <- cbind(features_c, dummy_matrix_c)
# Remove the original factor variables
features_t <- features_t %>%
  select(-any_of(factor_variables))
features_c <- features_c %>%
  select(-any_of(factor_variables))
# Create the data matrix for the XGBoost function
data_matrix_t <- xgb.DMatrix(data = as.matrix(features_t), label = as.matrix(label_t))
data_matrix_c <- xgb.DMatrix(data = as.matrix(features_c), label = as.matrix(label_c))

# Hyperparameters
params <- list(
  objective = "reg:squarederror", 
  eta = 0.1,                      
  max_depth = 4)

# Estimation of functions
# Perform k-fold cross-validation
cv_result <- xgb.cv(
  params = params,
  data = xgb.DMatrix(data = as.matrix(features_c), label = as.matrix(label_c)),
  nfold = 5,                     # Number of folds
  verbose = TRUE,
  nrounds = 100
  
)

cv <- data.frame(Boosting_Round =  cv_result$evaluation_log$iter, 
                 Train_Error = cv_result$evaluation_log$train_rmse_mean, 
                 Test_Error = cv_result$evaluation_log$test_rmse_mean)

train_error <- cv_result$evaluation_log$train_rmse_mean
test_error <- cv_result$evaluation_log$test_rmse_mean


ggplot(cv, aes(x = Boosting_Round)) +
  geom_line(aes(y = Train_Error, color = "Train Error"), size = 1) +
  geom_line(aes(y = Test_Error, color = "Test Error"), size = 1) +
  scale_color_manual(values = c("Train Error" = "blue", "Test Error" = "red")) +
  labs(x = "Boosting Round", y = "Error Rate") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(cv$Boosting_Round), by = 10))  
