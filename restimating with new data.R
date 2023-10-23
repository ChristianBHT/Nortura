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

load("wide_data_for_analysis.Rda")

wide_data <- subset(wide_data, hybrid == "Ross 308")
data <- wide_data
names(data)[names(data) == 'aceties'] <- 'ascites'
data$feed_name <- data$feed
data$feed_name <- as.factor(data$feed_name)
data$prod_type <- as.factor(data$prod_type)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$ascites_prev <- data$ascites/data$n_of_chicken

boxplot(data$ascites_prev ~ data$prod_type, 
        main = "Boxplot of var1 by group_id",
        xlab = "group_id",
        ylab = "var1")

data$frequent_month <- as.factor(data$frequent_month)
hist(data$ascites_prev)
# Formula total effect
formula_t <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse
# Formula direct effect
formula_d <- ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight

formula <- formula_t
#-----------------------------------------------------------------------------
# Total effect where W union X is prod_type, frequent_month, id_slaughterhouse
#-----------------------------------------------------------------------------

# Treatment feed 1
names(data)[names(data) == 'feed'] <- 'feed_group'

levels = 2  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
table(data$feed_group)
data$treatment_2 <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
data$treatment_3 <-  ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0)

data$treatment <- ifelse(data$treatment_3 == 1, NA, ifelse(data$treatment_2 == 1, 1,0))
data_t <- subset(data, treatment == 1)
data_c <- subset(data, treatment == 0) 
independent_vars <- all.vars(formula)[-1]

label_t <- subset(data_t, select = c( as.character(update(formula, . ~ .)[[2]])))
label_c <- subset(data_c, select = c( as.character(update(formula, . ~ .)[[2]])))

features_t <- data_t %>% select(independent_vars)
features_c <- data_c %>% select(independent_vars)
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
  max_depth = 3)

# Estimation of functions
# Perform k-fold cross-validation
cv_result <- xgb.cv(
  params = params,
  data = xgb.DMatrix(data = as.matrix(features_c), label = as.matrix(label_c)),
  nfold = 2,                     # Number of folds
  verbose = TRUE,
  nrounds = 60
  
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

#---------------Estimating new effects---------------------------------



summary(data$treatment)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_total <- boot(data=data, 
                        statistic = T_learner, 
                        formula = formula_t, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta_t = 0.01,                      
                        max_depth_t = 3,
                        nrounds_t = 50,
                        eta_c = 0.1,                      
                        max_depth_c = 4,
                        nrounds_c = 10)
T_total_boot_feed2 <- T_learner_total$t
hist(T_total_boot_feed2[,1])
