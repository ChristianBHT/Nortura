library(tidyr)
library(dplyr)
library(ggplot2)
library(dagitty)
library(caret)
library(ipred)
library(boot)
library(xgboost)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

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

formula <- ascites_prev ~ prod_type + frequent_month + id_slaughterhouse

label <- subset(data, select = c(as.character(update(formula, . ~ .)[[2]])))

independent_vars <- all.vars(formula)[-1]
features <- data %>% select(independent_vars)

#look for features factor variables 
factor_variables <- names(features)[sapply(features, is.factor)]

#Create dummy variables from factor variables
dummy_matrix <- model.matrix(~ . - 1, data = data[, factor_variables])
features <- cbind(features, dummy_matrix)
features <- features %>%
  select(-any_of(factor_variables))
data_matrix <- xgb.DMatrix(data = as.matrix(features), label = as.matrix(label))

params <- list(
  objective = "reg:squarederror", 
  eta = 0.1,                      
  max_depth = 3
)

# Perform k-fold cross-validation
cv_result <- xgb.cv(
  params = params,
  data = xgb.DMatrix(data = as.matrix(features), label = as.matrix(label)),
  nfold = 10,                     # Number of folds
  verbose = TRUE,
  nrounds = 300
  
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
  scale_x_continuous(breaks = seq(0, max(cv$Boosting_Round), by = 10))  #


# Estimating 
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

formula <- ascites_prev ~ treatment + prod_type + frequent_month + id_slaughterhouse
S_learner_total <- boot(data=data, 
                       statistic = S_learner, 
                       formula = formula, 
                       weights = dirichlet_w, 
                       R=rep(1,R),
                       eta = 0.1,                      
                       max_depth = 3,
                       nrounds = 25)

S_total_boot_result <- S_learner_total$t
save(S_total_boot_result,file="C:/broiler_acites/ascites_case/Results/S_total_boot_result.Rda")
hist(S_learner_total$t[,1])
