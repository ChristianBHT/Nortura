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

#-----------------------------------------------------------------------------
# Total effect where W union X is prod_type, frequent_month, id_slaughterhouse
#-----------------------------------------------------------------------------

# Treatment feed 1
levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
# table(data$feed_group)
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

formula <- ascites_prev ~ treatment + prod_type + frequent_month + id_slaughterhouse

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
  nrounds = 30
  
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

S_total_boot_feed1 <- S_learner_total$t
save(S_total_boot_feed1,file="C:/broiler_acites/ascites_case/Results/S_total_boot_feed1.Rda")

# hist(S_total_boot_result[,1])
# boxplot(S_total_boot_result[,1], 
#         main = "Boxplot Total effect",    # Title of the plot
#         ylab = "Values",            # Label for the y-axis
#         col = "lightblue",          # Color of the boxes
#         border = "blue",            # Color of the borders
#         horizontal = FALSE,
#         add = F # Set to TRUE for horizontal orientation
# )

# Treatment feed 2
# levels = 4  # Set the number of levels other than other
# data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
# table(data$feed_group)
data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)

# Estimating 
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_learner_total <- boot(data=data, 
                        statistic = S_learner, 
                        formula = formula, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta = 0.1,                      
                        max_depth = 3,
                        nrounds = 27)

S_total_boot_feed2 <- S_learner_total$t
save(S_total_boot_feed2,file="C:/broiler_acites/ascites_case/Results/S_total_boot_feed2.Rda")

hist(S_total_boot_feed2[,1])
boxplot(S_total_boot_result[,1], 
        main = "Boxplot Total effect",    # Title of the plot
        ylab = "Values",            # Label for the y-axis
        col = "lightblue",          # Color of the boxes
        border = "blue",            # Color of the borders
        horizontal = FALSE,
        add = F # Set to TRUE for horizontal orientation
)

# Estimating treatment effect for last feed group
# table(data$feed_group)
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Leg u/k", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_learner_total <- boot(data=data, 
                        statistic = S_learner, 
                        formula = formula, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta = 0.1,                      
                        max_depth = 3,
                        nrounds = 27)

S_total_boot_feed3 <- S_learner_total$t
save(S_total_boot_feed3,file="C:/broiler_acites/ascites_case/Results/S_total_boot_feed3.Rda")
treat1 <- data.frame(S_total_boot_feed1[,1])
treat2 <- data.frame(S_total_boot_feed2[,1])
treat3 <- data.frame(S_total_boot_feed3[,1])
treat1$feed <- "Feed 1"
colnames(treat1) <- c('values', 'feed')
treat2$feed <- "Feed 2"
colnames(treat2) <- c('values', 'feed')
treat3$feed <- "Feed 3"
colnames(treat3) <- c('values', 'feed')
df <- rbind(treat1, treat2, treat3)

ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("S-learner ATE Total") 

#-----------------------------------------------------------------------------
# Direct effect where W union X is Birds p m-sqr, Chicken type, Food consumption, Growth, Growth$^s$, Indoor Temperature, Month, Outdoor temperature, Slaughterhouse, Water consumption, start weight
#-----------------------------------------------------------------------------

# Treatment feed 1
levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
# table(data$feed_group)
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

formula <- ascites_prev ~ treatment + prod_type + frequent_month + id_slaughterhouse

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
  nrounds = 30
  
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



#-----------------------------------------------------------------------------
# Direct effect (attempting CATE) where W union X is prod_type, frequent_month, id_slaughterhouse
#-----------------------------------------------------------------------------



