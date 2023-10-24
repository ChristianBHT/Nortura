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

plot_total <- ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("S-learner ATE Total")

png("C:/broiler_acites/ascites_case/Results/plot_total_Sl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()
plot_total

#-----------------------------------------------------------------------------
# Direct effect where W union X is Birds p m-sqr, Chicken type, Food consumption, Growth, Growth$^s$, Indoor Temperature, Month, Outdoor temperature, Slaughterhouse, Water consumption, start weight
#-----------------------------------------------------------------------------

# table(data$feed_group)
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

formula <- ascites_prev ~ treatment + prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight

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
  nrounds = 12
  
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

N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_learner_direct <- boot(data=data, 
                        statistic = S_learner, 
                        formula = formula, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta = 0.1,                      
                        max_depth = 3,
                        nrounds = 12)

S_direct_boot_feed1 <- S_learner_direct$t
save(S_direct_boot_feed1,file="C:/broiler_acites/ascites_case/Results/S_direct_boot_feed1.Rda")

data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_learner_direct <- boot(data=data, 
                         statistic = S_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta = 0.1,                      
                         max_depth = 3,
                         nrounds = 12)

S_direct_boot_feed2 <- S_learner_direct$t
save(S_direct_boot_feed2,file="C:/broiler_acites/ascites_case/Results/S_direct_boot_feed2.Rda")


data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Leg u/k", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_learner_direct <- boot(data=data, 
                         statistic = S_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta = 0.1,                      
                         max_depth = 3,
                         nrounds = 12)

S_direct_boot_feed3 <- S_learner_direct$t
save(S_direct_boot_feed3,file="C:/broiler_acites/ascites_case/Results/S_direct_boot_feed3.Rda")

treat_d1 <- data.frame(S_direct_boot_feed1[,1])
treat_d2 <- data.frame(S_direct_boot_feed2[,1])
treat_d3 <- data.frame(S_direct_boot_feed3[,1])
treat_d1$feed <- "Feed 1"
colnames(treat_d1) <- c('values', 'feed')
treat_d2$feed <- "Feed 2"
colnames(treat_d2) <- c('values', 'feed')
treat_d3$feed <- "Feed 3"
colnames(treat_d3) <- c('values', 'feed')
df <- rbind(treat_d1, treat_d2, treat_d3)

plot_total <- ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("S-learner ATE Total")

png("C:/broiler_acites/ascites_case/Results/plot_direct_Sl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()

#-------------------------------------------------------------------------------------------------
# Direct effect (attempting CATE) where W union X is Birds p m sqr, Chicken type, Food consumption, Growth_linear, Growth sqr, Indoor Temperature, Indoor humidity, Kg per m sqr, Month, Outdoor humidity, Outdoor temperature, Slaughter age, Slaughterhouse, Water consumption, start weight
#-----------------------------------------------------------------------------------------------

# table(data$feed_group)
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

formula <- ascites_prev ~ treatment + birds_m_sqr + prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + indoor_mean_maxhumidity + kg_m_sqr + frequent_month + climate_mean_hum + climate_mean_temp + id_slaughterhouse + average_water + start_weight

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
  nrounds = 15
  
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
# Kromat Kylling 2 Enkel uk
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_learner_direct <- boot(data=data, 
                         statistic = S_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta = 0.1,                      
                         max_depth = 3,
                         nrounds = 15)

S_CATE_boot_feed1 <- S_learner_direct$t
save(S_CATE_boot_feed1,file="C:/broiler_acites/ascites_case/Results/S_CATE_boot_feed1.Rda")

# Topp Kylling Netto
data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)
S_learner_direct <- boot(data=data, 
                         statistic = S_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta = 0.1,                      
                         max_depth = 3,
                         nrounds = 15)

S_CATE_boot_feed2 <- S_learner_direct$t
save(S_CATE_boot_feed2,file="C:/broiler_acites/ascites_case/Results/S_CATE_boot_feed2.Rda")

# Topp Kylling Netto
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)
S_learner_direct <- boot(data=data, 
                         statistic = S_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta = 0.1,                      
                         max_depth = 3,
                         nrounds = 15)

S_CATE_boot_feed3 <- S_learner_direct$t
save(S_CATE_boot_feed3,file="C:/broiler_acites/ascites_case/Results/S_CATE_boot_feed3.Rda")

treat_cate1 <- data.frame(S_CATE_boot_feed1[,1])
treat_cate2 <- data.frame(S_CATE_boot_feed2[,1])
treat_cate3 <- data.frame(S_CATE_boot_feed3[,1])
treat_cate1$feed <- "Feed 1"
colnames(treat_cate1) <- c('values', 'feed')
treat_cate2$feed <- "Feed 2"
colnames(treat_cate2) <- c('values', 'feed')
treat_cate3$feed <- "Feed 3"
colnames(treat_cate3) <- c('values', 'feed')
df <- rbind(treat_cate1, treat_cate2, treat_cate3)

plot_total <- ggplot(data = df, aes(x = feed, y = values)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("S-learner ATE Total")
plot_total
png("C:/broiler_acites/ascites_case/Results/plot_cate_Sl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()



