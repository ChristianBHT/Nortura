library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(ipred)
library(forcats)
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
table(data$feed_group)
#--------------------------------------------------------------------------------
# The first part of the script is to tune the base models and get hyperparameters
#--------------------------------------------------------------------------------
data_t <- subset(data, treatment == 1)
data_c <- subset(data, treatment == 0) 
independent_vars <- all.vars(formula)[-1]

label_t <- subset(data_t, select = c( as.character(update(formula, . ~ .)[[2]])))
label_c <- subset(data_c, select = c( as.character(update(formula, . ~ .)[[2]])))

features_t <- data_t %>% select(independent_vars)
features_c <- data_c %>% select(independent_vars)
#look for features factor variables 
factor_variables <- names(features_t)[sapply(features_t, is.factor)]
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
  nrounds = 10
  
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
#------------------------------------------------------------------
# Estimating total effect treatment = "Kromat Kylling 2 Enkel u/k"
#------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)
formula <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse

# Get hyperparameters from the script above
T_learner_total <- boot(data=data, 
                        statistic = T_learner, 
                        formula = formula, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta_t = 0.02,                      
                        max_depth_t = 10,
                        nrounds_t = 60,
                        eta_c = 0.1,                      
                        max_depth_c = 4,
                        nrounds_c = 20)
T_total_boot_feed1 <- T_learner_total$t
save(T_total_boot_feed1,file="C:/broiler_acites/ascites_case/Results/T_total_boot_feed1.Rda")

#------------------------------------------------------------------
# Estimating total effect treatment = "Toppkylling Netto"
#------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)
formula <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse

# Get hyperparameters from the script above
T_learner_total <- boot(data=data, 
                        statistic = T_learner, 
                        formula = formula, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta_t = 0.1,                      
                        max_depth_t = 4,
                        nrounds_t = 15,
                        eta_c = 0.1,                      
                        max_depth_c = 4,
                        nrounds_c = 20)
T_total_boot_feed2 <- T_learner_total$t
save(T_total_boot_feed2,file="C:/broiler_acites/ascites_case/Results/T_total_boot_feed2.Rda")

#------------------------------------------------------------------
# Estimating total effect treatment = "Kromat Kylling 2 Leg u/k "
#------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Leg u/k", 1, 0)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica


dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)
formula <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse

# Get hyperparameters from the script above
T_learner_total <- boot(data=data, 
                        statistic = T_learner, 
                        formula = formula, 
                        weights = dirichlet_w, 
                        R=rep(1,R),
                        eta_t = 0.1,                      
                        max_depth_t = 4,
                        nrounds_t = 15,
                        eta_c = 0.1,                      
                        max_depth_c = 4,
                        nrounds_c = 20)
T_total_boot_feed3 <- T_learner_total$t
save(T_total_boot_feed3,file="C:/broiler_acites/ascites_case/Results/T_total_boot_feed3.Rda")
# Creating bocplots of the distributions
treat1 <- data.frame(T_total_boot_feed1[,1])
treat2 <- data.frame(T_total_boot_feed2[,1])
treat3 <- data.frame(T_total_boot_feed3[,1])
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
  ggtitle("T-learner ATE Total effect")

png("C:/broiler_acites/ascites_case/Results/plot_total_Tl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()
plot_total
#-------------------------------------------------------------------------------------------
# Estimating direct effect treatment = "Kromat Kylling 2 Enkel u/k"
#-------------------------------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

formula <- ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica


dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_direct <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.1,                      
                         max_depth_t = 4,
                         nrounds_t = 10,
                         eta_c = 0.1,                      
                         max_depth_c = 4,
                         nrounds_c = 15)
T_direct_boot_feed1 <- T_learner_direct$t
save(T_direct_boot_feed1,file="C:/broiler_acites/ascites_case/Results/T_direct_boot_feed1.Rda")
#-------------------------------------------------------------------------------------------
# Estimating direct effect treatment = "Toppkylling Netto"
#-------------------------------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)

N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica


dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_direct <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.1,                      
                         max_depth_t = 4,
                         nrounds_t = 10,
                         eta_c = 0.1,                      
                         max_depth_c = 4,
                         nrounds_c = 15)
T_direct_boot_feed2 <- T_learner_direct$t
save(T_direct_boot_feed2,file="C:/broiler_acites/ascites_case/Results/T_direct_boot_feed2.Rda")

#-------------------------------------------------------------------------------------------
# Estimating direct effect treatment = "Kromat Kylling 2 Leg u/k"
#-------------------------------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Leg u/k", 1, 0)

N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica


dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_direct <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.05,                      
                         max_depth_t = 10,
                         nrounds_t = 20,
                         eta_c = 0.1,                      
                         max_depth_c = 4,
                         nrounds_c = 15)
T_direct_boot_feed3 <- T_learner_direct$t
save(T_direct_boot_feed3,file="C:/broiler_acites/ascites_case/Results/T_direct_boot_feed3.Rda")
#-----------------------------------------------------------------------------------------
# Creating boxplots of the treatment effects
#-----------------------------------------------------------------------------------------
treat1 <- data.frame(T_direct_boot_feed1[,1])
treat2 <- data.frame(T_direct_boot_feed2[,1])
treat3 <- data.frame(T_direct_boot_feed3[,1])
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
  ggtitle("T-learner ATE Direct effect")

png("C:/broiler_acites/ascites_case/Results/plot_total_Tl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()
plot_total
#------------------
# CATE
#------------------
#-------------------------------------------------------------------------------------------
# Estimating conditional direct effect treatment = "Kromat Kylling 2 Enkel u/k"
#-------------------------------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

formula <- ascites_prev ~  birds_m_sqr + prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + indoor_mean_maxhumidity + kg_m_sqr + frequent_month + climate_mean_hum + climate_mean_temp + id_slaughterhouse + average_water + start_weight
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_cate <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.1,                      
                         max_depth_t = 3,
                         nrounds_t = 8,
                         eta_c = 0.1,                      
                         max_depth_c = 4,
                         nrounds_c = 15)
T_cate_boot_feed1 <- T_learner_cate$t
save(T_cate_boot_feed1,file="C:/broiler_acites/ascites_case/Results/T_cate_boot_feed1.Rda")
#-------------------------------------------------------------------------------------------
# Estimating direct effect treatment = "Toppkylling Netto"
#-------------------------------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)

N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica


dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_cate <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.01,                      
                         max_depth_t = 2,
                         nrounds_t = 50,
                         eta_c = 0.1,                      
                         max_depth_c = 4,
                         nrounds_c = 15)
T_cate_boot_feed2 <- T_learner_cate$t
save(T_cate_boot_feed2,file="C:/broiler_acites/ascites_case/Results/T_cate_boot_feed2.Rda")

#-------------------------------------------------------------------------------------------
# Estimating direct effect treatment = "Kromat Kylling 2 Leg u/k"
#-------------------------------------------------------------------------------------------
data$treatment <-  ifelse(data$feed_group == "Kromat Kylling 2 Leg u/k", 1, 0)

N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_cate <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.05,                      
                         max_depth_t = 4,
                         nrounds_t = 10,
                         eta_c = 0.1,                      
                         max_depth_c = 4,
                         nrounds_c = 10)
T_cate_boot_feed3 <- T_learner_cate$t
save(T_cate_boot_feed3,file="C:/broiler_acites/ascites_case/Results/T_cate_boot_feed3.Rda")
#-----------------------------------------------------------------------------------------
# Creating boxplots of the treatment effects
#-----------------------------------------------------------------------------------------
treat1 <- data.frame(T_cate_boot_feed1[,1])
treat2 <- data.frame(T_cate_boot_feed2[,1])
treat3 <- data.frame(T_cate_boot_feed3[,1])
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
  ggtitle("T-learner CATE Direct effect")

png("C:/broiler_acites/ascites_case/Results/plot_total_Tl.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot_total, col = "lightblue", main = " ", xlab = "Difference in R-Squared", freq = F)
dev.off()
plot_total
hist(T_cate_boot_feed1[,5])
summary(lm(formula = ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight,
           data = data))
