# In sample global validation
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
data$treatment <- sample(0:1, nrow(data), replace = TRUE)
# Formula total effect
formula_t <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse
# Formula direct effect
formula_d <- ascites_prev ~  prod_type + average_food + growth + sqr_growth + indoor_mean_maxtemp + frequent_month + climate_mean_temp + id_slaughterhouse + average_water + start_weight

data$treatment <- sample(0:1, nrow(data), replace = TRUE)
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
                        eta_t = 0.1,                      
                        max_depth_t = 4,
                        nrounds_t = 10,
                        eta_c = 0.1,                      
                        max_depth_c = 4,
                        nrounds_c = 10)
T_total_boot_fake <- T_learner_total$t
hist(T_total_boot_fake[,1])
save(T_total_boot_feed1,file="C:/broiler_acites/ascites_case/Results/T_total_boot_fake.Rda")

data$treatment <- sample(0:1, nrow(data), replace = TRUE)
N <- nrow(data) # Bootstrap obs per resampling
R = 1000 #Number of bootstrap replica

# Creating a matrix of dirichlet weights for Bayesian bootstrap
dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) 
dirichlet_w <- dirichlet / rowSums(dirichlet)

# Get hyperparameters from the script above
T_learner_direct <- boot(data=data, 
                         statistic = T_learner, 
                         formula = formula_d, 
                         weights = dirichlet_w, 
                         R=rep(1,R),
                         eta_t = 0.1,                      
                         max_depth_t = 3,
                         nrounds_t = 10,
                         eta_c = 0.1,                      
                         max_depth_c = 3,
                         nrounds_c = 10)
T_direct_boot_fake <- T_learner_direct$t
hist(T_direct_boot_fake[,1])
save(T_direct_boot_fake,file="C:/broiler_acites/ascites_case/Results/T_direct_boot_fake.Rda")


# Adding a random variable
data$random <- rnorm(nrow(data),mean = 0, sd = 1)
# Treatment feed 1
levels = 4  # Set the number of levels other than other
data$feed_group = fct_lump_n(data$feed_name, n = levels, other_level = "other")
table(data$feed_group)
data$treatment_1 <-  ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)
data$treatment_2 <-  ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
data$treatment_3 <-  ifelse(data$feed_group == "Kromat Kylling 2 Leg u/k", 1, 0)
data$treatment  <- ifelse(data$treatment_1 == 1 | data$treatment_2 == 1 | data$treatment_3 == 1 , 1, 0)
data$treatment <- ifelse(data$treatment_2 == 1 | data$treatment_3 == 1, NA, ifelse(data$treatment_1 == 1, 1,0))

formula_t <- ascites_prev ~  prod_type + frequent_month + id_slaughterhouse + random

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
T_total_boot_random <- T_learner_total$t
hist(T_total_boot_random[,1])
save(T_total_boot_random,file="C:/broiler_acites/ascites_case/Results/T_total_boot_fake.Rda")

hist(data$ascites_prev)
shape <- 2    # Shape parameter (α)
rate <- 1.5   # Rate parameter (β)

# Generate a gamma random variable
gamma_var <- rgamma(n = 1000, shape = shape, rate = rate)
hist(gamma_var)

library(MASS)
initial_params <- list(shape = 10, rate = 5)
# Fit a gamma distribution to your data
fit <- fitdistr(data$ascites_prev, densfun = "gamma", start = initial_params)

# Print the estimated parameters
cat("Estimated shape (alpha):", fit$estimate[1], "\n")
cat("Estimated rate (beta):", fit$estimate[2], "\n")




