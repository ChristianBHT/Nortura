
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(broom)
library(lubridate)
library(dagitty)
library(caret)
library(randomForest)
library(ipred)
library(parallel)
library(boot)
library(xgboost)
library(mlbench)
library(MLmetrics)
library(survey)

setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())

#We only need the following variables 

load("wide_data_for_analysis.Rda")

wide_data$feed_name <- str_replace(wide_data$feed_name, "???", "aa")
wide_data$feed_name <- str_replace(wide_data$feed_name, "???", "o")

table(wide_data$feed_name)

levels = 3  # Set the number of levels other than other
wide_data$feed_group = fct_lump_n(wide_data$feed_name, n = levels, other_level = "other")
table(wide_data$feed_group)

data <- wide_data
data$feed_group = as.factor(data$feed_group)
data$prod_type <- as.factor(data$prod_type)
data$id_slaughterhouse <- as.factor(data$id_slaughterhouse)
data$frequent_month <- as.factor(data$frequent_month)
data$aceties_prev <- 1000*data$aceties/data$n_of_chicken
table(data$feed_group)

data <- subset(data, select = c('feed_group', 
                                'aceties_prev', 
                                'prod_type', 
                                'intercept', 
                                'growth', 
                                'sqr_growth',
                                'indoor_mean_maxtemp',
                                'frequent_month', 
                                'climate_mean_temp',
                                'id_slaughterhouse',
                                'id_hatchery',
                                'id_feedfirm',
                                'average_food', 
                                'average_water',
                                'water_slope', 
                                'water_slope2', 
                                'birds_m_sqr',
                                'bird_slope', 
                                'bird_slope2',
                                'n_of_chicken')) 

data <- na.omit(data)

####################
# Creating Weights #
####################

# WEIGTHS FOR CHICKEN TYPE #
# Naming production types in wide_data
data$type_names <- ifelse(data$prod_type == 4, "Foredling",
                   ifelse(data$prod_type == 6, "Grillkylling",
                   ifelse(data$prod_type == 17, "McDonaldskylling",
                   ifelse(data$prod_type == 21, "Hubbard",
                   ifelse(data$prod_type == 22, "Kyllinggaarden", "Unknown")))))
# Load in Innsett data 
load("raw data/Innsett.Rdata")
Innsett$type_names <- ifelse(Innsett$FK_TypeProduksjon_Dim == 4, "Foredling",
                             ifelse(Innsett$FK_TypeProduksjon_Dim == 6, "Grillkylling",
                                    ifelse(Innsett$FK_TypeProduksjon_Dim == 17, "McDonaldskylling",
                                           ifelse(Innsett$FK_TypeProduksjon_Dim == 21, "Hubbard",
                                                  ifelse(Innsett$FK_TypeProduksjon_Dim == 22, "Kyllinggaarden", "Unknown")))))

Innsett <- subset(Innsett, type_names != "Unknown")
# Counting obs for each production type in Innsett
obs_count <- table(Innsett$type_names)
# Calculating share of production types
obs_share <- prop.table(obs_count)
# Creating a data frame with the distribution
type.dist <- data.frame(type_names = c(names(obs_share)),
                        Freq = nrow(data) * c(as.numeric(obs_share)))

# WEIGTHS FOR SLAUGHTERHOUSE #
# Naming slaughterhouse in data
data$slaughterhouse <- ifelse(data$id_slaughterhouse == 2, "Haa",
                       ifelse(data$id_slaughterhouse == 3, "Elverum",
                       ifelse(data$id_slaughterhouse == 5, "Norsk Kylling",
                       ifelse(data$id_slaughterhouse == 6, "Haerland",
                       ifelse(data$id_slaughterhouse == 7, "Ytteroykylling", "Unknown")))))
table(data$slaughterhouse)
# Load in Innsett data 
load("raw data/Innsett.Rdata")
Innsett$slaughterhouse <- ifelse(Innsett$FK_Slakteri_Dim == 2, "Haa",
                          ifelse(Innsett$FK_Slakteri_Dim == 3, "Elverum",
                          ifelse(Innsett$FK_Slakteri_Dim == 5, "Norsk Kylling",
                          ifelse(Innsett$FK_Slakteri_Dim == 6, "Haerland", "Unknown"))))
Innsett <- subset(Innsett, slaughterhouse != "Unknown")
# Counting obs for each production type in Innsett
obs_count <- table(Innsett$slaughterhouse)
# Calculating share of production types
obs_share <- prop.table(obs_count)
# Creating a data frame with the distribution
slaughter.dist <- data.frame(slaughterhouse = c(names(obs_share)),
                        Freq = nrow(data) * c(as.numeric(obs_share)))

# WEIGTHS FOR HATCHERY #
# Naming hatchery in data
data$hatchery <- ifelse(data$id_hatchery == 1, "Nortura",
                 ifelse(data$id_hatchery == 2, "Haa",
                 ifelse(data$id_hatchery == 3, "Hugaas","Unknown")))

table(data$hatchery)
# Load in Innsett data 
load("raw data/Innsett.Rdata")
Innsett$hatchery <- ifelse(Innsett$FK_Rugeri_Dim == 1, "Nortura",
                    ifelse(Innsett$FK_Rugeri_Dim == 2, "Haa",
                    ifelse(Innsett$FK_Rugeri_Dim == 3, "Hugaas", "Unknown")))
table(Innsett$hatchery)
Innsett <- subset(Innsett, hatchery != "Unknown")
# Counting obs for each production type in Innsett
obs_count <- table(Innsett$hatchery)
# Calculating share of production types
obs_share <- prop.table(obs_count)
# Creating a data frame with the distribution
hatch.dist <- data.frame(hatchery = c(names(obs_share)),
                             Freq = nrow(data) * c(as.numeric(obs_share)))
hatch.dist
table(data$hatchery)

# WEIGTHS FOR FEED PRODUCER #
data$feedfirm <- ifelse(data$id_feedfirm == 4, "FKRA",
                 ifelse(data$id_feedfirm == 9, "Strand Unikorn",
                 ifelse(data$id_feedfirm == 14, "Fiskaa Molle Flisa",
                 ifelse(data$id_feedfirm == 15, "Fiskaa Molle Rogaland",
                 ifelse(data$id_feedfirm == 16, "FKA Ila",
                 ifelse(data$id_feedfirm == 17, "FKA Kambo",
                 ifelse(data$id_feedfirm == 19, "Norgesfor Raade Molle",
                 ifelse(data$id_feedfirm == 22, "Ostmollene AS",
                 ifelse(data$id_feedfirm == 25, "Norgesfor Mysen",
                 ifelse(data$id_feedfirm == 31, "Norgesfor Orkla",
                 ifelse(data$id_feedfirm == 35, "Raade Molle", "Unknown")))))))))))
table(data$id_feedfirm)
load("raw data/Innsett.Rdata")
Innsett$feedfirm <- ifelse(Innsett$FK_Forfirma_Dim == 4, "FKRA",
                    ifelse(Innsett$FK_Forfirma_Dim == 9, "Strand Unikorn",
                    ifelse(Innsett$FK_Forfirma_Dim == 14, "Fiskaa Molle Flisa",
                    ifelse(Innsett$FK_Forfirma_Dim == 15, "Fiskaa Molle Rogaland",
                    ifelse(Innsett$FK_Forfirma_Dim == 16, "FKA Ila",
                    ifelse(Innsett$FK_Forfirma_Dim == 17, "FKA Kambo",
                    ifelse(Innsett$FK_Forfirma_Dim == 19, "Norgesfor Raade Molle",
                    ifelse(Innsett$FK_Forfirma_Dim == 22, "Ostmollene AS",
                    ifelse(Innsett$FK_Forfirma_Dim == 25, "Norgesfor Mysen",
                    ifelse(Innsett$FK_Forfirma_Dim == 31, "Norgesfor Orkla",
                    ifelse(Innsett$FK_Forfirma_Dim == 35, "Raade Molle", "Unknown")))))))))))

table(Innsett$feedfirm)
Innsett <- subset(Innsett, feedfirm != "Unknown")

table(data$feedfirm)
# Counting obs for each feed firm in Innsett
obs_count <- table(Innsett$feedfirm)
# Calculating share of production types
obs_share <- prop.table(obs_count)
# Creating a data frame with the distribution
feedfirm.dist <- data.frame(feedfirm = c(names(obs_share)),
                         Freq = nrow(data) * c(as.numeric(obs_share)))
feedfirm.dist
# Create weights
data.design <- small.svy.unweighted <- svydesign(ids=~1, data=data)
data.rake <- rake(design = data.design,
                       sample.margins = list(~type_names, ~slaughterhouse, ~hatchery, ~feedfirm),
                       population.margins = list(type.dist, slaughter.dist, hatch.dist, feedfirm.dist))
summary(weights(data.rake))
# Trime weights according to a rule of thumb
data.rake.trim <- trimWeights(data.rake, lower=0.3, upper=3,
                                   strict=TRUE)
summary(weights(data.rake.trim))
# Add weights to data set
data$weights <- weights(data.rake.trim)
####################
dummy_vars <- model.matrix(~ 0 + prod_type + frequent_month + id_slaughterhouse, data = data)
data <- cbind(data, dummy_vars)


boxplot(data$aceties_prev ~ feed_group,
        data = data, 
        xlab = "Growth feed type", 
        ylab = "Prevalens of ascites per 1000",
        names = c("Feed 1", "Feed 2", "Feed 3", "Other"))


# The single case
{
data$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)

Y <- 'aceties_prev' 
adjust_set <- c('prod_type4', 'prod_type6', 'prod_type21', 'prod_type22', 'birds_m_sqr', 'average_food',
                'frequent_month2', 'frequent_month3', 'frequent_month4', 'frequent_month5',
                'frequent_month6', 'frequent_month7', 'frequent_month8', 'frequent_month9',
                'frequent_month10', 'frequent_month11', 'frequent_month12', 
                'id_slaughterhouse3','id_slaughterhouse5','id_slaughterhouse6',
                'intercept', 'growth', 'sqr_growth', 'indoor_mean_maxtemp', 'climate_mean_temp')  
treatment_data <- subset(data, treatment == 1)
control_data <- subset(data, treatment == 0)
# The control parameters for cross-validation
ctrl  <- trainControl(
  method = "cv",
  number = 10)


inTraining <- createDataPartition(with(treatment_data, get(Y)), p = 0.65, list = FALSE)
training <- treatment_data[inTraining,]
testing  <- treatment_data[-inTraining,]

# Prepare the data

X_test <- as.matrix(testing[, adjust_set])
y_test <- with(testing, get(Y))
# Train the random forest model with cross-validation
treatment_model <- train(x = as.matrix(training[, adjust_set]), 
                         y =  with(training, get(Y)),
                         method = "xgbTree", 
                         trControl = ctrl, 
                         verbosity = 0)

# treatment_model <- treatment_model$finalModel
#train_predictions <- predict(treatment_model, newdata = X_train) 
test_predictions <- predict(treatment_model, newdata = X_test) 
#train_r2 <- cor(y_train, train_predictions)^2
test_r2_treat <- cor(y_test, test_predictions)^2


inTraining <- createDataPartition(with(control_data, get(Y)), p = 0.8, list = FALSE)
training <- control_data[inTraining,]
testing  <- control_data[-inTraining,]
# Train the random forest model with cross-validation
# Prepare the data

X_test <- as.matrix(testing[, adjust_set])
y_test <- with(testing, get(Y))

# Define the control model: random forest model with cross-validation
control_model <- train(x = as.matrix(training[, adjust_set]), 
                       y =  with(training, get(Y)),
                       method = "xgbTree", 
                       trControl = ctrl, 
                       verbosity = 0)

# Access the cross-validation results
control_model <- control_model$finalModel
#train_predictions <- predict(control_model, newdata = X_train) 
test_predictions <- predict(control_model, newdata = X_test) 

cor(y_test, test_predictions)^2

full_data <- as.matrix(data[, adjust_set])

y_1 <- predict(treatment_model, newdata = full_data)
y_0 <- predict(control_model, newdata = full_data) 

ITE <- y_1-y_0
mean(ITE)
sd(ITE)
}


# Creating a function to calculate bootstrapped statistics 
T_learner <- function(data, index, Y, adjust_set, cv_folds = 4, p = 0.5){ 

  #Easier to work with resample
  resample <- data[index, ]
  
  treatment_data <- subset(resample, treatment == 1)
  control_data <- subset(resample, treatment == 0)
  # The control parameters for cross-validation
  ctrl  <- trainControl(
    method = "cv",
    number = cv_folds)
  

  inTraining <- createDataPartition(with(treatment_data, get(Y)), p = p, list = FALSE)
  training <- treatment_data[inTraining,]
  testing  <- treatment_data[-inTraining,]
  
  # Prepare the data
  X_train <- as.matrix(training[, adjust_set])
  y_train <- with(training, get(Y))
  X_test <- as.matrix(testing[, adjust_set])
  y_test <- with(testing, get(Y))
  # Train the random forest model with cross-validation
  treatment_model <- train(x = X_train, 
                           y = y_train,
                           method = "xgbTree", 
                           trControl = ctrl, 
                           verbosity = 0)
  
  # treatment_model <- treatment_model$finalModel
  #train_predictions <- predict(treatment_model, newdata = X_train) 
  test_predictions <- predict(treatment_model, newdata = X_test) 
  #train_r2 <- cor(y_train, train_predictions)^2
  test_r2_treat <- cor(y_test, test_predictions)^2
  
  
  inTraining <- createDataPartition(with(control_data, get(Y)), p = p, list = FALSE)
  training <- control_data[inTraining,]
  testing  <- control_data[-inTraining,]
  # Train the random forest model with cross-validation
  # Prepare the data
  
  X_train <- as.matrix(training[, adjust_set])
  y_train <- with(training, get(Y))
  X_test <- as.matrix(testing[, adjust_set])
  y_test <- with(testing, get(Y))
  
  # Define the control model: random forest model with cross-validation
  control_model <- train(x = X_train, 
                         y = y_train,
                         method = "xgbTree", 
                         trControl = ctrl, 
                         verbosity = 0)
  
  # Access the cross-validation results
  control_model <- control_model$finalModel
  #train_predictions <- predict(control_model, newdata = X_train) 
  test_predictions <- predict(control_model, newdata = X_test) 
  
  #train_r2 <- cor(y_train, train_predictions)^2
  test_r2_cont <- cor(y_test, test_predictions)^2
  
  full_data <- as.matrix(resample[, adjust_set])
  
  y_1 <- predict(treatment_model, newdata = full_data)
  y_0 <- predict(control_model, newdata = full_data) 
  
  ITE <- y_1-y_0
  CATE <- mean(ITE)
  mu_0 <- mean(y_0)
  mu_1 <- mean(y_1)
  
  return(c(CATE, mu_0, mu_1, test_r2_treat, test_r2_cont))
}
# Direct Effect
Y <- 'aceties_prev' 
adjust_set <- c('prod_type4', 'prod_type6', 'prod_type21', 'prod_type22', 'birds_m_sqr', 'average_food',
                'frequent_month2', 'frequent_month3', 'frequent_month4', 'frequent_month5',
                'frequent_month6', 'frequent_month7', 'frequent_month8', 'frequent_month9',
                'frequent_month10', 'frequent_month11', 'frequent_month12', 
                'id_slaughterhouse3','id_slaughterhouse5','id_slaughterhouse6',
                'intercept', 'growth', 'sqr_growth', 'indoor_mean_maxtemp', 'climate_mean_temp')  

cl <- makeCluster(detectCores()-2, type = "PSOCK")
clusterExport(cl,  varlist=c('T_learner'), envir=environment() )
clusterEvalQ(cl, c(library('boot'), library('caret'), library('ipred'), library('xgboost')))

weights <- weights(data.rake.trim)
R = 100 #Number of bootstrap replica
# Setting treatment 1
data$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)
T_learner_boot_weighted <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, p = 0.7, weights = weights, parallel = 'multicore')
results_treat1 <- T_learner_boot_weighted$t
write.csv(results_treat1, "result_t_lear_kromatK2_w.csv")
 
# Setting treatment 2 
data$treatment <- ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
T_learner_boot_weighted <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, p = 0.7, weights = weights, parallel = 'multicore')
results_treat2 <- T_learner_boot_weighted$t
write.csv(results_treat2, "result_t_lear_toppkyl_w.csv")

# Setting treatment 3 
data$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0)
T_learner_boot_weighted <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, p = 0.7, weights = weights, parallel = 'multicore')
results_treat3 <- T_learner_boot_weighted$t
write.csv(results_treat3, "result_t_lear_kromat2E_w.csv")
stopCluster(cl)

treat1_w <- read.csv("result_t_lear_kromatK2_w.csv")
treat2_w <- read.csv("result_t_lear_toppkyl_w.csv")
treat3_w <- read.csv("result_t_lear_kromat2E_w.csv")
treat1_w$feed <- "Feed 1"
treat2_w$feed <- "Feed 2"
treat3_w$feed <- "Feed 3"
df <- rbind(treat1_w, treat2_w, treat3_w)
ggplot(data = df, aes(x = feed, y = V1)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle(" ") +
  ylim(-1.5, 3)

# Previously saved non-weighted results
treat1 <- read.csv("result_t_lear_kromatK2.csv")
treat2 <- read.csv("result_t_lear_toppkyl.csv")
treat3 <- read.csv("result_t_lear_kromat2E.csv")
treat1$feed <- "Feed 1"
treat2$feed <- "Feed 2"
treat3$feed <- "Feed 3"
df <- rbind(treat1, treat2, treat3)


ggplot(data = df, aes(x = feed, y = V1)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle(" ") +
  ylim(-1.5, 3)


# Total effect (Chicken type, Month, Slaughterhouse)
Y <- 'aceties_prev' 
adjust_set <- c('prod_type17', 'prod_type6', 'prod_type21', 'prod_type22', 
                'frequent_month2', 'frequent_month3', 'frequent_month4', 'frequent_month5',
                'frequent_month6', 'frequent_month7', 'frequent_month8', 'frequent_month9',
                'frequent_month10', 'frequent_month11', 'frequent_month12', 
                'id_slaughterhouse3','id_slaughterhouse5','id_slaughterhouse6')  

cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterExport(cl,  varlist=c('T_learner'), envir=environment() )
clusterEvalQ(cl, c(library('boot'), library('caret'), library('ipred'), library('xgboost')))

weights <- weights(data.rake.trim)
R = 100 #Number of bootstrap replica

# Setting treatment 1
data$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)
T_learner_boot <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, weights = weights, p = 0.7, parallel = 'multicore')
results_treat1 <- T_learner_boot$t
write.csv(results_treat1, "total_t_lear_kromatK2_w.csv")

# Setting treatment 2 
data$treatment <- ifelse(data$feed_group == "Toppkylling Netto", 1, 0)
T_learner_boot <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, weights = weights, p = 0.65, parallel = 'multicore')
results_treat2 <- T_learner_boot$t
write.csv(results_treat2, "total_t_lear_toppkyl_w.csv")

# Setting treatment 3 
data$treatment <- ifelse(data$feed_group == "Kromat Kylling 2 Laag u/k", 1, 0)
T_learner_boot <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, weights = weights, p = 0.65, parallel = 'multicore')
results_treat3 <- T_learner_boot$t
write.csv(results_treat3, "total_t_lear_kromat2E_w.csv")
stopCluster(cl)

treat1 <- read.csv("total_t_lear_kromatK2_w.csv")
treat2 <- read.csv("total_t_lear_toppkyl_w.csv")
treat3 <- read.csv("total_t_lear_kromat2E_w.csv")
treat1$feed <- "Feed 1"
treat2$feed <- "Feed 2"
treat3$feed <- "Feed 3"
df <- rbind(treat1, treat2, treat3)

ggplot(data = df, aes(x = feed, y = V1)) +
  geom_boxplot() +
  xlab("Growth Feed Type") +
  ylab("Treatment effect on the prevalence (1/1000) ascites") +
  ggtitle("Bootstrap distribution T-learner: Growth feed type -> ascites") +
  ylim(-1.5, 3) 


#----------------------------------------------------------------------------------
# Robustness check
# Placebo treatment
# Total effect (Chicken type, Month, Slaughterhouse)
Y <- 'aceties_prev' 
adjust_set <- c('prod_type17', 'prod_type6', 'prod_type21', 'prod_type22', 
                'frequent_month2', 'frequent_month3', 'frequent_month4', 'frequent_month5',
                'frequent_month6', 'frequent_month7', 'frequent_month8', 'frequent_month9',
                'frequent_month10', 'frequent_month11', 'frequent_month12', 
                'id_slaughterhouse3','id_slaughterhouse5','id_slaughterhouse6')  

cl <- makeCluster(detectCores()-1, type = "PSOCK")
clusterExport(cl,  varlist=c('T_learner'), envir=environment() )
clusterEvalQ(cl, c(library('boot'), library('caret'), library('ipred'), library('xgboost')))

weights <- weights(data.rake.trim)
R = 100 #Number of bootstrap replica

# Create a vector of factor levels
factor_levels <- c("Level1", "Level2", "Level3", "Level4")
num_obs <- nrow(data)  # Number of observations
data$random <- sample(factor_levels, num_obs, replace = TRUE)

table(data$random)
# Setting treatment 1
data$treatment <- ifelse(data$random == "Level1", 1, 0)
T_learner_boot <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, weights = weights, p = 0.7, parallel = 'multicore')
results_placebo1 <- T_learner_boot$t

write.csv(results_placebo1, "placebo1.csv")
# Setting treatment 1
data$treatment <- ifelse(data$random == "Level2", 1, 0)
T_learner_boot <- boot(data, statistic = T_learner, R = R, Y = Y, adjust_set = adjust_set, weights = weights, p = 0.7, parallel = 'multicore')
results_placebo2 <- T_learner_boot$t

write.csv(results_placebo1, "placebo2.csv")


#----------------------------------------------------------------------------------


