library(dagitty)
library(tidyverse)
library(caret)
library(ipred)
library(dplyr)
library(forcats)
library(stringr)
library(randomForest)
library(xgboost)
library(boot)
rm(list = ls())

setwd("C:/Users/christian.thorjussen/Project Nortura/")
source("C:/broiler_acites/ascites_case/step 1 merging dataframe.R")
source("C:/broiler_acites/ascites_case/step 3 data cleaning.R")
source("C:/broiler_acites/ascites_case/step 4a modeling growth curves.R")
source("C:/broiler_acites/ascites_case/step 4c data management.R")

load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")
# Assuming your data frame is named wide_data
wide_data <- subset(wide_data, hybrid == "Ross 308")

DAGmodell <- dagitty('dag {
bb="-4.723,-5.908,5.144,4.316"
"Birds p m-sqr" [pos="-2.761,-1.840"]
"Chicken type" [pos="3.292,3.308"]
"Feed mix" [exposure,pos="1.460,3.388"]
"Food consumption" [pos="-3.112,1.610"]
"Indoor Temperature" [pos="-2.217,3.750"]
"Indoor humidity" [pos="-2.754,3.048"]
"Kg per m-sqr" [pos="-3.298,-1.091"]
"Outdoor humidity" [pos="2.642,-2.036"]
"Outdoor temperature" [pos="2.656,-0.054"]
"Slaughter age" [pos="-2.375,-2.520"]
"Water consumption" [pos="-2.926,2.335"]
Ascites [outcome,pos="0.489,-2.058"]
CO2 [latent,pos="-3.291,0.863"]
Growth_linear [pos="-1.921,-3.100"]
Growth_sqr [pos="-0.925,-4.130"]
Month [pos="3.263,2.165"]
Slaughterhouse [adjusted,pos="0.489,-3.983"]
start_weight [pos="-1.399,-3.700"]
"Birds p m-sqr" -> "Kg per m-sqr"
"Birds p m-sqr" -> Ascites
"Chicken type" -> "Birds p m-sqr"
"Chicken type" -> "Feed mix"
"Chicken type" -> "Kg per m-sqr"
"Chicken type" -> "Slaughter age"
"Chicken type" -> Ascites
"Chicken type" -> Growth_linear
"Feed mix" -> "Food consumption"
"Feed mix" -> "Water consumption"
"Feed mix" -> Ascites
"Feed mix" -> Growth_linear
"Feed mix" -> Growth_sqr
"Feed mix" -> start_weight
"Food consumption" -> Ascites
"Food consumption" -> Growth_linear
"Indoor Temperature" -> "Indoor humidity"
"Indoor Temperature" -> "Water consumption"
"Indoor Temperature" -> Ascites
"Indoor Temperature" -> Growth_linear
"Indoor humidity" -> Ascites
"Kg per m-sqr" -> Ascites
"Outdoor humidity" -> "Indoor humidity"
"Outdoor humidity" -> Ascites
"Outdoor temperature" -> "Indoor Temperature"
"Outdoor temperature" -> "Indoor humidity"
"Outdoor temperature" -> "Outdoor humidity"
"Outdoor temperature" -> "Water consumption"
"Outdoor temperature" -> Ascites
"Outdoor temperature" -> CO2
"Slaughter age" -> Ascites
"Water consumption" -> Ascites
"Water consumption" -> Growth_linear
CO2 -> Ascites
Growth_linear -> "Slaughter age"
Growth_linear -> Ascites
Growth_sqr -> Ascites
Month -> "Feed mix"
Month -> "Indoor humidity"
Month -> "Outdoor humidity"
Month -> "Outdoor temperature"
Month -> Ascites
Month -> CO2
Month -> Growth_linear
Slaughterhouse -> Ascites
start_weight -> Ascites
start_weight -> Growth_linear
start_weight -> Growth_sqr
}')
#Show the model
plot(DAGmodell)
impliedConditionalIndependencies(DAGmodell)

# Birds p m-sqr _||_ Food consumption | Feed mix




#Create a feed_group variable instead of feed type since it has so many levels
levels = 7  # Set the number of levels other than other
wide_data$feed_group = fct_lump_n(wide_data$feed_name, n = levels, other_level = "other")

wide_data$feed_group = as.factor(wide_data$feed_group)

table(wide_data$feed_group)


##########################################
# Birds p m-sqr _||_ Food consumption | Feed mix
##########################################
randomf <- list()
bag <- list()
XGboost <- list()

for (i in 1:100){
  data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
  data <- na.omit(data)
  
  #Partitioning data into training and test
  inTraining <- createDataPartition(data$birds_m_sqr, p = 0.8, list = FALSE)
  training <- data[inTraining,]
  testing  <- data[-inTraining,]

  #Random Forest
  rf <- randomForest(birds_m_sqr  ~ average_food + feed_group, 
                   data=training,
                   ntree=500)
  test.features <- subset(testing, select=c(average_food, feed_group))
  test.target <- subset(testing, select=birds_m_sqr)[,1]
  predictions <- predict(rf, newdata = test.features)
  rf_R2 <- cor(test.target, predictions) ^ 2
  
  #Bagged tree
  bagged <- bagging(
    birds_m_sqr ~ average_food + feed_group,
    data = training,
    coob = TRUE,
  )
  
  test.features <- subset(testing, select=c(average_food, feed_group))
  test.target <- subset(testing, select=birds_m_sqr)[,1]
  predictions <- predict(bagged, newdata = test.features)
  bagged_R2 <- cor(test.target, predictions) ^ 2


  #XGBoost
  data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
  data <- na.omit(data)
  food <- data.frame(food = data$feed_group)
  dummy_vars <- model.matrix(~ factor(food), food)
  dummy_vars <- dummy_vars[, -1]

# Bind the dummy variables to the original data frame
  data <- cbind(data, dummy_vars)
  data <- subset(data, select = -feed_group)
  colnames(data) <- c('average_food', 'birds_m_sqr', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7')


  # Split the data into training and testing sets
  inTraining <- createDataPartition(data$birds_m_sqr, p = 0.8, list = FALSE)
  training <- data[inTraining,]
  testing  <- data[-inTraining,]

  label <- training$birds_m_sqr
  training <- as.matrix(training[, !(names(training) %in% c("birds_m_sqr"))])
 
  # Training XGBoost model
  
  xgb_model <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 20,
    eta = 0.3,                      
    max_depth = 6)

  # Make predictions on the test set
  test.target <- testing$birds_m_sqr
  testing <- as.matrix(testing[, !(names(testing) %in% c("birds_m_sqr"))])
  predictions <- predict(xgb_model, as.matrix(testing))

  #  Calculate r-squared
  XGB_R2 <- cor(test.target, predictions) ^ 2
  
  randomf <- append(randomf, rf_R2)
  bag <- append(bag, bagged_R2)
  XGboost <- append(XGboost, XGB_R2)
  
  print(paste0('Loop ',i,''))
}

rf <- unlist(randomf)
bag <- unlist(bag)
XGB <- unlist(XGboost)
summary(rf)
summary(bag)
summary(XGB)
rm(rf, bag, XGB)
write.csv(rf,file='C:/broiler_acites/ascites_case/Results/rf_intro_test Birds Food Feed.csv', row.names = FALSE)
write.csv(bag,file='C:/broiler_acites/ascites_case/Results/bag_intro_test Birds Food Feed.csv', row.names = FALSE)
write.csv(XGB,file='C:/broiler_acites/ascites_case/Results/XGB_intro_test Birds Food Feed.csv', row.names = FALSE)

# Tuning XGBoost
#XGBoost
#Create a data frame with the month variable
data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
data <- na.omit(data)
food <- data.frame(food = data$feed_group)

#Create dummy variables for feed variable
dummy_vars <- model.matrix(~ factor(food), food)

# Remove the intercept column
dummy_vars <- dummy_vars[, -1]

# Bind the dummy variables to the original data frame
data <- cbind(data, dummy_vars)
data <- subset(data, select = -feed_group)
colnames(data) <- c('average_food', 'birds_m_sqr', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7')
label <- data$birds_m_sqr
training <- as.matrix(data[, !(names(data) %in% c("birds_m_sqr"))])



params <- list(
  objective = "reg:squarederror", 
  nrounds = 20,
  eta = 0.3,                      
  max_depth = 6
)

# Perform k-fold cross-validation
cv_result <- xgb.cv(
  params = params,
  data = xgb.DMatrix(data = training, label = label),
  nfold = 5,                     # Number of folds
  nrounds = 100,                 # Number of boosting rounds
  verbose = TRUE
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
  theme_minimal()


#BOOCOIN testing
#Creating the test function based on model training above
#Again load the data
data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
data <- na.omit(data)
data$feed_group <- as.factor(data$feed_group)

boocoin <- function(data, index, p){
  resample <- data[index, ]
  resample <- data[sample(nrow(data),nrow(data),replace=TRUE),] #Uncomment for testing
  p = 0.8 #Uncomment for testing
  
  #Create the shuffled variable
  resample$shuffled_avgfood = sample(resample$average_food)
   
  #XGBoost
  #Create a data frame with the month variable
  food <- data.frame(food = resample$feed_group)
  
  #Create dummy variables for feed variable
  dummy_vars <- model.matrix(~ factor(food), food)
  
  # Remove the intercept column
  dummy_vars <- dummy_vars[, -1]
  
  # Bind the dummy variables to the original data frame
  resample <- cbind(resample, dummy_vars)
  data_1 <- subset(resample, select = -c(feed_group, shuffled_avgfood))
  colnames(data_1) <- c('average_food', 'birds_m_sqr', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7' )
  
  #Data Partition
  #Partitioning data into training and test
  inTraining <- createDataPartition(data_1$birds_m_sqr, p = p, list = FALSE)
  training <- data_1[inTraining,]
  testing  <- data_1[-inTraining,]
  
  # Splitting training data into dependent and features
  label <- training$birds_m_sqr
  training <- as.matrix(training[, !(names(training) %in% c("birds_m_sqr"))])
  
  xgb_model_1 <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 7)
  
  # Make predictions on the test set
  test.target <- testing$birds_m_sqr
  testing <- as.matrix(testing[, !(names(testing) %in% c("birds_m_sqr"))])
  predictions <- predict(xgb_model_1, as.matrix(testing))
  
  #  Calculate r-squared
  XGB_R2_1 <- cor(test.target, predictions) ^ 2
  
  data_2 <- subset(resample, select = -c(feed_group, average_food))
  colnames(data_2) <- c('birds_m_sqr', 'shuffled_avgfood', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5',  'ftype6', 'ftype7')
  
  #Data Partition
  #Partitioning data into training and test
  inTraining <- createDataPartition(data_2$birds_m_sqr, p = p, list = FALSE)
  training <- data_2[inTraining,]
  testing  <- data_2[-inTraining,]
  
  # Splitting training data into dependent and features
  label <- training$birds_m_sqr
  training <- as.matrix(training[, !(names(training) %in% c("birds_m_sqr"))])
  
  xgb_model_2 <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 7)
  
  # Make predictions on the test set
  test.target <- testing$birds_m_sqr
  testing <- as.matrix(testing[, !(names(testing) %in% c("birds_m_sqr"))])
  predictions <- predict(xgb_model_2, as.matrix(testing))
  
  #  Calculate r-squared
  XGB_R2_2 <- cor(test.target, predictions) ^ 2
  
  c(XGB_R2_1, XGB_R2_2 ,XGB_R2_1-XGB_R2_2)
  
  return(c(XGB_R2_1, XGB_R2_2 ,XGB_R2_1-XGB_R2_2))
}

N <- nrow(data) #number of observations in data set
R = 10000 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) #Creating a matrix of dirichlet weights 
dirichlet_w <- dirichlet / rowSums(dirichlet)

bayesian_boot <- boot(data=data, statistic = boocoin, weights = dirichlet_w, R=rep(1,R), p=0.85) #Bootstrapping 


plot1 <- hist(bayesian_boot$t[,3], xlim = range(0, 1), breaks = 60,  freq = FALSE, main = " ", xlab = "Difference in R-Squared", col = "lightblue")

png("C:/broiler_acites/ascites_case/Results/plot1_test_food_birds_feed.png", width = 800, height = 600)  # Adjust width and height as needed
plot(plot1, col = "lightblue", main = " ", xlab = "Difference in R-Squared", xlim = range(0, 1))
dev.off()
