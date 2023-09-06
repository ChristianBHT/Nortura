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

setwd("C:/Users/christian.thorjussen/Project Nortura/")
rm(list = ls())
load("wide_data_for_analysis.Rda")
wide_data$feed_name <- str_replace(wide_data$feed_name, "o?=", "aa")

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

# Birds p m-sqr ??? Food consumption | Feed mix
# Birds p m-sqr b _||_ Food consumption | Chicken type
# Birds p m-sqr b _||_ Water consumption | Feed mix, Month
# Birds p m-sqr b _||_ Water consumption | Chicken type


#Create a feed_group variable instead of feed type since it has so many levels
levels = 7  # Set the number of levels other than other
wide_data$feed_group = fct_lump_n(wide_data$feed_name, n = levels, other_level = "other")

wide_data$feed_group = as.factor(wide_data$feed_group)

##########################################
# Birds p m-sqr b _||_ Food consumption | Feed mix
##########################################
#Select the variables we want (leverandoer has to many levels, so must be left out)
randomf <- list()
bag <- list()
XGboost <- list()

for (i in 1:200){
  data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
  data <- na.omit(data)
  data$feed_group <- as.factor(data$feed_group)
  
  #Partitioning data into training and test
  inTraining <- createDataPartition(data$feed_group, p = 0.9, list = FALSE)
  training <- data[inTraining,]
  testing  <- data[-inTraining,]

  #Random Forest
  rf <- randomForest(average_food ~  birds_m_sqr + feed_group, 
                   data=training,
                   ntree=500)
  test.features <- subset(testing, select=c(birds_m_sqr, feed_group))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(rf, newdata = test.features)
  rf_R2 <- cor(test.target, predictions) ^ 2

  #Bagged tree
  bagged <- bagging(
    average_food ~  birds_m_sqr + feed_group,
    data = training,
    coob = TRUE,
  )
  test.features <- subset(testing, select=c(birds_m_sqr, feed_group))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(bagged, newdata = test.features)
  bagged_R2 <- cor(test.target, predictions) ^ 2


  #XGBoost
  #Create a data frame with the month variable
  food <- data.frame(food = data$feed_group)

  #Create dummy variables for feed variable
  dummy_vars <- model.matrix(~ factor(food), food)

  # Remove the intercept column
  dummy_vars <- dummy_vars[, -1]

# Bind the dummy variables to the original data frame
  data <- cbind(data, dummy_vars)
  data <- subset(data, select = -feed_group)
  colnames(data) <- c('average_food', 'birds_m_sqr', 'ftype1', 'ftype2', 'ftype3', 'ftype4', 'ftype5')


  # Split the data into training and testing sets
  inTraining <- createDataPartition(data$average_food, p = 0.9, list = FALSE)
  training <- data[inTraining,]
  testing  <- data[-inTraining,]

  label <- training$average_food
  training <- as.matrix(training[, !(names(training) %in% c("average_food"))])
 
  # Training XGBoost model

  xgb_model <- xgb.train(
    data = xgb.DMatrix(data = training, label = label),
    objective = "reg:squarederror", 
    nrounds = 20,
    eta = 0.3,                      
    max_depth = 6)

  # Make predictions on the test set
  test.target <- testing$average_food
  testing <- as.matrix(testing[, !(names(testing) %in% c("average_food"))])
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
#Random forest is best
#Tuning the number of trees in Random Forest
data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
data <- na.omit(data)
data$feed_group <- as.factor(data$feed_group)
data$shuffled_bird = sample(data$birds_m_sqr)
inTraining <- createDataPartition(data$average_food, p = 0.8, list = FALSE)
training <- data[inTraining,]
testing  <- data[-inTraining,]



tune_res <- list()

for (i in c(10, 15, 25, 35, 45, 55, 65, 75, 85, 100, 200)){
 
  rf <- randomForest(average_food ~  birds_m_sqr + feed_group, 
                     data=training,
                     ntree=i)
  
  test.features <- subset(testing, select=c(birds_m_sqr, feed_group))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(rf, newdata = test.features)
  rf_R2 <- cor(test.target, predictions) ^ 2
  
  tune_res <- append(tune_res, rf_R2)
}
tune_res

tune_resII <- list()
for (i in c(10, 15, 25, 35, 45, 55, 65, 75, 85, 100, 200)){
  rf <- randomForest(average_food ~ shuffled_bird + feed_group,
                    data = training,
                    ntree = i)
  
  test.features <- subset(testing, select=c(shuffled_bird, feed_group))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(rf, newdata = test.features)
  rf_R2 <- cor(test.target, predictions) ^ 2
  
  tune_resII <- append(tune_resII, rf_R2)
}
tune_resII
data$shuffled_bird = sample(data$birds_m_sqr)

plot(data$birds_m_sqr, data$average_food, ylab = "Average food consumption per day per bird", xlab = "Birds per sqr. meter at day 0")
plot(data$shuffled_bird, data$average_food, ylab = "Average food consumption per day per bird", xlab = "Birds per sqr. meter (shuffled)")
#randomForest with 50 or 100 trees seems good for model 1

#BOOCOIN testing
#Creating the test function based on model training above
#Again load the data
data <- subset(wide_data, select = c('average_food','birds_m_sqr', 'feed_group'))
data <- na.omit(data)
data$feed_group <- as.factor(data$feed_group)

boocoin <- function(data, index, p){
  resample <- data[index, ]
  # resample <- data[sample(nrow(data),nrow(data),replace=TRUE),] #Uncomment for testing
  # p = 0.8 #Uncomment for testing
  # 
  #Create the shuffeled variable
  resample$shuffled_bird = sample(resample$birds_m_sqr)
   
  #Data Partition
  inTraining <- createDataPartition(resample$average_food, p = p, list = FALSE)
  training <- resample[inTraining,]
  testing  <- resample[-inTraining,]
  
  model_1 <- randomForest(
    average_food ~  birds_m_sqr + feed_group,
    data = training,
    ntree = 65  
  )

  test.features <- subset(testing, select=c(birds_m_sqr, feed_group))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(model_1, newdata = test.features)
  R2_1 <- cor(test.target, predictions) ^ 2
  
  
  model_2 <- randomForest(
    average_food ~ shuffled_bird + feed_group,
    data = training,
    ntree = 200  
  )
  
  test.features <- subset(testing, select=c(shuffled_bird, feed_group))
  test.target <- subset(testing, select=average_food)[,1]
  predictions <- predict(model_2, newdata = test.features)
  R2_2 <- cor(test.target, predictions) ^ 2
  
  return(R2_1-R2_2)
}

N <- nrow(data) #number of observations in data set
R = 1000 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) #Creating a matrix of dirichlet weights 
dirichlet_w <- dirichlet / rowSums(dirichlet)

bayesian_boot <- boot(data=data, statistic = boocoin, weights = dirichlet_w, R=rep(1,R), p=0.85) #Bootstrapping 
hist(bayesian_boot$t, breaks = 30,  freq = FALSE, main = " ", xlab = "Difference in R-Squared", col = "lightblue")
test_1_results <- c(mean(bayesian_boot$t), 
                    2*pnorm(abs(mean(bayesian_boot$t)/sd(bayesian_boot$t) ), lower.tail = FALSE),
                    quantile(bayesian_boot$t, probs = 0.025),
                    quantile(bayesian_boot$t, probs = 0.975))
test_1_results

# Acidity increase percentage = (10^(pH2 - pH1) - 1) * 100
#After adding arrow from birds per square meter and then do DAG simplification
simplified_DAG <- dagitty('dag{
  bb="-4.723,-5.908,5.144,4.316"
  "Birds p m-sqr" [pos="-3.239,-2.262"]
  "Chicken type" [pos="2.266,0.682"]
  "Feed mix" [exposure,pos="0.510,3.444"]
  "Food consumption" [pos="-3.246,-1.005"]
  "Indoor Temperature" [pos="-3.253,2.018"]
  "Outdoor temperature" [pos="-3.740,0.851"]
  "Water consumption" [pos="-3.253,-0.077"]
  Ascites [outcome,pos="0.489,-2.058"]
  Farm [pos="2.266,-0.700"]
  Growth [pos="-2.096,-3.610"]
  Month [pos="2.280,-1.934"]
  Slaughterhouse [adjusted,pos="0.489,-3.983"]
  "Birds p m-sqr" -> "Food consumption"
  "Birds p m-sqr" -> Ascites
  "Chicken type" -> "Birds p m-sqr"
  "Chicken type" -> "Feed mix"
  "Chicken type" -> Ascites
  "Chicken type" -> Growth
  "Feed mix" -> "Food consumption"
  "Feed mix" -> "Water consumption"
  "Feed mix" -> Ascites
  "Feed mix" -> Growth
  "Food consumption" -> Ascites
  "Food consumption" -> Growth
  "Indoor Temperature" -> "Water consumption"
  "Indoor Temperature" -> Ascites
  "Indoor Temperature" -> Growth
  "Outdoor temperature" -> "Indoor Temperature"
  "Outdoor temperature" -> "Water consumption"
  "Outdoor temperature" -> Ascites
  "Water consumption" -> Ascites
  "Water consumption" -> Growth
  Farm -> "Birds p m-sqr"
  Farm -> "Feed mix"
  Farm -> "Indoor Temperature"
  Farm -> Ascites
  Farm -> Growth
  Growth -> Ascites
  Month -> "Feed mix"
  Month -> Ascites
  Month -> Growth
  Slaughterhouse -> Ascites
}')

plot(simlified_DAG)

impliedConditionalIndependencies(simlified_DAG)



#And then simplify even more
supersimple_DAG <- dagitty('dag {
bb="-4.19,-4.371,4.28,4.084"
"Birds p m-sqr" [pos="-2.037,-1.309"]
"Chicken type" [pos="0.900,-1.262"]
"Feed mix" [exposure,pos="0.135,1.884"]
"Food consumption" [pos="-2.025,-0.467"]
"Indoor Temperature" [pos="-1.983,1.135"]
"Outdoor temperature" [pos="-1.953,1.893"]
"Water consumption" [pos="-0.948,1.902"]
Ascites [outcome,pos="0.094,0.040"]
Farm [pos="0.894,-0.410"]
Growth [pos="-1.995,0.267"]
Month [pos="0.906,0.376"]
Slaughterhouse [adjusted,pos="0.912,1.275"]
"Birds p m-sqr" -> "Food consumption"
"Birds p m-sqr" -> Ascites
"Chicken type" -> "Feed mix"
"Chicken type" -> Ascites
"Feed mix" -> "Food consumption"
"Feed mix" -> "Water consumption"
"Feed mix" -> Ascites
"Feed mix" -> Growth
"Food consumption" -> Ascites
"Food consumption" -> Growth
"Indoor Temperature" -> Ascites
"Indoor Temperature" -> Growth
"Outdoor temperature" -> "Water consumption"
"Outdoor temperature" -> Ascites
"Water consumption" -> Ascites
"Water consumption" -> Growth
Farm -> "Feed mix"
Farm -> Ascites
Growth -> Ascites
Month -> "Feed mix"
Month -> Ascites
Slaughterhouse -> Ascites
}
')

plot(supersimple_DAG)

impliedConditionalIndependencies(supersimple_DAG)







#And then simplify even more
simplified_DAG <- dagitty('dag {
bb="-4.723,-5.908,5.144,4.316"
"Birds p m-sqr" [pos="-2.761,-1.840"]
"Chicken type" [pos="3.292,3.308"]
"Food per bird" [pos="-3.112,1.610"]
"Growth feed type" [exposure,pos="0.838,3.422"]
"Indoor Temperature" [pos="-2.217,3.750"]
"Indoor humidity" [pos="-2.754,3.048"]
"Kg per m-sqr" [pos="-3.298,-1.091"]
"Outdoor humidity" [pos="2.642,-2.036"]
"Outdoor temperature" [pos="2.656,-0.054"]
"Prevalence Acsites" [outcome,pos="0.489,-2.058"]
"Slaughter age" [pos="-2.375,-2.520"]
"Water per bird" [pos="-2.926,2.335"]
CO2 [latent,pos="-3.291,0.863"]
Growth_linear [pos="-1.921,-3.100"]
Growth_sqr [pos="-0.925,-4.130"]
Month [pos="3.263,2.165"]
Slaughterhouse [adjusted,pos="0.489,-3.983"]
start_weight [pos="-1.399,-3.700"]
"Birds p m-sqr" -> "Kg per m-sqr"
"Birds p m-sqr" -> "Prevalence Acsites"
"Chicken type" -> "Birds p m-sqr"
"Chicken type" -> "Growth feed type"
"Chicken type" -> "Kg per m-sqr"
"Chicken type" -> "Prevalence Acsites"
"Chicken type" -> "Slaughter age"
"Chicken type" -> Growth_linear
"Food per bird" -> "Prevalence Acsites"
"Food per bird" -> Growth_linear
"Growth feed type" -> "Food per bird"
"Growth feed type" -> "Prevalence Acsites"
"Growth feed type" -> "Water per bird"
"Growth feed type" -> Growth_linear
"Growth feed type" -> Growth_sqr
"Growth feed type" -> start_weight
"Indoor Temperature" -> "Indoor humidity"
"Indoor Temperature" -> "Prevalence Acsites"
"Indoor Temperature" -> "Water per bird"
"Indoor Temperature" -> Growth_linear
"Indoor humidity" -> "Prevalence Acsites"
"Kg per m-sqr" -> "Prevalence Acsites"
"Outdoor humidity" -> "Indoor humidity"
"Outdoor humidity" -> "Prevalence Acsites"
"Outdoor temperature" -> "Indoor Temperature"
"Outdoor temperature" -> "Indoor humidity"
"Outdoor temperature" -> "Outdoor humidity"
"Outdoor temperature" -> "Prevalence Acsites"
"Outdoor temperature" -> "Water per bird"
"Outdoor temperature" -> CO2
"Slaughter age" -> "Prevalence Acsites"
"Water per bird" -> "Prevalence Acsites"
"Water per bird" -> Growth_linear
CO2 -> "Prevalence Acsites"
Growth_linear -> "Prevalence Acsites"
Growth_linear -> "Slaughter age"
Growth_sqr -> "Prevalence Acsites"
Month -> "Growth feed type"
Month -> "Indoor humidity"
Month -> "Outdoor humidity"
Month -> "Outdoor temperature"
Month -> "Prevalence Acsites"
Month -> CO2
Month -> Growth_linear
Slaughterhouse -> "Prevalence Acsites"
start_weight -> "Prevalence Acsites"
start_weight -> Growth_linear
start_weight -> Growth_sqr
}')

plot(simplified_DAG)






