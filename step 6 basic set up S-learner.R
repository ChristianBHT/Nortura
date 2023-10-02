# Some basic data managment for testing 
data$prod_type <- as.factor(data$prod_type)
data$feed_name <- as.factor(data$feed_name)



S_learner <- function(data, index, formula){ 
  
  resample <- data[index, ]
  resample <- data
  formula <- ascites_prev <-   
  # Define the training control settings
  ctrl <- trainControl(method = "cv",   # Cross-validation
                       number = 10,    # Number of folds
                       verboseIter = TRUE)
  
  # Train the random forest model
  model <- train(
    formula,
    data = resample,
    method = "rf",        # Random Forest
    trControl = ctrl
  )
  
  train_r2 <-  cor(subset(df_total, select=get(Y))[,1], predict(model, dtrain))^2
  
  # Access the cross-validation results
  full_y <- with(resample, get(Y))
  
  resample$treatment = 1
  u_1 <- predict(model, dtrain) 
  
  resample$treatment = 0
  u_0 <- predict(model, dtrain) 
  
  ATE <- mean(u_1 - u_0) # Mean of individual treatment effects
  
  return(c(ATE, u_0, u_1, train_r2))
}


df_total <- subset(data, select = c('feed_group', 
                                    'ascites_prev', 
                                    'prod_type', 
                                    'frequent_month')) 

#Create dummy variables for produ variable

dummy_vars <- model.matrix(~ 0 + factor(df_total$prod_type))
df_total <- cbind(df_total, dummy_vars)
dummy_vars <- model.matrix(~ 0 + factor(df_total$frequent_month))
df_total <- cbind(df_total, dummy_vars)

df_total$treatment <- ifelse(df_total$feed_group == "Kromat Kylling 2 Enkel u/k", 1, 0)
colnames(df_total) <- c('feed', 'ascites', 'type', 'month', 
                        'prodT1', 'prodT2', 'prodT3', 'prodT4', 
                        'month1','month2','month3','month4','month5','month6','month7','month8','month9','month10','month11','month12',
                        'treatment')

label <- df_total$ascites
adjust_set <- as.matrix(df_total[, !(names(df_total) %in% c("ascites", 'type', 'month', 'prodtT1', 'month1', 'feed'))])

N <- nrow(df_total)


R = 50 #Number of bootstrap replica

dirichlet <- matrix( rexp(N * R, 1) , ncol = N, byrow = TRUE) #Creating a matrix of dirichlet weights 
dirichlet_w <- dirichlet / rowSums(dirichlet)

S_total_effect <- boot(data=df_total, 
                      statistic = S_learner, 
                      Y = 'ascites', 
                      adjust_set = c('prodT1', 'prodT2', 'prodT3', 'prodT4', 
                                     'month1','month2','month3','month4','month5','month6','month7','month8','month9','month10','month11','month12',
                                     'treatment') , 
                      weights = dirichlet_w, 
                      R=rep(1,R)) 

