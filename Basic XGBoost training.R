
#Data Partition
#Partitioning data into training and test
inTraining <- createDataPartition(df$Y, p = 0.8, list = FALSE)
training <- df_total[inTraining,]
testing  <- df_total[-inTraining,]

table(df_total$type)
label <- training$ascites
training <- as.matrix(training[, !(names(training) %in% c("Y"))])
# Define the training control settings
ctrl <- trainControl(method = "cv",   # Cross-validation
                     number = 10,    # Number of folds
                     verboseIter = TRUE,  # Show progress
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

# Train the random forest model
model <- train(
  formula,
  data = resample,
  method = "rf",        # Random Forest
  trControl = ctrl
)

params <- list(
  objective = "reg:squarederror", 
  nrounds = 300,
  eta = c(0.01),                      
  max_depth = 7
)

# Perform k-fold cross-validation
cv_result <- xgb.cv(
  params = params,
  data = xgb.DMatrix(data = training, label = label),
  nfold = 5,                     # Number of folds
  nrounds = 300,                 # Number of boosting rounds
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
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, max(cv$Boosting_Round), by = 10))  #
