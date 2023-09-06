X_learner <-  function(data, index, Y, adjust_set, cv_folds = 4){ 
  # Create formula
  formula <- as.formula(paste0(Y, ' ~ ', adjust_set))
  # Split data into treatment and control
  treatment_data <- subset(data, treatment == 1)
  control_data <- subset(data, treatment == 0)
  
  # The control parameters for cross-validation
  ctrl <- trainControl(method = "cv",  # Use cross-validation
                       number = cv_folds,     # Number of folds
                       verboseIter = F)  # Print progress during training (turn to TRUE)
  # Train the treatment model with cross-validation
  treatment_model <- train(formula, 
                           data = treatment_data,
                           method = "rf",  # Specify the ML method
                           trControl = ctrl,# Use the defined control parameters
                           tuneGrid = expand.grid(mtry = c(2, 4, 6))) #Different mtry values     
  
  # Train the control model with cross-validation
  control_model <- train(formula,   
                         data = control_data,
                         method = "rf",  # Random forest method
                         trControl = ctrl,
                         tuneGrid = expand.grid(mtry = c(2, 4, 6))) 
  
  # Imputed treatment effects with cross predictions (X-learner)
  control_data$D_0 <- predict(treatment_model, newdata = control_data) - control_data$aceties_prev  # Imputed minus observed
  treatment_data$D_1 <- treatment_data$aceties_prev - predict(control_model, newdata = treatment_data) # Observed Y minus imputed
  hist(control_data$D_0)
  hist(treatment_data$D_1) 
  ############## STAGE 2 ##################
  # Use the imputed treatment effects as response variables in new formulas
  t_formula <- as.formula(paste0('D_1 ~ ', adjust_set))
  c_formula <- as.formula(paste0('D_0 ~ ', adjust_set))
  
  tau_treatment <- train(t_formula, 
                         data = treatment_data,
                         method = "rf",  # Specify the ML method
                         trControl = ctrl)# Use the defined control parameters
  
  
  tau_control <- train(c_formula, 
                       data = control_data,
                       method = "rf",  # Specify the ML method
                       trControl = ctrl) #Different mtry values
  
  # Estimated treatment effects on the whole data set using the control model
  tau_0 <- predict(control_model, newdata = data)#[index, ]) 
  # Estimated treatment effects on the whole data set using the treatment model
  tau_1 <- predict(treatment_model, newdata = data)#[index, ]) 
  hist(tau_0)
  hist(tau_1)
  ############# Using Propensity Scores ########################
  # Estimating propensity scores
  p_formula <- as.formula(paste0('treatment ~ ', adjust_set))
  
  p_model <- glm(p_formula, data = data, family = "binomial")
  
  # Extract propensity scores
  propensity_scores <- predict(p_model, type = "response")
  
  # Using propensity scores to estimate the CATE
  CATE <- mean(propensity_scores*tau_0 + (1-propensity_scores)*tau_1)
  
  return(CATE) 
}

