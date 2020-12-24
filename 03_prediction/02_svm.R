rm(list=ls())  #clean environment

############################### PATH CONFIGURATION #############################
# Max
project_folder <- '/Users/admin/OneDrive/GitHub/R_Solar_Competition'  # MacOs


############################### LOAD LIBRARIES #############################
library('data.table')
library('lubridate')
library('foreach');
library('doParallel')
library('e1071')
library('sprint')

############################### LOAD DATA #############################
load(file.path(project_folder,'00_data/train_val_test.rda'))

############################### FUNCTIONS #############################
svm_pred <- function(col, pca_n){
  # model fitting    
  model <- svm(x = train[, ..col], y = train[, ..pca_n], kernel="radial",
               cost = c, epsilon = eps, gamma = gamma
  )
  
  # prediction
  predictions_train <- predict(model, newdata = train[, ..pca_n]);
  predictions_val <- predict(model, newdata = val[, ..pca_n]);
  
  # errors
  errors_train <- predictions_train - train[, ..col];
  errors_val <- predictions_val - val[, ..col];
  
  # Compute Metrics
  mae_train <- round(mean(abs(errors_train)), 2);
  mae_val <- round(mean(abs(errors_val)), 2);
  
  # Build comparison table
  result  <- data.table(c = c, eps = eps, gamma = gamma, 
                        mae_train = mae_train,
                        mae_val = mae_val)
  
  return(result)
}

############################## HYPER PARAMETER GRID ############################
c_values <- seq(from = 10^1, to = 10^5, length.out = 10)
eps_values <- seq(from = 10^-7, to = 10^-5, length.out = 10)
gamma_values <- seq(from = 10^-7, to = 10^-5, length.out = 10)

############################## START PARALLEL FITTING ############################
registerDoParallel(cores = detectCores())

# parallel loop structure
grid_results <-
  foreach (c = c_values, .combine = rbind) %:%
  foreach (eps = eps_values, .combine = rbind) %:%
  foreach (gamma = gamma_values, .combine = rbind) %:%
  foreach (col = 2:99, 
           .packages = c("e1071", "data.table"),
           .combine = rbind) %dopar% 
  svm_pred(col, pca_n=100)

             
# Order results by increasing mse and mae
grid_results <- grid_results[order(mae_val, mae_train)];

# Check results
best <- grid_results[1];

### Train final model
# train SVM model with best found set of hyperparamets
model <- svm(disp ~ ., data = train, kernel="radial",
             cost = best$c, epsilon = best$eps, gamma = best$gamma);

# Get model predictions
predictions_train <- predict(model, newdata = train);
predictions_val <- predict(model, newdata = val);
predictions_test <- predict(model, newdata = test);

# Get errors
errors_train <- predictions_train - train$disp;
errors_val <- predictions_val - val$disp;
errors_test <- predictions_test - test$disp;

# Compute Metrics
mae_train <- round(mean(abs(errors_train)), 2);
mae_val <- round(mean(abs(errors_val)), 2);
mae_test <- round(mean(abs(errors_test)), 2);

## Summary
sprintf("MAE_train = %s - MAE_val = %s - MAE_test = %s", mae_train, mae_val, mae_test);
