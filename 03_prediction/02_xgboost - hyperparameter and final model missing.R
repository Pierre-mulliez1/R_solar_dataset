rm(list=ls())  #clean environment

############################### PATH CONFIGURATION #############################
# Max
project_folder <- '/Users/admin/OneDrive/GitHub/R_Solar_Competition'  # MacOs


############################### LOAD LIBRARIES #############################
library('data.table')
library('lubridate')
library('foreach');
library('doParallel')
require('xgboost')

############################### LOAD DATA #############################
load(file.path(project_folder,'00_data/train_val_test.rda'))
data <- readRDS(file.path(project_folder,'00_data/solar_dataset.RData'))
dt <- data.table(data)

x_train <- as.matrix(train[,100:456])                         # assign all PC Variables used in training to matrix
y_train <- as.matrix(train[,2:99])                            # assign all weather-station values to matrix

x_val <- as.matrix(val[,100:456])                       
y_val <- as.matrix(val[,2:99])  

x_test <- as.matrix(test[,100:456])                       
y_test <- as.matrix(test[,2:99])  

x_predict <- as.matrix(dt[5114:6909,100:456])             
y_predict <- as.data.frame(dt[5114:6909, 1])              

############################### FUNCTIONS #############################
xgboost_fit <- function(col, depth, eta, rounds){
  # model fitting    
  model <- xgboost(data = x_train, label = y_train[,col],
                   max_depth = depth, eta = eta, nrounds = rounds, nthread = 4, 
                   objective = "reg:pseudohubererror")
  
  # prediction
  predictions_train <- predict(model, newdata = x_train);
  predictions_val <- predict(model, newdata = x_val);
  
  # errors
  errors_train <- predictions_train - y_train[, col];
  errors_val <- predictions_val - y_val[, col];
  
  # Compute Metrics
  mae_train <- round(mean(abs(errors_train)), 2);
  mae_val <- round(mean(abs(errors_val)), 2);
  
  # Build comparison table
  result  <- data.table(mae_train = mae_train,
                        mae_val = mae_val, 
                        depth = max_depth,
                        eta = eta,
                        rounds = nrounds)
  
  return(result)
}


xgboost_predict <- function(col, depth, eta, rounds){
  # model fitting    
  model <- xgboost(data = x_train, label = y_train[,col],
                   max_depth = depth, eta = eta, nrounds = rounds, nthread = 4, 
                   objective = "reg:pseudohubererror")
  
  # prediction
  predictions_test <- predict(model, newdata = x_test);
  return(predictions_test)
}

############################## HYPER PARAMETER GRID ############################
max_depth <- seq(from = 2, to = 300, length.out = 10)
eta <- seq(from = 0.1, to = 0.9, length.out = 0.2)
nrounds <- seq(from = 70, to = 300, length.out = 10)

############################## START PARALLEL FITTING ############################
registerDoParallel(2)

# parallel loop structure
# !!!attention!!! 
# this operation consumes a lot of RAM
grid_results <-
  foreach (depth = max_depth, .combine = rbind) %:%
  foreach (eta = eta, .combine = rbind) %:%
  foreach (rounds = nrounds, .combine = rbind) %:%
  foreach (col = 1:98, 
           .packages = c('xgboost', 'data.table'),
           .combine = rbind) %dopar% 
  xgboost_fit(col, depth, eta, rounds)


# Order results by increasing mse and mae
grid_results <- grid_results[order(
  mae_val, mae_train)];

# Check results
best <- grid_results[1];

### Train final model
predictions <-
  foreach (col = 1:98, 
           .packages = c('xgboost', 'data.table'),
           .combine = rbind) %dopar% 
  xgboost_fit(col, depth, eta, rounds)

write.csv(prediction$mean, file=paste("00_data/prediction_xgboost", col , '.csv', sep=""))




