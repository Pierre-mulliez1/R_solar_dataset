################################################################################
##################################### XGBoost ##################################
remove(list=ls()) # make clean environment
################################### LIBRARIES ##################################
#install.packages("xgboost")
library(forecast)
library(dplyr)
library(xts)
library(data.table)
library(glue)
library(foreach)
library(mgcv)
library(tidymv)
library(lubridate)
require(xgboost)

################################### FUNCTIONS ##################################
## FUNCTION TO ENABLE MULTICORE PROCESSING
get_predictions_xboost <- function(col){
  ## YOU CAN ADJUST THE VALUES TO GET DIFFERENT RESULTS
  model <- xgboost(data = x, label = y[,col], max.depth = 10, nthread = 4, nrounds = 10,  objective = "reg:pseudohubererror")
  prediction <- predict(model, x_predict)
  return(prediction)
}

## FUNCTION TO WRITE PREDICTIONS TO CSV AND SUBMIT IT VIA KAGGLE API 
submit_predictions <- function(predictions){
  write.csv(predictions,'/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/forecast.csv', row.names = F)
  ### IF YOU WANT TO USE THIS, YOU NEED TO INSTALL THE API (https://github.com/Kaggle/kaggle-api), AND SET THE PATH OF YOUR KAGGLE INSTALLATION, AND SUBMISSION-FILE
  system('/Users/admin/anaconda3/bin/kaggle competitions submit -c ams-2014-solar-energy-prediction-contest -f "/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/forecast.csv" -m "default"')
};

############################## LOAD/PREPARE DATA ###############################
data <- readRDS('/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/solar_dataset.RData')
dt <- data.table(data)
remove(data)

############################# MULTI CORE PREDICTION ############################
library(doMC)
registerDoMC(cores=4)                                      # register parallel Backend for multicore processing

# FUNCTION FOR HYPERPARAMETER TUNING COULD GO HERE

prediction <- foreach(a=1:98, combine = rbind) %dopar% get_predictions_xboost(a)    

############################# OUTPUT FORMATTING ################################
prediction_df <- as.data.table(prediction)                 # assign predicted values to dataframe
predictions <- cbind(predictions, prediction_df)           # append predicted values to dates dataframe
colnames(predictions) <- column_names[1:99]                # assign the correct column names to the final dataframe

############################# INSPECTION & SUBMISSION ##########################
# CHECK AGAINST PREDICTION COULD GO HERE
head(predictions)
submit_predictions(predictions)                            # can only be executed if conditions in description are fullfilled


