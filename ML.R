################ ML #####################
#########################################
# Evaluation Citeria #
# 1. Quality of code.
# 2. Code optimization e.g. use of foreach and/or vectorization.
# 3. Impact of pre-processing steps in the final score.
# 4. Correct splitting of data in train/validation/test and proper use of these three datasets.
# 5. Good hyperparameter tuning via train and validation datasets.
# 6. Test at least one advanced model (you have some examples in the ideas section), even if the final prediction does not come from this model because other gave better results.


#install.packages('forecast')
#install.packages("xts")  
#install.packages("foreach")
#install.packages("tidymv")

# Libraries
remove(list=ls())
library(forecast)
library(dplyr)
library(xts)
library(data.table)
library(glue)
library(foreach)
library(ggplot2)
theme_set(theme_bw())
library(mgcv)
library(tidymv)
library(lubridate)


############################### FUNCTIONS ########################################
## get model predictions
get_predictions <- function(col, model = 'naive'){
  if (model == 'naive'){
    prediction <- naive(x_ts[, col-1], h = 1796) 
  }else if(model == 'ses'){
    prediction <- ses(x_ts[, col-1], h = 1796)
  }else if(model == 'holt'){
    prediction <- holt(x_ts[, col-1], h = 1796) 
  }else if(model == 'arima'){
    fitted_model <- auto.arima(x_ts[, col-1], xreg=x_reg)
    prediction <- forecast::forecast(fitted_model, h=1796, xreg=y_reg)
  }else if(model == 'tbats'){
    fitted_model <- tbats(x_ts[, col-1], xreg=x_PC)
    prediction <- forecast::forecast(fitted_model, h = 1796, xreg=x_PC)
  }else if(model == 'nnetar'){
    fitted_model <- nnetar(x_num[, col-1], xreg=x_PC)
    prediction <- simulate(fitted_model, nsim=1796, xreg=x_PC)
  }else(print("no model specified"))
  return(c(prediction$mean))
};



## Submit predictions to kaggle for scoring
submit_predictions <- function(predictions){
  write.csv(predictions,'/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/forecast.csv', row.names = F)
  system('/Users/admin/anaconda3/bin/kaggle competitions submit -c ams-2014-solar-energy-prediction-contest -f "/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/forecast.csv" -m "default"')
};


## append data to data.table

################################ DATA PREPERATION ###################################

# Load Data
data <- readRDS('/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/solar_dataset.RData')
dt <- data.table(data)
remove(data)

# prepare training data / general formatting
df <- as.data.frame(dt)
df$Date <- strptime(df$Date, format='%Y%m%d')              # formatting date
df$Date <- as.POSIXct(df$Date)
column_names <- as.array(colnames(dt) )                   # create array with column names

# create x_ts
x <- as.data.frame(df[1:5113, 1:99])
x_ts <- xts(x[,-1], x[,1])                               # format x to timeseries

# create external regressor sets
df_reg <- df  %>%                                        # do some simple feature extraction
        mutate(year = year(Date)) %>% 
        mutate(month = month(Date)) %>% 
        mutate(day = day(Date)) %>%
        mutate(year =  as.numeric(year)) %>%
        mutate(month =  as.numeric(month)) %>%
        mutate(day =  as.numeric(day))

df_reg <- sapply(df_reg, as.numeric)                    #  
x_reg <- as.matrix(df_reg[1:5113,100:103])              # select the reg vars we want to use. df[1:5113,100:459]
y_reg <- as.matrix(df_rg[5114:6909,100:103]) 
                        
  
################################ PREDICTION ###################################
library(doMC)
registerDoMC(cores=4)                       # register parallel Backend for multicore processing
    
rm(prediction)
prediction <- foreach(a=2:99, combine = rbind) %dopar% get_predictions(a, 'arima')
  
################################ New Models ###################################

 # Draft
head(df)

head(x_ts)
head(x_reg)
head(y_reg)

fitted_model <- arima(x_ts[, col-1], xreg=x_reg)
prediction <- forecast::forecast(fitted_model, h=1796, xreg=y_reg)
  

############################# OUTPUT FORMATTING ################################
prediction_df <- as.data.table(prediction)
predictions <- cbind(predictions, prediction_df)
colnames(predictions) <- column_names[1:99]
head(predictions)
submit_predictions(predictions)




############################################################################
# Notes

# Predictions
# written for Python, but describes concepts of simple feature engineering well
# https://www.kaggle.com/danofer/getting-started-with-time-series-features        

# https://www.pluralsight.com/guides/time-series-forecasting-using-r
# https://otexts.com/fpp2/dynamic.html
# https://cran.r-project.org/web/packages/foreach/vignettes/foreach.html



# Random forest
# SVM
# xgboost

##############################################################################




