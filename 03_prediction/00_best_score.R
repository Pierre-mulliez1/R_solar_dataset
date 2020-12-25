rm(list=ls())  #clean environment

############################### PATH CONFIGURATION #############################
# Max

################################## LIBRARIES ###################################
# Packages
#install.packages('forecast')
#install.packages('dplyr')
#install.packages('xts')
#install.packages('data.table')
#install.packages('glue')
#install.packages('foreach')
#install.packages('mgcv')
#install.packages('tidymv')
#install.packages('lubridate')

# Libraries
library('forecast')
library('dplyr')
library('xts')
library('data.table')
library('glue')
library('foreach')
library('mgcv')
library('tidymv')
library('lubridate')


################################### FUNCTIONS ##################################
## get model predictions
get_predictions <- function(col, pca_n){
  fitted_model <- auto.arima(x_ts[, col], xreg=x_reg[,pca_n])
  prediction <- forecast::forecast(fitted_model, h=1796, xreg=y_reg[,pca_n])
  write.csv(prediction$mean, file=paste("00_data/prediction", col , '.csv', sep=""))
  return(c(prediction$mean))
};

################################ DATA PREPERATION ##############################

# Load Data
data <- readRDS('00_data/solar_dataset.RData')
dt <- data.table(data)
remove(data)

# prepare training data / general formatting
df <- as.data.frame(dt)
df$Date <- strptime(df$Date, format='%Y%m%d')              # formatting date
df$Date <- as.POSIXct(df$Date)
column_names <- as.array(colnames(dt))                   # create array with column names

# create x_ts
x <- as.data.frame(df[1:5113, 1:99])
x_ts <- xts(x[,-1], x[,1])                               # format x to timeseries

# create y
y_date <- as.data.frame(dt[5114:6909, 1])                 

# create external regressor sets
df_reg <- df  %>%                                        # do some basic feature extraction
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  mutate(day = day(Date)) %>%
  mutate(year =  as.numeric(year)) %>%
  mutate(month =  as.numeric(month)) %>%
  mutate(day =  as.numeric(day))

df_reg <- sapply(df_reg, as.numeric)                      
x_reg <- as.matrix(df_reg[1:5113,100:459])              # select the reg vars we want to use. df[1:5113,100:459]
y_reg <- as.matrix(df_reg[5114:6909,100:459]) 

################################# PREDICTION ###################################
library(doParallel)
registerDoParallel(cores = detectCores())
pca_n=100:178
prediction <- foreach(col=1:98, .packages = c("forecast", "xts", "data.table"), .combine = cbind) %dopar% get_predictions(col, pca_n)

############################# OUTPUT FORMATTING ################################
prediction_df <- as.data.table(prediction)
predictions <- cbind(y_date, prediction_df)
colnames(predictions) <- column_names[1:99]
head(predictions)
write.csv(predictions,file.path(project_folder,'00_data/forecast_windows.csv'), row.names = F)
system('/Users/admin/anaconda3/bin/kaggle competitions submit -c ams-2014-solar-energy-prediction-contest -f "/Users/admin/OneDrive/GitHub/R_Solar_Competition/00_data/forecast.csv" -m "default"')

###############################################################################

