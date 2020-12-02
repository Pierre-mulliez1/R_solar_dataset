#### this is the data exploration file used for the predictive analysis on R__solar system ####

# !! replace the file directory with your own directory for the dataset
solar_data <- readRDS('/Documents and Settings/Pierre Computer/Documents/IE_classes/R/group project/solar_dataset.RData')
summary(solar_data)
dim(solar_data)
class(solar_data)
head(solar_data)
tail(solar_data)

#data is already structured as a data table
library(data.table)

#check the class of each column of solar_data
#vector of column names of solar data 
column_name <- c()
counter <- 1
for (element in colnames(solar_data)){
  print(paste(element, class(element), sep= " "))
  column_name[counter] <-  element
  counter <- counter + 1
}

#####separate the dataset into 2: value to predict | known value
solar_data_dt <- solar_data[,list(solar_data[1:5113,])]
tail(solar_data_dt)
solar_data_predict <- solar_data[,list(solar_data[5114:6909,])]

#NA percentage by column 
nas <- function(x) {
ret <- 100 * sum(is.na(x)/length(x)) 
return(ret) 
}
sapply(solar_data_dt,nas)


#convert coltype
solar_data_num <- sapply(solar_data_dt[,2:length(solar_data_dt)],as.double)
solar_data_dt <- cbind(solar_data_dt[,1],solar_data_num)
solar_data_dt[,mean(ACME)]
sapply(solar_data_dt,mean)


