#### this is the data exploration file used for the predictive analysis on R__solar system ####
rm(ls=list())  #clean environment

# !! replace the file directory with your own directory for the dataset
#Pierre RDS source
solar_data <- readRDS('/Documents and Settings/Pierre Computer/Documents/IE_classes/R/group project/solar_dataset.RData')
#Ivan RDS source
solar_data <- readRDS('C:/Users/Ivan.Polakovic/Desktop/IE/R/group assignment/solar_dataset.RData')

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

#visualise means, not precise !
options(scipen = 999)
#Divide the recording center from aditionnal information 
visualisation_solar <- solar_data_dt[,2:98]
means <- data.frame(round(sapply(visualisation_solar,mean),2))
scatter.smooth(means)
means

#Randomly ordered what can influence our dataset ?
#scatter the mean per date 
 rowMax(solar_data_dt[,2:98], which = FALSE, ignore.zero = TRUE)
 setDT(solar_data_dt)
date_visualization <- solar_data_dt[, .(AVG = rowMeans(.SD)),Date]
scatter.smooth(date_visualization)
#Conclusion : clear seasonal pattern 

#check for monthly records 
Monthlymeans <- date_visualization[,substr(Date, 5, 6)]
plot(x = Monthlymeans,y = date_visualization$AVG,main = "average by date",xlab = "Date in int format",ylab = "Average of stations")
#records have a mean peak in summer which make sense more sun = more solar energy 
#standard deviation per month  
sd.d=function(x){sqrt((length(x)-1)/length(x))*sd(x)}
#logarythm to simulate normal distribution ?
dev_month <- date_visualization[,list(standarddev = sd.d(log(AVG))),list(Month = substr(Date, 5, 6))]
dev_month


#PCA analysis: variation in the Dataset 



