#### this is the data exploration file used for the predictive analysis on R__solar system ####
rm(list=ls())  #clean environment

# !! replace the file directory with your own directory for the dataset
#Pierre RDS source
solar_data <- readRDS('/Documents and Settings/Pierre Computer/Documents/IE_classes/R/group project/solar_dataset.RData')
#Ivan RDS source
solar_data <- readRDS('C:/Users/Ivan.Polakovic/Desktop/IE/R/group assignment/solar_dataset.RData')
#Max RDS source
solar_data <- readRDS('/Users/admin/OneDrive/Documents/IE - Madrid/Term 1/01 R Programming/Group Assignment/GitHub/data/solar_dataset.RData')
#Antonio RDS source
solar_data <- readRDS('solar_dataset.RData');


summary(solar_data)
dim(solar_data)
class(solar_data)
head(solar_data)
tail(solar_data)
colnames(solar_data)

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
x_raw_train  <- solar_data[1:5113, 2:99] 
x_PC_train  <- solar_data[1:5113, 100:456] 
y_train = solar_data[1:5113, 2:99] 

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
par(mfrow=c(1,1)) 
scatter.smooth(means)
means

#Randomly ordered what can influence our dataset ?
#scatter the mean per date 
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

#summary data table 
#data conversion for further analysis
analysis <- function(df){
  name <- as.character(colnames(df))
  IQR <- as.integer(lapply(df, IQR, na.rm = TRUE))
  median <- as.integer(lapply(df, median, na.rm = TRUE))
  mean <- as.integer(lapply(df, mean, na.rm = TRUE))
  stdv <- as.integer(lapply(df,sd,na.rm = TRUE))
  maxi <- as.integer(lapply(df,max,na.rm = TRUE))
  mini <- as.integer(lapply(df,min,na.rm = TRUE))
  analysed <- data.table(names = name,average = mean,median = median,quantile_range = IQR, standard_deviation = stdv, maximum = maxi, minimum = mini)
  return(analysed)
}

#summary of analysis 
solar_analyzed <- analysis(visualisation_solar)
solar_analyzed
#plot analysis = review 
plot(x = solar_analyzed$average,y = solar_analyzed$standard_deviation,xlab = "mean", ylab= "standard dev")
means <- mean(solar_analyzed$average)
stdev <- sd(solar_analyzed$average)
onebin <- means + stdev
twobin <- means + stdev*2
threebin <- means + stdev*3
monebin <- means - stdev
mtwobin <- means - stdev*2
mthreebin <- means - stdev*3
distribution <- data.frame(mthreebin,mtwobin,monebin,means,onebin,twobin,threebin)
distribution <- sapply(distribution, as.integer) 
#plots
par(mfrow=c(2,2)) 
plot(density(solar_analyzed$average), type = "l", col = "red",xlab = "averages by stations",main = "average station distribution")
plot(dev_month, type = "l", col = "red",xlab = "average by month",main = "average date deviation (log)")
plot(density(distribution), type = "l", col = "blue",xlab = "expected deviation",main = "expected station distribution")
hist(solar_analyzed$average, col = "green", breaks = 5,xlab = "average hist",main = "Histogram of stations averages");
par(mfrow=c(1,1)) 
#conclusion: station distribution skewed to the left with more scattered observation than expected 
#lower deviation for the summer month 

#outliers 
#values to detect outliers; it is assumed here that the sample is from a normal distribbution
distribution <- function(x, na.rm = TRUE, ...) {
  stdev <- sd(x)
  meanf <- mean(x)
  H <- meanf + 3 * stdev
  L <- meanf - 3 * stdev
  if (L < 0){L <- 0}
  out <- list(H,L)
  return(out)}

#detect the outliers, printing is added for comprehension, goes though every value within each column 
#compare with each value to the respective column lower and higher bounds for outliers (bounds defined in distribution fun)
outliers <- function(x){
  limit = sapply(X = x,distribution)
  y <- data.table(column = character(), Hlimit=numeric(), Llimit = numeric(),value=numeric())
  count <- -1
  countI <- 0
  dt <- data.frame(x)
  for (el in colnames(dt)){
    count <- count + 2
    countI <- countI + 2
    print( limit[[count]])
    print( limit[[countI]])
    for (ele in dt[el]){
      print(ele)
      val <-  data.table(column = el,Hlimit = limit[[count]],Llimit = limit[[countI]],value = ele)
      y <- rbind(y,val)
      y <- y[value > Hlimit || value < Llimit,.SD]
    }
  }
  return(y)
}


outliers(visualisation_solar)



# Scatterplots per Column
dt <- data.table(visualisation_solar)
x <- dt[1:5113, 2:99] 
res = cor(x)
corr_dt = as.data.table(res)
boxplot(x = as.list(as.data.frame(x)))

# Correlation Heatmat & Clustering
correlation <- visualisation_solar[,x:= list(cor(visualisation_solar[,.SD]))]
correlation <- as.data.frame(correlation$x[1])
# Use 'scale' to normalize
correlation <- sapply(X = correlation,FUN = as.double)
data <- as.matrix(correlation)
# Default Heatmap
heatmap(data, scale="column")



#install.packages("corrplot")
source("http://www.sthda.com/upload/rquery_cormat.r")
rquery.cormat(x_num, type='full', graphType="heatmap")
corr_list <- rquery.cormat(x_num, type="flatten", graph=FALSE)
corr_list




#PCA analysis: variation in the Dataset 



