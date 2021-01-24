rm(list=ls())  #clean environment

############################### PATH CONFIGURATION #############################
# Max
project_folder <- '/Users/admin/OneDrive/GitHub/R_Solar_Competition'  # MacOs

# Pierre
#project_folder <- '/Documents and Settings/Pierre Computer/Documents/IE_classes/R/group project/'

# Ivan
#project_folder <- 'C:/Users/Ivan.Polakovic/Desktop/IE/R/group assignment'

#Oxana 
#project_folder<-'/Users/opanvas/Downloads/R/Group project'  # MacOs

############################### LOAD LIBRARIES #############################
library('data.table')
library('dplyr')
library('ggmap')
library('lubridate')
library('tidyr')
library('purrr')
library("factoextra")

############################### LOAD DATA #############################
solar_data <- readRDS(file.path("~","Downloads","R","Group project","solar_dataset.RData"))
#had to change to work on markdown

############################### EDA #############################
# get overview 
summary(solar_data)
dim(solar_data)
class(solar_data)
head(solar_data)
tail(solar_data)
colnames(solar_data) 
sapply(solar_data, class)
 
# format date column to POSIXct
solar_data$Date <- as.POSIXct(solar_data$Date, format='%Y%m%d')

# get NULL value ratio per column
nas <- function(x) {
ret <- 100 * sum(is.na(x)/length(x)) 
return(ret) 
}

max(sapply(solar_data,nas)) # data set still contains prediction rows
solar_data <- solar_data[,list(solar_data[1:5113,])] #seperate prediction rows from data-set
max(sapply(solar_data,nas)) # no missing values now

# visualize means of production per stations
options(scipen = 999)
par(mfrow=c(1,1)) 
scatter.smooth(data.frame(round(sapply(solar_data[,2:99] ,mean),2))) # there are some stations with high, and many with low production

# visualize means of production per date
summary_month <- solar_data[, .(AVG = rowMeans(.SD)),keyby=Date]
scatter.smooth(summary_month)
  # clear seasonal pattern 

# visualize means of production per month
#means_monthly <- date_visualization[,substr(Date, 5, 6)]
plot(
  #x = month(solar_data$Date), 5, 6), y = date_visualization$AVG,
  #x = sapply(solar_data$Date, month), 
  x = summary_month$Date,
  y = summary_month$AVG,
  main = "average by date",
  xlab = "month", ylab = "average production"
  )    # records have a mean peak in summer which make sense more sun = more solar energy 

# standard deviation per month  
sd.d=function(x){sqrt((length(x)-1)/length(x))*sd(x)}
plot(summary_month[,list(standarddev = sd.d(log(AVG))),list(month = month(Date))])

# summary data table 
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
solar_analyzed <- analysis(solar_data[,2:99])
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
dev_month <- summary_month[,list(standarddev = sd.d(log(AVG))),list(Month = substr(Date, 5, 6))]

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
#values to detect outliers; it is assumed here that the sample is from a normal distribution
distribution <- function(x, na.rm = TRUE, ...) {
  stdev <- sd(x)
  meanf <- mean(x)
  H <- meanf + 1.5 * stdev
  L <- meanf - 1.5 * stdev
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
    }
  }
  y <- y[value > Hlimit | value < Llimit,.SD]
  return(y)
}

outliers(solar_data[,2:99])

# Scatterplots per Column
boxplot(x = as.list(solar_data[,2:99]))

# Correlation Heatmat & Clustering
correlation <- summary_month[,x:= list(cor(solar_data[,2:99]))]
correlation <- as.data.frame(correlation$x[1])
correlation <- sapply(X = correlation,FUN = as.double) # Use 'scale' to normalize
heatmap(as.matrix(correlation), scale="column")


# Principal Component Analysis
solar_data_t <- t(solar_data[,-1])

pca <- prcomp(solar_data_t, scale = TRUE)
# show eigenvalue of stations
fviz_eig(pca) 
# show individuals
fviz_pca_ind(pca, repel = TRUE)     
# PCA - biplot
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

################################# GGMAP ################################
#stations' coordinates
coordinates <- read.table(file.path(project_folder,'00_data/station_info.csv'),
                          header = TRUE, sep = ',')
head(coordinates)


par(mfrow=c(2,2))
map <- qmplot(elon, nlat, data = coordinates, colour = I('black'), size = I(3), darken = .3,main = 'map of stations')
elevation <- map + geom_point(data = coordinates, aes(color  = elev))
plot(elevation +  scale_colour_gradientn(colours = rev(rainbow(8)),
                                  breaks = c(150, 300, 450, 600, 850),
                                  trans = "log10"))
#search for pattern between coordinate, elevation and observations
mapo <- qmplot(elon, nlat, data = coordinates, colour = I('black'), size = I(3), darken = .3,main = 'map of observations')
#merge the summary with elevation
merged <- merge(x=solar_analyzed,y=coordinates,by.x = "names",by.y = "stid"  )
mapobserved <- mapo + geom_point(data = merged, aes(color  = average))
plot(mapobserved +  scale_colour_gradientn(colours = rev(rainbow(8)),
                                    breaks = c(150, 300, 450, 600, 850),
                                    trans = "log10"))
par(mfrow=c(1,1))
#conclusion: strong correlation between altitude and average observation


################################ DATA PROCESSING ###############################
# remove outliers
outlierreplacement <- function(dataframe){
  dataframe %>%          
    map_if(is.numeric, ~ replace(.x, .x %in% boxplot.stats(.x)$out, NA)) %>%
    bind_cols 
  return(dataframe)
}

solar_data <- outlierreplacement(solar_data)

# basic date feature extraction
solar_data <- solar_data  %>%
  mutate(year = year(Date)) %>% 
  mutate(month = month(Date)) %>% 
  mutate(day = day(Date)) %>%
  mutate(year =  as.numeric(year)) %>%
  mutate(month =  as.numeric(month)) %>%
  mutate(day =  as.numeric(day))



############################ TRAIN / TEST / VAL SPLIT ###########################
train_index <- sample(1:nrow(solar_data), 0.7*nrow(solar_data))
val_index <- sample(setdiff(1:nrow(solar_data), train_index), 0.15*nrow(solar_data));  
test_index <- setdiff(1:nrow(solar_data), c(train_index, val_index));

train <- solar_data[train_index]
val <- solar_data[val_index]
test <- solar_data[test_index]

# write files to data folder
save(list=c('train', 'val', 'test'),file=file.path(project_folder,'00_data/train_val_test.rda'))

