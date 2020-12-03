####add exploratory analysis #####

#### this is the data manipulation file used for the predictive analysis on R__solar system ###

# install outliers package
install.packages("outliers")
library(outliers)
library(dplyr)

#test on a subset, with only numeric args
test_dt <- outlier(solar_data_num, opposite =  TRUE)
rm.outlier(test_dt, fill = FALSE)  #should we remove outliers?
rm.outlier(test_dt, fill = TRUE, median = TRUE) #should we replace with the median/mean? 
#should we remove more than 1 outlier? basically we would need to remove the 1st and run the function again

#############
# DATES #
############
# we keep solar_data_dt as the main data table

solar_data_dt$Date <- as.Date(strptime(solar_data_dt$Date, format = "%Y%m%d"))
class(solar_data_dt$Date) # Date

############
# standardization with dplyr#
############

trial_dt <- solar_data_dt
length(colnames(trial_dt))
trial_dt %>% mutate_at(2:99, ~(scale(.) %>% as.vector))  #hmm not being able to standardize
#CAREFUL!
# columns 99:456 seem to be in another scale or already standardized




