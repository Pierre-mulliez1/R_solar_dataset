####add exploratory analysis #####

#### this is the data manipulation file used for the predictive analysis on R__solar system ###

# install outliers package
library(outliers)
library(dplyr)
library(tidyr)
library(purrr)

#test on a subset, with only numeric args
test_dt <- solar_data_dt

#test_dt <- outlier(solar_data_num, opposite =  TRUE)
#rm.outlier(test_dt, fill = FALSE)  #should we remove outliers?
#rm.outlier(test_dt, fill = TRUE, median = TRUE) #should we replace with the median/mean? 

#should we remove more than 1 outlier? basically we would need to remove the 1st and run the function again

# 2nd try

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 3 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

test_dt2 <- test_dt[,2:99]
test_dt2 <- as.data.table(apply(test_dt2, MARGIN = 2, FUN = remove_outliers))

identical(test_dt,test_dt2)


# 3rd try, this works, just not sure how to check how many outliers have been replaced
# we could aso use  3* IQR(x, na.rm = na.rm) to focus only on extreme cases

outlierreplacement <- function(dataframe){
  dataframe %>%          
    map_if(is.numeric, ~ replace(.x, .x %in% boxplot.stats(.x)$out, NA)) %>%
    bind_cols 
}
outlierreplacement(test_dt)



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




