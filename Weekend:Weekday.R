## Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" 
## indicating whether a given date is a weekday or weekend day.
complete_data_2 <- mutate(complete_data, day = weekdays(date))

for(i in 1:length(complete_data_2$day)){
  if(complete_data_2$day[i] %in% c("Saturday", "Sunday")){
  complete_data_2$day_type[i] = "Weekend"
  }else{
    complete_data_2$day_type[i] = "Weekday"
  }
}

## Make a panel plot containing a time series plot (i.e. type = "l") 
## of the 5-minute interval (x-axis) and the average number of steps taken, 
## averaged across all weekday days or weekend days (y-axis). 

by_day_type <- tapply(complete_data_2$steps, INDEX = list(complete_data_2$day_type, 
                                                          complete_data_2$interval), mean)

  ## Make this data frame not gross
library(tidyr)
by_day_type_df <- as.data.frame(by_day_type) %>%
  setDT(keep.rownames = "day_type") %>%
  gather(key = "interval", value = "steps", -day_type) %>%
  mutate(interval = as.numeric(interval))


ggplot(by_day_type_df, aes(interval, steps, group = 1)) + geom_line(col = "maroon") + 
  facet_grid(day_type ~ .)                                                                          
                                                                          


  