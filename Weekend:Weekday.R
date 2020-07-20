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

  ## Make interval into time
interval <- ifelse(nchar(by_day_type_df$interval) == 1, paste("000", by_day_type_df$interval, sep = ""),
                   ifelse(nchar(by_day_type_df$interval) == 2, paste("00", by_day_type_df$interval, sep = ""),
                   ifelse(nchar(by_day_type_df$interval) == 3, paste("0", by_day_type_df$interval, sep = ""),
                          by_day_type_df$interval)))

by_day_type_df$time <- as.POSIXct(interval, format = "%H%M")


ggplot(by_day_type_df, aes(time, steps, group = 1)) + geom_line(col = "maroon") + 
  facet_grid(day_type ~ .) +
  scale_x_datetime(labels = date_format("%H:%M")) +
  theme_bw()
                                                                          


  