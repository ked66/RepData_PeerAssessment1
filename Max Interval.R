## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and 
## the average number of steps taken, averaged across all days (y-axis)
library(data.table)
by_interval <- tapply(complete_data$steps, complete_data$interval, mean) %>%
  as.data.frame() %>%
  setDT(keep.rownames = TRUE)
names(by_interval) <- c("interval", "steps")
by_interval <- mutate(by_interval, interval = as.character(interval))

library(tidyr)
library(dplyr)

interval <- ifelse(nchar(by_interval$interval) == 1, paste("000", by_interval$interval, sep = ""),
                   ifelse(nchar(by_interval$interval) == 2, paste("00", by_interval$interval, sep = ""),
                   ifelse(nchar(by_interval$interval) == 3, paste("0", by_interval$interval, sep = ""),
                   by_interval$interval)))

by_interval$time <- as.POSIXct(interval, format = "%H%M", tz = "UTC")

plot(by_interval$time, by_interval$steps, 
     type = "l", col = "maroon",
     main = "Mean Number of Steps per 5-minute Interval",
     xlab = "5-Minute Interval",
     ylab = "Mean Number of Steps")

library(ggplot2)
library(scales)
ggplot(data = by_interval, aes(time, steps, group = 1)) + geom_line(col = "maroon") +
  labs(title = "Mean Number of Steps per 5-minute Interval", x = "Time of Day",
       y = "Mean Number of Steps") +
  scale_x_datetime(labels = date_format("%H:%M")) +
  theme_bw()

## Which 5-minute interval, on average across all the days in the dataset, 
## contains the maximum number of steps?
max <- max(by_interval$steps)
max_interval <- subset(by_interval, steps == max)
max_interval  ## Interval 835

