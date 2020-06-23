## Make a histogram of the total number of steps taken each day
    ## Remove NA values
complete_data <- na.omit(data)
    ## Calculate sum of steps by day
per_day <- tapply(complete_data$steps, complete_data$date, sum)
    ## Plot histogram
hist(per_day, main = "Histogram of Total Number of Steps per Day", 
     xlab = "Total Number of Steps", col = "maroon")

    ## Or with ggplot
library(ggplot2)
qplot(per_day, 
      geom = "histogram", 
      binwidth = 5000, 
      main = "Histogram of Total Number of Steps per Day",
      xlab = "Total Number of Steps",
      ylab = "Frequency",
      fill = I("maroon"), col = I("black"))

## Calculate and report the mean and median total number of steps taken per day
mean(per_day)   ## 10766.19
median(per_day) ## 10765
