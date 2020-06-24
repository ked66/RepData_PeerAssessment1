## Calculate and report the total number of missing values in the dataset 
## (i.e. the total number of rows with NAs)
sum(is.na(data)) ## 2304

## Devise a strategy for filling in all of the missing values in the dataset. 
## The strategy does not need to be sophisticated. For example, you could use the 
## mean/median for that day, or the mean for that 5-minute interval, etc.

  ## Replace each missing value with the mean value for that 5-minute interval

complete_data <- data

for(i in 1:length(complete_data$steps)){
  if(is.na(complete_data$steps[i])){
    complete_data$steps[i] <- by_interval[which(by_interval$interval == data$interval[i]), 2]
  }
}


## Create a new dataset that is equal to the original dataset but with the missing data filled in.

## Make a histogram of the total number of steps taken each day and 
## Calculate and report the mean and median total number of steps taken per day. 
## Do these values differ from the estimates from the first part of the assignment? 
## What is the impact of imputing missing data on the estimates of the total daily number of steps?

complete_per_day <- tapply(complete_data$steps, complete_data$date, sum)
hist(complete_per_day, main = "Histogram of Total Number of Steps per Day", 
     xlab = "Total Number of Steps", col = "maroon")

## Or with ggplot
library(ggplot2)
qplot(complete_per_day, 
      geom = "histogram", 
      binwidth = 5000, 
      main = "Histogram of Total Number of Steps per Day",
      xlab = "Total Number of Steps",
      ylab = "Frequency",
      fill = I("maroon"), col = I("black"))

mean(complete_per_day$)   ## 10766.19
median(complete_per_day) ## 10766.19

