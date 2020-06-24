## Unzip data and read csv file
data <- read.csv(unz("activity.zip", "activity.csv"))
str(data)

## Convert Date variable to POSIX.ct and Interval variable to factor
library(dplyr)
data <- mutate(data, date = as.POSIXct(date, tz = "", "%Y-%m-%d"))
  