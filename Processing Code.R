## Unzip data and read csv file
data <- read.csv(unz("activity.zip", "activity.csv"))
str(data)

## Convert Date variable to POSIX.ct
library(dplyr)
data <- mutate(data, date = as.POSIXct(date, tz = "", "%Y-%m-%d"))
  