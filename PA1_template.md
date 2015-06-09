# Reproducible Research: Peer Assessment 1
Tom Bruning  

## Initial setup


```r
require(dplyr)
require(tidyr)
require(ggplot2)
require(xtables)
require(data.table)
```
ts 
## Loading and preprocessing the data
Since I am using a static file, downloaded and unzipped once I am not including downloading the file step.  
I use the data table package since running in tables is faster than running as data frames.

```r
DT <- read.table("activity.csv", sep = ",", header = TRUE)
glimpse(DT)
```

```
## Observations: 17568
## Variables:
## $ steps    (int) NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
## $ date     (fctr) 2012-10-01, 2012-10-01, 2012-10-01, 2012-10-01, 2012...
## $ interval (int) 0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 100, 10...
```

```r
## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
```
