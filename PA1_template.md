# Reproducible Research: Peer Assessment 1

## 1 Loading and preprocessing the data
The data for this assignment can be downloaded from the course web site:  
- Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)  

The variables included in this dataset are:  
- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
- date: The date on which the measurement was taken in YYYY-MM-DD format  
- interval: Identifier for the 5-minute interval in which measurement was taken  

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


```r
# The following libraries need to be installed on this system to enable running this knitr script
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.1.3
```

```r
# The dataset used has to exist on the same directory as where this R script is stored
activity <- read.csv("activity.csv", colClasses = c("numeric", "character","numeric"))
```
### 1.1 Process input data  


```r
# 1.1 Process input data
activity$date <- as.Date(activity$date, "%Y-%m-%d")

names(activity)
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```
  
We will use subset of data so no transformations necessary. 
  

## 2 What is mean total number of steps taken per day?
### 2.1 Using aggregate function for total number of steps per day  

```r
# 2.1 Using aggregate function for total number of steps per day

TotalSteps <- aggregate(steps ~ date, data = activity, sum, na.rm = TRUE)
```
### 2.2 Histogram of total number of steps taken per day



```r
#2.2 Histogram of total number of steps taken per day

hist(TotalSteps$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

### 2.3 Mean and median of total number of steps taken per day  
Mean:   

```r
# 2.3 Mean and median of the total number of steps taken per day

mean1<-mean(TotalSteps$steps)
mean1
```

```
## [1] 10766.19
```
Median:  


```r
## Median 
median1<-median(TotalSteps$steps)
median1
```

```
## [1] 10765
```



## 3 What is the average daily activity pattern?
### 3.1 Average daily activity pattern -  Time-series plot of 5 minute intervals vs steps taken
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# 3.1 Average daily activity pattern -  Time-series plot of 5 minute intervals vs steps taken

time_series <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)

plot(row.names(time_series), time_series, type = "l", xlab = "5-min interval", ylab = "Average across all Days", main = "Average number of steps taken", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

### 3.2 5 minute max value of daily activity pattern
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# 3.2 5 minute max value of daily activity pattern

max_interval <- which.max(time_series)
names(max_interval)
```

```
## [1] "835"
```

## 4 Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

### 4.1 Total number of missing values


```r
# 4.1 Total number of missing values

activity_NA <- sum(is.na(activity))
activity_NA
```

```
## [1] 2304
```


### 4.2 Replace missing values by mean of 5 minute intervals
We can replace the missing values with the mean value of the 5-minute intervals by using a function that is conditional on the is.na and number of steps. 


```r
# 4.2 Use average number of non-NA steps per 5 minute interval over all days in dataset

StepsAverage <- aggregate(steps ~ interval, data = activity, FUN = mean)

fillNA <- numeric()

for (i in 1:nrow(activity)) {
    obs <- activity[i, ]
    if (is.na(obs$steps)) {
        steps <- subset(StepsAverage, interval == obs$interval)$steps
    } else {
        steps <- obs$steps
    }
    fillNA <- c(fillNA, steps)
}
```

### 4.3 Create a new dataset including adapted missing data

```r
# 4.3 Create a new dataset including adapted missing data with average values per 5 minute interval in case original was NA

new_activity <- activity
new_activity$steps <- fillNA
```

### 4.4 Histogram of new dataset including adapted missing data

#### 4.4.1 Histogram, mean and median of updated dataset


```r
# 4.4.1 Histogram, mean and median of updated dataset
StepsTotal2 <- aggregate(steps ~ date, data = new_activity, sum, na.rm = TRUE)
hist(StepsTotal2$steps, main = "Total steps by day", xlab = "day", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

#### 4.4.2 Mean and median of total number of steps taken per day  
Mean:   

```r
# 4.4.2 Mean and median of the total number of steps taken per day
mean2<-mean(StepsTotal2$steps)
mean2
```

```
## [1] 10766.19
```
Median:  


```r
## Median 
median2<-median(StepsTotal2$steps)
median2
```

```
## [1] 10766.19
```

```r
# The mean is the same, the median has shifted a little bit, though.
```
The mean is the same, the median has shifted a little bit, though.  

## 5 Are there differences in activity patterns between weekdays and weekends?
Calculating differences in activity patterns between weekdays and weekends
Using the filled-in dataset the differences between weekdays and weekends are determined.  

### 5.1 Activity patterns per weekday or weekend


```r
# 5.1 Activity patterns per weekday or weekend

# Use weekdays() function here to get the day of the date mentioned in data set.

# Create a new factor variable in the dataset with two levels ? ?weekday? and ?weekend? indicating whether a given date is a weekday or weekend day.
day <- weekdays(activity$date)
daylevel <- vector()
for (i in 1:nrow(activity)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity$daylevel <- daylevel
activity$daylevel <- factor(activity$daylevel)

stepsByDay <- aggregate(steps ~ interval + daylevel, data = activity, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
```

### 5.2 Panel plot 
A panel plot is created containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
# 5.2 Panel plot
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-16-1.png) 
