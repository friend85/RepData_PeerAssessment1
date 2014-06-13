# Reproducible Research: Peer Assessment 1


It is now possible to collect a large amount of data about personal movement using 
activity monitoring devices, and this assignment makes use of a set of data 
collected at 5-minute intervals throughout the day. 
This data consisits of two months of data of an anonymous individual 
collected during the months of October 
and November, 2012 and include the number of steps taken in 5 minutes intervals each day.

## Loading and preprocessing the data
The data can be downloaded from the course website as below.

[Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:
- **steps**: Number of steps taking in a 5-minute interval.
- **date**: The date on which the measurement was taken in YYYY-MM-DD format.
- **interval**: Identifier for the 5-minute interval in which measurement was taken.

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

```r
activity <- read.csv("activity.csv")
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
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?
To compute means and medians, we have to split the data frame with dates, and apply 'mean' and 'median' on each date frame for a given date.

```r
activityByDate <- split(activity, activity$date)
nStepsList1 <- lapply(activityByDate, function(x) sum(x$steps, na.rm=TRUE))
```

Then, combine these results into a data frame,
and make a histogram for mean total number of steps taken per day.

```r
dates <- unique(activity$date)
nSteps1 <- unsplit(nStepsList1, dates)
hist(nSteps1, breaks=10, xlab="total number of steps per day", 
     main="Histogram of total number of steps per day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

Then, the mean and median of total number of steps per day are as below.

```r
mean(nSteps1, na.rm=TRUE)
```

```
## [1] 9354
```

```r
median(nSteps1, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Now, we calculate the average number of steps taken in a given time interval of the day,
and show the plot as a time series.

```r
intervals <- unique(activity$interval)
activityByInterval <- split(activity, activity$interval)
nStepsList2 <- lapply(activityByInterval, function(x) mean(x$steps, na.rm=TRUE))
plot(intervals, nStepsList2, type="l", xlab="interval", ylab="average number of steps",
  main="Time series of average number of steps in a day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

The interval with the maximum number of steps is as below.

```r
nSteps2 <- unsplit(nStepsList2, intervals)
nStepsByInterval <- data.frame(intervals, nSteps2)
nStepsByInterval[which.max(nStepsByInterval$nSteps2),1]
```

```
## [1] 835
```

The result, 835, indicates that the interval is between 8:35 and 8:40 in the morning, 
and that is the time when people take the most steps in a day. 

## Imputing missing values
Since there is no missing values for 'date' and 'interval' columns, 
the total number of missing values (the total number of rows with NAs) is 
2304.

```r
sum(is.na(activity$date))
```

```
## [1] 0
```

```r
sum(is.na(activity$interval))
```

```
## [1] 0
```

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

Now we will fill in all of missing values with the mean for that 5-minute interval 
over 2-month period,
and store values in a new data frame, called 'activity2'.

```r
activity2 <- activity
nObs <- dim(activity2)[1]
for (i in 1:nObs) {
  if (is.na(activity2[i,1])) {
    activity2[i,1] <- nStepsByInterval[nStepsByInterval$intervals==activity2[i,3],2]
  }
}
```

The histogram of this new data frame, and the mean and median total number of steps 
taken per day are as below.

```r
activityByDate3 <- split(activity2, activity2$date)
nStepsList3 <- lapply(activityByDate3, function(x) sum(x$steps, na.rm=TRUE))
dates <- unique(activity2$date)
nSteps3 <- unsplit(nStepsList3, dates)
hist(nSteps3, breaks=10, xlab="total number of steps per day", 
     main="Histogram of total number of steps per day")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
mean(nSteps3, na.rm=TRUE)
```

```
## [1] 10766
```

```r
median(nSteps3, na.rm=TRUE)
```

```
## [1] 10766
```

The results show that, if NAs are filled in with mean values, 8 days with zero steps,
because of NAs shown previously, are replaced with reasonable values.

```r
sum(nSteps1==0)  # Number of days with zero total number of days for original data.
```

```
## [1] 8
```

```r
sum(nSteps3==0)  # Number of days with zero total number of days for new filled-in data.
```

```
## [1] 0
```

This is also why the median value increased and became the same as the mean
in the new data.

## Are there differences in activity patterns between weekdays and weekends?
To answer this question, we have to add an additional column to the new data frame.
This column says the day is either a weekday or a weekend day.

```r
activity2$weekday <- as.factor(ifelse(as.POSIXlt(activity[,2], format="%Y-%m-%d")$wday %in% 1:5,
                            "weekday", "weekend"))
str(activity2)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ weekday : Factor w/ 2 levels "weekday","weekend": 1 1 1 1 1 1 1 1 1 1 ...
```

Now we split the data frame using the new column, and make the same computation
for the mean number of steps for each interval for weekdays and weekends respectively.

```r
activityByInterval2 <- split(activity2, activity2$interval)
nStepsList4 <- lapply(activityByInterval2, function(x) mean(x[x$weekday=="weekday",1], 
                                                            na.rm=TRUE))
nStepsList5 <- lapply(activityByInterval2, function(x) mean(x[x$weekday=="weekend",1], 
                                                            na.rm=TRUE))
par(mfcol=c(2,1))
plot(intervals, nStepsList4, type="l", xlab="interval", ylab="average number of steps",
  main="Time series of average number of steps in a weekday")
plot(intervals, nStepsList5, type="l", xlab="interval", ylab="average number of steps",
  main="Time series of average number of steps in a weekend day")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13.png) 

Results clearly show the difference: in weekdays, this person's activity is the highest
during the morning between 8 and 9, while, 
in weekends, the activity is still higher in the morning, but the activity stays high
throughout the day. 

## Summary
We analyzed the activity data of an individual 
collected every five minutes for two months as a peer-review assignment of this course.
We looked at the total numbers of steps taken per day and mean
numbers of steps per interval in various situations, 
and observed some meaningful behavior.
