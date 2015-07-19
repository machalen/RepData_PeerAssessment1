Peer Assessment 1
========================================================

First of all the data is loaded from the activity.zip file, it consist with 17568 rows and 3 columns with information about the steps, and the corresponding date and interval when the measures were taken.


```r
datafile <- read.csv(unzip("activity.zip"))
dim(datafile)  #17568     3
```

```
## [1] 17568     3
```


## What is mean total number of steps taken per day?

Build a table with the total number of steps taken per day.

```r
stepsXday <- aggregate(. ~ date, data = datafile[, c(1, 2)], sum, na.action = NULL)
stepsXday
```

```
##          date steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```


Histogram of the total number of steps taken per day.

```r
hist(stepsXday$steps, breaks = 6, main = "Hitogram with the steps per day", 
    xlab = "Steps", col = "lightgreen")
```

![plot of chunk Steps per day 2](figure/Steps_per_day_2.png) 


The mean and the median of the total number of steps taken per day is shown. The mean is 10766.19 and the median is 10765.

```r
meanXday <- mean(stepsXday$steps, na.rm = TRUE)
meanXday
```

```
## [1] 10766
```

```r
medianXday <- median(stepsXday$steps, na.rm = TRUE)
medianXday
```

```
## [1] 10765
```


## What is the average daily activity pattern?

A data.frame with the mean of the number of steps taken in each interval of time is built. The plot shows the mean of the steps (y axes) and the interval (x axes).

```r
stepsXint <- aggregate(. ~ interval, data = datafile[, c(1, 3)], mean)
str(stepsXint)
```

```
## 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
```

```r
plot(stepsXint$interval, stepsXint$steps, type = "l", xlab = "Interval", ylab = "Number of steps mean")
```

![plot of chunk Steps per interval](figure/Steps_per_interval.png) 


With the value of 206.1698, the interval with the maximum mean of the number of steps is the 835. 

```r
stepsXint[stepsXint$steps == max(stepsXint$steps), "interval"]
```

```
## [1] 835
```


## Imputing missing values

There are 2304 entries with NA values.


```r
sum(is.na(datafile$steps))
```

```
## [1] 2304
```


In order to solve this problem, missing values are replaced by the mean of steps in the same interval.


```r
# Detect the days with NA values
NAdays <- stepsXday[is.na(stepsXday$steps), "date"]
# Delete the entries with NA from the original data.frame
dataWOna <- datafile[!is.na(datafile$steps), ]
# Make a loop in order to add those dates with missing values with the
# interval mean
for (day in NAdays) {
    steps <- stepsXint$steps
    date <- rep(day, 288)
    interval <- stepsXint$interval
    newdate <- data.frame(steps, date, interval)
    dataWOna <- rbind(dataWOna, newdate)
}
dataWOna <- dataWOna[order(dataWOna$date), ]
```


The histogram with the total number of steps, the mean and the median are made again in order to find the diferences when replacing the NA. No significative diference is shown in the histogram. The mean has not changed and continue being 10766.19. However the median do change to be the same value of the mean.


```r
NAstepsXday <- aggregate(. ~ date, data = dataWOna[, c(1, 2)], sum, na.action = NULL)
NAstepsXday
```

```
##          date steps
## 1  2012-10-01 10766
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08 10766
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01 10766
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04 10766
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09 10766
## 41 2012-11-10 10766
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14 10766
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30 10766
```

```r

hist(NAstepsXday$steps, breaks = 6, main = "Steps per day WO NAs", xlab = "Steps", 
    col = "lightgreen")
```

![plot of chunk hist mean and median](figure/hist_mean_and_median.png) 

```r

NAmeanXday <- mean(NAstepsXday$steps, na.rm = TRUE)
NAmeanXday
```

```
## [1] 10766
```

```r
NAmedianXday <- median(NAstepsXday$steps, na.rm = TRUE)
NAmedianXday
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?

The days of the week are identifyied among the dates and the number of steps is divided depending on: "weekday" or "weekend"


```r
weekdays <- weekdays(as.Date(dataWOna$date))
list1 <- unique(weekdays)
list2 <- c(rep("weekday", 4), rep("weekend", 2), "weekday")
map = setNames(list2, list1)
weekID <- map[weekdays]
weektable <- cbind(dataWOna, weekID = as.factor(weekID))
weekdayTabl <- weektable[weekID == "weekday", ]
weekendTabl <- weektable[weekID == "weekend", ]
```


Results of the mean of the steps per interval in weekdays are shown below in a plot:

```r
stepsXintWD <- aggregate(. ~ interval, data = weekdayTabl[, c(1, 3)], mean)
dim(stepsXintWD)  #288   2
```

```
## [1] 288   2
```

```r
plot(stepsXintWD$interval, stepsXintWD$steps, type = "l", xlab = "Interval", 
    ylab = "Average of steps in weekdays")
```

![plot of chunk stepsxinterval in weekdays](figure/stepsxinterval_in_weekdays.png) 


Results of the mean of the steps per interval in weekend are shown below in a plot:

```r
stepsXintWN <- aggregate(. ~ interval, data = weekendTabl[, c(1, 3)], mean)
dim(stepsXintWN)  #288   2
```

```
## [1] 288   2
```

```r
plot(stepsXintWN$interval, stepsXintWN$steps, type = "l", xlab = "Interval", 
    ylab = "Average of steps in weekend")
```

![plot of chunk stepsxinterval in weekends](figure/stepsxinterval_in_weekends.png) 


It looks like in the weekends a higher number of steps are reported in the late intervals. Moreover, the increase of the steps takes place in earlier intervals during the weekdays.

.........
