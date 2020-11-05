---
title: "RepData PeerAssesssment 1"
author: "Jochen van Waasen"
date: '2020-11-05'
output:
  html_document:
    keep_md: yes
---

## Loading and preprocessing the data
* Load data file

```r
fitdata <- read.csv("RepData_PeerAssessment1/activity.csv")
```

* Process data: steps~date

```r
totalSteps <- aggregate(steps~date, data=fitdata, sum, na.rm=TRUE)
```

## What is mean total number of steps taken per day?

* Histogram of the total number of steps taken each day

```r
hist(totalSteps$steps)
```

![](PeerAssignment1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

* Mean of the total number of steps taken per day

```r
mean(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

* Median of the total number of steps taken per day

```r
median(totalSteps$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

* Plot daily activity pattern of the 5-minute interval and average number of steps taken

```r
stepsInterval<-aggregate(steps~interval,data=fitdata,mean,na.rm=TRUE)
plot(steps~interval,data=stepsInterval,type="l")
```

![](PeerAssignment1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
* 5-minute interval with maximum number of steps on average across all days

```r
stepsInterval[which.max(stepsInterval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

* Number of rows in dataset with missing values

```r
sum(is.na(fitdata))
```

```
## [1] 2304
```

* Strategy:  Use the mean for that 5-minute interval to fill in missing values (utilizing below function) 


```r
meanIntervalSteps<-function(interval){
  stepsInterval[stepsInterval$interval==interval,]$steps
}
```

* Create a copy of the original data and fill NA rows with chosen strategy:
  

```r
fitdataComplete<-fitdata             
for(i in 1:nrow(fitdataComplete)){
  if(is.na(fitdataComplete[i,]$steps)){
    fitdataComplete[i,]$steps<-meanIntervalSteps(fitdataComplete[i,]$interval)
  }
}
```
* Process complete data: steps~date

```r
totalStepsComplete <- aggregate(steps~date, data=fitdataComplete, sum, na.rm=TRUE)
```

* Mean of the total number of steps taken per day without missing values

```r
mean(totalStepsComplete$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

* Median of the total number of steps taken per day

```r
median(totalStepsComplete$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

* Basically the values are the same (not identical)
* The **mean** value is the **identical** as the value before imputing missing data.
* The median value shows **a little** difference.

## Are there differences in activity patterns between weekdays and weekends?


```r
fitdataComplete$date <- as.Date(fitdataComplete$date)

weekDay.End <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
```

* Create new factor variable weekday/weekend

```r
fitdataComplete$day <- sapply(fitdataComplete$date, FUN=weekDay.End)
totalStepsWeekDay.End <- aggregate(steps ~ interval + day, data=fitdataComplete, mean)
```

* Panel plot weekday/weekend

```r
library(ggplot2)

ggplot(totalStepsWeekDay.End, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5-minute interval") + ylab("Number of steps")
```

![](PeerAssignment1_files/figure-html/unnamed-chunk-16-1.png)<!-- -->
