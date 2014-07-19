---
output:
  html_document:
    keep_md: yes
---
# Reproducible Research: Peer Assessment 1
Janne Simonen  

This document is a project for the Coursera online course *Reproducible Research*.
The assignment makes use of data from a personal activity monitoring device. 
This device collects data at 5 minute intervals through out the day. The data 
consists of two months of data from an anonymous individual collected during the 
months of October and November, 2012 and include the number of steps taken in 
5 minute intervals each day.

The data has the following variables  

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
- date: The date on which the measurement was taken in YYYY-MM-DD format  
- interval: Identifier for the 5-minute interval in which measurement was taken.  

## Loading and preprocessing the data

The activity monitoring data is provided as a zipped CSV file, so we first unzip
it and then read it into a table. For the analysis, we also need to convert the date 
strings into date format. The following code takes care of all of that.


```r
unzip("activity.zip")
activity <- read.csv("activity.csv",stringsAsFactors=FALSE)
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## Total number of steps taken per day

Let's calculate the number of steps taken per day. We ignore the missing values.


```r
library(plyr) 
stepsperday <- ddply(activity, .(date),summarize,steps = sum(steps, na.rm=TRUE))
meansteps <- mean(stepsperday$steps)
mediansteps <- median(stepsperday$steps)
```

The mean steps per day is 9354 and median 1.0395 &times; 10<sup>4</sup>. 
Let's plot a histogram.


```r
hist(stepsperday$steps,breaks=20,xlab="Steps per day",main="Histogram of steps per day")
```

![plot of chunk stepsogram](figure/stepsogram.png) 

## What is the average daily activity pattern?

Let us study the daily activity pattern by averaging the number of steps for each 5 minute interval
across all day in the dataset.


```r
library(plyr) 
stepsperint <- ddply(activity, .(interval),summarize,averageSteps = mean(steps, na.rm=TRUE))
maxavesteps <- max(stepsperint$averageSteps)
maxavestepsIndex <- which.max(stepsperint$averageSteps)
maxinterval <- activity$interval[maxavestepsIndex]
```

The maximum number of steps, on average, is 206 
which happens in time interval 835. This can be seen from the following
time series plot


```r
with(stepsperint,plot(interval,averageSteps,type="l",xlab="Interval",
                      ylab="Average steps",main="Average daily activity"))
```

![plot of chunk dailyactivityplot](figure/dailyactivityplot.png) 


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?