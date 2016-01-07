# Reproducible Research: Peer Assessment 1
davedev44 @ github  

## Loading and preprocessing the data
We unzip the data and load it into a variable.

```r
unzip("activity.zip")
stepsdata <- read.csv("activity.csv")
```

We need to preprocess the data - we want sums per day, regardless of interval.

```r
library(plyr)

# Sum over the steps variable for each date with ddply
dailystepsdata <- ddply(stepsdata, .(date), summarize, totSteps=sum(steps))
```

## What is mean total number of steps taken per day?

```r
# First, we display a histogram of the steps per day.
hist(dailystepsdata$totSteps, xlab="steps per day", ylab="frequency", main="Histogram of Steps per Day")
```

![](PA1_template_files/figure-html/dailystepshistmean-1.png) 

```r
# Next, simply compute the mean, without NA values
dailymeansteps <- mean(dailystepsdata$totSteps, na.rm=TRUE)
dailymeansteps
```

```
## [1] 10766.19
```

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
