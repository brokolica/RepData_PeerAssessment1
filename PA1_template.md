# Reproducible Research: Peer Assessment 1


```r
library(dplyr)
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
filename <- "activity.zip"
```


## Loading and preprocessing the data

```r
data <- read.csv(unz(filename, "activity.csv"),
                 header=TRUE,
                 colClasses=c("numeric","Date","numeric"))

data <- tbl_df(data)

with.NAs <- data
without.NAs <- filter(data, !is.na(steps))
```


## What is mean total number of steps taken per day?


```r
# we must group data by date
data.1 <- group_by(without.NAs, date)

# the next step is adding new variable, which contains sum of steps by days
data.1 <- mutate(data.1, steps.by.days=sum(steps))

# we want unique days, because each day with one date has the same sum of steps  
data.1 <- distinct(data.1, date)

# finally, we can draw a histogram
hist(data.1$steps.by.days,
     xlab="Num. of steps",
     main="Histogram of number of steps taken by days",
     labels=TRUE,
     col="orange")
```

![](PA1_template_files/figure-html/meanMedianAndHistogram-1.png) 

```r
mean(data.1$steps.by.days)
```

```
## [1] 10766.19
```

```r
median(data.1$steps.by.days)
```

```
## [1] 10765
```



## What is the average daily activity pattern?

```r
# we must group data by intervals
data.2 <- group_by(without.NAs, interval)

# the next step is adding new variable, which contains average of steps by intervals
data.2 <- mutate(data.2, average.steps.by.intervals=sum(steps)/n())

# we want unique inervals, because each interval with the same number has the same average of steps  
data.2 <- distinct(data.2, interval)

# finally, we can draw a plot
with(data.2, plot(interval, average.steps.by.intervals, type="l"))
```

![](PA1_template_files/figure-html/averageDailyPattern-1.png) 

```r
# we must ungroup the data so that we can find interval with maximal average steps taken
data.2 <- ungroup(data.2)

# now we can find needed interval
filter(data.2, average.steps.by.intervals==max(average.steps.by.intervals))$interval
```

```
## [1] 835
```

## Imputing missing values

```r
sum(is.na(with.NAs))
```

```
## [1] 2304
```

```r
# data <- group_by(data, interval)
# data <- mutate(data, interval.mean=mean(steps, na.rm=TRUE))
# backup <- mutate(backup, new.steps=ifelse(is.na(steps), interval.mean, steps))
```

## Are there differences in activity patterns between weekdays and weekends?
V tejto casti treba pouzit data, kde su NA hodnoty nahradene uz!!!

```r
# we simply add a variable containing type of the current day
data.4 <- mutate(without.NAs, day.type=ifelse(weekdays(date) %in% c('sobota','nedela'),'weekend','weekday'))

# we must group data by intervals and by date.type simultaneously
data.4 <- group_by(data.4, interval, day.type)

data.4 <- mutate(data.4, average.steps.by.day.type=sum(steps)/n())


data.4 <- distinct(data.4, interval, day.type)


xyplot(average.steps.by.day.type ~ interval | day.type, data=data.4, layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png) 

```r
data.4 <- ungroup(data.4)

filter(data.4, average.steps.by.day.type==max(average.steps.by.day.type))
```

```
## Source: local data frame [1 x 5]
## 
##   steps       date interval day.type average.steps.by.day.type
## 1   173 2012-10-06      915  weekend                  276.1429
```

















