# Reproducible Research: Peer Assessment 1

Before starting we have to load some libraries neccessary for next work. Also, we set a name of directory, in which our dataset is.

```r
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
library(lattice, quietly=TRUE)
library(knitr)
filename <- "activity.zip"
```


## Loading and preprocessing the data

```r
data <- read.csv(unz(filename, "activity.csv"),
                 header=TRUE,
                 colClasses=c("numeric","Date","numeric"))

data <- tbl_df(data)

# we save datasets twice - with NAs & without NAs
with.NAs <- data
without.NAs <- filter(data,
                      !is.na(steps))
```


## What is mean total number of steps taken per day?

```r
# we must group data by date
data.1 <- group_by(without.NAs,
                   date)

# the next step is adding new variable, which contains sum of steps by days
data.1 <- mutate(data.1,
                 steps.by.days=sum(steps))

# we want unique days, because each day with one date has the same sum of steps  
data.1 <- distinct(data.1,
                   date)

# finally, we can draw a histogram
hist(data.1$steps.by.days,
     xlab="Number of Steps Taken",
     ylab="Number of Days",
     main="Histogram of number of steps taken by days",
     labels=TRUE,
     col="orange")
```

<img src="PA1_template_files/figure-html/meanMedianAndHistogram-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
data1.mean <- sprintf("%.2f", mean(data.1$steps.by.days))
data1.median <- sprintf("%.2f", median(data.1$steps.by.days))
```
**Mean** total number of steps taken per day is **10766.19**, while the **median** has value of **10765.00**.


## What is the average daily activity pattern?

```r
# we must group data by intervals
data.2 <- group_by(without.NAs,
                   interval)

# the next step is adding new variable, which contains average of steps by intervals
data.2 <- mutate(data.2,
                 average.steps.by.intervals=sum(steps)/n())

# we want unique inervals, because each interval with the same number has the same average of steps  
data.2 <- distinct(data.2,
                   interval)

# finally, we can draw a plot
with(data.2, plot(interval,
                  average.steps.by.intervals,
                  type="l",
                  xlab="Intervals",
                  ylab="Average Number of Steps Taken"))
```

<img src="PA1_template_files/figure-html/averageDailyPattern-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
# we must ungroup the data so that we can find interval with maximal average steps taken
data.2 <- ungroup(data.2)

# now we can find needed interval
maximum.number.of.steps <- filter(data.2,
                                  average.steps.by.intervals==max(average.steps.by.intervals))$interval
```
Interval **835** contains the **maximum number of steps**, on average across all the days in the dataset.


## Imputing missing values

```r
sum.of.NAs <- sum(is.na(with.NAs))
```
The total **number of missing values** in the dataset (i.e. the total number of rows with `NA`s) is **2304**.

In the next code chunk we firstly group data by inervals (including rows with `NA`s). That is made because we are going to replace all `NA`s with mean number of steps taken in each interval.

```r
data.3 <- group_by(with.NAs, interval)
```

So after grouping the data is done, we add a variable called inerval.mean, which contains mean number of steps taken for each interval separately.

```r
data.3 <- mutate(data.3, interval.mean=mean(steps, na.rm=TRUE))
```

The last step is done with an ease of `ifelse` statement, which allows us to simply replace `NA`s with mean number of steps for current interval, or to let there original number of steps.

```r
data.3 <- mutate(data.3, steps=ifelse(is.na(steps), interval.mean, steps))
data.3 <- select(data.3, steps, date, interval)

# this is just for saving our time - we store the modified dataset for next task
data.4 <- data.3
```

The last chunk of the code is for drawing a histogram, similarly as in the first task of this assignment.

```r
data.3 <- ungroup(data.3)

data.3 <- group_by(data.3,
                   date)

data.3 <- mutate(data.3,
                 steps.by.days=sum(steps))

data.3 <- distinct(data.3,
                   date)

hist(data.3$steps.by.days,
     xlab="Number of Steps Taken",
     ylab="Number of Days",
     main="Histogram of number of steps taken by days",
     labels=TRUE,
     col="orange")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-4-1.png" title="" alt="" style="display: block; margin: auto;" />

```r
data3.mean <- sprintf("%.2f", mean(data.3$steps.by.days))
data3.median <- sprintf("%.2f", median(data.3$steps.by.days))
```
**Mean** total number of steps taken per day with `NA`s replaced is **10766.19**, while the **median** has value of **10766.19**. After inspecting the new histogram made of dataset with `NA`s included we can see slightly different results. The total amount of days is increased by 8 which has an impact also on the median, which is greater than before and equals to mean number of steps taken.


## Are there differences in activity patterns between weekdays and weekends?
In this part we have got one restriction, which is to use the dataset made few steps before, where all `NA`s were replaced.

```r
data.4 <- ungroup(data.4)

# we simply add a variable containing type of the current day
data.4 <- mutate(data.4,
                 day.type=ifelse(weekdays(date) %in% c('sobota','nedela'),'weekend','weekday'))

# we must group data by intervals and by date.type simultaneously
data.4 <- group_by(data.4,
                   interval,
                   day.type)

data.4 <- mutate(data.4,
                 average.steps.by.day.type=sum(steps)/n())

data.4 <- distinct(data.4,
                   interval,
                   day.type)

xyplot(average.steps.by.day.type ~ interval | day.type,
       data=data.4,
       xlab="Intervals",
       ylab="Average Number of Steps Taken",
       layout=c(1,2),
       type="l")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-5-1.png" title="" alt="" style="display: block; margin: auto;" />
