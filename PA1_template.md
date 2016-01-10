# Reproducible Research: Peer Assessment 1


```r
library(plyr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
```

## Loading and preprocessing the data


```r
activity.data = read.csv(unz("activity.zip", "activity.csv"))

activity.data = mutate(activity.data, date = as.Date(activity.data$date, "%Y-%m-%d"))
```

## What is mean total number of steps taken per day?


```r
total.steps.per.day = ddply(activity.data, .(date), summarize, total.steps = sum(steps))

total.steps = with(total.steps.per.day, total.steps[!is.na(total.steps)])
hist(total.steps, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)\

```r
mean.total.steps = mean(total.steps)
median.total.steps = median(total.steps)
print(mean.total.steps)
```

```
## [1] 10766.19
```

```r
print(median.total.steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
mean.steps.per.interval = ddply(activity.data, .(interval), summarize, mean.steps = mean(steps, na.rm = T))
with(mean.steps.per.interval, plot(interval, mean.steps, type="l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)\

```r
max.interval = with(mean.steps.per.interval, interval[which.max(mean.steps)])
print(max.interval)
```

```
## [1] 835
```

## Imputing missing values


```r
total.na = sum(is.na(activity.data$steps))
print(total.na)
```

```
## [1] 2304
```

There is only missing data when there are no measurements whatsoever for an entire day. Therefore, we must transplant information from the other days in the dataset. The most reasonable procedure available is to take the mean steps from every recorded interval as the reported steps for missing intervals.  


```r
activity.data.imputed = activity.data
activity.data.imputed$steps[is.na(activity.data.imputed$steps)] = mean.steps.per.interval$mean.steps

total.steps.per.day.imputed = ddply(activity.data.imputed, .(date), summarize, total.steps = sum(steps))
total.steps.imputed = total.steps.per.day.imputed$total.steps
hist(total.steps.imputed, breaks = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)\

```r
mean.total.steps.imputed = mean(total.steps.imputed)
median.total.steps.imputed = median(total.steps.imputed)
print(mean.total.steps.imputed)
```

```
## [1] 10766.19
```

```r
print(median.total.steps.imputed)
```

```
## [1] 10766.19
```

The mean number of total steps hasn't changed, but the median number of total steps has. This makes sense, as we've added eight more days to the dataset where the total number of steps is equal to the mean number of steps  

## Are there differences in activity patterns between weekdays and weekends?


```r
day.of.week = as.factor(ifelse(weekdays(activity.data.imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday"))

activity.data.imputed$day.of.week = day.of.week
mean.steps.per.interval.imputed = ddply(activity.data, .(day.of.week, interval), summarize, mean.steps = mean(steps, na.rm = T))
with(mean.steps.per.interval.imputed, qplot(interval, as.integer(mean.steps), geom="line", facets = (day.of.week ~ .)))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)\
