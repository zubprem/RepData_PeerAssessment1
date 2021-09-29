---
title : "Reproducible Research Programming Assingment Report"
output: 
  html_document:
    keep_md: true
---
This report contains analysis of personal activity data that is acquired from activity monitoring devices.
[Data Source](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

## Loading and Processing the data 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = 'rpasg.zip')

activity_data <- read.csv(unzip('rpasg.zip'))
activity_data$date <- as.Date(activity_data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Let's make the histogram for the total number of steps taken each day

```r
total_daily_steps <- activity_data %>% group_by(date) %>% summarise(sum(steps, na.rm = TRUE))
colnames(total_daily_steps)[2] = "steps"

hist(total_daily_steps$steps, xlab = "daily steps", main = "Total daily steps histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Let's calculate the **mean** and **median** total number of steps taken per day

```r
mean_steps <- mean(total_daily_steps$steps)
median_steps <- median(total_daily_steps$steps)
print(paste("the mean no. of steps:", mean_steps))
```

```
## [1] "the mean no. of steps: 9354.22950819672"
```

```r
print(paste("the median no. of steps:", median_steps))
```

```
## [1] "the median no. of steps: 10395"
```

## What is the average daily pattern ?
Let's make the time series plot of the 5-minute interval and average number of steps taken across all days

```r
interval_avg_steps <- activity_data %>% group_by(interval) %>% summarise(mean(steps, na.rm = TRUE))
colnames(interval_avg_steps)[2] <- "steps"
with(interval_avg_steps, plot(interval, steps, type = "l"))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_steps <- interval_avg_steps$interval[which.max(interval_avg_steps$steps)]
print(paste("max steps 5 minute interval:", max_steps))
```

```
## [1] "max steps 5 minute interval: 835"
```

## Inputting missing values
Let's calculate the total number of missing values in the dataset

```r
print(paste("total number of missing values:", sum(is.na(activity_data))))
```

```
## [1] "total number of missing values: 2304"
```
Let's replace all NA's for steps data with mean value for that particular interval and create new dataset

```r
activity_data_1 <- activity_data
activity_data_1 <- activity_data_1 %>% inner_join(interval_avg_steps, by= "interval") %>% 
                   mutate(steps = coalesce(steps.x, steps.y)) %>%
                   select(interval, steps)
activity_data_1$date <- activity_data$date
```
Now let's make a histogram of total steps per day with the new dataset

```r
total_daily_steps_1 <- activity_data_1 %>% group_by(date) %>% summarise(sum(steps, na.rm = TRUE))
colnames(total_daily_steps_1)[2] <- "steps"
hist(total_daily_steps_1$steps, xlab = "total daily steps", main = "Histogram of total daily steps after filling in NA's")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

As we can see by comparing with previous histogram, after filling is NA's the histogram shape has slightly converged to a bell shaped curve

And now the **mean** and **median** of the total steps per day

```r
mean_steps_1 <- mean(total_daily_steps_1$steps)
median_steps_1 <- median(total_daily_steps_1$steps)
print(paste("the mean no. of steps:", mean_steps_1))
```

```
## [1] "the mean no. of steps: 10766.1886792453"
```

```r
print(paste("the median no. of steps:", median_steps_1))
```

```
## [1] "the median no. of steps: 10766.1886792453"
```
By comparing with previous values we can see that both mean and median are higher than their previous values and now this
time both mean and median have the same value

## Are there differences in activity patterns between weekdays and weekends?
Let's create two factor levels "weekday" and "weekend"

```r
activity_data_1$weekday <- weekdays(activity_data_1$date)
activity_data_1$day_type <- ifelse(activity_data_1$weekday %in% c("Saturday", "Sunday"), "weekend", "weekday")
```
Let's make a panel plot of 5-minute interval vs average no. of steps taken for weekdays and weekends

```r
interval_avg_steps_1 <- activity_data_1 %>% group_by(day_type, interval) %>% summarise(mean(steps, na.rm = TRUE))
```

```
## `summarise()` has grouped output by 'day_type'. You can override using the `.groups` argument.
```

```r
colnames(interval_avg_steps_1)[3] <- "steps"
library(ggplot2)
t <- ggplot(interval_avg_steps_1, aes(interval, steps)) + geom_line()
t + facet_wrap(~day_type)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Finally we can see that comparing the weekday and weekend plots that the weekday plot has a spike at aroung 8:30 am which could be explained by the fact that most people step out for work/school during this peak time hence more number of steps fot that time period. We can also see how number of steps are more uniform for weekends as expected as these are off days.










