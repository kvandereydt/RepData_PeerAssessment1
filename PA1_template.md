---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv("/home/rstudio/Reproducible Research/week2/activity.csv")

head(data)
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


## What is mean total number of steps taken per day?

```r
# Load libraries
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
library(ggplot2)

# 1. Calculate the total number of steps taken each day
total_steps_per_day <- data %>% 
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
head(total_steps_per_day)
```

```
## # A tibble: 6 x 2
##   date       total_steps
##   <chr>            <int>
## 1 2012-10-01           0
## 2 2012-10-02         126
## 3 2012-10-03       11352
## 4 2012-10-04       12116
## 5 2012-10-05       13294
## 6 2012-10-06       15420
```

```r
# 2.Create a histogram of total steps per day
ggplot(total_steps_per_day, aes(x=date, y=total_steps)) +
  geom_bar(stat = "identity") +
  labs(title = "Histogram of total steps per day", 
       x = "Date",
       y = "Total steps") 
```

![](PA1_template_files/figure-html/steps-1.png)<!-- -->

```r
# 3. Calculate mean and median total steps per day
mean_total_steps <- mean(total_steps_per_day$total_steps)
mean_total_steps
```

```
## [1] 9354.23
```

```r
median_total_steps <- median(total_steps_per_day$total_steps)
median_total_steps
```

```
## [1] 10395
```

The mean number of steps per day is 9354.23. The median number of steps per day is 10395. 


## What is the average daily activity pattern?

```r
# 1. Calculate the average number of steps for each 5-minute interval
average_steps_per_interval <- data %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Create a time series plot
plot(average_steps_per_interval$interval,
     average_steps_per_interval$mean_steps,
     type = "l",
     col = "blue",
     lwd = 2,
     main = "Average Daily Activity Pattern",
     xlab = "5-Minute Interval",
     ylab = "Average Number of Steps")
```

![](PA1_template_files/figure-html/average daily activity pattern-1.png)<!-- -->

```r
# 2. Find the interval with the maximum average steps
max_interval <- average_steps_per_interval %>%
  filter(mean_steps == max(mean_steps))

max_interval
```

```
## # A tibble: 1 x 2
##   interval mean_steps
##      <int>      <dbl>
## 1      835       206.
```
Interval 835 has the highest average number of steps. 

## Imputing missing values

```r
# 1. Calculate number of missing values in the dataset
total_NA <- sum(is.na(data))
total_NA
```

```
## [1] 2304
```

```r
total_NA_steps <- sum(is.na(data$steps))
total_NA_steps
```

```
## [1] 2304
```

```r
total_NA_interval <- sum(is.na(data$interval))
total_NA_interval
```

```
## [1] 0
```

```r
# 2. Impute missing data
# Replace NA with the mean of the 5-min interval across all days
# Create a data frame of average steps per interval
average_steps_per_interval <- data %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# Merge the average steps with the original data
data_imputed <- data %>%
  left_join(average_steps_per_interval, by = "interval") %>%
  mutate(steps = ifelse(is.na(steps), mean_steps, steps)) %>%
  select(steps, date, interval)

# 3. New dataset
total_steps_per_day_imputed <- data_imputed %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
# 4. Make a histogram of total number of steps taken each day
ggplot(total_steps_per_day_imputed, aes(x=date, y=total_steps)) +
  geom_bar(stat = "identity") +
  labs(title = "Histogram of total steps per day (imputed data)",
       x = "Date",
       y = "Total steps") 
```

![](PA1_template_files/figure-html/missing values-1.png)<!-- -->

```r
# 5. Report the mean and median of total number of steps taken each day
# Calculate mean and median
mean_total_steps_imputed <- mean(total_steps_per_day_imputed$total_steps)
mean_total_steps_imputed
```

```
## [1] 10766.19
```

```r
median_total_steps_imputed <- median(total_steps_per_day_imputed$total_steps)
median_total_steps_imputed
```

```
## [1] 10766.19
```
Imputing data changes the mean and median values. 

# Are there differences in activity patterns between weekdays and weekends?

```r
# 1. Create new factor variable for weekday/weekend
data_imputed <- data_imputed %>%
  mutate(day_type = ifelse(weekdays(as.Date(date)) %in% c("Saturday", "Sunday"),
                           "weekend", "weekday")) %>%
  mutate(day_type = factor(day_type, levels = c("weekday", "weekend")))

# 2. Panel plot 
# Calculate the average steps for each interval and day type
average_steps_by_day_type <- data_imputed %>%
  group_by(interval, day_type) %>%
  summarise(mean_steps = mean(steps))
```

```
## `summarise()` regrouping output by 'interval' (override with `.groups` argument)
```

```r
# Plot
ggplot(average_steps_by_day_type, aes(x = interval, y = mean_steps)) +
  geom_line(color = "blue") +
  facet_wrap(~ day_type, ncol = 1, scales = "free_y") +
  labs(title = "Average Daily Activity Pattern by Day Type",
       x = "5-Minute Interval",
       y = "Average Number of Steps")
```

![](PA1_template_files/figure-html/weekdays vs weekends-1.png)<!-- -->



