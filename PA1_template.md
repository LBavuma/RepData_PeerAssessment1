---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


``` r
# Load required libraries
library(ggplot2)
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

``` r
# Set global options
knitr::opts_chunk$set(echo = TRUE)
```


``` r
# Download and load the data if it doesn't exist
if (!file.exists("activity.csv")) {
    cat("Downloading activity data...\n")
    download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                  "activity.zip", method = "auto")
    unzip("activity.zip")
    file.remove("activity.zip")
    cat("Download complete!\n")
}

# Load the data
activity_data <- read.csv("activity.csv", header = TRUE)

# Process/transform the data
activity_data$date <- as.Date(activity_data$date)

# Display basic information about the dataset
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
head(activity_data)
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

``` r
summary(activity_data)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

## What is mean total number of steps taken per day?

For this part of the assignment, missing values in the dataset are ignored.


``` r
# Calculate the total number of steps taken per day
daily_steps <- aggregate(steps ~ date, activity_data, sum, na.rm = TRUE)

# Make a histogram of the total number of steps taken each day
hist(daily_steps$steps, 
     breaks = 20,
     main = "Histogram of Total Number of Steps Taken Each Day",
     xlab = "Total Steps per Day",
     ylab = "Frequency",
     col = "skyblue",
     border = "black")
```

![](PA1_template_files/figure-html/steps_per_day-1.png)<!-- -->

``` r
# Calculate and report the mean and median
mean_steps <- mean(daily_steps$steps, na.rm = TRUE)
median_steps <- median(daily_steps$steps, na.rm = TRUE)

cat("Mean total number of steps taken per day:", round(mean_steps, 2), "\n")
```

```
## Mean total number of steps taken per day: 10766.19
```

``` r
cat("Median total number of steps taken per day:", median_steps, "\n")
```

```
## Median total number of steps taken per day: 10765
```

The mean total number of steps taken per day is **10,766.19** and the median is **10,765**.

## What is the average daily activity pattern?


``` r
# Calculate average number of steps for each 5-minute interval
interval_avg <- aggregate(steps ~ interval, activity_data, mean, na.rm = TRUE)

# Make a time series plot
plot(interval_avg$interval, interval_avg$steps,
     type = "l",
     main = "Average Daily Activity Pattern",
     xlab = "5-minute Interval",
     ylab = "Average Number of Steps",
     col = "blue",
     lwd = 2)
```

![](PA1_template_files/figure-html/daily_pattern-1.png)<!-- -->

``` r
# Find the interval with maximum number of steps
max_interval <- interval_avg$interval[which.max(interval_avg$steps)]
max_steps <- max(interval_avg$steps)

cat("The 5-minute interval with the maximum average number of steps is:", max_interval, "\n")
```

```
## The 5-minute interval with the maximum average number of steps is: 835
```

``` r
cat("Maximum average steps:", round(max_steps, 2), "\n")
```

```
## Maximum average steps: 206.17
```

The **835** interval (8:35 AM) contains the maximum number of steps on average across all days, with **206.17** steps.

## Imputing missing values


``` r
# Calculate total number of missing values
total_nas <- sum(is.na(activity_data$steps))
cat("Total number of missing values in the dataset:", total_nas, "\n")
```

```
## Total number of missing values in the dataset: 2304
```

``` r
# Strategy: Use the mean for that 5-minute interval to fill missing values
# Create a new dataset with filled-in missing values
activity_filled <- activity_data

for (i in 1:nrow(activity_filled)) {
    if (is.na(activity_filled$steps[i])) {
        interval_value <- activity_filled$interval[i]
        activity_filled$steps[i] <- interval_avg$steps[interval_avg$interval == interval_value]
    }
}

# Verify no missing values remain
cat("Missing values after imputation:", sum(is.na(activity_filled$steps)), "\n")
```

```
## Missing values after imputation: 0
```

``` r
# Calculate new daily totals
daily_steps_filled <- aggregate(steps ~ date, activity_filled, sum)

# Make histogram of imputed data
hist(daily_steps_filled$steps, 
     breaks = 20,
     main = "Histogram of Total Steps per Day (After Imputation)",
     xlab = "Total Steps per Day",
     ylab = "Frequency",
     col = "lightgreen",
     border = "black")
```

![](PA1_template_files/figure-html/missing_values-1.png)<!-- -->

``` r
# Calculate new mean and median
mean_steps_filled <- mean(daily_steps_filled$steps)
median_steps_filled <- median(daily_steps_filled$steps)

cat("Mean total steps per day (after imputation):", round(mean_steps_filled, 2), "\n")
```

```
## Mean total steps per day (after imputation): 10766.19
```

``` r
cat("Median total steps per day (after imputation):", round(median_steps_filled, 2), "\n")
```

```
## Median total steps per day (after imputation): 10766.19
```

``` r
# Compare with original values
cat("\nComparison:\n")
```

```
## 
## Comparison:
```

``` r
cat("Original mean:", round(mean_steps, 2), "vs Imputed mean:", round(mean_steps_filled, 2), "\n")
```

```
## Original mean: 10766.19 vs Imputed mean: 10766.19
```

``` r
cat("Original median:", median_steps, "vs Imputed median:", round(median_steps_filled, 2), "\n")
```

```
## Original median: 10765 vs Imputed median: 10766.19
```

**Strategy for imputing missing values:** Missing values were replaced with the mean number of steps for the corresponding 5-minute interval across all days.

**Impact of imputing missing data:** 
- The mean remained the same at 10,766.19 steps per day
- The median increased slightly from 10,765 to 10,766.19 steps per day
- By using interval means for imputation, we maintained the overall daily activity patterns while filling in missing data

## Are there differences in activity patterns between weekdays and weekends?


``` r
# Create a new factor variable for weekday/weekend
activity_filled$day_type <- ifelse(weekdays(activity_filled$date) %in% c("Saturday", "Sunday"), 
                                  "weekend", "weekday")
activity_filled$day_type <- as.factor(activity_filled$day_type)

# Calculate average steps by interval and day type
interval_day_avg <- aggregate(steps ~ interval + day_type, activity_filled, mean)

# Create panel plot using base plotting system
par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))

# Weekday plot
weekday_data <- interval_day_avg[interval_day_avg$day_type == "weekday", ]
plot(weekday_data$interval, weekday_data$steps,
     type = "l",
     main = "weekday",
     xlab = "",
     ylab = "Number of steps",
     col = "blue",
     lwd = 2)

# Weekend plot
weekend_data <- interval_day_avg[interval_day_avg$day_type == "weekend", ]
plot(weekend_data$interval, weekend_data$steps,
     type = "l",
     main = "weekend",
     xlab = "Interval",
     ylab = "Number of steps",
     col = "blue",
     lwd = 2)
```

![](PA1_template_files/figure-html/weekday_weekend-1.png)<!-- -->

``` r
# Reset plotting parameters
par(mfrow = c(1, 1))

# Summary statistics by day type
weekday_avg <- mean(activity_filled$steps[activity_filled$day_type == "weekday"])
weekend_avg <- mean(activity_filled$steps[activity_filled$day_type == "weekend"])

cat("Average steps on weekdays:", round(weekday_avg, 2), "\n")
```

```
## Average steps on weekdays: 35.61
```

``` r
cat("Average steps on weekends:", round(weekend_avg, 2), "\n")
```

```
## Average steps on weekends: 42.37
```

**Differences between weekdays and weekends:**

1. **Weekdays** show a sharp peak in activity around 8:35 AM (likely commuting to work), followed by relatively lower activity during work hours (average: 35.61 steps per interval).

2. **Weekends** show more distributed activity throughout the day, with less pronounced morning peak but generally higher overall activity levels (average: 42.37 steps per interval).

3. Weekend activity patterns suggest more leisurely, sustained activity throughout the day compared to the concentrated morning activity on weekdays.

## Summary

This analysis of personal activity monitoring data reveals several key insights:

- The individual averaged **10,766 steps per day** over the two-month period
- Peak activity consistently occurs around **8:35 AM** with an average of **206 steps**
- **Weekend activity** is more evenly distributed and averages **19% higher** than weekdays
- The imputation strategy using interval means successfully filled **2,304 missing values** while preserving the underlying activity patterns
- Clear behavioral differences exist between weekday and weekend activity patterns, reflecting typical work-week vs. leisure-time behaviors
