---
title: "Reproducible Research: Peer Assessment 1"
output: html_document
keep_md: true
editor_options: 
  chunk_output_type: console
---
```{r wd, echo=FALSE}
# set wd
setwd("C:/Users/desmo/OneDrive/HSA/Coursera/R/datasci_spec/mod5/week2")
```

## Loading and preprocessing the data
```{r load, echo=TRUE}
# unzip data
unzip("activity.zip")
stepsdt <- read.csv("activity.csv", header = TRUE)
head(stepsdt)
```

## What is the mean total number of steps taken per day?
```{r steps_day, echo=TRUE}
# packages
library(dplyr)

# summarise steps by date
steps_day <- stepsdt %>% select(date, steps) %>% group_by(date) %>% summarize(total = sum(steps)) %>% na.omit()

# plot histogram
hist(steps_day$total,
     xlab = "Steps",
     main = "Total Daily Steps",
     breaks = 20)

# calculate mean and median steps per day
mean(steps_day$total)
median(steps_day$total)
```

## What is the average daily activity pattern?
```{r activity_day, echo=TRUE}
# packages
library(ggplot2)

# summarise steps by interval
steps_int <- stepsdt %>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(total = mean(steps))

# plot data
ggplot(steps_int, aes(x = interval, y = total)) + geom_line()

# calculate 5-min interval that contains max steps
steps_int[which(steps_int$total == max(steps_int$total)),]
```

## Imputing missing values
```{r missing, echo=TRUE}
# packages
library(dplyr)

# number of missing values
missing <- sum(is.na(stepsdt))
missing

# use mean for 5-min interval to replace NA
replace_mean <- function(x) {
    replace(x, is.na(x), mean(x, na.rm = TRUE))
}
new_stepsdt <- stepsdt %>% group_by(interval) %>% mutate(steps = replace_mean(steps))

# check data
head(new_stepsdt)

# summarize new data by date, then plot histogram with new data
new_steps_day <- aggregate(new_stepsdt$steps, by = list(new_stepsdt$date), sum)
names(new_steps_day)[1] = "date"
names(new_steps_day)[2] = "total_steps"
head(new_steps_day)
hist(new_steps_day$total_steps, 
     xlab = "Steps", 
     ylab = "Frequency", 
     main = "Total Daily Steps",
     breaks = 20)

# compare mean and median of old and new data
# old mean
mean(steps_day$total, na.rm = TRUE)
# new mean
mean(new_steps_day$total_steps)
# old median
median(steps_day$total, na.rm = TRUE)
# new median
median(new_steps_day$total_steps)
```
### Imputing missing data did not affect the mean daily number of steps, however, imputing pushes the median of the data closer to the mean.


## Are there any differences in activity patterns between weekdays and weekends?
```{r week, echo=TRUE}
# packages
library(ggplot2)

# add weekday and weekend labels to data
new_stepsdt$date <- as.Date(new_stepsdt$date)
new_stepsdt$day <- weekdays(new_stepsdt$date)
new_stepsdt$type <- ifelse(new_stepsdt$day == "Saturday" | new_stepsdt$day == "Sunday",
                              "weekend", "weekday")
new_stepsdt$day <- as.factor(new_stepsdt$day)
new_stepsdt$type <- as.factor(new_stepsdt$type)

# plot weekday vs weekend data
dt_combined <- aggregate(new_stepsdt$steps, by = list(new_stepsdt$type, new_stepsdt$interval), na.omit(mean))
names(dt_combined) <- c("type", "interval", "steps")
ggplot(dt_combined, aes(x = interval, y = steps, color = type)) + 
    geom_line() +
    facet_grid(type ~.) +
    xlab("Interval") +
    ylab("Mean Number of Steps") +
    ggtitle("Comparison of Mean Number of Steps by Interval (Weekday vs Weekend)")
```

### There seems to be a larger variation of steps at the beginning of the day for weekdays. This could likely due to be activities such as getting to work. On the weekends, there are lesser fluctuation of steps throughout the day. Overall, there seems to be a larger number of steps over the weekend as comparedto weekdays.
