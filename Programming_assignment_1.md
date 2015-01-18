---
title: "Programming_assignment_1"
author: "Johan Slot"
date: "17 Jan 2015"
output: html_document
---
  
In this programming assignment we will analyze data taken from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

First, we will read the data from a csv file, called 'activity.csv', and print it's first 10 lines


```r
data_raw <- read.csv("activity.csv")
head(data_raw, n = 10)
```

```
##    steps       date interval
## 1     NA 2012-10-01        0
## 2     NA 2012-10-01        5
## 3     NA 2012-10-01       10
## 4     NA 2012-10-01       15
## 5     NA 2012-10-01       20
## 6     NA 2012-10-01       25
## 7     NA 2012-10-01       30
## 8     NA 2012-10-01       35
## 9     NA 2012-10-01       40
## 10    NA 2012-10-01       45
```

It is clear that this data frame **data_raw** contains missing values. These we ignore for now. It is interesting to study the total number of steps that this anonymous individual took on each day of this two-month period. Therefore, let's show a histogram of this total number of steps per day.


```r
total_steps_daily <- tapply(data_raw[,1], data_raw$date, sum)
hist(total_steps_daily, main = "Frequency of total number of steps per day", xlab = "number of daily steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

From this distribution of the total number of steps per day we can infer that it's **mean** and **median** are given by


```r
mean(total_steps_daily, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps_daily, na.rm = TRUE)
```

```
## [1] 10765
```

For the next task we will first remove all missing values in the number of steps from the data frame **data_raw**. The resulting data frame we will name **data_noNA**. 


```r
data_noNA <- data_raw[!is.na(data_raw$steps),]
```

In the following graph we plot the the average number of steps taken by this anonymous individual in each of the 5-min intervals in a day, averaged across all days in this two-month period. There are 288 of these 5-min intervals in a day. Each 5-min interval is represented by a unique number indicating the particular hour and the particular 5-min interval in that hour. The hours in one day are represented by the integers from 0 to 23 (24 hour notation) and the 5-min intervals in one hour by the minutes at which the interval begins, so 0, 5, 10, 15, ..., 50 and 55. So for instance, the 5-min interval 1125 refers to the interval between 11.25 and 11.30.


```r
ind_5min <- data_noNA$interval[1:288]
av_steps_5min <- tapply(data_noNA[,1], data_noNA$interval, mean)
plot(ind_5min, av_steps_5min, type = 'l', main = "Average number of steps in each 5-min interval in a day", xlab = "5-min interval", ylab = "average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

From this graph we can infer that the maximum average number of steps taken in a 5-min interval is given by


```r
sm <- max(av_steps_5min)
sm
```

```
## [1] 206.1698
```

and the particular 5-min interval in which this maximum was obtained is 


```r
ind_5min[match(sm, av_steps_5min)]
```

```
## [1] 835
```

i.e. between 8.35 and 8.40 in the morning.  

The difference between the data frames **data_raw** and **data_noNA** is that in the latter we have removed the missing values from the former. The total number of missing values in **data_raw** is therefore given by


```r
nrow(data_raw) - nrow(data_noNA)
```

```
## [1] 2304
```

We will now replace the missing values in the number of steps in the data frame **data_raw** by the average number of steps in the corresponding 5-minute interval. These average number of steps we have calculated before and are given by the array **av_steps_5min**. The resulting data frame we call **data_proc**


```r
data_proc <- data_raw
data_proc[is.na(data_proc[,1]),1] <- av_steps_5min[match(data_proc[is.na(data_proc[,1]),3], ind_5min)]
```

It is interesting to study the total number of steps each day after this replacement. Therefore, let's again show the histogram of this total number of steps per day.


```r
total_steps_daily <- tapply(data_proc[,1], data_proc$date, sum)
hist(total_steps_daily, main = "Frequency of total number of steps per day after replacement", xlab = "number of daily steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

The **mean** and **median** in this case are now given by


```r
mean(total_steps_daily, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(total_steps_daily, na.rm = TRUE)
```

```
## [1] 10766.19
```

It is clear that if we compare these last two values with the corresponding values we calculated earlier that there is no difference in **mean** and hardly any difference in **median**. So, it seems that imputing missing data has a small effect on the estimates of the total daily number of steps.

For the last part of this assigment we will convert the dates in the data frame **data_proc** to either **weekday** or **weekend**, depending on whether they refer to a day in the week or in the weekend. This factor variable **kind_of_day** we will append to this data frame as a fourth column. The data frame that we then obtain we will call **data_final**. The first 10 lines of this data frame are shown. We also introduce two auxiliairy data sets.


```r
days <- weekdays(as.Date(data_proc[,2]))
days[days == "Saturday"] <- "weekend"
days[days == "Sunday"] <- "weekend"
days[!(days == "weekend")] <- "weekday"
kind_of_day <- as.factor(days)
data_final <- cbind(data_proc, kind_of_day)
head(data_final, n = 10)
```

```
##        steps       date interval kind_of_day
## 1  1.7169811 2012-10-01        0     weekday
## 2  0.3396226 2012-10-01        5     weekday
## 3  0.1320755 2012-10-01       10     weekday
## 4  0.1509434 2012-10-01       15     weekday
## 5  0.0754717 2012-10-01       20     weekday
## 6  2.0943396 2012-10-01       25     weekday
## 7  0.5283019 2012-10-01       30     weekday
## 8  0.8679245 2012-10-01       35     weekday
## 9  0.0000000 2012-10-01       40     weekday
## 10 1.4716981 2012-10-01       45     weekday
```

```r
weekday_data <- data_final[data_final[,4] == "weekday",]
weekend_data <- data_final[data_final[,4] == "weekend",]
```

Finally, we show a panel plot in which we display the average number of steps in each of the 5-min intervals in a day, averaged across all **weekdays** and that same quantity averaged across all **weekend** days.


```r
par(mfrow = c(2,1))
av_steps_5min_1 <- tapply(weekday_data[,1], weekday_data$interval, mean)
av_steps_5min_2 <- tapply(weekend_data[,1], weekend_data$interval, mean)
plot(ind_5min, av_steps_5min_1, type = 'l', main = "Average number of steps in each 5-min interval in a weekday", xlab = "5-min interval", ylab = "average number of steps")
plot(ind_5min, av_steps_5min_2, type = 'l', main = "Average number of steps in each 5-min interval in a weekend day", xlab = "5-min interval", ylab = "average number of steps")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

