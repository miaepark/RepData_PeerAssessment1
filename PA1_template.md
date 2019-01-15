---
title: "Course Project"
author: "Miae Park"
date: "January 15, 2019"
output: html_document
---

Loading and preprocessing the data
```{r, echo=TRUE}
activity = read.csv("~/Desktop/data/activity.csv")
```
What is mean total number of steps taken per day?
```{r}
totalSteps <- aggregate(steps ~ date, activity, FUN=sum)
head(totalSteps)
```
What is the average daily activity pattern?
```{r, echo=TRUE}
library(ggplot2)
meanStepsByInt <- aggregate(steps ~ interval, activity, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) + geom_line() + ggtitle("Average Daily Activity Pattern") + xlab("5-minute Interval") + ylab("Average Number of Steps") 
```

Imputing missing values
```{r}
missingVals <- is.na(activity$steps)
new_activity <- transform(activity, steps = ifelse(is.na(activity$steps), meanStepsByInt$steps[match(activity$interval, meanStepsByInt$interval)], activity$steps)) 
```
Are there differences in activity patterns between weekdays and weekends?
```{r}
DayType <- function(date) {
   day <- weekdays(date)
   if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday'))
       return ("weekeday")
   else if (day %in% c('Saturday', 'Sunday'))
       return ("weekend")
   else
       stop ("Invalid Date Format.")
 }
new_activity$date <- as.Date(new_activity$date)
new_activity$day <- sapply(new_activity$date, FUN = DayType)
meanStepsByDay <- aggregate(steps ~ interval + day, new_activity, mean) 

```
