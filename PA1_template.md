## Loading and preprocessing the data
```{r, echo=TRUE}
activity = read.csv("~/Desktop/data/activity.csv")
```
## What is mean total number of steps taken per day?
```{r, echo=TRUE}
totalSteps <- aggregate(steps ~ date, activity, FUN=sum)
head(totalSteps)
hist(totalSteps$steps, main="Total Steps per Day", xlab="Number of Steps")
meanSteps <- mean(totalSteps$steps)
medianSteps <- median(totalSteps$steps)
meanSteps
medianSteps
```
## What is the average daily activity pattern?
```{r, echo=TRUE}
library(ggplot2)
meanStepsByInt <- aggregate(steps ~ interval, activity, mean)
ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) + geom_line() + ggtitle("Average Daily Activity Pattern") + xlab("5-minute Interval") + ylab("Average Number of Steps") 
meanStepsByInt <- aggregate(steps ~ interval, activity, mean)
maxInt <- meanStepsByInt[which.max(meanStepsByInt$steps),]
maxInt
```

## Imputing missing values
```{r, echo=TRUE}
missingVals <- is.na(activity$steps)
new_activity <- transform(activity, steps = ifelse(is.na(activity$steps), meanStepsByInt$steps[match(activity$interval, meanStepsByInt$interval)], activity$steps)) 
```
## What is mean total number of steps taken per day with new data?
```{r, echo=TRUE}
totalNumber <- aggregate(steps ~ date, new_activity, FUN=sum)
hist(totalNumber$steps, main = "Imputed Number of Steps per Day", xlab = "Number of Steps")
meanNumber <- mean(totalNumber$steps)
medianNumber <- median(totalNumber$steps)
meanNumber
medianNumber
```
## Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
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
ggplot(data = meanStepsByDay, aes(x=interval, y=steps)) +
	geom_line() +
	facet_grid(day ~.) +
	ggtitle("Average Daily Activity Pattern") +
	xlab("5-minute Interval") +
	ylab("Average Number of Steps") +
	theme(plot.title = element_text(hjust=0.5))

```

