library(ggplot2)

## load data
if(!file.exists('activity.csv')) unzip('activity.zip')
monitorData <- read.csv('activity.csv')

## plot and get mean and median of total steps
totalSteps <- tapply(monitorData$steps, monitorData$date, sum, na.rm=TRUE)
plot <- qplot(totalSteps, xlab='Total number of steps per day', binwidth=500)
print(plot)
stepsMean <- mean(totalSteps)
stepsMedian <- median(totalSteps)

## plot step means per 5 min interval
averageStepsPerInterval <- aggregate(x=list(steps=monitorData$steps), by=list(interval=monitorData$interval), mean, na.rm=TRUE)
plot2 <- ggplot(averageStepsPerInterval, aes(x=interval, y=steps)) +
  geom_line() +
  xlab("Interval (5 min)") +
  ylab("Average number of steps") 
print(plot2)

## get 5-min max
max5min <- averageStepsPerInterval[which.max(averageStepsPerInterval$steps),]

## total No. entries with NA values
totalNaSteps <- sum(is.na(monitorData$steps))

## copy monitor data and fill missing values with mean
filledMonitorData <- monitorData 
for (i in 1:nrow(filledMonitorData)) {
  if (is.na(filledMonitorData$steps[i])) {
    filledMonitorData$steps[i] <- averageStepsPerInterval[which(filledMonitorData$interval[i]==averageStepsPerInterval$interval),]$steps
  }
}
filledTotalNaSteps <- sum(is.na(filledMonitorData$steps))

## replot total steps and calculate mean and median with new (filled) data set
totalStepsFilled <- tapply(filledMonitorData$steps, filledMonitorData$date, sum)
plot3 <- qplot(totalStepsFilled, binwidth=500, xlab="Total number of steps per day")
print(plot3)
stepsfilledMean <- mean(totalStepsFilled)
stepsfilledMedian <- median(totalStepsFilled)

## new factor with two levels: weekday & weekend
daysofweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
filledMonitorData$dayOfWeek <- factor((weekdays(as.Date(filledMonitorData$date)) %in% daysofweek), 
                                     levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

## plot average steps per interval with weekend/weekday facets
averageSteps <- aggregate(steps ~ interval + dayOfWeek, data=filledMonitorData, mean)
plot4 <- ggplot(filledMonitorData, aes(interval, steps)) + 
  geom_line() + 
  facet_grid(dayOfWeek ~ .) +
  xlab("Interval (5 min)") + 
  ylab("Number of steps (mean)")
print(plot4)

