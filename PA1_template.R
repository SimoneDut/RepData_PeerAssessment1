## Dependencies
library(dplyr)
library(tidyr)
library(lattice)

# Set constants
zippedFileName <- "activity.zip"
unzippedFileName <- "activity.csv"

## Unzipping the raw data file
unzip(zippedFileName)

## Read the dataset
activity <- read.csv(unzippedFileName, colClasses = c("integer", "factor", "factor"))

## Calculate the daily totals ignoring NA values
dailyTotal <- with(activity, tapply(steps, date, function(x) {sum(x, na.rm = TRUE)}))

## Plot the histogram of the total number of steps taken each day (without imputing)
par(mfrow = c(1, 1))
hist(dailyTotal, col = "green", main = "Total number of steps taken each day (without imputing)",
     ylim = c(0, 50), xlab = "Number of steps")

## Calculate the mean and median of the total number of steps taken per day and plot them
meanDaily <- mean(dailyTotal)
abline(v = meanDaily, col = "red", lwd = 2)
text(meanDaily - 650, 40, "Mean", col = "red", srt = 90)
medianDaily <- median(dailyTotal)
abline(v = medianDaily, col = "blue", lwd = 2)
text(medianDaily - 650, 48, "Median", col = "blue", srt = 90)

## Calculate the average number of steps taken for each 5-minute interval
intervalAverage <- with(activity, tapply(steps, interval, function(x) {mean(x, na.rm = TRUE)}))
interval <- as.integer(rownames(intervalAverage))
intervalAverage <- intervalAverage %>%
  data.frame() %>%
  mutate(interval = interval) %>%
  arrange(interval)
colnames(intervalAverage) <- c("average", "interval")

## Make a time series plot of the 5-minute interval vs the average number of steps taken
with(intervalAverage, plot(interval, average, type = "l", col = "blue",
                           main = "Average number of steps by 5-minute inteval (without imputing)",
                           xlab = "Interval", ylab = "Number of steps"))

## Calculate the interval that contains the maximum number of steps
maxInterval <- intervalAverage[which.max(intervalAverage$average), "interval"]

## Calculate and report the total number of missing values in the dataset
missingValues <- is.na(activity)
totalMissingValues <- apply(missingValues, 2, sum)
totalMissingSteps <- totalMissingValues[1]

## Explain the imputing strategy (average for that 5-minute interval, rounded)

## Create a new dataset with imputing
hours <- as.integer(intervalAverage$interval / 100)
minutes <- intervalAverage$interval - hours * 100
index <- as.integer(hours * 12 + minutes %/% 5 + 1)
activityImputed <- activity
activityImputed[missingValues[, "steps"], "steps"] <- as.integer(round(intervalAverage$average[index]))

## Calculate the daily totals (after imputing)
dailyTotalImputed <- with(activityImputed, tapply(steps, date, function(x) {sum(x, na.rm = TRUE)}))

## Plot the histogram of the total number of steps taken each day (after imputing)
hist(dailyTotalImputed, col = "green", main = "Total number of steps taken each day (after imputing)",
     ylim = c(0, 50), xlab = "Number of steps")

## Calculate the mean and median of the total number of steps taken per day and plot them (after imputing)
meanDailyImputed <- mean(dailyTotalImputed)
abline(v = meanDailyImputed, col = "red", lwd = 2)
text(meanDailyImputed - 650, 40, "Mean", col = "red", srt = 90)

medianDailyImputed <- median(dailyTotalImputed)
abline(v = medianDailyImputed, col = "blue", lwd = 2)
text(medianDailyImputed - 650, 48, "Median", col = "blue", srt = 90)

## Calculate the change in the mean and the median
deltaMean <- meanDailyImputed - meanDaily
deltaMedian <- medianDailyImputed - medianDaily

## Create a new factor variable indicating whether a given date is a weekday or weekend day
activityImputed <- activityImputed %>%
  mutate(factorWeekdayWeekend = factor(ifelse(weekdays(as.Date(activityImputed$date)) %in% c("Saturday",
                                                                                             "Sunday"),
                                              "weekend", "weekday")))

## Calculate the average number of steps taken for each 5-minute interval for each subset
intervalAverageImputed <- with(activityImputed,
                               tapply(steps, list(factor1 = factorWeekdayWeekend, factor2 = interval),
                                      function(x) {mean(x, na.rm = TRUE)}))
intervalAverageImputed <- data.frame(t(intervalAverageImputed))
intervalImputed <- as.integer(rownames(intervalAverageImputed))

intervalAverageImputed <- intervalAverageImputed %>%
  mutate(interval = intervalImputed) %>%
  gather(factorWeekdayWeekend, average, -interval) %>%
  mutate(factorWeekdayWeekend = as.factor(factorWeekdayWeekend)) %>%
  arrange(factorWeekdayWeekend, interval)

## Make a time series plot of the 5-minute interval vs the average number of steps taken by weekday/weekend
p <- xyplot(average ~ interval | factorWeekdayWeekend, data = intervalAverageImputed, type = "l",
            layout = c(1, 2), xlab = "Interval", ylab = "Number of steps")
print(p)