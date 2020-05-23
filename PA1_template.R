# Reproducible Research Week 2 Project

# Loading and preprocessing the data
# Unzip archive and read data
library(ggplot2)
unzip("activity.zip")
baseData <- read.csv("activity.csv")

# Summarize Data
head(baseData)
dim(baseData)
summary(baseData)

#NA and Date String to Date Class modification
baseData$date <- as.Date(as.character(baseData$date))
baseDataNA <- is.na(baseData$steps)
cleanBase <- baseData[!baseDataNA,]


# What is mean total number of steps taken per day?
#1. Calculate the total number of steps taken per day
SummedDataByDay <- aggregate(baseData$steps, by=list(baseData$date), sum)
names(SummedDataByDay)[1] ="date"
names(SummedDataByDay)[2] ="totalsteps"
head(SummedDataByDay,15)

#2. Make a histogram of the total number of steps taken each day
ggplot(SummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

#3. Calculate and report the mean and median of the total number of steps taken per day
mean(SummedDataByDay$totalsteps,na.rm=TRUE)

median(SummedDataByDay$totalsteps,na.rm=TRUE)

# What is the average daily activity pattern?

#1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
nonNASubset <- baseData[!baseDataNA,]
MeanDataByInterval <- aggregate(nonNASubset$steps, by=list(nonNASubset$interval), mean)
# set the column names
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"

ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line(color="red") 

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval

# Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missingVals <- sum(baseDataNA)
missingVals

#2/3. Devise a strategy for filling in all of the missing values in the dataset.Create a new dataset that is equal to the original dataset but with the missing data filled in.
baseData2 <- baseData

NABase2 <- baseData2[is.na(baseData2$steps),]
cleanBase2 <- baseData2[!is.na(baseData2$steps),]

MeanData2ByInterval <- aggregate(cleanBase2$steps, by=list(cleanBase2$interval), sum)
names(MeanData2ByInterval)[1] ="interval"
names(MeanData2ByInterval)[2] ="steps"

baseData2 <- baseData
missingData <- is.na(baseData2$steps)
meanVals <- tapply(cleanBase$steps, cleanBase$interval, mean, na.rm=TRUE, simplify=TRUE)
baseData2$steps[missingData] <- meanVals[as.character(baseData2$interval[missingData])]

#4a. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.
FullSummedDataByDay <- aggregate(baseData2$steps, by=list(baseData2$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)

ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")

mean(FullSummedDataByDay$totalsteps)

median(FullSummedDataByDay$totalsteps)


#4b/4c. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

#Mean and median values are higher after imputing missing data. The reason is that in the 
#original data, there are some days with steps values NA for any interval. The total number 
#of steps taken in such days are set to 0s by default. However, after replacing missing steps
#values with the mean steps of associated interval value, these 0 values are removed from 
#the histogram of total number of steps taken each day.The effect of using mean data per 
#interval as a data impute method for missing values seems to push overall data towards the mean.

# Are there differences in activity patterns between weekdays and weekends?
baseData2$weekday <- weekdays(baseData2$date)
baseData2$weekend <- ifelse (baseData2$weekday == "Saturday" | baseData2$weekday == "Sunday", "Weekend", "Weekday")
#baseData2$weekend <- as.factor(baseData2$weekend)
head(baseData2,5)

MeanDataWeekendWeekday <- aggregate(baseData2$steps, by=list(baseData2$weekend, baseData2$interval), mean)
names(MeanDataWeekendWeekday)[1] ="weekend"
names(MeanDataWeekendWeekday)[2] ="interval"
names(MeanDataWeekendWeekday)[3] ="steps"

ggplot(MeanDataWeekendWeekday, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")

#There seems to be variation in the beginning of the day during weekdays, likely due to 
#workplace activities. There seems to be an overall slightly larger incidence of steps during the weekends.






