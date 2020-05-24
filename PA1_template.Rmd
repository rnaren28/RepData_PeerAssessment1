# Reproducible Research Week 2 Project

## Loading and preprocessing the data

### Unzip archive and read data
```{r loaddata}
library(ggplot2)
unzip("activity.zip")
baseData <- read.csv("activity.csv")
```

### Summarize Data
```{r summary}
head(baseData)

dim(baseData)

summary(baseData)
```


### NA and Date String to Date Class modification
```{r preprocess}
baseData$date <- as.Date(as.character(baseData$date))
baseDataNA <- is.na(baseData$steps)
cleanBase <- baseData[!baseDataNA,]
```

## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day
```{r steps}
SummedDataByDay <- aggregate(baseData$steps, by=list(baseData$date), sum)
names(SummedDataByDay)[1] ="date"
names(SummedDataByDay)[2] ="totalsteps"
```

### 2. Make a histogram of the total number of steps taken each day
```{r stepshistogram}
ggplot(SummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(binwidth=1000,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")+
  theme(plot.title = element_text(hjust = 0.5))
```

### 3. Calculate and report the mean and median of the total number of steps taken per day
```{r mean}
m1 = mean(SummedDataByDay$totalsteps,na.rm=TRUE)

sprintf("Mean steps taken per day is: %s", m1)
```

```{r median}
m2 = median(SummedDataByDay$totalsteps,na.rm=TRUE)

sprintf("Median steps taken per day is: %s", m2)
```

## What is the average daily activity pattern?

### 1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r pattern}
nonNASubset <- baseData[!baseDataNA,]
MeanDataByInterval <- aggregate(nonNASubset$steps, by=list(nonNASubset$interval), mean)
# set the column names
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"

ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "interval", y = "steps")+
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5))
```

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r maxinteral}
maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
m3 = maxInterval[1]
m4 = maxInterval[2]

sprintf("Interval with maximum number of stpes: %s", m3)
sprintf("This interval has: %s steps", m4)

```

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```{r nalist}
missingVals <- sum(baseDataNA)
sprintf("Total missing values: %s", missingVals)
```

### 2/3. Devise a strategy for filling in all of the missing values in the dataset.Create a new dataset that is equal to the original dataset but with the missing data filled in.

Using Mean to inpute missing values

```{r missingvalue}
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

```

### 4a. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```{r stepshistogramnew}
FullSummedDataByDay <- aggregate(baseData2$steps, by=list(baseData2$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"

ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(binwidth=1000,fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")+
    theme(plot.title = element_text(hjust = 0.5))

m5 = mean(FullSummedDataByDay$totalsteps)

sprintf("Mean steps taken per day is: %s", m5)

m6 = median(FullSummedDataByDay$totalsteps)

sprintf("Median steps taken per day is: %s", m6)

```

### 4b/4c. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with steps values NA for any interval. The total number of steps taken in such days are set to 0s by default. However, after replacing missing steps values with the mean steps of associated interval value, these 0 values are removed from the histogram of total number of steps taken each day.
The effect of using mean data per interval as a data impute method for missing values seems to push overall data towards the mean.

## Are there differences in activity patterns between weekdays and weekends?

```{r weekend}
baseData2$weekday <- weekdays(baseData2$date)
baseData2$weekend <- ifelse (baseData2$weekday == "Saturday" | baseData2$weekday == "Sunday", "Weekend", "Weekday")

MeanDataWeekendWeekday <- aggregate(baseData2$steps, by=list(baseData2$weekend, baseData2$interval), mean)
names(MeanDataWeekendWeekday)[1] ="weekend"
names(MeanDataWeekendWeekday)[2] ="interval"
names(MeanDataWeekendWeekday)[3] ="steps"

ggplot(MeanDataWeekendWeekday, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")+
    theme(plot.title = element_text(hjust = 0.5))


```

There seems to be variation in the beginning of the day during weekdays, likely due to workplace activities. There seems to be an overall slightly larger incidence of steps during the weekends.






