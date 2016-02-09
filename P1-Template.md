# Assignment

# Loading and preprocessing the data

```r
data<-read.csv("activity.csv")
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

# What is mean total number of steps taken per day?
###  Calculate the total number of steps taken per day
###  Make a histogram of the total number of steps taken each day
###  Calculate and Report mean and median of the total number of steps taken per day


```r
library(plyr)
totsteps = ddply(data,.(date), summarise, sumstep = sum(steps))
a=mean(totsteps$sumstep, na.rm = T)
b=median(totsteps$sumstep, na.rm = T)
cat("mean and median",a,b)
```

```
## mean and median 10766.19 10765
```

```r
hist(totsteps$sumstep,main = "Total steps", xlab = "day")
```

![](P1-Template_files/figure-html/unnamed-chunk-2-1.png) 

# What is the average daily activity pattern?
###  Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
###  Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
library(ggplot2)
timeseries <- tapply(data$steps, data$interval, mean, na.rm=T)
plot(row.names(timeseries),timeseries, type = "l", xlab = "time interval", ylab="steps")
```

![](P1-Template_files/figure-html/unnamed-chunk-3-1.png) 

```r
maxinterval<- names(which.max(timeseries))
cat( "The 5-minute interval contains the maximum number of steps is", maxinterval)
```

```
## The 5-minute interval contains the maximum number of steps is 835
```

# Imputing missing values
###  Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
###  Devise a strategy for filling in all of the missing values in the dataset. 
###  Create a new dataset that is equal to the original dataset but with the missing data filled in.
###  Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
na<-sum(is.na(data))
cat("the total number of rows with NAs", na)
```

```
## the total number of rows with NAs 2304
```

```r
data$steps[is.na(data$steps)] <- mean(data$steps, na.rm = T)
newdata<-data
newtimeseries <- tapply(newdata$steps, newdata$interval, sum, na.rm=T)
plot(row.names(newtimeseries),newtimeseries, type = "l", xlab = "time interval", ylab="steps")
```

![](P1-Template_files/figure-html/unnamed-chunk-4-1.png) 

```r
maxinterval<- names(which.max(newtimeseries))

newtotsteps = ddply(newdata,.(date), summarise, sumstep = sum(steps))
newa=mean(newtotsteps$sumstep, na.rm = T)
newb=median(newtotsteps$sumstep, na.rm = T)
cat("new mean and median:",newa,newb)
```

```
## new mean and median: 10766.19 10766.19
```

```r
a==newa
```

```
## [1] TRUE
```

```r
b==newb
```

```
## [1] FALSE
```

```r
hist(newtotsteps$sumstep, main = "Total steps", xlab = "day")
```

![](P1-Template_files/figure-html/unnamed-chunk-4-2.png) 

# Are there differences in activity patterns between weekdays and weekends?

### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
newdate <- weekdays(as.Date(newdata$date), abbreviate=T)
newdata$date <- newdate
newdata$day<-with(newdata, "Weekday")
newdata$day[newdata$date=="Sun"] = "Weekend"
newdata$day[newdata$date=="Sat"] = "Weekend"
weekdayseries<-tapply(newdata$steps[newdata$day=="Weekday"],newdata$interval[newdata$day=="Weekday"], mean)
weekendseries<-tapply(newdata$steps[newdata$day=="Weekend"],newdata$interval[newdata$day=="Weekend"],mean)
#dev.off()
par(mfrow=c(2,1))
par(mar=rep(2,4))
plot(row.names(weekdayseries),weekdayseries, type = "l", main = "weekdays")
plot(row.names(weekendseries),weekendseries, type = "l", main = "weekend")
```

![](P1-Template_files/figure-html/unnamed-chunk-5-1.png) 


