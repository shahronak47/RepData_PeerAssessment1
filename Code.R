#Load the data (i.e. read.csv())
activity <- read.csv("E:/My Stuff/R/activity.csv", stringsAsFactors=FALSE)

#Process/transform the data (if necessary) into a format suitable for your analysis
#keeping only the data wihout NA
temp <- activity[!is.na(activity$steps),]

#Calculate the total number of steps taken per day
temp$date <- as.Date(temp$date)
#Splitting according to date
mydata <- split(temp$steps, temp$date)
steps_per_day <- sapply(mydata, sum)

# Make a histogram of the total number of steps taken each day

hist(steps_per_day)
#Calculate and report the mean and median of the total number of steps taken per day

mean(steps_per_day)
# [1] 10766.19

median(steps_per_day)
# [1] 10765

# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

mydata <- split(temp$steps, temp$interval)
#converting the interval field to factor
temp$interval<-substr(as.POSIXct(sprintf("%04.0f", temp$interval), format='%H%M'), 12, 16)
temp$interval <- as.factor(temp$interval)
y_axis <- lapply(mydata,mean)
plot(unique(temp$interval), y_axis, main = "Average steps per 5 - minute interval", 
     type = "l", xlab="time intervals", ylab="avg. no. of steps", xaxt =  "n")

axis(1,at=1:288, xlab=unique(temp$interval))

# Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
y_array <- unlist(y_axis)
which(y_array == max(y_array))
# 835
#Hence, the time interval 8:35 has the maximum number of steps 


#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

summary(activity)
#     steps            date              interval     
#Min.   :  0.00   Length:17568       Min.   :   0.0  
#1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
#Median :  0.00   Mode  :character   Median :1177.5  
#Mean   : 37.38                      Mean   :1177.5  
#3rd Qu.: 12.00                      3rd Qu.:1766.2  
#Max.   :806.00                      Max.   :2355.0  
#NA's   :2304                                    

#Hence, there are 2304 NA values 
#Devise a strategy for filling in all of the missing values in the dataset. 
#Replacing NA's with 0

activity[is.na(activity)] <-  0

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
summary(activity)
#    steps            date              interval     
#Min.   :  0.00   Length:17568       Min.   :   0.0  
#1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
#Median :  0.00   Mode  :character   Median :1177.5  
#Mean   : 32.48                      Mean   :1177.5  
#3rd Qu.:  0.00                      3rd Qu.:1766.2  
#Max.   :806.00                      Max.   :2355.0  
#No NA values

#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

mydata <- split(activity$steps, activity$date)
steps_per_day <- sapply(mydata, sum)
hist(steps_per_day)
mean(steps_per_day)
# [1] 9354.23

median(steps_per_day)
# [1] 10395

#Hence, we can see that as Na's are now replaced with 0's, there is lot of difference
#in mean and median

# Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity$date <- as.Date(activity$date)
activity[,4] <- NA
colnames(activity)[4] <- "day"
activity$day <- weekdays(activity$date)


activity[,5] <- NA
colnames(activity)[5] <- "type"

activity$type <- ifelse(activity$day %in% c("Saturday","Sunday") , "Weekend" , "Weekday")
activity$type <- as.factor(activity$type)

# Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

mydata <- split(activity, activity$type)
temp <- as.data.frame(mydata[1])
temp1 <- as.data.frame(mydata[2])
dates_weekday <- split(temp$Weekday.steps, temp$Weekday.interval)
dates_weekend <- split(temp1$Weekend.steps, temp1$Weekend.interval)


#dates_weekday$interval<-substr(as.POSIXct(sprintf("%04.0f", dates_weekday$interval), format='%H%M'), 12, 16)
#dates_weekday$interval <- as.factor(dates_weekday$interval)
y_axis <- lapply(dates_weekday,mean)
plot(unique(temp$Weekday.interval), y_axis, main = "Weekday", 
     type = "l", xlab="", ylab="avg. no. of steps", xaxt =  "n")


plot(unique(temp1$Weekend.interval), y_axis, main = "Weekend", 
     type = "l", xlab="", ylab="avg. no. of steps", xaxt =  "n")


