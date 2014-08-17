# Reproducible Research: Peer Assessment 1
library(plyr)
library(lubridate)


## Loading and preprocessing the data
activity.df <- read.csv(file="activity.csv",colClasses=c("numeric","character","numeric"))
activity.df$Date <- strptime(activity.df$date,"%Y-%m-%d")
activity.df$Minnutes <- (activity.df$interval%/%100)*60 + activity.df$interval%%100
activity.df$Daytype <- ifelse(lubridate::wday(activity.df$Date, label=TRUE) %in% c("Sun","Sat"),"Weekend", 
                              ifelse(lubridate::wday(activity.df$Date, label=TRUE) %in% c("Mon","Tues","Wed","Thurs","Fri"),
                                     "Weekday",NA))

activity_day.df <- ddply(activity.df, .(date), summarise,
                         sum_steps = sum(steps, na.rm = TRUE))
activity_minnutes.df <- ddply(activity.df, .(Minnutes), summarize,
                              mean_steps = mean(steps, na.rm = TRUE))
activity_minnutes_day_type.df <- ddply(activity.df, .(Minnutes,Daytype), summarize,
                                       mean_steps = mean(steps, na.rm = TRUE))



## What is mean total number of steps taken per day?
par(mfrow=c(1,1))
hist(activity_day.df$sum_steps)


mean(activity_day.df$sum_steps)
median(activity_day.df$sum_steps)


## What is the average daily activity pattern?
#rint(activity_minnutes.df)
par(mfrow=c(1,1))
plot(activity_minnutes.df$Minnutes/60,activity_minnutes.df$mean_steps,type="l",ylab="Number of steps",xlab="Hour of day - 5 minnutes interval",
     xaxp = c(2,22,10))



## Imputing missing values

nrow(activity.df)
nrow(subset(activity.df,is.na(steps)))


steps_impute <- function(pSteps, pDaytype, pMinnutes) {
  
  if (pSteps == -1) {
    ret_val <- round(subset(activity_minnutes_day_type.df,Daytype==pDaytype & Minnutes==pMinnutes)$mean_steps)
  }
  else {
    ret_val <- pSteps
  }
  ret_val
}

for(i in 1:nrow(activity.df)) {
  activity.df[i,]$steps <- steps_impute(ifelse(is.na(activity.df[i,]$steps),-1,activity.df[i,]$steps),activity.df[i,]$Daytype,activity.df[i,]$Minnutes)
}

nrow(subset(activity.df,is.na(steps)))


### How do data look now on daily afctivity


new_activity_day.df <- ddply(activity.df, .(date), summarise,
                             sum_steps = sum(steps, na.rm = TRUE))


par(mfrow=c(1,1))
hist(new_activity_day.df$sum_steps)


mean(new_activity_day.df$sum_steps)
median(new_activity_day.df$sum_steps)


### And the daily activity pattern?


new_activity_minnutes.df <- ddply(activity.df, .(Minnutes), summarize,
                                  mean_steps = mean(steps, na.rm = TRUE))



par(mfrow=c(1,1))
plot(new_activity_minnutes.df$Minnutes/60,new_activity_minnutes.df$mean_steps,type="l",ylab="Number of steps",xlab="Hour of day - 5 minnutes interval",
     xaxp = c(2,22,10))



## Are there differences in activity patterns between weekdays and weekends?



new_activity_minnutes_day_type.df <- ddply(activity.df, .(Minnutes,Daytype), summarize,
                                           mean_steps = mean(steps, na.rm = TRUE))

print(new_activity_minnutes_day_type.df)

new_activity_minnutes_weekday.df <- subset(new_activity_minnutes_day_type.df,Daytype=="Weekday")
new_activity_minnutes_weekend.df <- subset(new_activity_minnutes_day_type.df,Daytype=="Weekend")

print(new_activity_minnutes_weekday.df)

print(new_activity_minnutes_weekend.df)


par(mfrow=c(2,1))
plot(new_activity_minnutes_weekend.df$Minnutes/60,new_activity_minnutes_weekend.df$mean_steps,type="l",ylab="Number of steps",xlab="Weekends - Hour of day - 5 minnutes interval",
     xaxp = c(2,22,10))

plot(new_activity_minnutes_weekday.df$Minnutes/60,new_activity_minnutes_weekday.df$mean_steps,type="l",ylab="Number of steps",xlab="Weekdays - Hour of day - 5 minnutes interval",
     xaxp = c(2,22,10))

