#set working directory

setwd("/Users/nat/Desktop/Coursera-Data Science/5_Reproducible Research/Peer1/")
getwd()

#download zip file

url<- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
tempFile<- tempfile()
download.file(url,destfile=tempFile)
file<- unzip(tempFile)
data<- read.csv(file, colClasses=c("integer", "Date","integer"),na.strings="NA")

#mean steps per day

stepsPday <- aggregate(steps~date,data,sum)
names(stepsPday) <- c("date","total steps")
hist(stepsPday[,2],xlab="Steps per day",ylab="Frequency",
     main="Histogram of steps per day")
mean<- mean(stepsPday[,2],na.rm=TRUE)
median<- median(stepsPday[,2],na.rm=TRUE)
paste0 ("The mean number of steps per day is ",mean, " and the median
        number of steps per day is ",median," where NA values are ignored")

#average daily activity pattern

daypattern<- aggregate(steps~interval,data,FUN="mean",rm.na=TRUE)
plot(daypattern$interval,daypattern$steps,type="l",xlab="Interval in day",
     ylab="Average steps taken",main="Average number of steps taken
     in 5 minute intervals throughout day across dataset")
max<- data[which.max(daypattern$steps),]
max<- max[1,3]
summary(daypattern)
paste0("The 5-minute interval with the greatest number of steps is: ",max)

#how many missing values
NAs<- sum(is.na(data$steps))
NAs
paste0("There are ",NAs," NA step values in the data")

#Imputing missing values
data[,4]<- paste0(data[,1])
names(data)<- c("steps","date","interval","newSteps")

#average steps in one day

#replace missing steps values in data with the corresponding interval value
for (i in 1:nrow(data))
{
       if(is.na(data[i,1]) )
        {    
             data[i,4]<-daypattern[daypattern[,1]== data[i,3] ,2]
             
       }
}


#histogram of steps taken with  missing values
hist(stepsPday[,2],xlab="Steps per day",ylab="Frequency",
     main="Histogram of steps per day")

#histogram of steps taken with replacements for missing values
library(stats)
data$newSteps<-as.numeric(data$newSteps)
stepsPDnoNA<- aggregate(newSteps~date,data,sum)
hist(stepsPDnoNA[,2],main="Total steps taken each day",xlab="total steps taken
     with missing values replaced by value for that interval",
     ylab="Frequency")
mean<- mean(stepsPDnoNA[,2])
mean
median<- median(stepsPDnoNA[,2])
median
paste0 ("The mean number of steps per day is ",mean, " and the median number of steps per day is ",median," where NA values are replaced by average values for that interval")

#weekdays

no<- "weekday"
yes<-"weekend"
for (i in 1:nrow(data))
{
   data$weekday<- weekdays(data[,2]) 
   data$WE<- ifelse(data$weekday=="Saturday"|data$weekday=="Sunday",
                     yes,no)
}
table(data$WE)
data$WE<-as.factor(data$WE)

library (lattice)
daypattern2<- aggregate(steps~interval+WE,data,FUN="mean")
xyplot(steps~interval|WE,daypattern2,type="l",ylab="Number of steps",xlab="Interval")
