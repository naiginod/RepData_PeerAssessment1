Reproducible Research Project
=============================
        
        Data Loading and Preprocessing
------------------------------
        The dataset is found in a zip file downloaded from the following link: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
This contains a zip file titled "activity.csv" which is extracted.
The dataset will be loaded into variable "dataset".


```r
dataset <- read.csv("./activity.csv")
```

Mean and Median of the total steps taken during the day
--------------------------------------------------------
        
        First the "dataset" variable must be cleansed of omit's.  The dplyr package will be loaded in order to group the days together.  
Then mean and median of total steps per day taken will be output under column names avg_steps and median, respectively.
The ggplot2 package will be loaded for the histogram of total steps per day.

```r
library(dplyr)
library(ggplot2)
library(knitr)
library(lattice)
dataset2 <- na.omit(dataset)%>%
group_by(date)%>%
summarize(avg_steps = mean(steps), median = median(steps))
histdata <- na.omit(dataset)%>%
group_by(date)%>%
summarize(Total = sum(steps))
png(filename="./REP1.png", height=480, width=480, bg="white")
p <- ggplot(histdata, aes(x=date, y=Total, width=.5)) + geom_histogram(stat="identity")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
```

```
## pdf 
##   2
```

```r
return(dataset2)
```

```
## Source: local data frame [53 x 3]
## 
##          date  avg_steps median
## 1  2012-10-02  0.4375000      0
## 2  2012-10-03 39.4166667      0
## 3  2012-10-04 42.0694444      0
## 4  2012-10-05 46.1597222      0
## 5  2012-10-06 53.5416667      0
## 6  2012-10-07 38.2465278      0
## 7  2012-10-09 44.4826389      0
## 8  2012-10-10 34.3750000      0
## 9  2012-10-11 35.7777778      0
## 10 2012-10-12 60.3541667      0
## 11 2012-10-13 43.1458333      0
## 12 2012-10-14 52.4236111      0
## 13 2012-10-15 35.2048611      0
## 14 2012-10-16 52.3750000      0
## 15 2012-10-17 46.7083333      0
## 16 2012-10-18 34.9166667      0
## 17 2012-10-19 41.0729167      0
## 18 2012-10-20 36.0937500      0
## 19 2012-10-21 30.6284722      0
## 20 2012-10-22 46.7361111      0
## 21 2012-10-23 30.9652778      0
## 22 2012-10-24 29.0104167      0
## 23 2012-10-25  8.6527778      0
## 24 2012-10-26 23.5347222      0
## 25 2012-10-27 35.1354167      0
## 26 2012-10-28 39.7847222      0
## 27 2012-10-29 17.4236111      0
## 28 2012-10-30 34.0937500      0
## 29 2012-10-31 53.5208333      0
## 30 2012-11-02 36.8055556      0
## 31 2012-11-03 36.7048611      0
## 32 2012-11-05 36.2465278      0
## 33 2012-11-06 28.9375000      0
## 34 2012-11-07 44.7326389      0
## 35 2012-11-08 11.1770833      0
## 36 2012-11-11 43.7777778      0
## 37 2012-11-12 37.3784722      0
## 38 2012-11-13 25.4722222      0
## 39 2012-11-15  0.1423611      0
## 40 2012-11-16 18.8923611      0
## 41 2012-11-17 49.7881944      0
## 42 2012-11-18 52.4652778      0
## 43 2012-11-19 30.6979167      0
## 44 2012-11-20 15.5277778      0
## 45 2012-11-21 44.3993056      0
## 46 2012-11-22 70.9270833      0
## 47 2012-11-23 73.5902778      0
## 48 2012-11-24 50.2708333      0
## 49 2012-11-25 41.0902778      0
## 50 2012-11-26 38.7569444      0
## 51 2012-11-27 47.3819444      0
## 52 2012-11-28 35.3576389      0
## 53 2012-11-29 24.4687500      0
```

Average Daily Activity
----------------------

In order to graph the data of average steps per 5 minutes over the duration of the project, I must first group the dataset by the 
variable "interval".  This new dataset be saved as "intervaldf".


```r
library(dplyr)
intervaldf <- na.omit(dataset)%>%
group_by(interval) %>%
arrange(date, interval, steps)%>%
summarize(avg_steps=mean(steps))
intervaldf <- as.data.frame(intervaldf)
stepmax <- max(intervaldf$avg_steps)
intervalmax <- vector()
for(i in 1:nrow(intervaldf)){
if(intervaldf[i,2]==stepmax){
intervalmax <- intervaldf[i,1]
}
}
```
Here is the plot of the intervals (x-axis) in coordination with the average steps taken (y-axis):

```r
png(filename="./REP2.png", height=480, width=480, bg="white")
plot(intervaldf, type="l")
dev.off()
```

```
## pdf 
##   2
```

The interval with the highest step average is:

```r
return(intervalmax)
```

```
## [1] 835
```

Inputing Missing Values
-----------------------

The sum of the missing values totals to:

```r
df <- read.csv("./activity.csv")
sum(is.na(df))
```

```
## [1] 2304
```

There are many missing values titled "NA" in this dataset.  I have chosen to replace those values with the average step taken for that interval that was discovered above.  The new name for this data set is "df".


```r
for(j in 1:nrow(df)){
if(is.na(df[j,1])){
for(h in 1:nrow(intervaldf)){
if(df[j,3]==intervaldf[h,1]){
df[j,1] <- intervaldf[h,2]
}
}
}
}
```

Then a mean and median, by date, for this new dataset need to be procurred.  They will be saved in "df1".  I will create a dataset with a total steps per day "histdf".


```r
df1 <- group_by(df, date)%>%
summarize(avg_steps = mean(steps), median = median(steps))
histdf <-  group_by(df, date)%>%
summarize(Total = sum(steps))
ratingA <- data.frame(rating=rep("With NA",53))
histdata <- cbind(histdata,ratingA)
ratingB <- data.frame(rating=rep("Without NA",61))
histdf <- cbind(histdf,ratingB)
totaldf <- rbind(histdata,histdf)
png(filename="./REP3.png", height=480, width=480, bg="white")
q <- ggplot(totaldf, aes(x=date, y=Total, fill=rating, width=.5)) + geom_histogram(stat="identity",binwidth=.5)
q + theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
```

```
## pdf 
##   2
```

```r
return(df1)
```

```
## Source: local data frame [61 x 3]
## 
##          date  avg_steps   median
## 1  2012-10-01 37.3825996 34.11321
## 2  2012-10-02  0.4375000  0.00000
## 3  2012-10-03 39.4166667  0.00000
## 4  2012-10-04 42.0694444  0.00000
## 5  2012-10-05 46.1597222  0.00000
## 6  2012-10-06 53.5416667  0.00000
## 7  2012-10-07 38.2465278  0.00000
## 8  2012-10-08 37.3825996 34.11321
## 9  2012-10-09 44.4826389  0.00000
## 10 2012-10-10 34.3750000  0.00000
## 11 2012-10-11 35.7777778  0.00000
## 12 2012-10-12 60.3541667  0.00000
## 13 2012-10-13 43.1458333  0.00000
## 14 2012-10-14 52.4236111  0.00000
## 15 2012-10-15 35.2048611  0.00000
## 16 2012-10-16 52.3750000  0.00000
## 17 2012-10-17 46.7083333  0.00000
## 18 2012-10-18 34.9166667  0.00000
## 19 2012-10-19 41.0729167  0.00000
## 20 2012-10-20 36.0937500  0.00000
## 21 2012-10-21 30.6284722  0.00000
## 22 2012-10-22 46.7361111  0.00000
## 23 2012-10-23 30.9652778  0.00000
## 24 2012-10-24 29.0104167  0.00000
## 25 2012-10-25  8.6527778  0.00000
## 26 2012-10-26 23.5347222  0.00000
## 27 2012-10-27 35.1354167  0.00000
## 28 2012-10-28 39.7847222  0.00000
## 29 2012-10-29 17.4236111  0.00000
## 30 2012-10-30 34.0937500  0.00000
## 31 2012-10-31 53.5208333  0.00000
## 32 2012-11-01 37.3825996 34.11321
## 33 2012-11-02 36.8055556  0.00000
## 34 2012-11-03 36.7048611  0.00000
## 35 2012-11-04 37.3825996 34.11321
## 36 2012-11-05 36.2465278  0.00000
## 37 2012-11-06 28.9375000  0.00000
## 38 2012-11-07 44.7326389  0.00000
## 39 2012-11-08 11.1770833  0.00000
## 40 2012-11-09 37.3825996 34.11321
## 41 2012-11-10 37.3825996 34.11321
## 42 2012-11-11 43.7777778  0.00000
## 43 2012-11-12 37.3784722  0.00000
## 44 2012-11-13 25.4722222  0.00000
## 45 2012-11-14 37.3825996 34.11321
## 46 2012-11-15  0.1423611  0.00000
## 47 2012-11-16 18.8923611  0.00000
## 48 2012-11-17 49.7881944  0.00000
## 49 2012-11-18 52.4652778  0.00000
## 50 2012-11-19 30.6979167  0.00000
## 51 2012-11-20 15.5277778  0.00000
## 52 2012-11-21 44.3993056  0.00000
## 53 2012-11-22 70.9270833  0.00000
## 54 2012-11-23 73.5902778  0.00000
## 55 2012-11-24 50.2708333  0.00000
## 56 2012-11-25 41.0902778  0.00000
## 57 2012-11-26 38.7569444  0.00000
## 58 2012-11-27 47.3819444  0.00000
## 59 2012-11-28 35.3576389  0.00000
## 60 2012-11-29 24.4687500  0.00000
## 61 2012-11-30 37.3825996 34.11321
```

Activity Patterns between Weekdays and Weekends
===============================================
First the data frame must be separated between weekend and weekdays.  Then it is organized and divided by "weekday" and "weekend".  This helps to complete the graph which follows below.


```r
df$day <- weekdays(as.Date(df$date))


for(g in 1:nrow(df)){
if(df[g,4]=="Saturday"){
df[g,5] <- "Weekend"
}
else if(df[g,4]=="Sunday"){
df[g,5] <- "Weekend"                
}
else{
df[g,5] <- "Weekday"
}

}
names(df)[names(df)=="V5"] <- "Day_End"

DayEnddf <- group_by(df, Day_End, interval)%>%
summarize(avg_steps=mean(steps))%>%
arrange(Day_End, interval, avg_steps)
dedf <- as.data.frame(DayEnddf)
png(filename="./REP4.png", height=480, width=480, bg="white")
ggplot(dedf, aes(x=interval, y=avg_steps, color=Day_End, width=.5)) + geom_line(stat="identity",size=1)
dev.off()
```

```
## pdf 
##   2
```
