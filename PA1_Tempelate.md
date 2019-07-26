R Markdown
----------

This is an R Markdown document for the peer project of the reproducible
research at JHU-Cousera course. \#\# Loading and preprocessing the data
This data will examine the relationship between three major variables

Explanatory Examination
-----------------------

    head(activities)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

    str(activities)

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

    summary(activities)

    ##      steps                date          interval     
    ##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
    ##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
    ##  Median :  0.00   2012-10-03:  288   Median :1177.5  
    ##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
    ##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
    ##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
    ##  NA's   :2304     (Other)   :15840

What is mean total number of steps taken per day?
-------------------------------------------------

Question 1: What is mean total number of steps taken per day?
=============================================================

    sum_up <- aggregate(activities$steps ~ activities$date, FUN=sum, )
    colnames(sum_up)<- c("Date", "Steps")
    hist(sum_up$Steps, breaks=5, xlab="Steps", main = "Graph1: Steps per Day")

![](PA1_Tempelate_files/figure-markdown_strict/unnamed-chunk-1-1.png)
Calculating the mean of the sum\_up as the integers

    as.integer(mean(sum_up$Steps))

    ## [1] 10766

calculating the median of the sum\_up as the integers

    as.integer(median(sum_up$Steps))

    ## [1] 10765

What is the average daily activity pattern?
-------------------------------------------

Question 2: What is the average daily activity pattern?
=======================================================

    library(ggplot2)
    library(plyr)
    sum_clean <- activities[!is.na(activities$steps),]
    sum_up2 <- aggregate(sum_clean$steps ~ sum_clean$interval, FUN = sum)
    intervalTable <- ddply(sum_clean, .(interval), summarize, Avg = mean(steps))
    p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
    p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Graph2: Average Number of Steps per Interval")

![](PA1_Tempelate_files/figure-markdown_strict/unnamed-chunk-4-1.png)

Maximum of the days' steps

    maxi <- max(intervalTable$Avg)
    maxi 

    ## [1] 206.1698

Interval that contain the maximum of steps

    intervalTable[intervalTable$Avg==max(intervalTable$Avg),1]

    ## [1] 835

Imputing missing values
-----------------------

Question 3: Imputing missing values
===================================

3.1 Compute number of the NA data

    a2<- nrow(activities[is.na(activities$steps), ])
    a2

    ## [1] 2304

3.2 Replace the NA with means of the day

    library(plyr)
    #replacing with mean

    act_2 <- activities
    act_2$steps[is.na(activities$steps)] = mean(sum_up$Steps/(12*24))

3.3 New dataset is the act\_2 3.4 Histogram & Summaries:

    act_sum <- aggregate(act_2$steps ~ act_2$date, FUN=sum, )
    hist(act_sum$`act_2$steps`, main = "Graph3: Total Number of Steps after Subsitituition")

![](PA1_Tempelate_files/figure-markdown_strict/unnamed-chunk-9-1.png)
Nothing changed after the substituition method of replacing the NAs by
the daily average steps.

3.4 Mean & Median:

    act_mean <- mean(act_sum$`act_2$steps`)
    # Mean of modified dataset
    act_mean

    ## [1] 10766.19

    act_median <- median(act_sum$`act_2$steps`)
    # Median of modified dataset
    act_median

    ## [1] 10766.19

Indeed, comparing with the result in the question 1, the difference is
in the expectation. Indeed, the substituition of the data via the mean
of the overall data divided into the interval reinforces the previous
result, especially the mean.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Question 4: Are there differences in activity patterns between weekdays and weekends?
=====================================================================================

4.1 Using weekdays to differentiate weekdays and weekends

    library(plyr)
    library(lubridate)

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:plyr':
    ## 
    ##     here

    ## The following object is masked from 'package:base':
    ## 
    ##     date

    activities$week <- weekdays(as_date(activities$date))
    activities$week[which(activities$week == 'Monday')] = 'weekday'
    activities$week[which(activities$week == 'Tuesday')] = 'weekday'
    activities$week[which(activities$week == 'Wednesday')] = 'weekday'
    activities$week[which(activities$week == 'Thursday')] = 'weekday'
    activities$week[which(activities$week == 'Friday')] = 'weekday'
    activities$week[which(activities$week == 'Saturday')] = 'weekend'
    activities$week[which(activities$week == 'Sunday')] = 'weekend'

    activities$steps[is.na(activities$steps)] = mean(sum_up$Steps/(12*24))

    intervalTable2 <- ddply(activities, .(interval,week), summarize, Avg = mean(steps))

    library(lattice)
    xyplot(Avg~interval | week, data = intervalTable2, type = 'l', layout = c(1,2), main="Graph4: Average Steps per Interval Based on Type of Day", ylab="Average Number of Steps", xlab="Interval" )

![](PA1_Tempelate_files/figure-markdown_strict/unnamed-chunk-11-1.png)
The steps of the individuals that make during the weekdays and weekends
are different, even through the pattern of weekend and weekdays are
similar. The possible reason is that, in the weekends, people have more
time to participate in the sport activities.
