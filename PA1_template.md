# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



```r
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
```

Data for this project comes from http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

Download the data, and capture and report metadata



```r
zipFile <-"activity.zip"
dataFile <- "activity.csv"
dataInfoFile <- "dataSourceInformation.csv"

    #Lectures say to download data from the original source. The assigment says
    #to use the data in the repo. I am going with what the assignement says,
    #however, if one were to follow the lectures, uncomment the next line.
#download.file(url,zipFile)
zipInfo <- unzip(zipFile,list=TRUE)
    #Expecting a single csv file, abort if that is not the case
stopifnot(nrow(zipInfo)==1)
stopifnot(zipInfo[1,"Name"]==dataFile)
    #extract the data file, and confirm it is what we are expecting.
unzip(zipFile)
stopifnot(file.exists(dataFile))
    #Create record of data source. Again, good practice per lectures, but not
    #part of the assignement.
    #todo: structure this as rows instead of columns. Time permitting.
write.csv( data.frame(
                Source=url
              , Download_Date = as.POSIXlt(file.mtime(zipFile),"UTC")
              , File_Date = zipInfo[1,"Date"])
        , file = dataInfoFile
        , row.names = FALSE )
print(read.csv( dataInfoFile ))
```

```
##                                                               Source
## 1 http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
##         Download_Date           File_Date
## 1 2015-09-12 20:52:34 2014-02-11 10:08:00
```

```r
activity <- read.csv(dataFile)
```


## What is mean total number of steps taken per day?



```r
steps<-aggregate(steps ~ date, activity, sum)[,2]
```

Steps Per Day

- Mean:   10766.19  
- Median:  10765




```r
hist(steps,breaks=10,main="Histogram of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
