x <- c(1,3,2,5)
x
x=c(1,6,2)
x
y=c(1,4,3)
y=c(1,4,3)
funcname
?funcname
length(x)
length(y)
x+y
1s()
ls()
rm(x,y)
ls()
rm(list-ls())
?matrix
x=matrix(data=c(1,2,3,4),nrow=2,ncol=2)
x
x=matrix
x=matrix(data-c(1,2,3,4),2,2)
x=matrix(c(1,2,3,4),2,2)
X
MATRIX (X(1,2,3,4),2,2,byrow=TRUE)
sqrt(x)
x^2
rnorn()
rnorm()
x=rnorm(50)
y=x+norm(50,mean=50, sd=.1)
cor(x,y)
1
set.seed(1303)
rnorm(50)
x=rnorm(100)
y=rnorm(100)
plotx,y
plot (x,y)
pdf("figure.pdf")
plot(x,y,col="green")
dev.off()
nulldevice
nulldevice1
x=seq(1,10)
x
x
x=seq(-pi,pi,length=50)
y=x
f=outer(x,y,function(x,y)cos(y)/(1+x^2))
contour(x,y,f)
A=matrix(1:16,4,4)
a
A
A[2,3]
dim(A)
q()
data=matrix(data=c(0,2,0,0,-1,1,3,0,1,1,0,1,0,0,3,2,1,1),6,3
q()
x=matrix(c(0,2,0,0,-1,1,3,0,1,1,0,1,0,0,3,2,1,1),6,3
x=matrix(c(0,2,0,0,-1,1,3,0,1,1,0,1,0,0,3,2,1,1),6,3
data=matrix(c(0,2,0,0,-1,1,3,0,1,1,0,1,0,0,3,2,1,1),6,3
q()
q()
data=matrix(data=c(0,2,0,0,-1,1,3,0,1,1,0,1,0,0,3,2,1,1) ,6 ,3)
data
sqrt(colSums(data)^2)# the L2 distance from (0,0,0)
data=matrix(data=c(0,2,0,0,-1,1,3,0,1,1,0,1,0,0,3,2,1,1) ,6 ,3,byrow=TRUE)
data
sqrt(data)
data^2
deuclid=distance matrix(data,d=euclid)
deuclid=distancematrix(data,d=euclid)
sqrt(data^2)# the L2 distance from ( 0,0,0)
sqrt(colSums(t( data) ^ 2) )# the L2 distnace from ( 0,0,0)
sqrt(colSums(t( data) ^ 2) )# the L2 distance from ( 0,0,0)
sqrt(rowSums(t( data) ^ 2) )# the L2 distance from ( 0,0,0)
q()
q()
q()
q()
read.csv("activity.csv")
data <- read.csv("activity.csv")
##mean total stpes taken per day
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)
gplot(total.steps, binwidth = 1000, xlab = "total number of steps taken each day")
install.packages(ggplot2)
install.packages
ggplot2
insall.packages("ggplot2")
install.package("ggplot2")
install.packages("ggplot2")
install.packages("swirl")
install.packages("dplyr")
om
q()
install.packages("chron")
library(ggplot2)
library(dplyr)
library(chron)
a <- read.csv("activity.csv"), header = TRUE)
read.csv("activity.csv")
a <- read.csv("actvity.csv", header = TRUE)
a <- read.csv("activity.csv", header = TRUE)
head(a)
##histogram - mean and median total steps per day
aggsteps <- aggregate (steps ~ date, a, FUN=sum)
head(aggsteps)
hist(aggsteps$steps, col="blue", xlab = "Frequency", ylab = "Steps", main = "Total Number of Steps Taken Each day")
##mean and median
amean <- mean(aggsteps$steps)
amedian <- median(aggsteps$steps)
amean
amedian
## average daily activity pattern
agginterval <- aggregate(steps ~ interval, a, FUN=sum)
plot(agginterval$interval, agginterval$steps, type = "1", lwd = 2, xlab = "
Interval", ylab = "total STeps", main = "Total STeps vs 5-Minute Interval")
plot(agginterval$interval, agginterval$steps, 
     type = "l", lwd = 2,
     xlab = "Interval", 
     ylab = "Total Steps",
     main = "Total Steps vs. 5-Minute Interval")
##which 5 min interval on ave across all days, has max number steps:
filter(agginterval, steps==max(steps))
##imputing missing data
## use mean
##calculate total number missing values
table(is.na(a))
##strategy to fill in missing data
meaninterval <- aggregate(steps ~ interval, a, FUN=mean)
anew <- merge(x=1, y=meaninterval, by="interval")
anew <- merge(x=a, y=meaninterval, by="interval")
anew$steps <- ifelse(is.na(anew$steps.x), anew$steps.y, anew$steps.x)
head(anew)
##create new dataset = original without missing data
anew <- select(anew, steps, date, interval)
head(anew)
##histogram of total number steps taken each day with mean and median.  do values differ?
aggsteps_new<- aggregate(steps ~ date, anew, FUN=sum)
par(mfrow=c(1,2))
hist(aggsteps_new$steps, 
     col="green",
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(After imputing NA values with \n mean of 5-min interval)",
     cex.main = 0.7)
##histogram of original dataset
hist(aggsteps$steps, 
     col="red", 
     xlab = "Steps", 
     ylab = "Frequency",
     ylim = c(0,35),
     main = "Total Number Of Steps Taken Each day \n(Orginal Dataset)",
)
par(mfrow=c(1,1))
amean_new <- mean(aggsteps_new$steps)
amedian_new <- median(aggsteps_new$steps)
paste("New Mean      :", round(amean_new,2), "," ,  
      " Original Mean :", round(amean,2),"," , 
      " Difference :",round(amean_new,2) -  round(amean,2))
paste("New Median    :", amedian_new, ",", 
      " Original Median :", amedian,"," , 
      " Difference :",round(amedian_new-amedian,2))
##ANSWER - means are the same; medians differ slightly
##differences in activity patterns between weekdays and weekends?
##create 2 level data "weekdays" and "weekends"
table(is.weekend(anew$date))
anew$dayofweek <- ifelse(is.weekend(anew$date), "weekend", "weekday")
table(anew$dayofweek)
head(anew)
##make a panel plot for weekends and weekdays
meaninterval_new<- aggregate(steps ~ interval + dayofweek, anew, FUN=mean)
head(meaninterval_new)
ggplot(meaninterval_new, aes(x=interval, y=steps)) + 
  geom_line(color="blue", size=1) + 
  facet_wrap(~dayofweek, nrow=2) +
  labs(x="\nInterval", y="\nNumber of steps")
source("C:\\Users\\ANSANN\\Documents\\R\\R-3.3.2\\PA1_templatev3.Rmd")
source("C:\\Users\\ANSANN\\Documents\\R\\R-3.3.2\\PA1_templatev3.r")
local({fn<-choose.files(filters=Filters[c('R','txt','All'),],index=4)
file.show(fn,header=fn,title='')})
load("C:\\Users\\ANSANN\\Desktop\\PA1_templatev3.RData")
save.image("C:\\Users\\ANSANN\\Desktop\\predictive analytics\\PA1_templatev3.RData")
> a <- read.csv("actvity.csv", header = TRUE)
Error in file(file, "rt") : cannot open the connection
In addition: Warning message:
In file(file, "rt") :
  cannot open file 'actvity.csv': No such file or directory
> a <- read.csv("activity.csv", header = TRUE)
> head(a)
  steps      date interval
1    NA 10/1/2012        0
2    NA 10/1/2012        5
3    NA 10/1/2012       10
4    NA 10/1/2012       15
5    NA 10/1/2012       20
6    NA 10/1/2012       25
> ##histogram - mean and median total steps per day
> aggsteps <- aggregate (steps ~ date, a, FUN=sum)
> head(aggsteps)
        date steps
1 10/10/2012  9900
2 10/11/2012 10304
3 10/12/2012 17382
4 10/13/2012 12426
5 10/14/2012 15098
6 10/15/2012 10139
> hist(aggsteps$steps, col="blue", xlab = "Frequency", ylab = "Steps", main = "Total Number of Steps Taken Each day")
> ##mean and median
> amean <- mean(aggsteps$steps)
> amedian <- median(aggsteps$steps)
> amean
[1] 10766.19
> amedian
[1] 10765
> ## average daily activity pattern
> agginterval <- aggregate(steps ~ interval, a, FUN=sum)
> plot(agginterval$interval, agginterval$steps, type = "1", lwd = 2, xlab = "
+ Interval", ylab = "total STeps", main = "Total STeps vs 5-Minute Interval")
Error in plot.xy(xy, type, ...) : invalid plot type '1'
> plot(agginterval$interval, agginterval$steps, 
+      type = "l", lwd = 2,
+      xlab = "Interval", 
+      ylab = "Total Steps",
+      main = "Total Steps vs. 5-Minute Interval")
> ##which 5 min interval on ave across all days, has max number steps:
> filter(agginterval, steps==max(steps))
  interval steps
1      835 10927
> ##imputing missing data
> ## use mean
> ##calculate total number missing values
> table(is.na(a))
FALSE  TRUE 
50400  2304 
> ##strategy to fill in missing data
> meaninterval <- aggregate(steps ~ interval, a, FUN=mean)
> anew <- merge(x=1, y=meaninterval, by="interval")
Error in fix.by(by.x, x) : 'by' must specify a uniquely valid column
> anew <- merge(x=a, y=meaninterval, by="interval")
> 
> anew$steps <- ifelse(is.na(anew$steps.x), anew$steps.y, anew$steps.x)
> head(anew)
  interval steps.x       date  steps.y    steps
1        0      NA  10/1/2012 1.716981 1.716981
2        0       0 11/23/2012 1.716981 0.000000
3        0       0 10/28/2012 1.716981 0.000000
4        0       0  11/6/2012 1.716981 0.000000
5        0       0 11/24/2012 1.716981 0.000000
6        0       0 11/15/2012 1.716981 0.000000
> ##create new dataset = original without missing data
> anew <- select(anew, steps, date, interval)
> head(anew)
     steps       date interval
1 1.716981  10/1/2012        0
2 0.000000 11/23/2012        0
3 0.000000 10/28/2012        0
4 0.000000  11/6/2012        0
5 0.000000 11/24/2012        0
6 0.000000 11/15/2012        0
> ##histogram of total number steps taken each day with mean and median.  do values differ?
> aggsteps_new<- aggregate(steps ~ date, anew, FUN=sum)
> 
> par(mfrow=c(1,2))
> hist(aggsteps_new$steps, 
+      col="green",
+      xlab = "Steps", 
+      ylab = "Frequency",
+      ylim = c(0,35),
+      main = "Total Number Of Steps Taken Each day \n(After imputing NA values with \n mean of 5-min interval)",
+      cex.main = 0.7)
> ##histogram of original dataset
> hist(aggsteps$steps, 
+      col="red", 
+      xlab = "Steps", 
+      ylab = "Frequency",
+      ylim = c(0,35),
+      main = "Total Number Of Steps Taken Each day \n(Orginal Dataset)",
+ 
+ )
> par(mfrow=c(1,1))
> amean_new <- mean(aggsteps_new$steps)
> amedian_new <- median(aggsteps_new$steps)
> paste("New Mean      :", round(amean_new,2), "," ,  
+       " Original Mean :", round(amean,2),"," , 
+       " Difference :",round(amean_new,2) -  round(amean,2))
[1] "New Mean      : 10766.19 ,  Original Mean : 10766.19 ,  Difference : 0"
> paste("New Median    :", amedian_new, ",", 
+       " Original Median :", amedian,"," , 
+       " Difference :",round(amedian_new-amedian,2))
[1] "New Median    : 10766.1886792453 ,  Original Median : 10765 ,  Difference : 1.19"
> ##ANSWER - means are the same; medians differ slightly
> ##differences in activity patterns between weekdays and weekends?
> ##create 2 level data "weekdays" and "weekends"
> table(is.weekend(anew$date))
FALSE  TRUE 
12960  4608 
> anew$dayofweek <- ifelse(is.weekend(anew$date), "weekend", "weekday")
> table(anew$dayofweek)
weekday weekend 
  12960    4608 
> head(anew)
     steps       date interval dayofweek
1 1.716981  10/1/2012        0   weekday
2 0.000000 11/23/2012        0   weekday
3 0.000000 10/28/2012        0   weekend
4 0.000000  11/6/2012        0   weekday
5 0.000000 11/24/2012        0   weekend
6 0.000000 11/15/2012        0   weekday
> ##make a panel plot for weekends and weekdays
> meaninterval_new<- aggregate(steps ~ interval + dayofweek, anew, FUN=mean)
> head(meaninterval_new)
  interval dayofweek      steps
1        0   weekday 2.25115304
2        5   weekday 0.44528302
3       10   weekday 0.17316562
4       15   weekday 0.19790356
5       20   weekday 0.09895178
6       25   weekday 1.59035639
> ggplot(meaninterval_new, aes(x=interval, y=steps)) + 
+   geom_line(color="blue", size=1) + 
+   facet_wrap(~dayofweek, nrow=2) +
+   labs(x="\nInterval", y="\nNumber of steps")
> source("C:\\Users\\ANSANN\\Documents\\R\\R-3.3.2\\PA1_templatev3.Rmd")
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: Warning message:
In file(filename, "r", encoding = encoding) :
  cannot open file 'C:\Users\ANSANN\Documents\R\R-3.3.2\PA1_templatev3.Rmd': No such file or directory
> source("C:\\Users\\ANSANN\\Documents\\R\\R-3.3.2\\PA1_templatev3.r")
Error in file(filename, "r", encoding = encoding) : 
  cannot open the connection
In addition: Warning message:
In file(filename, "r", encoding = encoding) :
  cannot open file 'C:\Users\ANSANN\Documents\R\R-3.3.2\PA1_templatev3.r': No such file or directory
> local({fn<-choose.files(filters=Filters[c('R','txt','All'),],index=4)
+ file.show(fn,header=fn,title='')})
> load("C:\\Users\\ANSANN\\Desktop\\PA1_templatev3.RData")
Error in readChar(con, 5L, useBytes = TRUE) : cannot open the connection
In addition: Warning message:
In readChar(con, 5L, useBytes = TRUE) :
  cannot open compressed file 'C:\Users\ANSANN\Desktop\PA1_templatev3.RData', probable reason 'No such file or directory'
> save.image("C:\\Users\\ANSANN\\Desktop\\predictive analytics\\PA1_templatev3.RData")
