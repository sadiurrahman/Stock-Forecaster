setwd("/home/saienthan/College/SE/R/tests");
library(forecast)
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}



ms = read.csv("input.csv",header = TRUE, sep=",",row.names=NULL)
dates <- ms$Date
ms$Date <-as.Date(dates,"%d-%b-%y")
num_rows <- nrow(ms)
num_tests <- 50
v2 <- as.ts(ms$Value,start=c(2012,2),end=c(2015,1),frequency=1)
test_points <- sample(20:(num_rows-20), num_tests, replace=F)
d2 <- diff(v2)
d2 <- d2>0
d2 <- d2*2 -1 
k <- 5
cutoff <- 10
scores <- sapply(k:(length(d2)-k), FUN=function(i){
  score <- abs(mean(-d2[ i-1:k ], na.rm=T) + mean(d2[ i+0:k ], na.rm=T))
})
scores <- sapply(k:(length(v2)-k), FUN=function(i){
  left <- (v2[sapply(i-1:k, max, 1) ]<v2[i])*2-1
  right <- (v2[sapply(i+1:k, min, length(v2)) ]<v2[i])*2-1
  
  score <- abs(sum(left) + sum(right))
})
inflections <- (k:(length(v2)-k))[scores>=cutoff]
passed <- 0
op <- file("output.txt",open="wt")
sink(op)
cat("--------------------------------\n")
for(i in test_points){ 
  days <- sample(1:4, 1, replace=F)
  v <- window(v2,start=1,end=i)
  fit <- HoltWinters(v,gamma=FALSE)
  forecasted <- forecast(fit,days)
  means <- c(forecasted$mean)
  lower <- c(forecasted$lower)
  upper <- c(forecasted$upper)
  cat(lower[days],v2[i+days],upper[days],"\n")  
  if((v2[i+days]<upper[days])&&(v2[i+days]>lower[days])){
    passed <- passed+1
    cat("PASSED\n")
  }  
  else
  {
    cat("FAILED\n")
  }
  cat("--------------------------------\n")
}
cat(paste("Tests conducted:",length(test_points),"\n"))
cat(paste("Tests passed:",passed))
close(op)
jpeg(paste('Error Distribution.jpg',sep=''))
plotForecastErrors(forecasted$residuals)
dev.off()

