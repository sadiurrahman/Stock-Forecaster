setwd("/home/saienthan/College/SE/R");
library(forecast)
ms = read.csv("Normalized.csv",header = TRUE, sep=",",row.names=NULL)
dates <- ms$Date
ms$Date <-as.Date(dates,"%d-%b-%y")
v1 <- as.ts(ms$Value,start=c(2012,2),end=c(2015,1),frequency=1)
#v2 <- as.ts(ms$Value,ms$Date)
v2 <- window(v1,start=1,end=300)
#v2 <- ms$Open
plot(v2, type="l", col="darkblue", lwd=2)
lines(v2, lwd=1, col="red")
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

plot(v2, type="l")
print(inflections)
fit <- HoltWinters(v2, gamma=FALSE)
plot(fit)
plot(inflections)
forecasted <- forecast(fit,3)
plot(forecasted)
abline(v=inflections, col="red", lwd=1)

