
library(reshape2)
library(zoo)
library(geotoppoints)

set.seed(1223)
tz="A"
start <- as.POSIXlt("2003-02-01 UTC",tz=tz)
end <- as.POSIXlt("2006-02-28 UTC",tz=tz)
time <- seq(from=start,to=end,by=3600)

x1 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x2 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))

names(x1) <- sprintf("VAR%02d",1:ncol(x1))
names(x2) <- sprintf("VAR%02d",1:ncol(x1))

x1 <- as.zoo(x1)
x2 <- as.zoo(x2)


index(x1) <- time
index(x2) <- time

x <- list(x1,x2)

str(x)

outA <- meltFromZooList(x,aggregate=3600*24,FUN=mean)
outB <- meltFromZooList(x,aggregate=3600*24,FUN=max) 
 