#
#
#

library(zoo)
library(geotoppoints)

set.seed(1223)
tz="A"
start <- as.POSIXlt("2003-08-01 UTC",tz=tz)
end <- as.POSIXlt("2003-08-10 UTC",tz=tz)
time <- seq(from=start,to=end,by=24*3600)
 
 
x1 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x2 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x3 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
 
x1a <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x2a <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x3a <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
 
 
names(x1) <- sprintf("APPLE%02d",1:ncol(x1))
names(x2) <- sprintf("PEAR%02d",1:ncol(x1))
names(x3) <- sprintf("APRICOT%02d",1:ncol(x1))
 
names(x1a) <- names(x1)
names(x2a) <- names(x2)
names(x3a) <- names(x3)
 
x1 <- as.zoo(x1)
x2 <- as.zoo(x2)
x3 <- as.zoo(x3)
 
x1a <- as.zoo(x1a)
x2a <- as.zoo(x2a)
x3a <- as.zoo(x3a)
 
 
index(x1) <- time
index(x2) <- time
index(x3) <- time+24*3600
#' 
index(x1a) <- time
index(x2a) <- time-24*3600
index(x3a) <- time
#' 
#' 
#' zm <- merge(x1,x2,x3)
#' 
l1 <- list(locA=x1,locB=x1a)
l2 <- list(locA=x2,locB=x2a)
l3 <- list(locA=x3,locB=x3a)
#' 
#' 
zl <- mergeZooList(l1,l2,l3)
#' 
#
