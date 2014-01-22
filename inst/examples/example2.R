
#
#
#
# author Emanuele Cordano
#
# file: example2.R
#

library(zoo)
 
set.seed(1223)
tz="A"
start <- as.POSIXlt("2003-08-01 UTC",tz=tz)
end <- as.POSIXlt("2003-08-10 UTC",tz=tz)
time <- seq(from=start,to=end,by=24*3600)
 
 
x1 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x2 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x3 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))




names(x1) <- sprintf("APPLE%02d",1:ncol(x1))
names(x2) <- sprintf("PEAR%02d",1:ncol(x1))
names(x3) <- sprintf("APRICOT%02d",1:ncol(x1))
 
x1 <- as.zoo(x1)
x2 <- as.zoo(x2)
x3 <- as.zoo(x3)
 
index(x1) <- time
index(x2) <- time
index(x3) <- time+24*3600
 
zm <- merge(x1,x2,x3)

 
 