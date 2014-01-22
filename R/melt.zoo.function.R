NULL
#' melt 
#' 
#' Function \code{\link{melt}} applied to a list of \code{zoo} object returned by \code{\link{get.geotop.inpts.keyword.value}}.
#' 
#' @param x a \code{zoo} object 
#' @param aggregate aggregatiom time interval expressed in seconds
#' @param FUN function used for time aggregation. See \code{\link{aggregate.zoo}}.
#' 
#' 
#' @param na.rm logical value. If \code{TRUE} (default) \code{NA} vaues will be not considered
#' @param offset time interval offset from the start point used for time aggregation (expressed in seconds). 
#' @param tz time zone. Default iis \code{"A"}.
#' @param ... further arguments
#' 
#' 
#' 
#' @title meltFromZooList
#' 
#' @seealso \code{\link{melt}}
#' @export
#' @examples 
#' library(reshape2)
#' 
#' set.seed(1223)
#' tz="A"
#' start <- as.POSIXlt("2003-02-01 UTC",tz=tz)
#' end <- as.POSIXlt("2006-02-28 UTC",tz=tz)
#' time <- seq(from=start,to=end,by=3600)
#' 
#' 
#' x1 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
#' x2 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
#' 
#' names(x1) <- sprintf("VAR%02d",1:ncol(x1))
#' names(x2) <- sprintf("VAR%02d",1:ncol(x1))
#' 
#' x1 <- as.zoo(x1)
#' x2 <- as.zoo(x2)
#' 
#' 
#' index(x1) <- time
#' index(x2) <- time
#' 
#' x <- list(x1,x2)
#' 
#' str(x)
#' 
#' outA <- meltFromZooList(x,aggregate=3600*24,FUN=mean)
#' outB <- meltFromZooList(x,aggregate=3600*24,FUN=max) 
#' 
#' 




# x <- as.data.frame(array(rnorm(length(time)*2,c(length(time),2)))
# names(x) <- sprintf("VAR%02d",1:ncol(x))
# x <- as.zoo(x)
# index(x) <- time
# 
# str(x)
# 
# out1 <- meltFromZooList(x,aggregate=3600*24,FUN=mean)
# out2 <- meltFromZooList(x,aggregate=3600*24,FUN=max) 
# 
#'

#
#
#
#
#x1 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
#x2 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
#
#names(x1) <- sprintf("VAR%02d",1:ncol(x1))
#names(x2) <- sprintf("VAR%02d",1:ncol(x1))
#
#x1 <- as.zoo(x1)
#x2 <- as.zoo(x2)
#
#
#index(x1) <- time
#index(x2) <- time
#
#x <- list(x1,x2)
#
#str(x)
#
#outA <- meltFromZooList(x,aggregate=3600*24,FUN=mean)
#outB <- meltFromZooList(x,aggregate=3600*24,FUN=max) 


meltFromZooList <- function(x,aggregate=3600*24,FUN=mean,na.rm=TRUE,offset=0,tz="A",...) {
	
####	if (aggregate) 


	if (!is.list(x)) x <- list(x)
	
	if (is.null(names(x))) names(x) <- sprintf("X%04d",1:length(x))
				
				
	out <- lapply(X=x,FUN=function(x,aggr,fun,na.rm,offset) {
		
			out1 <- x
			if (offset>0) {
				
				index(x) <- index(x)-offset
				
			}
			
			time <- index(x)
		####	itime <- as.time(time-time[1],units="secs"
			
			if (!is.na(aggr) & !is.null(aggr)) {
				timesec <- as.double(time-time[1],units="secs")
				by <- timesec/aggr
				
				
				by <- trunc(by)
                ## correggere qui 			
				
				
				aggr.index <- time[1]+by*aggr
				
				#str(aggr.index)
				#str(x)
				
				out1 <- aggregate(x,by=aggr.index,FUN=fun,na.rm=na.rm)
				#str(out1)
				
				time1 <- index(out1)
			#	str(aggr.index)
				
	#			print(aggr)
	#			print(aggr.index[1:48])
	#			print(by[1:48])
				#str(time1)
				out1 <- as.data.frame(out1)
				out1$Time <- time1
				
			}
		return(out1)
	},aggr=aggregate,fun=FUN,na.rm=na.rm,offset=offset)
	

	out <- melt(out,id="Time",...)
	
###	out$Name <- names(x)[out$L1]
	
	
	
	return(out)
	
#	out <- lapply(X=out,FUN=function(x,tz){
#				
#				format_date <- "TIME %Y-%m-%d %H:%M:%S"
#				names <- as.character(index(x),format=format_date,tz=tz)
#				out <- as.data.frame(x)
#				row.names(out) <- names
#				return(out)
#			},tz=tz)
#	return(out)
	
}