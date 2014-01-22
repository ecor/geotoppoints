\name{meltFromZooList}
\alias{meltFromZooList}
\title{meltFromZooList}
\usage{
meltFromZooList(x, aggregate = 3600 * 24, FUN = mean, na.rm = TRUE,
  offset = 0, tz = "A", ...)
}
\arguments{
  \item{x}{a \code{zoo} object}

  \item{aggregate}{aggregatiom time interval expressed in
  seconds}

  \item{FUN}{function used for time aggregation. See
  \code{\link{aggregate.zoo}}.}

  \item{na.rm}{logical value. If \code{TRUE} (default)
  \code{NA} vaues will be not considered}

  \item{offset}{time interval offset from the start point
  used for time aggregation (expressed in seconds).}

  \item{tz}{time zone. Default iis \code{"A"}.}

  \item{...}{further arguments}
}
\description{
melt
}
\details{
Function \code{\link{melt}} applied to a list of \code{zoo}
object returned by
\code{\link{get.geotop.inpts.keyword.value}}.
}
\examples{
library(reshape2)

set.seed(1223)
tz="A"
start <- as.POSIXlt("2003-02-01 UTC",tz=tz)
end <- as.POSIXlt("2006-02-28 UTC",tz=tz)
time <- seq(from=start,to=end,by=3600)


x1 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))
x2 <- as.data.frame(array(rnorm(length(time)*2),c(length(time),2)))

names(x1) <- sprintf("VAR\%02d",1:ncol(x1))
names(x2) <- sprintf("VAR\%02d",1:ncol(x1))

x1 <- as.zoo(x1)
x2 <- as.zoo(x2)


index(x1) <- time
index(x2) <- time

x <- list(x1,x2)

str(x)

outA <- meltFromZooList(x,aggregate=3600*24,FUN=mean)
outB <- meltFromZooList(x,aggregate=3600*24,FUN=max)
}
\seealso{
\code{\link{melt}}
}

