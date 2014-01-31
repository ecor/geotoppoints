# file postprocessGeotop1dSimulation.R 
#
#
# This file contains a script which plots point value of pressure head or other variables calculated by a GEOtop simulation versus time 
#
#
# author: Emanuele Cordano on 19-12-2013
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.
#
#You should have received a copy of the GNU General Public License
#along with this program.  If not, see <http://www.gnu.org/licenses/>.

###############################################################################

rm(list=ls())
library(geotopbricks)
library(reshape2)
library(ggplot2)
library(RClimMAWGEN)
library(gridExtra)
library(geotoppoints)



wpath <-  '/media/GONGOLO/IDROCLIMA/simulazioni_geotop/Trentino_500_dstr_GEOtop_2_011'   #### '/Volumes/My\ Passport/Trentino_500_dstr_GEOtop_1_225_9_007/Trentino_500_dstr_GEOtop_1_225_9_007' ## ENTER GEOTOP SIMULATION PATH
ContinuousRecovery <- 10


## toponyms of the check pints 


points <- c("BONDONE-VIOTE","LAVARONE","SAVIGNANO","SAN MICHELE A.A.","MEZZOLOMBARDO")
points_meteoid <- c("T0327","T0082","T0147","SMICH","T0090") ## T0147 ROVERETO 

CoordinatePointX <- get.geotop.inpts.keyword.value("CoordinatePointX",wpath=wpath,numeric=TRUE)
CoordinatePointY <- get.geotop.inpts.keyword.value("CoordinatePointY",wpath=wpath,numeric=TRUE)
level <- 1:length(CoordinatePointX)

names(CoordinatePointX) <- points
names(CoordinatePointY) <- points
names(level) <- points

## Set time parameters

tz <- "A"
start <- as.POSIXct("1991-01-01 UTC",tz=tz)
end <- as.POSIXct("1998-01-01 UTC",tz=tz)
day <- 3600*24

## Get pressure head values 

psiliq <- get.geotop.inpts.keyword.value("SoilLiqWaterPressProfileFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",isNA = -9*10^6,ContinuousRecovery=ContinuousRecovery)
psiliq <- lapply(X=psiliq,FUN=function(x){
			out <- x[,str_detect(names(x),"X")]
			names(out) <- str_replace(names(out),"X","psi_z")
			return(out)
		})
names(psiliq) <- names(level)

## Get soil water content  values  "SoilLiqContentProfileFile"
thetaliq <- get.geotop.inpts.keyword.value("SoilLiqContentProfileFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",isNA = -9*10^6,ContinuousRecovery=ContinuousRecovery)
thetaliq <- lapply(X=thetaliq,FUN=function(x){
			out <- x[,str_detect(names(x),"X")]
			names(out) <- str_replace(names(out),"X","theta_z")
			return(out)
		})
names(thetaliq) <- names(level)



## Get other values referred to the "points"

pointvalues <- get.geotop.inpts.keyword.value("PointOutputFile",wpath=wpath,data.frame=TRUE,level=level,start_date=start,end_date=end,date_field="Date12.DDMMYYYYhhmm.",ContinuousRecovery=ContinuousRecovery)
names(pointvalues) <- names(level)





## To see the returned variables for each point
names(pointvalues[[1]])


## Get precipitation

id <- c("Prain_over_canopy.mm.","Prain_under_canopy.mm.","LE.W.m2.","Evap_surface.mm.","Trasp_canopy.mm.")

pointvalues_rev <- lapply(X=pointvalues,FUN=function(x,id){x[,id]},id=id)
### from W/m^2 to mm/hr 




pointvalues_rev <- lapply(X=pointvalues_rev,FUN=function(x) {
			
			id <- "LE.W.m2."
			new_id <- "EP.mm"
			latent_heat <- 2272*1000 # J/kg 
			water_density <- 1000    # kg/m^3
			print(id)
			str(x)
			val <- x[,id]/(latent_heat*water_density)*1000  ##  mm/s
		###	print(val)
		
			dt <- as.double(diff(index(x)),units="secs")
	#		print(dt)
			val <- c(dt[1],dt)*val
	#		print("val")
		
		
			names(x)[names(x)==id] <- new_id
			x[,new_id] <- val
			dt <- as.double(diff(index(x)),units="secs")
			return(x)
			
		})
#ET$value$value <- -ET$value$value/(latent_heat*water_density)*3600*1000*24
#####pointvalues_compr <- mergeZooList(psiliq,pointvalues_rev) 


time_aggregation <- 3600*24 # daily aggragation

pointvalues_melt <- meltFromZooList(x=pointvalues_rev,aggregate=time_aggregation,FUN=sum)
psiliq_melt <- meltFromZooList(x=psiliq,aggregate=time_aggregation,FUN=mean)
thetaliq_melt <- meltFromZooList(x=thetaliq,aggregate=time_aggregation,FUN=mean)
psithr <- -5000
psiliq_melt$value[psiliq_melt$value<psithr] <- psithr

pointvalues_melt <- rbind(pointvalues_melt,psiliq_melt,thetaliq_melt)


## GET OBSERVED PRECIPITETION trentino_1958_2010 dataset 

data(trentino_1958_2010)
when <- seq(from=start,to=end,by=day,tz=tz)
PRECIPITATION$Date <- as.POSIXct(paste(PRECIPITATION$year,PRECIPITATION$month,PRECIPITATION$day,sep="-"),tz=tz)
names(LOCATION) <- STATION_NAMES 
PRECIPITATION <- PRECIPITATION[PRECIPITATION$Date %in% when,c("Date",points_meteoid)]
names(PRECIPITATION) <- c("Time",points)
PRECIPITATION_melt <- melt(PRECIPITATION,id="Time")
PRECIPITATION_melt$L1 <- PRECIPITATION_melt$variable
PRECIPITATION_melt$variable <- as.factor("prec_RClimMAWGEN.mm")
 

## 

cond <- names(PRECIPITATION_melt)==names(pointvalues_melt)
cond <- length(which(cond))>0

if (cond)  {
	
	
	print("print merging GEOtop output with RClimMWGEN trentino_1958_2010 dataset")
	pointvalues_melt <- rbind(pointvalues_melt,PRECIPITATION_melt)
	
	
}

variables <- unique(pointvalues_melt$variable)
locations <- unique(pointvalues_melt$L1)

#[1] Prain_over_canopy.mm.  Prain_under_canopy.mm. EP.mm                 
#[4] psi_z15.000000         psi_z80.000000         psi_z230.000000       
#[7] psi_z580.000000        psi_z1430.000000       psi_z3530.000000      
#[10] psi_z7530.000000       psi_z16030.000000      psi_z34530.000000     
#[13] prec_RClimMAWGEN.mm   
#
#[1] Prain_over_canopy.mm.  Prain_under_canopy.mm. EP.mm                 
#[4] psi_z15.000000         psi_z80.000000         psi_z230.000000       
#[7] psi_z580.000000        psi_z1430.000000       psi_z3530.000000      
#[10] psi_z7530.000000       psi_z16030.000000      psi_z34530.000000     
#[13] prec_RClimMAWGEN.mm   
#
variable <- c("Evap_surface.mm.","Trasp_canopy.mm.","EP.mm")
#variable <- c("psi_z580.000000","psi_z1430.000000","psi_z3530.000000","psi_z16030.000000")
#variable <- c("theta_z580.000000","theta_z1430.000000","theta_z3530.000000","theta_z16030.000000")
##variable <- c("Prain_over_canopy.mm.","Prain_under_canopy.mm.","EP.mm")
location <- locations

#

pointvalues_plot <- pointvalues_melt[(pointvalues_melt$variable %in% variable),]

qp_EP <- qplot(Time, value, data = pointvalues_plot, geom = "line", group = variable) +
		facet_grid(variable ~ L1, scale = "free_y")
## scale = "free_y"
print(qp_EP)


### THS <-  5000
### test <- pointvalues_melt[(pointvalues_melt$variable %in% c("psi_z580.000000")),]
### test <- test[test$L1=="MEZZOLOMBARDO",]
### test <- test[test$value>THS,]
