## plot 2
## plot  global active power against Time from datafile "household_power_consumption.txt"
## data is cached in order to speed up things 
## usage:
## > setwd("TheLocationOfDataAndRfiles")
## > source("plot2.R")
## > x <- cacheTheData() #only if you didn't already
## > plot2(x)
##
plot2 <- function(x){
    
    hhpcFeb07 <- x$get()
    if(!is.null(hhpcFeb07)) {
        message("getting cached plot data")
        #return(hhpc)
    }else{ 
        #read all the pretty data... 
        #        setwd("/Users/edwin/Desktop/DataScience/04_ExploratoryDataAnalysis/Assignment1")
        hhpc <- read.table("household_power_consumption.txt",header=TRUE,sep=";",na.strings ="?")
        hhpc$Date <- as.Date(hhpc$Date, "%d/%m/%Y")
        day1 <- subset(hhpc, Date=="2007-02-01", rm.na=TRUE)
        day2 <- subset(hhpc, Date=="2007-02-02", rm.na=TRUE)
        hhpcFeb07 <- rbind(day1,day2)
        ## add column with merged date & time for further plotting
        hhpcFeb07$Fulldate <- strptime(paste(hhpcFeb07$Date,hhpcFeb07$Time),"%Y-%m-%d %H:%M:%S")
        x$set(hhpcFeb07)
        #clean up
        rm(day1)
        rm(day2)
    }
    
# plot global active power against Time
    postick <- c(as.POSIXlt("2007-02-01 00:01:00"),as.POSIXlt("2007-02-02 00:01:00"),as.POSIXlt("2007-02-02 23:59:00"))
    postickNum <- as.numeric(postick)
    posttickn <- c("Thu","Fri","Sat")
    
    par(mfrow = c(1,1))
    par(mar=c(3,5,2,2))
    plot(hhpcFeb07$Global_active_power ~ as.numeric(hhpcFeb07$Fulldate), type="l",xlab="", ylab="Global Active Power (kilowatts)",xaxt="n")
    axis(side =1,at=postickNum,labels=posttickn)
    #save as png
    dev.copy(png,file="plot2.png")
    dev.off()

}

cacheTheData <- function() {
    hhpcFeb07 <- NULL
    get <- function() hhpcFeb07
    set <- function(data) hhpcFeb07 <<- data
    list(get = get, set=set)
}