## plot 3
## plot  energy sub metering 1-3 against Time from datafile "household_power_consumption.txt"
## data is cached in order to speed up things 
## usage:
## > setwd("TheLocationOfDataAndRfiles")
## > source("plot3.R")
## > x <- cacheTheData() #only if you didn't already
## > plot3(x)
##
plot3 <- function(x){
    
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
    
    #set xtick labels to weekdays in English (computer setting is German)
    postick <- c(as.POSIXlt("2007-02-01 00:01:00"),as.POSIXlt("2007-02-02 00:01:00"),as.POSIXlt("2007-02-02 23:59:00"))
    postickNum <- as.numeric(postick)
    posttickn <- c("Thu","Fri","Sat")
    
    # make the plot
    par(mfrow = c(1,1))
    par(mar=c(3,5,2,3))
    plot(hhpcFeb07$Sub_metering_1 ~ as.numeric(hhpcFeb07$Fulldate), type="l",xlab="", ylab="Energy sub metering",col="black",xaxt="n")
    lines(hhpcFeb07$Sub_metering_2 ~ as.numeric(hhpcFeb07$Fulldate), type="l",col="red")
    lines(hhpcFeb07$Sub_metering_3 ~ as.numeric(hhpcFeb07$Fulldate), type="l",col="blue")
    axis(side =1,at=postickNum,labels=posttickn)
    legend("topright", legend =colnames(hhpcFeb07[7:9]),lty = c(1,1,1),col=c("black","red","blue"))
    
    #save as png
    dev.copy(png,file="plot3.png")
    dev.off()

}

cacheTheData <- function() {
    hhpcFeb07 <- NULL
    get <- function() hhpcFeb07
    set <- function(data) hhpcFeb07 <<- data
    list(get = get, set=set)
}