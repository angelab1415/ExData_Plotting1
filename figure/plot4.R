## plot 4
## plot gap,voltage,esm and grp against time 4 plots in 1 png from datafile "household_power_consumption.txt"
## data is cached in order to speed up things 
## usage:
## > setwd("TheLocationOfDataAndRfiles")
## > source("plot4.R")
## > x <- cacheTheData() #only if you didn't already
## > plot4(x)
##
plot4 <- function(x){

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
    
    # plot 
    par(mfrow = c(2,2))
    par(mar=c(5,5,1,1))
    #top left
    plot(hhpcFeb07$Global_active_power ~ as.numeric(hhpcFeb07$Fulldate), type="l",xlab="", ylab="Global Active Power (kilowatts)",xaxt="n")
    axis(side =1,at=postickNum,labels=posttickn)
    #top right
    plot(hhpcFeb07$Voltage ~ as.numeric(hhpcFeb07$Fulldate), type="l",xlab="", ylab="Voltage",xaxt="n")
    axis(side =1,at=postickNum,labels=posttickn)
    #bottom left
    plot(hhpcFeb07$Sub_metering_1 ~ as.numeric(hhpcFeb07$Fulldate), type="l",xlab="datetime", ylab="Energy sub metering",col="black",xaxt="n")
    lines(hhpcFeb07$Sub_metering_2 ~ as.numeric(hhpcFeb07$Fulldate), type="l",col="red")
    lines(hhpcFeb07$Sub_metering_3 ~ as.numeric(hhpcFeb07$Fulldate), type="l",col="blue")
    axis(side =1,at=postickNum,labels=posttickn)
    legend("topright", legend =colnames(hhpcFeb07[7:9]),lty = c(1,1,1),col=c("black","red","blue"), bty = "n",cex=0.75)
    #bottom right
    plot(hhpcFeb07$Global_reactive_power ~ as.numeric(hhpcFeb07$Fulldate), type="l",xlab="datetime", ylab="Global rective Power", xaxt="n")
    axis(side =1,at=postickNum,labels=posttickn)
    
    #save as png
    dev.copy(png,file="plot4_test.png")
    dev.off()

}

cacheTheData <- function() {
    hhpcFeb07 <- NULL
    get <- function() hhpcFeb07
    set <- function(data) hhpcFeb07 <<- data
    list(get = get, set=set)
}