## plot 1
## plot a histogramm from datafile "household_power_consumption.txt"
## data is cached in order to speed up things 
## usage:
## > setwd("TheLocationOfDataAndRfiles")
## > source("plot1.R")
## > x <- cacheTheData()
## > plot1(x)
##
plot1 <- function(x){
    
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
        rm(hhcp)
        rm(day1)
        rm(day2)
    }
    
    ## plot histogramm
    par(mfrow = c(1,1))
    par(mar=c(5,5,2,2))
    hist(as.numeric(hhpcFeb07$Global_active_power),xlab="Global active power (kilowatts)", main="Global Active Power", col="red")
    #save as png
    dev.copy(png,file="plot1.png")
    dev.off()
    
}

cacheTheData <- function() {
    hhpcFeb07 <- NULL
    get <- function() hhpcFeb07
    set <- function(data) hhpcFeb07 <<- data
    list(get = get, set=set)
}

