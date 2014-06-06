plot2 <- function(){
  data <-  read.table("household_power_consumption.txt", header = TRUE, sep = ";", nrows=75000)           #read the roughy first 2 months
  data[,1] <- as.Date(data$Date, format = "%d/%m/%Y")                                                     #convert to Date object
  subdata <- data.frame()                                                                                 #create container
  for(i in 1:nrow(data)) {                                                                                #iterate over everything
    if(data[i,1] == "2007-02-01") {                                                                       #check for first date
      subdata <- rbind(subdata, data[i,])                                                                 #save if it matches
    } else if(data[i,1] == "2007-02-02") {                                                                #same thing for date 2
      subdata <- rbind(subdata, data[i,])
    }
  }
  subdata[,3] <- as.numeric(as.character(subdata[,3]))                                                    #cast from factor
  subdata$Time <- as.character(subdata$Time)                                                              #same thing
  subdata$DateTime <- paste(subdata$Date, subdata$Time)                                                   #merge into a new column
  subdata$DateTime <- strptime(subdata$DateTime, format = "%Y-%m-%d %H:%M:%S")                            #convert to POSIX
  png(filename = "plot2.png", width = 480, height = 480)                                                  #make graphics object
  plot(subdata[,10], subdata[,3], ylab = "Global Active Power (kilowatts)", xlab = "", type = "l")        #make plot
  dev.off()                                                                                               #close it
}

