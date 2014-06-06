plot4 <- function() {
  data <-  read.table("household_power_consumption.txt", header = TRUE, sep = ";", nrows=75000)                #the same thing as the other 3 files
  data[,1] <- as.Date(data$Date, format = "%d/%m/%Y")
  
  subdata <- data.frame()
  for(i in 1:nrow(data)) {
    if(data[i,1] == "2007-02-01") {
      subdata <- rbind(subdata, data[i,])
    } else if(data[i,1] == "2007-02-02") {
      subdata <- rbind(subdata, data[i,])
    }
  }
  
  subdata[,3] <- as.numeric(as.character(subdata[,3]))                                                         #I should have set stringsAsFactors=False
  subdata[,4] <- as.numeric(as.character(subdata[,4]))                                                         #maybe next time
  subdata[,5] <- as.numeric(as.character(subdata[,5]))
  subdata[,6] <- as.numeric(as.character(subdata[,6]))
  subdata[,7] <- as.numeric(as.character(subdata[,7]))
  subdata[,8] <- as.numeric(as.character(subdata[,8]))
  subdata[,9] <- as.numeric(as.character(subdata[,9]))
  subdata$Time <- as.character(subdata$Time)
  subdata$DateTime <- paste(subdata$Date, subdata$Time)                                                       #POSIX dates
  subdata$DateTime <- strptime(subdata$DateTime, format = "%Y-%m-%d %H:%M:%S")
  
  png(filename = "plot4.png", width = 480, height = 480)                                                     #make the plot
  par(mfrow = c(2, 2))                                                                                       #2x2 grid of graphs
  with(subdata, {                                                                                            #all plots go in graph
    plot(subdata[,10], subdata[,3], ylab = "Global Active Power", xlab = "", type = "l")                     #plot 1
    plot(subdata[,10], subdata[,5], xlab = "datetime", ylab = "Voltage", type = "l")                         #plot 2
    plot(subdata[,10], subdata[,7], ylab = "Energy sub metering", xlab = "", type = "l", col ="black")       #plot 3
    lines(subdata[,10], subdata[,8], col = "red")                                                            #var 2 of plot 3
    lines(subdata[,10], subdata[,9], col = "blue")                                                           #var 3 of plot 3
    legend(x = "topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1), #build legend
           col=c("black", "red", "blue"), bty = "n")
    plot(subdata[,10], subdata[,4], xlab = "datetime", ylab = "Global_reactive_power", type = "l")           #plot 4
  })
  dev.off()                                                                                                  #close!
}