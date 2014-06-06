plot3 <- function() {
  data <-  read.table("household_power_consumption.txt", header = TRUE, sep = ";", nrows=75000)                #read the first ~2 months
  data[,1] <- as.Date(data$Date, format = "%d/%m/%Y")                                                          #convert to Date type

  subdata <- data.frame()                                                                                      #empty container
  for(i in 1:nrow(data)) {                                                                                     #search for dates
    if(data[i,1] == "2007-02-01") {
      subdata <- rbind(subdata, data[i,])
    } else if(data[i,1] == "2007-02-02") {
      subdata <- rbind(subdata, data[i,])
    }
  }

  subdata[,7] <- as.numeric(as.character(subdata[,7]))                                                         #cast from factor
  subdata[,8] <- as.numeric(as.character(subdata[,8]))
  subdata[,9] <- as.numeric(as.character(subdata[,9]))
  subdata$Time <- as.character(subdata$Time)
  subdata$DateTime <- paste(subdata$Date, subdata$Time)                                                        #merge into a new column
  subdata$DateTime <- strptime(subdata$DateTime, format = "%Y-%m-%d %H:%M:%S")                                 #convert to POSIX
  png(filename = "plot3.png", width = 480, height = 480)                                                       #make graphics object
  plot(subdata[,10], subdata[,7], ylab = "Energy sub metering", xlab = "", type = "l", col ="black")           #plot it
  lines(subdata[,10], subdata[,8], col = "red")                                                                #add second variable
  lines(subdata[,10], subdata[,9], col = "blue")                                                               #add number 3
  legend(x = "topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1, 1, 1),     #build legend
         col=c("black", "red", "blue"))
  dev.off()                                                                                                    #the end
}