plot1 <- function(){
 data <-  read.table("household_power_consumption.txt", header = TRUE, sep = ";", nrows=75000) #read the roughly first 2 months
 data[,1] <- as.Date(data$Date, format = "%d/%m/%Y")                                           #convert the Date column to Date objects
 subdata <- data.frame()                                                                       #create holder frame
 for(i in 1:nrow(data)) {                                                                      #iterate over the entire set
   if(data[i,1] == "2007-02-01") {                                                             #search for date 1
     subdata <- rbind(subdata, data[i,])                                                       #if found, save the record
   } else if(data[i,1] == "2007-02-02") {                                                      #same thing for date 2
     subdata <- rbind(subdata, data[i,])
   }
 }
 subdata[,3] <- as.numeric(as.character(subdata[,3]))                                          #convert from factor to numeric
 png(filename = "plot1.png", width = 480, height = 480)                                        #create graphics object
 hist(subdata[,3], main = "Global Active Power", xlab = "Global Active Power (kilowatts)", 
      ylab = "Frequency", col= "red")                                                          #create graph
 dev.off()                                                                                     #close
}