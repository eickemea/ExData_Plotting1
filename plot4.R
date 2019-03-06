plot4 <- function(){
      # Download data to working directory if necessary
      if(!file.exists("household_power_consumption.txt")){
            temp <- tempfile()
            download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = temp)
            unzip(temp)
            unlink(temp)
      }
      
      # Read a suitable subset of data into R (to reduce memory usage)
      start <- grep("1/2/2007", readLines("household_power_consumption.txt"))[1] - 1
      stops <- grep("2/2/2007", readLines("household_power_consumption.txt"))
      stop <- stops[length(stops)] + 1
      variableNames <- read.table(file = "household_power_consumption.txt", sep = ";", nrows = 1, stringsAsFactors = FALSE)
      data <- read.table(file = "household_power_consumption.txt", sep = ";",
                         skip = start, nrows = stop - start, stringsAsFactors = FALSE)
      colnames(data) <- unlist(variableNames)
      
      # Create a new column representing a POSIXlt/POSIXt variable DateTime, which combines corresponding 
      # Date and Time variable observations into a single observation 
      data$DateTime <- paste(data$Date, data$Time)
      data$DateTime <- strptime(data$DateTime, format = "%d/%m/%Y %H:%M:%S")
      
      #Change variables from character class to appropriate class (This also changes the missing values represented by '?' to NAs)
      data$Global_active_power <- suppressWarnings(as.numeric(data$Global_active_power))
      data$Global_reactive_power <- suppressWarnings(as.numeric(data$Global_reactive_power))
      data$Voltage <- suppressWarnings(as.numeric(data$Voltage))
      data$Global_intensity <- suppressWarnings(as.numeric(data$Global_intensity))
      data$Sub_metering_1 <- suppressWarnings(as.numeric(data$Sub_metering_1))
      data$Sub_metering_2 <- suppressWarnings(as.numeric(data$Sub_metering_2))
      
      # Subset the data to include observations from only the desired dates
      data2 <- subset(data, data$Date == "1/2/2007" | data$Date == "2/2/2007")
      
      # Set device to PNG and set mfrow parameter to (2, 2) so that 4 plots may be generated together in a 2 x 2 arrangement
      png(filename = "plot4.png")
      par(mfrow = c(2, 2))
      
      # In top-left, generate a chart connecting points representing Global Active Power at specific Date-Times with lines
      with(data2, plot(DateTime, Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "n"))
      with(data2, lines(DateTime, Global_active_power))
      
      # In top-right, generate a chart connecting points representing Voltage at specific Date-Times with lines
      with(data2, plot(DateTime, Voltage, xlab = "datetime", ylab = "Voltage", type = "n"))
      with(data2, lines(DateTime, Voltage))
      
      # In bottom-left, generate a chart connecting points representing values of Sub_metering_1, Sub_metering_2, 
      # and Sub_metering_3 at specific Date-Times with lines of different colors
      with(data2, plot(DateTime, Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "n"))
      # Add black lines representing frequencies of Sub_metering_1
      with(data2, lines(DateTime, Sub_metering_1))
      # Add red lines representing frequencies of Sub_metering_2
      with(data2, lines(DateTime, Sub_metering_2, col = "red"))
      # Add blue line representing frequencies of Sub_metering_3
      with(data2, lines(DateTime, Sub_metering_3, col = "blue")) 
      # Add legend
      legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
             bty = "n")
      
      # In bottom-right, generate a chart connecting points representing values of Global Reactive Power at specific
      # Date-Times with lines
      with(data2, plot(DateTime, Global_reactive_power, xlab = "datetime", ylab = "Global_reactive_power", type = "n"))
      with(data2, lines(DateTime, Global_reactive_power))
      
      # Disconnect from PNG device
      dev.off()
}