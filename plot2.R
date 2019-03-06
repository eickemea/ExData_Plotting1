plot2 <- function(){
      # Download data to working directory if necessary
      #if(!file.exists("household_power_consumption.txt")){
            #temp <- tempfile()
            #download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", destfile = temp)
            #unzip(temp)
            #unlink(temp)
      #}
      
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
      
      # generate a chart connecting points representing Global Active Power at specific Date-Times with lines to PNG device
      png(filename = "plot2.png")
      with(data2, plot(DateTime, Global_active_power, xlab = "", ylab = "Global Active Power (kilowatts)", type = "n"))
      with(data2, lines(DateTime, Global_active_power))
      dev.off()
}