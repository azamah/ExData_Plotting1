# Functionn to create plot4
createPlot4<-function(){
  # Load Data
  hpc <- read.table("ExData_Plotting1/household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")
  
  # Format date
  hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
  
  # Filter date
  hpc <- subset(hpc,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
  
  # Remove invalid data
  hpc <- hpc[complete.cases(hpc),]
  
  # Combine Date and Time
  dateTime <- paste(hpc$Date, hpc$Time)
  
  # Add column
  hpc <- cbind(dateTime, hpc)
  hpc$dateTime <- as.POSIXct(dateTime)
  
  # Create Plot4
  par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
  with(hpc, {
    plot(Global_active_power~dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
    plot(Voltage~dateTime, type="l", ylab="Voltage (volt)", xlab="")
    plot(Sub_metering_1~dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
    lines(Sub_metering_2~dateTime,col='Red')
    lines(Sub_metering_3~dateTime,col='Blue')
    legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    plot(Global_reactive_power~dateTime, type="l", ylab="Global Active Power (kilowatts)",xlab="")
  })
  
  # Save plot
  dev.copy(png, "ExData_Plotting1/plot4.png", width=480, height=480)
  dev.off()
}