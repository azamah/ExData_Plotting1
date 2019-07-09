# Functionn to create plot 1
createPlot1<-function(){
  # Load Data
  hpc <- read.table("ExData_Plotting1/household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?")
  
  # Format date
  hpc$Date <- as.Date(hpc$Date, "%d/%m/%Y")
  
  # Filter date
  hpc <- subset(hpc,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2"))
  
  # Remove invalid data
  hpc <- hpc[complete.cases(hpc),]
  
  # Create Plot 1
  hist(hpc$Global_active_power, main="Global Active Power", xlab = "Global Active Power (kilowatts)", col="red")
  
  # Save plot
  dev.copy(png, "ExData_Plotting1/plot1.png", width=480, height=480)
  dev.off()
}