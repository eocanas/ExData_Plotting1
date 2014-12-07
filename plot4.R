## Creates multiple plots (4 plots in a 2x2 arrangement)
## White background used for better presentation in browser

plot4<-function(date1="2007/02/01", date2="2007/02/02"){
  ## Reading the data
  data<-read.csv("household_power_consumption.txt", sep=";", header=T, colClasses="character")
  data$Date<-as.Date(data$Date, format="%d/%m/%Y")
  date1<-as.Date(date1)
  date2<-as.Date(date2)
  ## Subsetting
  data<-data[data$Date>=date1 & data$Date<=date2,]
  ## Preparing Time column
  data<-within(data, Time <- paste(Date, Time, sep=' '))
  data$Time<-strptime(data$Time,format="%Y-%m-%d %H:%M:%S")
  ## General adaptation
  data$Global_active_power <- as.numeric(data$Global_active_power)
  data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
  data$Global_intensity <- as.numeric(data$Global_intensity)
  data$Voltage <- as.numeric(data$Voltage)
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
  ## Plot creation and saving
  png(filename = "plot4.png", width = 480, height = 480, units = "px")
  ## Two by two arrangement
  par(mfrow=c(2,2))
  ## Plot1
  with(data, plot(data$Time,data$Global_active_power, type="l", xlab="", ylab= "Global Active Power (kilowatts)")) 
  ## Plot 2
  with(data, plot(data$Time,data$Voltage ,type="l", xlab="datetime", ylab="Voltage")) 
  ## Plot 3
  with(data, plot(data$Time,data$Sub_metering_1 ,type="l", xlab="", ylab= "Energy sub metering")) 
  lines(data$Time,data$Sub_metering_2, col="red")
  lines(data$Time,data$Sub_metering_3, col="blue")
  legend("topright",c("Sub_Metering_1","Sub_Metering_2", "Sub_Metering_2"),lty=c(1,1,1), lwd=c(2,2,2),col=c("black", "red","blue")) 
  ## Plot 4
  with(data, plot(data$Time,data$Global_reactive_power, type="l", xlab="datetime", ylab= "Global_Reactive_Power")) 
  ## Close device
  dev.off()
}