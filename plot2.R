## Creates line plot2, x axis = data$Time, y axis = data$Global_active_power
## White background used for better presentation in browser


plot2<-function(date1="2007/02/01", date2="2007/02/02"){
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
  png(filename = "plot2.png", width = 480, height = 480, units = "px", bg="white")
  with(data, plot(data$Time,data$Global_active_power, type="l", xlab="", ylab= "Global Active Power (kilowatts)")) 
  ## Closing device
  dev.off()
}