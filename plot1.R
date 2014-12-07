## Creation of an histogram with default parameters from 2007-02-01 to 2007-02-02. x axis as Global Active Power
## Background chosen white for a more standard presentantion in different browsers


plot1<-function(date1="2007/02/01", date2="2007/02/02"){
  ## Reading the data
  data<-read.csv("household_power_consumption.txt", sep=";", header=T, colClasses="character")
  data$Date<-as.Date(data$Date, format="%d/%m/%Y")
  date1<-as.Date(date1)
  date2<-as.Date(date2)
  ## Subsetting
  data<-data[data$Date>=date1 & data$Date<=date2,]
  ## General adaptation of columns. Only data$Global_active_power,
  ## is needed for the process but all of them were processed for the purpose of recycling
  data$Global_active_power <- as.numeric(data$Global_active_power)
  data$Global_reactive_power <- as.numeric(data$Global_reactive_power)
  data$Global_intensity <- as.numeric(data$Global_intensity)
  data$Voltage <- as.numeric(data$Voltage)
  data$Sub_metering_1 <- as.numeric(data$Sub_metering_1)
  data$Sub_metering_2 <- as.numeric(data$Sub_metering_2)
  data$Sub_metering_3 <- as.numeric(data$Sub_metering_3)
  ## Device begins
  png(filename = "plot1.png", width = 480, height = 480, units = "px", bg="white")
  ## Histogram
  hist(data$Global_active_power,breaks = seq(0,7.5,by=.5),right = F, col="red", xlab="Global Active Power (kilowatts)", main="Global Active Power")  
  ## Devide ends
  dev.off()
}