plot3 <- function(filepath){
    
    if(!file.exists(filepath)){
        stop("file does not exist!");
    }
    
    ## read data from file
    data <- read.csv(filepath, header=TRUE, sep=";", na.strings="?");
    
    ## convert data$Date to Date type
    data$Date <- as.Date(data$Date, "%d/%m/%Y");
    
    ## get records from the dates 2007-02-01 and 2007-02-02
    data <- data[data$Date >= as.Date("2007-2-1") & data$Date <= as.Date("2007-2-2"),];
    
    ## add a new colomn DateTime to dataset
    data <- cbind(data, DateTime = "");    
    data$DateTime <- paste(data$Date, data$Time);
    data$DateTime <- strptime(data$DateTime, "%Y-%m-%d %H:%M:%S");
    
    ## draw the third plot
    plot(data$DateTime, data$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering");
    points(data$DateTime, data$Sub_metering_1, type = "l");
    points(data$DateTime, data$Sub_metering_2, type = "l", col = "red");
    points(data$DateTime, data$Sub_metering_3, type = "l", col = "blue")
    legend("topright", legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), pch = "-", col = c("black","red","blue"));
      
    ## save plot3 to a .PNG file
    dev.copy(png, file = "plot3.png", width = 480, height = 480);
    dev.off();
}