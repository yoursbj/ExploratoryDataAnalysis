plot2 <- function(filepath){
    
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
    
    ## draw the second plot
    plot(data$DateTime, data$Global_active_power, type = "l", xlab = "", ylab = "Global Active Power (kilowatts)");
    
    ## save plot2 to a .PNG file
    dev.copy(png, file = "plot2.png", width = 480, height = 480);
    dev.off();
}