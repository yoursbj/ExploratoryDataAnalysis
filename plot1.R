plot1 <- function(filepath){
    
    if(!file.exists(filepath)){
        stop("file does not exist!");
    }
    
    ## read data from file
    data <- read.csv(filepath, header=TRUE, sep=";", na.strings="?");
    
    ## convert data$Date to Date type
    data$Date <- as.Date(data$Date, "%d/%m/%Y");
    
    ## get records from the dates 2007-02-01 and 2007-02-02
    data <- data[data$Date >= as.Date("2007-2-1") & data$Date <= as.Date("2007-2-2"),];
    
    ## draw the first plot
    hist(data$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")
    
    ## save plot1 to a .PNG file
    dev.copy(png, file = "plot1.png", width = 480, height = 480);
    dev.off();
}
