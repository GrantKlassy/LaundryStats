# Required libraries for this program
require(ggplot2)
require(reshape)
require(stringr)
require(data.table)
require(scales)

# Set our working directory to the project dir
setwd("C:/Users/Grant/Documents/R/Laundry")

# It's useful to increase the max print in case we want to look at table data or debug
options(max.print=100000)

# A function to get the current load of a floor at a specific time
# x = the table used to calculate the load
# firstI = The first index of the table to use for calculations
# secondI = The second index of the table to use for calculations
# For example. calcLoad(log, 3, 5) will calculate the washer load because washers values are stored in indecies 3-5
# TODO: Is there a way to forward declare functions in R? It would be nice to have this at the end
calcLoad <- function(x, firstI, secondI) {
  numMachines = 0   # The number of machines that exist
  numAvailable = 0  # The number of machines that are avialable
  
  # For all of the rows of the vector...
  for (i in firstI:secondI) {
    
    # If the row is A then a machine exists and is avialable
    if (x[i] == "A") {
      numMachines = numMachines + 1
      numAvailable = numAvailable + 1
    }
    # If the rows is I then a machine exists but is not avialable
    if (x[i] == "I") {
      numMachines = numMachines + 1
    }
    
  }
  
  return ((1 - (numAvailable / numMachines)) * 100)
}

# Try to read in our CSV
# https://www.datacamp.com/community/tutorials/r-data-import-tutorial#csv
# https://stackoverflow.com/questions/9564489/opening-all-files-in-a-folder-and-applying-a-function
# https://stackoverflow.com/questions/13441204/using-lapply-and-read-csv-on-multiple-files-in-r
filenames <- list.files("./logs/", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, function(i){
  read.csv(i, header=FALSE)
})

# Compress the list of tables into a single table
log <- rbindlist(ldf)

# Set the column names to be correct
colnames(log) <- c("time", "floor", "w1", "w2", "w3", "d1", "d2", "d3")

# NOTE: If we're doing analysis on only one floor, uncomment the next two lines
# This will remove any data that isn't from the floor "analysisFloor"
    #analysisFloor = 5
    #log <- log[!(log$floor!=analysisFloor),]
   
# Floor 9 has incorrect data. For now, remove all data that is from floor 9
log <- log[!(log$floor==9),]

# Add two new load columns and use the calcLoad function to calculate the load of washers and dryers
log$loadWashers <- apply(log, 1, FUN=function(log) calcLoad(log, 3,5))
log$loadDryers <- apply(log, 1, FUN=function(log) calcLoad(log, 6,8))

# Calculate the average load and save that to a new column
log$loadAverage <- apply(log, 1, FUN=function(log) calcLoad(log, 3,8))

# Melt the log data
# I don't know if this is neccesary or not, but I think it is if we want to use cast later on
# From what I can tell, all this does is add the "variable" and "value" columns which we use later for casting
newLog <- melt(log, id=c("time", "floor", "w1", "w2", "w3", "d1", "d2", "d3", "loadWashers", "loadDryers"))

# Next, cast the data. This will cause all rows with the same time variable to become one row
# Their new loadAverage will be the mean of all the floors with that timestamp
newLog <- cast(newLog, time~variable, mean)

# Create a graph of the data
# https://stackoverflow.com/questions/38959566/axis-labels-and-limits-with-ggplot-scale-x-datetime

# NOTE: This commented out plot is useful when analyzing one floor only. 
#plot <- ggplot(data=log, aes(x=as.POSIXct(time))) + 
#        geom_line(aes(y=loadWashers, color="red")) + 
#        geom_line(aes(y=loadDryers, color="blue")) +
#        scale_color_discrete(name = "Machine type", labels = c("Dryers", "Washers")) + 
#        ylab("% Machines Used") + 
#        xlab("Time") +
#        theme(plot.title = element_text(hjust = 0.5)) +
#        ggtitle("title"Laundry Use")

# This plot is good for analyzing the total average load over all the floors
plot <- ggplot(data=newLog, aes(y=loadAverage, x=as.POSIXct(time))) +
        scale_x_datetime( 
            breaks = date_breaks("1 day"), 
            minor_breaks = date_breaks("6 hour")) + 
        geom_line(aes(color="red")) + 
        geom_smooth(span=0.2, method="loess") + # Blue best fit line
        scale_color_discrete(
            name = "Floor Number",
            labels = c("Average")) + 
        ylab("% Machines Used") + 
        xlab("Time") +
        ylim(0, 100) +
        theme(plot.title = element_text(hjust = 0.5)) +
        ggtitle("Laundry Use")

# Display the inage
plot