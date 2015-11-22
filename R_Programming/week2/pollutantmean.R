pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
  
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
  
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
        
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    totalMean <- c()
    allEntries <- c()
    for (monitor in id) {
        if (monitor < 10) {
            monitorFile <- paste("00",monitor, sep = "")
        } else if (monitor < 100) {
            monitorFile <- paste("0",monitor, sep = "")
        } else {
            monitorFile <- monitor
        }   
        fileComp <- paste(directory,"/",monitorFile,".csv",sep = "")
        ##print(fileComp)
        if (file.exists(fileComp) == TRUE) {
            monitorDate <- read.csv(fileComp, header = TRUE, sep = ",")    
        } else {
            monitorDate <- c()
        }
        
        if (pollutant == "sulfate") {
            entries <- monitorDate[,2]
        } else if (pollutant == "nitrate") {
            entries <- monitorDate[,3]
        } else {
            ## Not a valid pollutant in the data files
            entries <- c()
        }
        if (length(entries) > 0) {
            notvalid <- is.na(entries)
            goodEntries <- entries[!notvalid]
            ##totalMean <- append(totalMean,mean(goodEntries))
            allEntries <- append(allEntries, goodEntries)
        }    
        ##totalMean
        allEntries
        ##allEntries <- append(allEntries, goodEntries)
    }
    ##mean(allEntries)
    avg <- if (length(allEntries) > 0) {
        mean(allEntries)
    } else {
        NA
    }
    avg
}