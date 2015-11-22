complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    totalCount <- 0
    monitorCountDetails <- c()
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
            comp <- complete.cases(monitorDate)        
            compCount <- table(comp)[2]            
        } else {
            monitorDate <- c()
            compCount <- 0
        }
        
        monitorCount <- c(monitor, compCount)
        
        monitorCountDetails <- append(monitorCountDetails, monitorCount)
        
        ##totalMean
        #totalCount <- totalCount + compCount
        ##allEntries <- append(allEntries, goodEntries)
    }
    ##mean(allEntries)
    #print(monitorCountDetails)
    monitorCountDetails
#     if (length(monitorCountDetails) > 0) {
#         print(paste("dimensions of monitorCountDetails:",dim(monitorCountDetails)[1]))
         
#         print(paste("size:",size))
#         size
#         if (!is.null(size)) {
             monComplete <- matrix(monitorCountDetails,ncol = 2,byrow = TRUE)
             size <- dim(monComplete)[1]
             cnames <- c("id", "nobs")
             colnames(monComplete) <- cnames
             rownames(monComplete) <- c(1:size)
#         } else {
#             monComplete <- c()
#         }            
#     } else {
#         monComplete <- c()
#     }    
     monComplete
     as.data.frame(monComplete)
    
}