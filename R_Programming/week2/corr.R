corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    #     completeCounts <- complete(directory)
    #     notvalid <- is.na(completeCounts[,2])
    #     completeValidCounts <- completeCounts[!notvalid,]
    #     aboveThreshold <- completeValidCounts[,2] > threshold
    #     completeAboveCounts <- completeValidCounts[aboveThreshold,]
    allEntries <- c()
    corrResults <- c()    
    if (threshold >= 0) {
        
        
        
        completeAboveCounts <- filesWithCompleteCases(directory,threshold)
        
        #correlation <- cor(compAboveCounts[,1],compAboveCounts[,2])
        #correlation
        #completeAboveCounts
        
        #print(length(completeAboveCounts))
        if (length(completeAboveCounts) > 0) {
            for(i in 1:nrow(completeAboveCounts)) {
                #print("Must be some data in completeAboveCounts")
                row <- completeAboveCounts[i,]
                #frame <- getCompleteCasesWithThreshold(directory, threshold, i)
                frame <- getCompleteCases(directory, row[1])
                #frame <- getCompleteCases(directory, 1)
                #print(data)
                #d <- data.frame(data[1],data[2])
                #if (threshold > 0) {
                corrResults <- cor(frame$nitrate, frame$sulfate)        
                #}
                #print(corrResults)
                allEntries <- append(allEntries, corrResults)
            }    
        } else {
            allEntries <- vector(mode="numeric", length=0)
        }
        ##d <- data.frame(allEntries)
        ##corr(d)
        #d <- data.frame(data[1],data[2])
        #corr(d)
    } else {
        allEntries <- vector(mode="numeric", length=0)
    }
    allEntries
    #corrResults
}

filesWithCompleteCases <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    completeCounts <- complete(directory)
    notvalid <- is.na(completeCounts[,2])
    completeValidCounts <- completeCounts[!notvalid,]
    aboveThreshold <- completeValidCounts[,2] > threshold
    counts <- table(aboveThreshold)["TRUE"]
    
    #print(is.na(counts))
    if (!is.na(counts)) {
        completeAboveCounts <- completeValidCounts[aboveThreshold,]
    } else {
        completeAboveCounts <- c()
    }
    completeAboveCounts   
}    

getCompleteCases <- function(directory, id = 1:332) {
    allEntries <- c()
    valid <- c()
    goodEntries <- c()
    data <- c()
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
            monitorData <- read.csv(fileComp, header = TRUE, sep = ",")    
        } else {
            monitorData <- c()
        }
        
        ## Retrieve only the nitrate/sulfate data
        entries <- monitorData[,2:3]
        
        if (length(monitorData) > 0) {
            valid <- complete.cases(entries)
            #notvalid <- is.na(entries)
            goodEntries <- entries[valid,]
            ##totalMean <- append(totalMean,mean(goodEntries))
            allEntries <- append(allEntries, goodEntries)
        }    
        ##totalMean
        #allEntries
        ##allEntries <- append(allEntries, goodEntries)
    }
    ##mean(allEntries)
    
    #allEntries
    data <- data.frame(allEntries[1],allEntries[2])
    data
    #goodEntries
    #notvalid
}

getCompleteCasesWithThreshold <- function(directory, threshold = 0, id = 1:332) {
    allEntries <- c()
    valid <- c()
    goodEntries <- c()
    data <- c()
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
            monitorData <- read.csv(fileComp, header = TRUE, sep = ",")    
        } else {
            monitorData <- c()
        }
        
        ## Retrieve only the nitrate/sulfate data
        entries <- monitorData[,2:3]
        
        if (length(monitorData) > 0) {
            valid <- complete.cases(entries)
            #notvalid <- is.na(entries)
            goodEntries <- entries[valid,]
            if (length(goodEntries) > 0) {
                goodEntries <- goodEntries[1:threshold,]
            }
            ##totalMean <- append(totalMean,mean(goodEntries))
            allEntries <- append(allEntries, goodEntries)
        }    
        ##totalMean
        #allEntries
        ##allEntries <- append(allEntries, goodEntries)
    }
    ##mean(allEntries)
    
    #allEntries
    data <- data.frame(allEntries[1],allEntries[2])
    data
    #goodEntries
    #notvalid
}