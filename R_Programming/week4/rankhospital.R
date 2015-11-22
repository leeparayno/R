rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
    ## Convert to numeric numbers
    outcomeData[,11] <- as.numeric(outcomeData[,11])
    outcomeData[,17] <- as.numeric(outcomeData[,17])
    outcomeData[,23] <- as.numeric(outcomeData[,23])    
    
    ## Check that state and outcome are valid    
    states <- outcomeData[,7]
    statesWithData <- unique(states)
    
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    
    if (!(state %in% statesWithData)) {
        stop("invalid state")
    } else {
        outcomeData <- outcomeData[outcomeData$State == state,]
    }
    if (!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    } else {
        ## Need to order the data frame by the outcome specified
        if (outcome == "heart attack") {
            valid <- outcomeData[,11] != "Not Available"
            outcomeData <- outcomeData[valid,]
            outcomeDetails <- outcomeData[order(outcomeData[,11], outcomeData[,2], na.last = NA),]
        } else if (outcome == "heart failure") {
            valid <- outcomeData[,17] != "Not Available"
            outcomeData <- outcomeData[valid,]
            outcomeDetails <- outcomeData[order(outcomeData[,17], outcomeData[,2], na.last = NA),]
        } else if (outcome == "pneumonia") {
            valid <- outcomeData[,23] != "Not Available"
            outcomeData <- outcomeData[valid,]
            outcomeDetails <- outcomeData[order(outcomeData[,23], outcomeData[,2], na.last = NA),]
        }
    }    
    
    #cols <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    #          TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
    #          FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    #          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    #          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    #outcomeDetails <- outcomeDetails[,cols]    
    if (is.character(num)) {
        if (num == "best") {
            results <- outcomeDetails[1,2]        
        } else if (num == "worst") {
            results <- outcomeDetails[length(outcomeDetails[,1]),2]        
        }
    } else if (is.numeric(num) && length(num) == 1) {
        if (num > length(outcomeDetails[,1])) {
            results <- NA
        } else {
            results <- outcomeDetails[num,2]            
        }
    } else if (is.vector(num)) {
        results <- outcomeDetails[num,2]
    }
    results
    #outcomeDetails[1,2]
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
}
