## best - find the hospital in the state, with the best (ie. lowest) 30-day
## mortality rate for specified outcome

best <- function(state, outcome) {
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
        if (outcome == "heart attack") {
            ## outcomeDetails <- as.numeric(outcomeData[,11])
            outcomeDetails <- outcomeData[order(outcomeData[,11], outcomeData[,2]),]
        } else if (outcome == "heart failure") {
            outcomeDetails <- outcomeData[order(outcomeData[,17], outcomeData[,2]),]#as.numeric(outcomeData[,17])
        } else if (outcome == "pneumonia") {
            outcomeDetails <- outcomeData[order(outcomeData[,23], outcomeData[,2]),]#as.numeric(outcomeData[,23])
        }
        
        
    }
    outcomeDetails[1,2]
    #outcomeDetails[1:5,]
    #cols <- c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, 
    #          TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE,
    #          FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    #          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    #          FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
    #outcomeDetails[1:5,cols]
    ## Return hospital name in that state with lowest 30-day death
    ## rate    
}