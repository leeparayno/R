rankall <- function(outcome, num = "best") {
    ## Read outcome data
    fullOutcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")

    ## Convert to numeric numbers
    fullOutcomeData[,11] <- as.numeric(fullOutcomeData[,11])
    fullOutcomeData[,17] <- as.numeric(fullOutcomeData[,17])
    fullOutcomeData[,23] <- as.numeric(fullOutcomeData[,23])    
    
        
    ## Check that state and outcome are valid    
    states <- fullOutcomeData[,7]
    statesWithData <- sort(unique(states))
    
    validOutcomes <- c("heart attack", "heart failure", "pneumonia")
    

    if (!(outcome %in% validOutcomes)) {
        stop("invalid outcome")
    } 
    
    stateData <- c()
    ## For each state, find the hospital of the given rank
    for (state in statesWithData) {
        results <- c()
        ## Limit to the current state
        outcomeData <- fullOutcomeData[fullOutcomeData$State == state,]
        ## Need to order the data frame by the outcome specified
        if (outcome == "heart attack") {
            valid <- outcomeData[,11] != "Not Available"
            validData <- outcomeData[valid,]
            outcomeDetails <- validData[order(validData[,11], validData[,2], na.last = NA),]
        } else if (outcome == "heart failure") {
            valid <- outcomeData[,17] != "Not Available"
            validData <- outcomeData[valid,]
            outcomeDetails <- validData[order(validData[,17], validData[,2], na.last = NA),]
        } else if (outcome == "pneumonia") {
            valid <- outcomeData[,23] != "Not Available"
            validData <- outcomeData[valid,]
            outcomeDetails <- validData[order(validData[,23], validData[,2], na.last = NA),]
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
        #results        
        stateData <- append(stateData, results)
        # Calling rankhospital
        ##aRankItem <- rankhospital(state, outcome, num)
        #record <- c(state, aRankItem)
        #print(record)
        #stateData <- append(stateData, aRankItem)
    }
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    cols <- c("hospital","state")
    state.data <- data.frame(stateData, statesWithData)
    names(state.data) <- cols
    state.data
}

rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("rprog-data-ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
    
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
