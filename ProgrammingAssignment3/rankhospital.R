rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data, required to validate state parameter
    data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")

    ## Check that state and outcome are valid
    
    ## Uppercase state, check if state in set
    state <- toupper(state)
    if (! state %in% levels(data$State)) stop("invalid state")
    
    ## Lower outcome, check if outcome is in set
    outcome <- tolower(outcome)
    if (! outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    
    ## Validate num, set numCol
    if (is.character(num)){
        if (tolower(num) == "best"){
            numRow <- 1 
        }  
        else if (tolower(num) == "worst"){
            ## Last index currently unknown, set numRow to worst for now.
            ## Optimize?
            numRow <- tolower(num)  
        }
        else{
            stop("invalid string for num parameter")
        }
    }
    else if (is.numeric(num)) {
        numRow <- num
    }
    else{
        stop("invalid value for num parameter")
    }
    
    ## Set the outcome column
    if (outcome == "heart attack")  oCol <- 11
    if (outcome == "heart failure") oCol <- 17
    if (outcome == "pneumonia")     oCol <- 23  
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    ## Optimize?
    stateData <- data[data$State == state & ! is.na(data[oCol]), c(2,oCol)]
    stateData <- stateData[order(stateData[2], stateData[1]),]
   
    ## Set numRow for "worst", return NA if num exceeds number of valid rows.
    if (numRow == "worst") numRow <- nrow(stateData)
    if (numRow > nrow(stateData)){
        return(NA)
    }
    as.character(stateData[numRow,1])
}