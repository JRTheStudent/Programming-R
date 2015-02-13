best <- function(state, outcome) {
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

    ## Set the outcome column
    if (outcome == "heart attack")  oCol <- 11
    if (outcome == "heart failure") oCol <- 17
    if (outcome == "pneumonia")     oCol <- 23  
    
    ## Return hospital name in that state with lowest 30-day death rate
    stateData <- data[data$State == state & ! is.na(data[oCol]), c(2,oCol)]
    as.character(stateData[which.min(stateData[[2]]),1])
}