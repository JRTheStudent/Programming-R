rankall <- function(outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available")
    
    ## Check that state and outcome are valid
    
    ## Lower outcome, check if outcome is in set
    outcome <- tolower(outcome)
    if (! outcome %in% c("heart attack", "heart failure", "pneumonia")){
        stop("invalid outcome")
    }
    
    ## Validate num, set numRow
    if (is.character(num)){
        if (tolower(num) == "best"){
            numRow <- 1 
        }  
        else if (tolower(num) == "worst"){
            numRow <- quote(nrow(df))
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
 
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    subData <- data[order(data[oCol], data[2]), c(2, 7, oCol)]
    subSplit <- split(subData, subData[2])
    output <- data.frame(hospital = character(), state = character(), 
                         stringsAsFactors = F)
    for (df in subSplit){
        df <- df[complete.cases(df), ] 
        if (eval(numRow) > nrow(df)){
            output[nrow(output) + 1,] <- c(NA, as.character(df[[1,2]]))
        }
        else{
            output[nrow(output) + 1,] <- c(as.character(df[[eval(numRow), 1]]),
                                           as.character(df[[eval(numRow), 2]]))
        }
    }
    rownames(output) <- output[[2]]
    return(output)
}