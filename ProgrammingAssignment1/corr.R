corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    
    # Validate Parameters
    stopifnot(length(directory) == 1 & file.info(directory)$isdir)
    stopifnot(length(threshold) == 1 & is.numeric(threshold))
    
    correlations <- numeric()
    
    # Evidently the intent is to build up from the "complete" function
    # This seems like an unnatural fit. "directory" is a shared parameter,
    # but "id" is going to be defaulted as it is not passed in this function. 
    
    dfC <- complete(directory)
    ids <- dfC[dfC$nobs > threshold, "id"]
    
    for (i in ids){
        file <- file.path(directory, sprintf("%03d.csv", i))
        if (file.access(file, mode=4) == 0){
            data <- read.csv(file)
            dfP <- data[complete.cases(data),c("sulfate","nitrate")]
            #correlations[length(correlations) + 1] <- as.numeric(sprintf("%.5f",cor(dfP$sulfate, dfP$nitrate)))
            correlations[length(correlations) + 1] <- cor(dfP$sulfate, dfP$nitrate)
        }
        else {
            warning("Skipping ", file, ": does not exist or is not readible.")
        }
    }
    #options(digits = 6) #epic jankiness to fix summary output
    return(correlations)
}