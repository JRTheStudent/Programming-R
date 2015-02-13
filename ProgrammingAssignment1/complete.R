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
    
    # Validate Parameters
    stopifnot(length(directory) == 1 & file.info(directory)$isdir)
    stopifnot(is.integer(id) | is.numeric(id))
    
    df <- data.frame(id = numeric(), nobs = numeric())
    for (i in id){
        file <- file.path(directory, sprintf("%03d.csv", i))
        # Validate file is readible and adds index/sum to frame 
        # If not warns and skips.
        if (file.access(file, mode=4) == 0){
            data <- read.csv(file)
            df[nrow(df) + 1,] <- c(i,sum(complete.cases(data)))
        }
        else {
            warning("Skipping ", file, ": does not exist or is not readible.")
        }
    }
    df
}