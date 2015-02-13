pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)

    # Validate Parameters
    stopifnot(length(directory) == 1 & file.info(directory)$isdir)
    
    pollutant <- tolower(pollutant)
    stopifnot(length(pollutant) == 1 & pollutant %in% c("sulfate", "nitrate"))
    
    stopifnot(is.integer(id) | is.numeric(id))
    
    files <- character()
    for (i in id){
        file <- file.path(directory, sprintf("%03d.csv", i))
        # Validate file is readible and adds to vector; warns/skips if not.
        if (file.access(file, mode=4) == 0){
            files[length(files) + 1] <- file
        }
        else {
            warning("Skipping ", file, ": does not exist or is not readible.")
        }
    }
    # Validate there are files to process.
    if (! length(files) > 0){
        stop("No files to process, exiting.")
    }    
    pValues <- numeric()
    for (file in files){
        pData <-read.csv(file)
        pValues <- c(pValues, na.omit(pData[[pollutant]]))  
    }
    as.numeric(sprintf("%.3f", mean(pValues)))
}
