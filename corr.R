# Coursera - Kamal Mishra  
# Program Assignment - 1, Part 3
# Write a function that takes a directory of data files and a threshold for complete cases 
# and calculates the correlation between sulfate and nitrate for monitor locations
# where the number of completely observed cases (on all variables) is greater than the threshold.

corr <- function(directory, threshold = 0) {
    
  completes <- complete(directory, 1:332)
  completes <- subset(completes, nobs > threshold )
  
  
  ## Initialize variables
  correlations <- vector()
  
  
  ## Loop over the passed id's
  for(i in completes$id ) {
    
    
    ## Pad the i to create a filename
    filename <- sprintf("%03d.csv", i)
    filepath <- paste(directory, filename, sep="/")
    
    
    ## Load the data
    data <- read.csv(filepath)
    
    
    ## Calculate and store the count of complete cases
    completeCases <- data[complete.cases(data),]
    count <- nrow(completeCases)
    
    
    ## Calculate and store the count of complete cases
    ## if threshhold is reached
    if( count >= threshold ) {
      correlations <- c(correlations, cor(completeCases$nitrate, completeCases$sulfate) )
    }
  }
    
  ## Return a numeric vector of correlations
  correlations
}

