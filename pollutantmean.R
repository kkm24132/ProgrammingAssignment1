# Coursera - Kamal Mishra  
# Program Assignment - 1, Part 1
# Calculates the mean of pollutant (sulfate or nitrate) across a specified list of monitors
# The function pollutantmean takes 3 arguments
pollutantmean <- function (directory, pollutant, id=1:332){
  
  ## Get a list of filenames
  filenames <- list.files(path=directory,pattern="*.csv")
  
  ## Initialize a vector to hold values
  vals <- vector()
  
  ## Loop over the passed id's
  for (i in id){
    
    ## Pad the i to create a filename
    filename <- sprintf("%03d.csv",i)
    filepath <- paste(directory,filename,sep="/")
    
    ## Load the data
    data <- read.csv(filepath)
    
    ## Select our column
    d <- data[,pollutant]
    
    ## Ignore or Remove NAs i.e. missing values
    d <- d[!is.na(d)]
    
    ## Append to our vector
    vals <- c(vals, d)
    
  }
  
  ## Return the value rounded to 3 decimal places
  round(mean(vals),3)
  
}
