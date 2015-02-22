complete <- function(directory, id = 1:332) { 
  ## This line creates the correct file names
  filenames <- paste("./", directory,"/", sprintf("%03d",id),".csv",sep="")
  ## This creates an empty dataset with the correct column names
  data <- data.frame(id=numeric(), nobs=numeric(0))
  n <- length(id)
  ## This loop adds a row with the id and number of complete cases
  for (i in 1:n){
    ccase <- complete.cases(read.csv(filenames[i]))
    data[i, ] <- c(id[i], sum(ccase))

  }
  ## Return dataset with id and number of complete cases for each file
  data
}