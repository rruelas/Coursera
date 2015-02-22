pollutantmean <- function(directory, pollutant, id = 1:332) { 
  ## This line creates the correct file names
  filenames <- paste("./", directory,"/", sprintf("%03d",id),".csv",sep="")
  ## This creates an empty dataset then reads and combines all datasets
  data <- data.frame()
  n <- length(id)
  for (i in 1:n){
    data <- rbind(data, read.csv(filenames[i]))
  }
  ## We create a subset called datasub1 with only the pollutant values
  datasub1 <- data[ , pollutant, ]
  ## We identify and remove the NA values of the subset here
  navalues <- is.na(datasub1)
  datasub2 <- datasub1[!navalues]
  ## Then we find the mean of the remainging values
  mean(datasub2)
}