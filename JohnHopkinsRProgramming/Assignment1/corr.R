corr <- function(directory, threshold = 0) {
  compcase <- complete(directory)
  monlocat <- compcase[compcase[ , 2] > threshold, 1]
  ## This line creates the correct file names
  filenames <- paste("./", directory,"/", sprintf("%03d",monlocat),".csv",sep="")
  if(length(monlocat) != 0){
  
  
  ## This creates an empty dataset then reads and combines all datasets
  n <- length(filenames)
  correls <- numeric(0)
  for (i in 1:n){
    data <- read.csv(filenames[i])
    correls[i] <- cor(data[ , "sulfate"], data[ , "nitrate"], use="complete.obs")
  }
  }else {
    correls <-numeric(0)
  }
  
  correls
  
}