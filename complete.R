complete <- function(directory, idd = 1:332){
  id <- c()
  nobs <- c()
  for(i in idd){
    if(i<10){
      j <- paste("00", i, sep = "")
    }
    else if(i<100){
      j <- paste("0", i, sep = "")
    }
    else{
      j <- paste(i)
    }
    id <- c(id, i)
    data <- read.csv(paste(directory, "/", j, ".csv", sep = ""))
    nobs <- c(nobs, sum(!is.na(data["sulfate"]) & !is.na(data["nitrate"])))
  }
  data.frame(id, nobs)
}