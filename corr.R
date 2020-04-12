corr <- function(directory, threshold = 0){
  cor_vector <- c()
  for(i in 1:332){
    if(i<10){
      j <- paste("00", i, sep = "")
    }
    else if(10 <= i & i < 100){
      j <- paste("0", i, sep = "")
    }
    else{
      j <- paste(i)
    }
    file_path <- paste(directory, "/", j, ".csv", sep = "")
    data <- read.csv(file_path)
    if(complete(directory, i)["nobs"] > threshold){
      good1 <- !is.na(data[, "sulfate"])
      good2 <- !is.na(data[, "nitrate"])
      good <- good1 & good2
      cor_vector <- c(cor_vector, cor(data[good, "sulfate"], data[good, "nitrate"])) 
    }
  }
  cor_vector
}