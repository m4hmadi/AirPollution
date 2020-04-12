pollutantmean <- function(directory, pollutant, id = 1:332){
  num_items <- 0
  sum_items <- 0
  for(i in id){
    if(i<10){
      j <- paste("00", i, sep = "")
    }
    else if(i<100){
      j <- paste("0", i, sep = "")
    }
    else{
      j <- paste(i)
    }
    data <- read.csv(paste(directory, "/", j, ".csv", sep = ""))
    sum_items <- sum_items + sum(data[pollutant], na.rm = TRUE)
    num_items <- num_items + sum(!is.na(data[pollutant]))
  }
  sum_items / num_items
}