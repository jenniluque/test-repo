corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  resultat = numeric(0)
  j = 1
  for(i in 1:332){
    if(i < 10){
      auz = paste("C:/Users/jenni/Desktop/", directory, "/00",as.character(i),".csv")
    }else if (i < 100){
      auz = paste("C:/Users/jenni/Desktop/",directory, "/0",as.character(i),".csv")
    }else{
      auz = paste("C:/Users/jenni/Desktop/",directory,"/",as.character(i),".csv")
    }
    
    auz = gsub(" ", "", auz)
    data = read.csv(as.character(auz), header = T)
    rows = which(!is.na(data$sulfate))
    data = data[rows,]
    rows = which(!is.na(data$nitrate))
    data = data[rows,]
    if(nrow(data) > threshold){
        resultat[j] = cor(data$sulfate, data$nitrate)
        j = j+1
    }
  }
  return(resultat)
}

