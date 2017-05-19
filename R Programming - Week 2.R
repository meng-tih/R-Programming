home <- "C:/Users/PoissonQuiCoule/Documents/"


pollutantmean <- function(directory, pollutant, id = 1:332) {
  setwd(paste(home, directory, sep = "/"))
  
  all_files <- dir()
  all_data <- c()
  for (i in id) {
    current_frame <- read.csv(all_files[i], header = TRUE)
    current_vector <- current_frame[pollutant][!is.na(current_frame[pollutant])]
    all_data <- c(all_data, current_vector)
  }
  mean(all_data)
}

complete <- function(directory, id = 1:332) {
  setwd(paste(home, directory, sep = "/"))
  
  all_files <- dir()
  all_data <- data.frame()
  for (i in id) {
    current_frame <- read.csv(all_files[i], header = TRUE)
    current_vector <- current_frame["ID"][!is.na(current_frame["Date"]) & !is.na(current_frame["sulfate"])
                                                                        & !is.na(current_frame["nitrate"])
                                                                        & !is.na(current_frame["ID"])]
    all_data <- rbind(all_data, c(i, length(current_vector)))    
  }
  colnames(all_data) <- c("id","nobs")
  all_data
}

corr <- function(directory, threshold = 0) {
  setwd(paste(home, directory, sep = "/"))
  
  all_files <- dir()
  complete_full <- complete(directory)
  ids <- complete_full$id[complete_full$nobs >= threshold]
  corr_vector <- rep(0, length(ids))
  j <- 1
  for (i in ids) {
    current_frame <- read.csv(all_files[i], header = TRUE)
    corr_vector[j] <- cor(current_frame$sulfate, current_frame$nitrate, use = "pairwise.complete.obs")
    j <- j + 1
  }
  corr_vector
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)
cc <- complete("specdata", 54)
print(cc$nobs)
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))