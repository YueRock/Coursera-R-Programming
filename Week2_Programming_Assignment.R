################## 
######Part 1######
##################

setwd("/Users/liu/Downloads/specdata")
filenames <- list.files(pattern="*.csv")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  setwd("/Users/liu/Downloads")
  dataAllMonitors <- data.frame()
  for (i in id) {
    dataAllMonitors <- rbind(dataAllMonitors,read.csv(paste(directory,'/',filenames[i], sep="")))
  }
  mean(dataAllMonitors[,pollutant],na.rm = TRUE)
}
setwd("C:/Users/yl4zd/Desktop/Personal/Coursera")
# 1
pollutantmean("specdata", "sulfate", 1:10)

# 2
pollutantmean("specdata", "nitrate", 70:72)

# 3
pollutantmean("specdata", "sulfate", 34)

# 4
pollutantmean("specdata", "nitrate")

################## 
######Part 2######
##################

setwd("/Users/liu/Downloads/specdata")
filenames <- list.files(pattern="*.csv")

complete <- function(directory, id=1:322) {
  setwd("/Users/liu/Downloads")
  complete_obs <- data.frame()
  for (i in id) {
    monitor <- read.csv(paste(directory,'/',filenames[i], sep=""))
    n_obs <- sum(complete.cases(monitor))
    cases <- data.frame(i,n_obs)
    complete_obs <- rbind(complete_obs, cases)
  }
  colnames(complete_obs) <- c("id","nobs")
  complete_obs
}

setwd("C:/Users/yl4zd/Desktop/Personal/Coursera")

# 5
cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

# 6
cc <- complete("specdata", 54)
print(cc$nobs)

# 7
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

################## 
######Part 3######
##################

setwd("/Users/liu/Downloads/specdata")
filenames <- list.files(pattern="*.csv")

corr <- function(directory, threshold=0) {
  setwd("/Users/liu/Downloads")
  corr_2var <- vector(mode = "numeric", length = 0)
  for (i in 1:length(filenames)) {
    file_path <- paste(directory,'/',filenames[i], sep="")
    monitor <- read.csv(file_path)
    monitor <- monitor[which(!is.na(monitor$sulfate) & !is.na(monitor$nitrate)),]
    n_obs <- sum(complete.cases(monitor))
    if (n_obs > threshold) {
      corr_2var <- c(corr_2var, cor(monitor$sulfate, monitor$nitrate))
    }
  }
  return(corr_2var)
}

corr("specdata", 150)

# 8
cr <- corr("specdata")                
cr <- sort(cr)                
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

# 9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)                
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

# 10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))
