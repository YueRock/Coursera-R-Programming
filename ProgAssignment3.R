# Coursera R Programming Week 3 Assignment
# Part 1
setwd("C:/Users/yl4zd/Desktop/Personal/Coursera/ProgAssignment3/")
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
outcome[, 11] <- as.numeric(outcome[, 11])

hist(outcome[, 11])

# Part 2
best <- function(state, outcome) {
  ## Read outcome data
  outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if(state %in% unique(outcome_measures$State)) {
    outcome_state <- outcome_measures[outcome_measures$State == state,]
    if(outcome == 'heart attack') {
      outcome_order <- outcome_state
      outcome_order[,11] <- as.numeric(outcome_order[,11])
      outcome_order <- outcome_order[which(!is.na(outcome_order[,11])),]
      outcome_order <- outcome_order[order(outcome_order[,11], outcome_order[,2]),]
      return(outcome_order[1,2])
    }
    if(outcome == 'heart failure') {
      outcome_order <- outcome_state
      outcome_order[,17] <- as.numeric(outcome_order[,17])
      outcome_order <- outcome_order[which(!is.na(outcome_order[,17])),]
      outcome_order <- outcome_order[order(outcome_order[,17], outcome_order[,2]),]
      return(outcome_order[1,2])
    }
    if(outcome == 'pneumonia') {
      outcome_order <- outcome_state
      outcome_order[,23] <- as.numeric(outcome_order[,23])
      outcome_order <- outcome_order[which(!is.na(outcome_order[,23])),]
      outcome_order <- outcome_order[order(outcome_order[,23], outcome_order[,2]),]
      return(outcome_order[1,2])
    }
    else stop("invalid outcome")
  }
  else stop("invalid state")
}

test <- outcome[outcome$State=='MD',]
test[,11] <- as.numeric(test[,11])
test_noNA <- test[which(!is.na(test[,11])),]
test_noNA <- test_noNA[order(test_noNA[,11], test_noNA[,2]),]

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")

# 1.
best("SC", "heart attack")
# 2.
best("NY", "pneumonia")
# 3.
best("AK", "pneumonia")

# Part 3
rankhospital <- function(state, outcome, num='best') {
  ## Read outcome data
  outcome_measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  if(state %in% unique(outcome_measures$State)) {
    outcome_state <- outcome_measures[outcome_measures$State == state,]
    # determine the rank
    if(num=='best') {
      rank <- 1
      # print(rank)
    }
    else if(num == 'worst') {
      rank <- nrow(outcome_state)
      # print(rank)
    } 
    else if(num <= nrow(outcome_state)) {
      rank <- num
    }
    else return(NA)
    # validate outcome
    if(outcome == 'heart attack') {
      outcome_order <- outcome_state
      outcome_order[,11] <- as.numeric(outcome_order[,11])
      outcome_order <- outcome_order[which(!is.na(outcome_order[,11])),]
      outcome_order <- outcome_order[order(outcome_order[,11], outcome_order[,2]),]
      if (num=='worst') {
        rank <- nrow(outcome_order)
        # print(rank)
      } else rank
      return(outcome_order[rank,2])
    }
    if(outcome == 'heart failure') {
      outcome_order <- outcome_state
      outcome_order[,17] <- as.numeric(outcome_order[,17])
      outcome_order <- outcome_order[which(!is.na(outcome_order[,17])),]
      outcome_order <- outcome_order[order(outcome_order[,17], outcome_order[,2]),]
      if (num=='worst') {
        rank <- nrow(outcome_order)
      } else rank
      return(outcome_order[rank,2])
    }
    if(outcome == 'pneumonia') {
      outcome_order <- outcome_state
      outcome_order[,23] <- as.numeric(outcome_order[,23])
      outcome_order <- outcome_order[which(!is.na(outcome_order[,23])),]
      outcome_order <- outcome_order[order(outcome_order[,23], outcome_order[,2]),]
      if (num=='worst') {
        rank <- nrow(outcome_order)
      } else rank
      return(outcome_order[rank,2])
    }
    else stop("invalid outcome")
  }
  else stop("invalid state")
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")

# 4.
rankhospital("NC", "heart attack", "worst")
# 5.
rankhospital("WA", "heart attack", 7)
# 6.
rankhospital("TX", "pneumonia", 10)
# 7.
rankhospital("NY", "heart attack", 7)

# Part 4
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv")
  
  ## Check that outcome is valid
  if (!((outcome == "heart attack") | (outcome == "heart failure")
        | (outcome == "pneumonia"))) {
    stop ("invalid outcome")
  }
  
  ## For each state, find the hospital of the given rank
  col <- if (outcome == "heart attack") {
    11
  } else if (outcome == "heart failure") {
    17
  } else {
    23
  }
  
  data[, col] <- suppressWarnings(as.numeric(levels(data[, col])[data[, col]]))
  data[, 2] <- as.character(data[, 2])
  
  # Generate an empty vector that will be filled later, row by row, to 
  # generate the final output.
  output <- vector()
  
  states <- levels(data[, 7])
  
  for(i in 1:length(states)) {
    statedata <- data[grep(states[i], data$State), ]
    orderdata <- statedata[order(statedata[, col], statedata[, 2], 
                                 na.last = NA), ]
    hospital <- if(num == "best") {
      orderdata[1, 2]
    } else if(num == "worst") {
      orderdata[nrow(orderdata), 2]
    } else{
      orderdata[num, 2]
    }
    output <- append(output, c(hospital, states[i]))
  }
  
  ## Return a data frame with the hospital names and the (abbreviated) 
  ## state name
  output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
  colnames(output) <- c("hospital", "state")
  rownames(output) <- states
  
  output
}

head(rankall("heart attack", 20), 10)
tail(rankall("pneumonia", "worst"), 3)
tail(rankall("heart failure"), 10)

# 8.
r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)

# 9.
r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)

# 10.
r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)
