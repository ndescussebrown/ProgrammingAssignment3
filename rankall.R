rankall <- function(outcomeinp, num="best") { 
  ## Read outcome data
  
  column <- NULL
  hosp_names <-NULL
  resultsperstate <- NULL
  j <- NULL

  
  outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character") 
  
  if(outcomeinp=="heart attack") {
    column <- 11
  }
  else if(outcomeinp=="heart failure"){
    column <- 17
  }
  else if(outcomeinp=="pneumonia") {
    column <- 23
  }
  else {
    print("invalid outcome")
    stop()
  }
  
  resultsperstate <- split(outcome,outcome$State)
  dfrankall <- data.frame(hospital=rep(NA,54),state=rep(NA,54))
  
  
  ## For each state, find the hospital of the given rank
  
  for (j in 1:54) {
    ties_order <- NULL
    ties <- NULL
    rank <- NULL
    

  ranking <- sort(as.numeric(resultsperstate[[j]][,column]),na.last=NA,decreasing=FALSE)
  
  
  if(num =="best") {
    rank <- 1
  }
  else if(num =="worst"){
    rank <- length(ranking)
    if(j == 52) {print(rank)}

  }
  else if(num > length(ranking)) {
    rank <- NA
  }
  else if(num <= length(ranking)) {
    rank <- num
  }
  else {
    print("invalid num")
    stop()
  }
  
  
  
  ties <- which(as.numeric(resultsperstate[[j]][,column])==ranking[rank])
  rankindex <- which(ranking==ranking[rank])
  
  if(j==52)
  {print(ranking)}
  
  if(length(ties)>=1){
    
    ties_order <- sort(resultsperstate[[j]][ties,2])
    ##dfrankall[j,1] <- ties_order[1]
    dfrankall[j,1] <- ties_order[rank-rankindex[1]+1]
    dfrankall[j,2] <- resultsperstate[[j]][1,7]
    }
  else
  {
    ties_order <- ties

  }

  }
  
  ## Return a data frame with the hospital names and the ## (abbreviated) state name
  
  dfrankall
  
  
}