rankhospital <- function(stateinp, outcomeinp, num = "best") { ## Read outcome data
 
  ## Read outcome data
  
  column <- NULL
  vrankhospital <-NULL
  resultsperstate <- NULL
  minlist <- NULL
  
  outcome <- read.csv("outcome-of-care-measures.csv",colClasses="character") 
  
  ## Check that state and outcome are valid
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
  
  if(length(which(outcome[,7]==stateinp))==0) {
    print("invalid state")
    stop()
  }
  
  ## Return hospital name in that state with the given rank ## 30-day death rate
  resultsperstate <- split(outcome,outcome$State)
  ranking <- sort(as.numeric(resultsperstate[[stateinp]][,column]),na.last=NA,decreasing=FALSE)
  
  
  if(num =="best") {
    rank <- 1
  }
  else if(num =="worst"){
    rank <- length(ranking)

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
  
  
  
  ties <- which(as.numeric(resultsperstate[[stateinp]][,column])==ranking[rank])
  rankindex <- which(ranking==ranking[rank])

  if(length(ties)>=1){
    
    ties_order <- sort(resultsperstate[[stateinp]][ties,2])
    vrankhospital <- ties_order[rank-rankindex[1]+1]
  }
  else
    {
      ties_order <- ties
      
    }
  vrankhospital
    
}
  
  

