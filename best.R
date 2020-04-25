## The 'best' function reads the outcome-of-care-measures.csv ???le and returns a
## character vector with the name of the hospital that has the best (i.e. lowest) 
## 30-day mortality for the speci???ed outcome in that state.
## The function takes two arguments: the 2-character abbreviated name of a state and an outcome name.

best <- function(stateinp, outcomeinp) { 
  ## Read outcome data
  
  column <- NULL
  hosp_names <-NULL
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

  ## Return hospital name in that state with lowest 30-day death ## rate
  
  resultsperstate <- split(outcome,outcome$State)
  ##outcomeout <- data.frame(resultsperstate[[stateinp]][,2],resultsperstate[[stateinp]][,column]) 
  minlist <- which(as.numeric(resultsperstate[[stateinp]][,column])==min(as.numeric(resultsperstate[[stateinp]][,column]),na.rm=TRUE))
  hosp_names <- resultsperstate[[stateinp]][minlist,2]
  min(hosp_names)
  
}