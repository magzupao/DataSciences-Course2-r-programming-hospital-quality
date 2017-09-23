rankhospital <- function(state, outcome, num = "best") {
  
  ## read file
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check outcome
  validOutcome = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% validOutcome) { 
    stop("invalid outcome")
  }
  
  ## Check state - we saved to declare a variable
  if (!state %in% data[,7]){
    stop("invalid state")
  }
  
  if (outcome == "heart attack") {
    data_column <- 11	## Column name = Hospital 30-Day Death (Mortality) Rates from Heart Attack
  }
  else if (outcome == "heart failure") {
    data_column <- 17	## Column name = Hospital 30-Day Death (Mortality) Rates from Heart Failure
  }
  else {
    data_column <- 23	## Column name = Hospital 30-Day Death (Mortality) Rates from Pneumonia
  }	
  
  newdata <- data[data$State==state,]	
  newdatasorted <- newdata[order( suppressWarnings(as.numeric(newdata[,data_column])),newdata["Hospital.Name"],decreasing=FALSE,na.last=NA), ]
  ## suppressWarnings
  ## not print
  ## Warning message: NAs introduced by coercion 
  
  if (num=="best"){
    num = 1
  }
  if (num=='worst'){
    num = nrow(newdatasorted)
  }
  
  newdatasorted[num,"Hospital.Name"]
}