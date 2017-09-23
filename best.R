best <- function(state, outcome) {

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
  
  newdata <- data[data$State==state,]
  idRegistro <- which.min(suppressWarnings(as.double(newdata[,data_column])))
  ## suppressWarnings
  ## not print
  ## Warning message: NAs introduced by coercion 
  newdata[idRegistro,"Hospital.Name"]	
  
}