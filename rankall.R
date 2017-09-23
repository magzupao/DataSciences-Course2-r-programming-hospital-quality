rankall <- function() {
  
  ## load state
  validState = sort(unique(data[,7]))
  
  data_column <- 11 ## Column name = Hospital 30-Day Death (Mortality) Rates from Heart Attack
  
  hospital<-character(0)
  
  for (i in seq_along(validState)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    newdata <- data[data$State==validState[i],]
    # order data by outcome
    newdatasorted <- newdata[order(suppressWarnings(as.numeric(newdata[,data_column])),newdata[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    # best hospital = 1
    hospital[i] <- newdatasorted[1,"Hospital.Name"]
  }
  ## Return a data frame with the hospital names and the (abbreviated) state name
  data.frame(hospital=hospital,state=validState,row.names=validState)   
  
}

#test function
#head(rankall())