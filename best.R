## Takes two parameters (name of state, outcome name)
## name of state: 2 character abb name of state
## outcome: one of three, heart attack, heart failure, pneumonia
## retuns the list of best hospital by alphabetical order if tied

best <- function(state, outcome){
  ##read outcome data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  ##check that state and outcome are valid
  if(!state %in% data[,7]){
    stop('invalid state')
  }
  if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
    message('Invalid outcome')
    stop('invalid state')
  }
  
  ##return hospital name in that state with lowest 30 days death rate
  ## hospital in that state
  present <- data$State == state
  data_state = data[present,]
  out <- -1
  if(outcome == 'heart attack'){
    out <- 11
    data_state[,11] = as.numeric(data_state[,11])
  }else if(outcome == 'heart failure'){
    out <- 17
    data_state[,17] = as.numeric(data_state[,17])
  }else{
    out <- 23 
    data_state[,23] = as.numeric(data_state[,23])
  }
  data_outcome = data_state[order(data_state[,out],data_state[,2]),]
  
  data_outcome[1,2]
  
}