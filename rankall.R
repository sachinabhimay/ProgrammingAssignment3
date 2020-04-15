## takes 2 arguments outcome : 
## 1. outcome : heart attack, heart failure or pneumonia
## 2. num : rank of hospital including best and worst
## retruns a data frame of list of hospitals in different states on that rank



rankall <- function(outcome, num = 'best'){
  
  ##reading data
  data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  ##check that outcome is valid
  if(!outcome %in% c('heart attack', 'heart failure', 'pneumonia')){
    
    message('Invalid outcome')
    stop('invalid state')
    
  }
  
  ##return hospital name in that state with lowest 30 days death rate
  
  out <- 0
  
  if(outcome == 'heart attack'){          ## Selecting column for different outcomes
    
    out <- 11
    data[,11] = as.numeric(data[,11])
  
  }else if(outcome == 'heart failure'){
    
      out <- 17
      data[,17] = as.numeric(data[,17])
  
  }else{
    
      out <- 23 
      data[,23] = as.numeric(data[,23])
  
  }
  
  sl <- split(data, data$State)             ## spliting data into list on the basis of states
  
  if(num == 'best')                         ## Setting row value for different positions
    num = 1
  
  else if(num == 'worst'){
    
    num = 1
    
    l <- as.data.frame(do.call(rbind, lapply(sl, function(x){         ##sapply on state list with fuction defined inside
      
      y <- data.frame(x)
      y <- y[order(-y[,out],y[,2],na.last = NA),]   ##order is set in decs with '-' sign and na.last = NA to remove NAs
      c(y[num,2],y[num,7])                                        ##returing 1 row in hospital name column
    
      })),stringsAsFactors = FALSE)
    
    names(l) <- c('hospital','state')               ##adding name to data.frame
    
    return(l)
    
  }else{
    
    num <- as.numeric(num)                        ## changing character to number for num
  
    }
  
  l <- as.data.frame(do.call(rbind, lapply(sl, function(x){         ##same as with worst case except order is asc
                                                                    ##do.call(rbind, ...) is used to converts 
                                                                    ##elements of list into rows of data frame
    y <- data.frame(x)
    y <- y[order(y[,out],y[,2],na.last = NA),]
    c(y[num,2],y[num,7])
  
    })), stringsAsFactors = FALSE)
  
  names(l) <- c('hospital','state')
  l

}





