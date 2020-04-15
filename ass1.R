ass1 <- function(){

##reading file
outcome <- read.csv('outcome-of-care-measures.csv',colClasses = 'character')

##changing character vector to numeric
outcome[,11] <- as.numeric(outcome[,11])

##drawing histogram
hist(outcome[,11])

#summary
summary(outcome[,11])

}

