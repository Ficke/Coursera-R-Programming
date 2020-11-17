# Author: Adam Ficke
# R Programming Week 4 Course Project

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)

#plot histogram of 30-day deaths from heart attack

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

# a function called best that take two arguments: the 2-character abbreviated name of a state and an
# outcome name. The function reads the outcome-of-care-measures.csv file and returns a character vector
# with the name of the hospital that has the best (i.e. lowest) 30-day mortality for the specified outcome
# in that state. 
outcome <- "heart attack"
state <- "MD"
best <- function(state, outcome) {
        require(dplyr)
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        keep.cols <- c(2,7,11,17,23)
        df.less <- df[,keep.cols]
        #rename columns
        colnames(df.less) <- c("Name","State","heart attack", "heart failure","pneumonia")
        
        #sort by alpha
        df.less <- df.less[order(df.less$Name),]
        
        ## Check that state and outcome are valid
        #check input state 
        if (state %in% df.less$State == FALSE ){
                stop("invalid state")
        }
        #check input outcome
        if (outcome %in% names(df.less[,3:5]) == FALSE ){
                stop("invalid outcome")
        }
                

        ## Return hospital name in that state with lowest 30-day death
        ## rate
        result <-  df.less %>% 
                filter(State == state & !!as.symbol(outcome) != "Not Available") %>%
                slice_max(!!as.symbol(outcome))
        result$Name
}
        
