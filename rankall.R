#return dataframe with the nth ranked hospital in every state 

outcome<- "pneumonia"
num = 60
require(dplyr)
rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        keep.cols <- c(2,7,11,17,23)
        df.less <- df[,keep.cols]
        
        #rename columns
        colnames(df.less) <- c("Name","State","heart attack", "heart failure","pneumonia")
        
        ## Check that state and outcome are valid
        #check input outcome
        if (outcome %in% names(df.less[,3:5]) == FALSE ){
                stop("invalid outcome")
        }

        ## For each state, find the hospital of the given rank
        mortality <-  df.less %>%
                filter(!!as.symbol(outcome) != "Not Available")
        
        mortality[,outcome] <- as.numeric(mortality[,outcome])
        
        #handle best vs worst - still figuring out when num > number of hospitals
        if (num == "best"){
                ordered <- mortality %>% 
                        group_by(State) %>%
                        arrange(!!as.symbol(outcome), .by_group = TRUE) %>%
                        slice_head()
        }
        if (num =="worst"){
                ordered <- mortality %>% 
                        group_by(State) %>%
                        arrange(!!as.symbol(outcome), .by_group = TRUE) %>%
                        slice_tail()
        } else {
                ordered <- mortality %>% 
                        group_by(State) %>%
                        arrange(!!as.symbol(outcome), .by_group = TRUE) %>%
                        slice(num)
        }
        
        #build dataframe with all states and <NA>s 

        output <- data.frame("state"=unique(df$State))
        colnames(ordered) <- c("hospital","state","heart attack", "heart failure","pneumonia")
        left_join()
        print(ordered$State)
        
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}