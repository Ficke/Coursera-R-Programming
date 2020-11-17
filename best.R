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