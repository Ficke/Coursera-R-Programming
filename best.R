best <- function(state, outcome) {
        require(dplyr)
        ## Read outcome data
        df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        keep.cols <- c(2,7,11,17,23)
        df.less <- df[,keep.cols]
        #rename columns
        colnames(df.less) <- c("Name","State","heart attack", "heart failure","pneumonia")
        
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
        mortality <-  df.less %>%
                filter(State == state & !!as.symbol(outcome) != "Not Available")
        #convert to numeric
        mortality[,outcome] <- as.numeric(mortality[,outcome])
        #find lowest mortality rate
        mortality.min <- mortality %>%
                slice_min(!!as.symbol(outcome))
        #sort alphabetically in case there are multiple
        mortality.min <- mortality.min[order(mortality.min$Name),]
        mortality.min$Name
}