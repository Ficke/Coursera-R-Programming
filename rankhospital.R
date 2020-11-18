#a function to return the nth rank hospital in a given state 
#for a specified health measure 

state = "NY"
outcome = "pneumonia"
num = 2

rankhospital <- function(state, outcome, num = "best") {
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
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        mortality <-  df.less %>%
                filter(State == state & !!as.symbol(outcome) != "Not Available")
        #convert to numeric
        mortality[,outcome] <- as.numeric(mortality[,outcome])
        
        #alphabetize 
        mortality <- mortality[order(mortality$Name),]
        
        #rank by mortality
        mortality <- mortality[order(mortality[outcome]),]
        
        if (num == "best"){
                num = 1
        }
        if (num == "worst"){
                num = length(mortality$Name)
        }
        if (num > length(mortality$Name)){
                return(NA)
        }

        mortality[num,]$Name
        
}
