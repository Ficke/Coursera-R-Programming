#return dataframe with the nth ranked hospital in every state 

outcome<- "pneumonia"
num = 60
require(tidyverse)
rankall <- function(outcome, num = "best") {
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
        
        #filter out NAs 
        mortality <-  df.less %>%
                filter(!!as.symbol(outcome) != "Not Available")
        
        #alphabetize 
        mortality <- mortality[order(mortality$Name),]
        
        #convert mortality stats so we can rank order later 
        mortality[,outcome] <- as.numeric(mortality[,outcome])
        
        
        #handle best vs worst
        if (num == "best"){
                ordered <- mortality %>% 
                        group_by(State) %>%
                        arrange(!!as.symbol(outcome), .by_group = TRUE) %>%
                        slice_head()
        }
        else if (num =="worst"){
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
        
        #sort by abbreviation
        output <- output %>% 
                arrange(state)
        
        #align column names
        colnames(ordered) <- c("hospital","state","heart attack", "heart failure","pneumonia")
        
        #leftjoin to total dataframe 
        hospitalState <- left_join(output,ordered,by="state")[,c(1,2)]
        
        #replace NAs with <NA>
        hospitalState <- hospitalState %>% column_to_rownames(., var = "state") %>%
                replace_na(list(hospital = "<NA>", y = "<NA>"))

        #add back state column
        hospitalState <- cbind(hospitalState,state = rownames(hospitalState))
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        hospitalState
}