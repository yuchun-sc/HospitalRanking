## this function finds the best hospital in a state 
## based on the outcome measure, if there is a tie, 
## the first one based on the alphabatic order is returned 

best <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", 
                        header = T);
    
    ## check if the input state name is valid
    stateNames <- as.factor(data$State);
    
    if(!(state %in% stateNames)) {
        stop("invalid state"); 
    }
    
    ## check if the input outcome measure is valid 
    validOutcome <- list(c(11, 17, 23), 
                         c("heart attack", "heart failure", "pneumonia"));
    
    if(!(outcome %in% validOutcome[[2]])) {
        stop("invalid outcome");    
    } 
    
    dataState <- data[data$State == state, ];
    colNo <- validOutcome[[1]][match(outcome, validOutcome[[2]])];
    dataState <- dataState[!is.na(dataState[,colNo]),];
    dataState[,colNo] <- as.numeric(dataState[,colNo]);
    winner <- min(dataState[,colNo]);
    winHosp <- as.character(dataState[dataState[,colNo] == winner,2]);
    sort(winHosp);
    winHosp[1]
}