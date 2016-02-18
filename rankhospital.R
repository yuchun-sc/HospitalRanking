# This function take 3 inputs: the 2-character abbreviated name of a
# state (state), an outcome (outcome), and the ranking of a hospital in that state for that outcome (num).
# The function reads the outcome-of-care-measures.csv file and returns a character vector with the name
# of the hospital that has the ranking specified by the num argument. For example, the call
# rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 5th lowest 30-day death rate
# for heart failure. The num argument can take values “best”, “worst”, or an integer indicating the ranking
# (smaller numbers are better). If the number given by num is larger than the number of hospitals in that
# state, then the function should return NA

rankhospital <- function(state, outcome, num = "best") {
    # read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", 
                     header = T);
    
    # check if the input state name is valid
    stateNames <- as.factor(data$State);
    
    if(!(state %in% stateNames)) {
        stop("invalid state"); 
    }
    
    # check if the input outcome measure is valid 
    validOutcome <- list(c(11, 17, 23), 
                         c("heart attack", "heart failure", "pneumonia"));
    if(!(outcome %in% validOutcome[[2]])) {
        stop("invalid outcome");    
    }
    
    # check if the input num is valid 
    validNum <- c("best", "worst"); 
    if (!(num %in% validNum) & !is.integer(as.integer(num))) {
        stop("invalid num");    
    }
    
    dataState <- data[data$State == state, ];
    colNo <- validOutcome[[1]][match(outcome, validOutcome[[2]])];
    
    dataState <- dataState[!is.na(dataState[,colNo]),];
    
    dataState[,colNo] <- as.numeric(dataState[,colNo]);
    
    
    #print(length(dataState[,colNo]))
    #print(length(dataState[,"Hospital.Name"]))
    
    dataState <- dataState[order(dataState[,colNo], 
                                 dataState[,"Hospital.Name"]), ]
    # print(dataState[,2]);
    if (is.integer(as.integer(num))) {
        if (num <= 0 || num > length(dataState[,1])) {
            result <- NA;
        } else {
            result <- dataState[as.integer(num),2];
        }
    } 
    
    if (num == "best") {
        result <- dataState[1,2];
    } 
    
    if (num == "worst") {
        result <- dataState[length(dataState[,1]),2];
    }
    
    result <- as.character(result)
    result;
}