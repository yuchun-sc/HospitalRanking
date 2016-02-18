


rankall <- function(outcome, num = "best") {
    # read outcome data
    data <- read.csv("data/outcome-of-care-measures.csv", 
                     header = T);
    
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
    
    colNo <- validOutcome[[1]][match(outcome, validOutcome[[2]])];
    
    data <- data[!is.na(data[,colNo]),];
    
    result <- c("a", "a");
    
    for (st in unique(data[,"State"])) {
        
        dataSt <- data[data[,"State"] == st, ];
        dataSt <- dataSt[order(dataSt[,colNo], 
                           dataSt[,"Hospital.Name"]), ];
        
        if (is.integer(as.integer(num))) {
            if (num <= 0 || num > length(dataSt[,1])) {
                temp <- NA;
            } else {
                temp <- dataSt[as.integer(num),c(2,7)];
            }
        } 
        
        if (num == "best") {
            temp <- dataSt[1,c(2,7)];
        } 
        
        if (num == "worst") {
            temp <- dataSt[length(dataSt[,1]),c(2,7)];
        }
        
        result <- rbind(result, temp);
    }
    
    result
}