
outcome <- read.csv("data/outcome-of-care-measures.csv", 
                    header = T, colClasses = "character");
head(outcome)
ncol(outcome)
names(outcome)

outcome[,11] <- as.numeric(outcome[,11]);
hist(outcome[,11])