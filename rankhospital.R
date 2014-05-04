## The function reads the "Outcome-of-care-measures" dataset and returns 
## a character vector with the name of the hospital that has the ranking 
## specified by the num argument. The num argument can take values "best", 
"worst", or an integer indicating the ranking (smaller number - higher rank)

rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses  = "character")
	
	## Check that state and outcome are valid
	states <- unique(data$State)
	if (!state %in% states) {
		stop("invalid state")
	}
	
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if (!outcome %in% validOutcomes) {
		stop("invalid outcome")			
	}	

	## Return hospital name in that state with the given rank 30-day death rate
	
}