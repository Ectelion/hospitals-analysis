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
	## Find hospital name in that state with lowest 30-day death rate
	## Columns
	hospitalCol <- 2
	stateCol <- 7
	deathReasonPctCol <- 23 # Pneumonia column by default
	
	if (outcome=="heart attack") {
		deathReasonPctCol <- 11	
	}
 	else if (outcome=="heart failure") {
		deathReasonPctCol <- 17
	}
	
	## Shrink data to the observed state
	stateData <- data[data$State==state, c(hospitalCol, deathReasonPctCol)]
	
	## Parsing death reason percentage column to integer
	suppressWarnings(stateData [, 2] <- as.numeric(stateData [, 2]))
	stateDataOmitted <- stateData[!is.na(stateData[2]), c(1, 2)]
	
	if (num=="best") {
		minVal <- min(stateDataOmitted [, 2])
		## if there are other mins, break the ties lexicographically
		bestHospitals <- stateDataOmitted[stateDataOmitted[, 2]==minVal, c(1, 2)]
		best <- min(bestHospitals[, 1])
		best
	} 
	else if (num=="worst") {
		maxVal <- max(stateDataOmitted [, 2])
		## if there are other hospitals with the max value, 
		## break the ties lexicographically
		worstHospitals <- stateDataOmitted[stateDataOmitted[, 2]==maxVal, c(1, 2)]
		worst <- max(bestHospitals[, 1])
		worst
	}
}