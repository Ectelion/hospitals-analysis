## The function reads the "Outcome-of-care-measures" dataset and returns 
## a character vector with the name of the hospital that has the ranking 
## specified by the num argument. The num argument can take values "best", 
## "worst", or an integer indicating the ranking (smaller number - higher rank)

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
	stateData <- data[data$State==state, c(deathReasonPctCol, hospitalCol)]
	
	## Parsing death reason percentage column to integer
	suppressWarnings(stateData [, 1] <- as.numeric(stateData [, 1]))
	stateDataOmitted <- stateData[!is.na(stateData[1]), c(1, 2)]
	
	if (num=="best") {
		minVal <- min(stateDataOmitted [, 1])
		## If there are other mins, break the ties lexicographically
		bestHospitals <- stateDataOmitted[stateDataOmitted[, 1]==minVal, c(1, 2)]
		best <- min(bestHospitals[, 2])
		best
	} 
	else if (num=="worst") {
		maxVal <- max(stateDataOmitted [, 1])
		## If there are other hospitals with the max value, 
		## break the ties lexicographically
		worstHospitals <- stateDataOmitted[stateDataOmitted[, 1]==maxVal, c(1, 2)]
		worst <- max(worstHospitals[, 2])
		worst
	} else {
		## In this case num expected to be an integer number indicating the rank
		if (num <= nrow(stateDataOmitted)) {
			cols <- colnames(stateDataOmitted);
			sortedData <- stateDataOmitted[order(stateDataOmitted[cols[1]], stateDataOmitted[cols[2]]), ]
			sortedData [num, 2]
		}
		else {
			NA
		}
	}
}

## Test cases:
## + rankhospital("NC", "heart attack", "worst")
## + rankhospital("NY", "heart attak", 7)
## + rankhospital("WA", "pneumonia", 1000)
## + rankhospital("WA", "heart attack", 7)
## + rankhospital("MD", "heart attack", "worst")
## + rankhospital("MN", "heart attack", 5000)
## + rankhospital("TX", "heart failure", 4)
## + All passed