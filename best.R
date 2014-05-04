## The function reads the Outcome-of-care-measures dataset and returns a character 
## vector with the name of the hospital that has the best (i.e. lowest) 30-day 
## mortality for the specied outcome in that state. The hospital name is the name 
## provided in the Hospital.Name variable. The outcomes can be one of "heart attack", 
## "heart failure", or "pneumonia". Hospitals that do not have data on a particular
## outcome are be excluded from the set of hospitals when deciding the rankings.
## Ties are broked lexicographically

best <- function(state, outcome) {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses  = "character")
	
	## Check that the state argument is valid
	states <- unique(data$State)
	if (!state %in% states) {
		stop("invalid state");
	}
	
	## Check that the outcome argument is valid
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if (!outcome %in% validOutcomes) {
		stop("invalid outcome");			
	}
	
	## Find hospital name in that state with lowest 30-day death rate
	## Columns
	hospitalCol <- 2
	stateCol <- 7
	deathReasonPctCol <- 23 # Pneumonia column by default
	
	if (outcome=="heart attack") {
		deathReasonPctCol <- 11;	
	}
 	else if (outcome=="heart failure") {
		deathReasonPctCol <- 17;
	}
	
	## Shrink data to the observed state
	stateData <- data[data$State==state, c(hospitalCol, deathReasonPctCol)]
	
	## Parsing death reason percentage column to integer
	suppressWarnings(stateData [, 2] <- as.numeric(stateData [, 2]))
	stateDataOmitted <- stateData[!is.na(stateData[2]), c(1, 2)]
	minVal <- min(stateDataOmitted [, 2])
	
	## if there are other mins, break the ties lexicographically
	bestHospitals <- stateDataOmitted[stateDataOmitted[, 2]==minVal, c(1, 2)]
	best <- min(bestHospitals[, 1])
	best
}