## The function reads the "Outcome-of-care-measures" dataset and returns a 2-column 
## data frame containing the hospital in each state that has the ranking specified in num. 
## Hospitals that do not have data on a particular outcome are excluded from the set of
## hospitals when deciding the rankings. Ties are broken in the same way as in the rankhospital.R
## Note: because of the task and for the purpose of efficiency the rankhospital function is not called here

rankall <- function(outcome, num = "best") {
	## Read outcome data
	data <- read.csv("outcome-of-care-measures.csv", colClasses  = "character")
	
	## Check that the outcome is a valid argument
	validOutcomes <- c("heart attack", "heart failure", "pneumonia");
	if (!outcome %in% validOutcomes) {
		stop("invalid outcome")			
	}	
	
	## Goal: for each state, find the hospital of the given rank
	## Columns: 
	hospitalCol <- 2
	stateCol <- 7
	deathReasonPctCol <- 23 # Pneumonia column by default
	
	if (outcome=="heart attack") {
		deathReasonPctCol <- 11	
	}
 	else if (outcome=="heart failure") {
		deathReasonPctCol <- 17
	}	
	deathReasonPctColName = colnames(data)[deathReasonPctCol];	
	
	## Shrink data 
	data <- data[, c(deathReasonPctCol, hospitalCol, stateCol)]
	
	## Parsing death reason percentage column to integer
	suppressWarnings(data [, 1] <- as.numeric(data [, 1]))
	dataOmitted <- data[!is.na(data[1]), c(1, 2, 3)]
	splitByStates <- split(dataOmitted, dataOmitted$State) 
	hospitals <- character();
	states <- character();

	stateHospitalMapping <- lapply(splitByStates, 
		function(state) {
			val <- NA
			if (num=="best") {
				minVal <- min(state [, 1])
				## If there are other mins, break the ties lexicographically
				bestHospitals <- state[state[, 1]==minVal, c(1, 2, 3)]
				val <- min(bestHospitals[, 2])
			} 
			else if (num=="worst") {	
				maxVal <- max(state [, 1])
				## If there are other mins, break the ties lexicographically
				worstHospitals <- state[state[, 1]==maxVal, c(1, 2, 3)]
				val <- max(worstHospitals[, 2])
			} else {
				## In this case num expected to be an integer number indicating the rank
				if (num <= nrow(state)) {
					cols <- colnames(state);
					sortedData <- state[order(state[cols[1]], state[cols[2]]), ]
					val <- c(sortedData [num, 2])
				}
			}
			states <<- rbind(states, unique(state$State));
			hospitals <<- rbind(hospitals, c(val));
		}
	)
	
	## Return a data frame with the hospital names and the (abbreviated) state name
	data.frame(hospital=hospitals, state=states)
}

## Test cases:
## + head(rankall("heart attack", 20), 10)
## + tail(rankall("pneumonia", "worst"), 3)
## + tail(rankall("heart failure"), 10)
## + rankall("heart attack", 4)
## + rankall("pneumonia", "worst") 
## + rankall("heart failure", 10)
## + All passed