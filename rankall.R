## The function reads the "Outcome-of-care-measures" dataset and returns a 2-column 
## data frame containing the hospital in each state that has the ranking specified in num. 
## Hospitals that do not have data on a particular outcome are excluded from the set of
## hospitals when deciding the rankings. Ties are broken in the same way as in the rankhospital.R

rankall <- function(outcome, num = "best") {
	## Read outcome data
	
	## Check that state and outcome are valid
	
	## For each state, find the hospital of the given rank
	
	## Return a data frame with the hospital names and the (abbreviated) state name
}