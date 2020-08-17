rankhospital <- function(state, outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	
	
	options <- c("heart attack","heart failure","pneumonia")
	options_adj <- c("Heart.Attack","Heart.Failure","Pneumonia")
	
	
	if (state%in%data$State == FALSE) {
		stop("invalid state")
	}
	if (outcome%in%options == FALSE) {
		stop("invalid outcome")
	}
	if (num <= 0) {
		stop("invalid ranking")
	}
	
	
	outcome_adj <- options_adj[grep(outcome,options)]
	outcome_col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome_adj,sep="")
	data[,outcome_col] <- suppressWarnings(as.numeric(data[,outcome_col]))
	
	
	state_outcome <- data[which(data$State==state),c("State","Hospital.Name",outcome_col)]
	state_outcome <- state_outcome[order(state_outcome[,outcome_col],state_outcome[,"Hospital.Name"]),]
	state_outcome <- na.omit(state_outcome)
	
	
	if (num == "best") {
		num <- 1
	} else if (num == "worst") {
		num <- nrow(state_outcome)
	}
	
	
	if (num > nrow(state_outcome)) {
		print(NA)
	} else {
		state_outcome[num,"Hospital.Name"]
	}

	
}


