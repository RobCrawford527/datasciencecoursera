best <- function(state,outcome=c("heart attack","heart failure","pneumonia")) {
	data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	
	
	options <- c("heart attack","heart failure","pneumonia")
	options_adj <- c("Heart.Attack","Heart.Failure","Pneumonia")
	
	
	if (state%in%data$State == FALSE) {
		stop("invalid state")
	}
	if (outcome%in%options == FALSE) {
		stop("invalid outcome")
	}
	
	
	outcome_adj <- options_adj[grep(outcome,options)]
	outcome_col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome_adj,sep="")
	data[,outcome_col] <- suppressWarnings(as.numeric(data[,outcome_col]))
	

	min_rate <- suppressWarnings(min(data[which(data$State==state),outcome_col],na.rm=TRUE))
	
	
	hospital <- data$Hospital.Name[which(data$State==state & data[,outcome_col]==min_rate)]
	if (length(hospital)>1) {
		hospital <- hospital[order(hospital,decreasing=FALSE)[1]]
	}
	hospital
	
}
