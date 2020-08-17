rankall <- function(outcome, num = "best") {
	data <- read.csv("outcome-of-care-measures.csv",colClasses="character")
	
	
	options <- c("heart attack","heart failure","pneumonia")
	options_adj <- c("Heart.Attack","Heart.Failure","Pneumonia")
	
	
	if (outcome%in%options == FALSE) {
		stop("invalid outcome")
	}
	if (num <= 0) {
		stop("invalid ranking")
	}
	
	
	outcome_adj <- options_adj[grep(outcome,options)]
	outcome_col <- paste("Hospital.30.Day.Death..Mortality..Rates.from.",outcome_adj,sep="")
	data[,outcome_col] <- suppressWarnings(as.numeric(data[,outcome_col]))
	
	
	bystate <- split(data[,c("State","Hospital.Name",outcome_col)],as.factor(data[,"State"]))
	bystate <- lapply(bystate,function(x) x[order(x[,outcome_col],x[,"Hospital.Name"]),])
	bystate <- lapply(bystate,na.omit)
	
	
	if (num == "best") {
		num <- rep(1,length(bystate))
	} else if (num == "worst") {
		num <- sapply(bystate,nrow)
	} else {
		num <- rep(num,length(bystate))
	}
	
	
	output <- data.frame(hospital=rep(NA,length(bystate)),state=rep(NA,length(bystate)))
	row.names(output) <- names(bystate)
	
	
	for (i in 1:length(bystate)) {
		if (num[i] > nrow(bystate[[i]])) {
			output[i,] <- c(NA,names(bystate)[i])
		} else {
			output[i,] <- bystate[[i]][num[i],c("Hospital.Name","State")]
		}
	}
	
	
	output
	
}
