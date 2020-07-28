complete <- function(directory,id = 1:332){
	files <- list.files(directory)
	
	data <- list()
	
	for (i in id){
		data[[paste(i)]] <- read.csv(paste(directory,files[i],sep="/"))
	}
	
	complete <- data.frame(id=id,nobs=0)
	
	for (i in 1:length(id)){
		complete[i,"nobs"] <- sum(!is.na(data[[paste(id[i])]]$sulfate) & !is.na(data[[paste(id[i])]]$nitrate))
	}
	
	complete
}
