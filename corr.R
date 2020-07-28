corr <- function(directory,threshold = 0){
	completecases <- complete(directory)
	abovethreshold <- completecases$id[which(completecases$nobs>threshold)]
	
	files <- list.files(directory)
	data <- list()
	for (i in abovethreshold){
		data[[paste(i)]] <- read.csv(paste(directory,files[i],sep="/"))
	}
	
	corr <- as.numeric(vector(length=length(abovethreshold)))
	if (length(abovethreshold)==0){
		print("None above threshold")
	} else {
		for (i in 1:length(abovethreshold)){
			corr[i] <- cor(data[[paste(abovethreshold[i])]]$sulfate,data[[paste(abovethreshold[i])]]$nitrate,use="complete.obs",method="pearson")
		}
		
		names(corr) <- abovethreshold
	}
	corr
}
