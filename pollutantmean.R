pollutantmean <- function(directory,pollutant,id = 1:332){
	files <- list.files(directory)
	
	example <- read.csv(paste(directory,files[1],sep="/"),header=TRUE,nrows=1)
	
	data <- as.data.frame(matrix(nrow=0,ncol=ncol(example)))
	colnames(data) <- colnames(example)
	
	for (i in id){
		data <- rbind.data.frame(data,read.csv(paste(directory,files[i],sep="/")))
	}
	
	mean <- mean(data[[pollutant]],na.rm=TRUE)
	mean
}
