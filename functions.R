add2 <- function(x,y) {
	x + y
}

above10 <- function(x) {
	x[x>10]
}

columnmean <- function(y) {
	means <- numeric(ncol(y))
	for (i in 1:ncol(y)){
		means[i] <- mean(y[,i])
	}
	means
}


