absolute <- function(layerList = list(), rowN = c(1,2,3), colN = 1) {

	sapply(layerList, function(z) z[rowN, colN])
	
}

