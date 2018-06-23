getLayerForest <- function( ancestryForestObj = list() ) {
	
	sapply(ancestryForestObj, function(z) c(1, sapply(z[-1], length) + 1))
	
}

