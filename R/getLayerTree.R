getLayerTree <- function( ancestryTreeObj = list() ) {
	
	c(1, sapply(ancestryTreeObj[-1], length) + 1)
	
}

