plot.LevelsVsLayerByMAF <- function(x = list(), 
		main_vec = paste("MAF =", c("0.05", "0.10", "0.25", "0.40")), ...) {
	
	if ( is.null(main_vec) ) main_vec <-  names(x)
	length_x <- length(x)
	
	layout( matrix(c(1:length_x), byrow = TRUE, ncol = 2) )
	for ( i in seq_len(length_x) ) { 
		plot.LevelsVsLayer(x[[i]], main = main_vec[i], ...)
	}
	
}

