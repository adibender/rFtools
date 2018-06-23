barplot.LevelsVsLayerByMAF <- function(height = list(), col = 1:3, beside = TRUE, 
		 legend.text = 1:3, cex.axis = 0.8, names.arg = NULL, main = NULL, ...) {
	
	names <- names(height)
	
	layout(matrix(1:length(height), ncol = 2, byrow = TRUE))
	
	for ( i in 1:length(height) ) {
		
		if ( !is.null(main) ) {
			if ( length(main) != length(names) ) { 
				warning("argument 'main' has wrong length. Using default names.")
				main_i <- names[i]
			}
			else main_i <- main[i] 
		}
		else main_i = names[i]
		
		tab_maf.i <- height[[i]]
		if ( is.null(names.arg) ) names.arg = 1:ncol(tab_maf.i)
		
		barplot(tab_maf.i, col = col, beside = beside, main = main_i, 
			legend.text = 1:3, names.arg = names.arg,  cex.axis = cex.axis, ...)
	
	}
	
}

