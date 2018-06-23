plot.LevelsVsLayer <- function(x, main = character(), col_vec = NULL, 
		type_vec = NULL, pch_vec = NULL, legend_text = NULL, xlab = "layer", 
		ylab = "relative frequency", ...) {
	
	tab <- addmargins(x)
	seq_tab <- 1 : (dim(tab)[2] - 1)
	col.sums <- tab[4, seq_tab]
	relFreqCat1 <- tab[1, seq_tab] / col.sums
	relFreqCat2 <- tab[2, seq_tab] / col.sums
	relFreqCat3 <- tab[3, seq_tab] / col.sums
	
	ylim = c(0, max(sapply(list(relFreqCat1, relFreqCat2, relFreqCat3), max)))
	if(is.null(col_vec)) col_vec <- 1:3
	if(is.null(type_vec)) type_vec <- rep("o", 3)
	if(is.null(pch_vec)) pch_vec <- rep(4, 3)
	
	plot(seq_tab, relFreqCat1, ylim = c(0, 1), main = main, xlab = xlab, 
			ylab = ylab, col = col_vec[1], type = type_vec[1],  
			pch = pch_vec[1], ...)
	lines(seq_tab, relFreqCat2, lty = 2, col = col_vec[2], 
			type = type_vec[2], pch = pch_vec[2])
	lines(seq_tab, relFreqCat3, lty = 4, col = col_vec[3], 
			type = type_vec[3], pch = pch_vec[3])	
	
	if ( is.null(legend_text) ) legend_text <- paste(1:3, "categorical")
	legend("top", "center", legend = legend_text, lty = c(1, 2, 4), col = col_vec)
	
}

