plot.LevelsVsLayerCFbyMAF <- 
		function(x, norm.by.nodes = TRUE, legPos = "topleft", col = 1:4, 
		 ylab = NULL, xlab = "layer", type = "o", pch = 4, legend = NULL, ...) {							
	
	if ( norm.by.nodes ) {
		node.counts <- x$n.nodes
		tab <- x$VarCountByLayerAndMAF
		plot.obj <- sapply(1 : ncol(tab), function(z) tab[,z]/node.counts)
		if ( is.null(ylab) ) 
			ylab = "# variables selected for splitting / #nodes per layer"
	}
	else { 
		plot.obj <- x$VarCountByLayerAndMAF
		ylab = "# variables selected for splitting"
	}
	
	if ( is.null(legend) ) legend = paste("MAF = ", c(0.05, 0.10, 0.25, 0.40))
	plot(plot.obj[, 1], type = type, pch = pch, ylim = c(0, max(plot.obj)), 
			col = col[1], xlab = xlab, 
			ylab = ylab, ...)
	for ( i in 2:ncol(plot.obj) )
		lines(plot.obj[, i], col = col[i], type = type, pch = pch)
	legend(legPos, legend = legend , lty = 1, col = col)
}

