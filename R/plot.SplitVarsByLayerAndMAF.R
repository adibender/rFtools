plot.SplitVarsByLayerAndMAF <- function(x = list(),  fun = "relative",
		row.num = 1, col.num = 1, row.denom = NULL, col.denom = NULL,
		col = 1:4,  main = character(), ylab = character(), ylim, xlim, 
		legPos = "topright", norm.by.nodes = FALSE, layerForest, ...) {
	
	if(norm.by.nodes) {
		if ( missing(layerForest) ) stop("layerForest object is missing!")
		if ( fun == "absolute" ) {
			if ( missing(ylab) ) ylab <- "# candidates / # nodes per layer"
			candidates_obj <- lapply(x, absolute, rowN = row.num, colN = col.num)
		}
		if ( fun == "relative" ) {
			if ( missing(ylab) ) ylab <- "relative # candidates / # nodes per layer"
			if ( is.null(row.denom) ) row.denom <- row.num
			if ( is.null(col.denom) ) col.denom <- 3
			candidates_obj <- lapply(x, relative, row.num, col.num, row.denom, col.denom)
		}
		length_obj <- length(candidates_obj)
		layCount <- as.numeric( table(unlist(layerForest)) )
		
		
		plot_obj <- sapply( seq_len(length_obj), function(z) 
					candidates_obj[[z]] / layCount[ 1:length(candidates_obj[[z]]) ] )
		
		## check/set graphic parameters
		if ( missing(main) ) {
			main <- switch(row.num, "2-categorical", "3-categorical", "2- & 3-categorical")
		}
		if ( missing(ylim) ) {
			ylim = c(0, max(sapply(plot_obj, max, na.rm = TRUE)))
		}
		if ( missing(xlim) ) {
			xlim = c(1, max(sapply(plot_obj, length)))
		}
		
		#actual plotting
		plot(1:length(plot_obj[[1]]), plot_obj[[1]], type = "o", pch = 4, 
				xlab = "layer", ylab = ylab, ylim = ylim, xlim = xlim, main = main)
		for (i in 2:length_obj) {
			lines(1:length(plot_obj[[i]]), plot_obj[[i]], type = "o", pch = 4, col = i)
		}
		legend(legPos, legend = names(x), col = col, lty = 1)
	}
	
	else if ( !norm.by.nodes ) {
		## prepare object to be ploted
		if ( fun == "relative" ) { 
			if ( missing(ylab) ) ylab <- "relative frequency"
			if ( is.null(row.denom) ) row.denom <- row.num
			if ( is.null(col.denom) ) col.denom <- 3
			plot.obj <- lapply(x, relative, row.num, col.num, row.denom, col.denom)
		}
		else {
			if ( fun == "absolute" ) {
				if ( missing(ylab) ) ylab <- "frequency"
				plot.obj <- lapply(x, absolute, rowN = row.num)
			}
			else{
				fun <- eval(parse(paste(text = fun)))
				plot.obj <- lapply(x, fun, row.num, col.num, row.denom, col.denom)
			}
		}
		
		## check/set graphic parameters
		if ( missing(main) ) {
			main <- switch(row.num, 
					"2-categorical", "3-categorical", "2- & 3-categorical")
		}
		if ( missing(ylim) ) {
			ylim = c(0, max(sapply(plot.obj, max, na.rm = TRUE)))
		}
		if ( missing(xlim) ) {
			xlim = c(1, max(sapply(plot.obj, length)))
		}
		
		
		## actual plotting
		plot( 1:length(plot.obj[[1]]), plot.obj[[1]], type = "o", pch = 4, 
				xlab = "layer", ylab = ylab, ylim = ylim, xlim = xlim, 
				main = main, col = col[1], ...)
		
		for ( i in 2:length(x) ) { 
			
			lines(1:length(plot.obj[[i]]), plot.obj[[i]], 
					type = "o", pch = 4, col = col[i])
		}
		
		legend(legPos, legend = names(x), col = col, lty = 1)
	}
	
}
