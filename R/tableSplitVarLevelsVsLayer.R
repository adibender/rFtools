tableSplitVarLevelsVsLayer <- function(nlevels = numeric(), layerVec = numeric(),
		uniqueLayers = numeric()) {
	
	tab <- .Call("tableSplitVarLevelsVsLayer",
			nlevels, layerVec, uniqueLayers,
			PACKAGE = "rFtools")
	
	tab <- as.table(tab)
	
	class(tab) <- c("SplitVarLevelsVsLayer", "table")
	
	return(tab)
	
}

