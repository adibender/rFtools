tableLevelsVsLayer <- 
function(nlevels = numeric(), layerVec = numeric(), uniqueLayers = numeric()) {

	tab <- .Call("tableLevelsVsLayer", 
					nlevels, layerVec, uniqueLayers, 
					PACKAGE = "rFtools")
			
	tab <- as.table(tab)
	
	class(tab) <- c("LevelsVsLayer", "table")
	
	return(tab)
}

