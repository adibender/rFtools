tableLevelsVsLayerByMAF <- function(cF, rF, 
						mafIntervals = list(maf0.05 = 1:50, maf0.10 = 51:100, 
										maf0.25 = 101:150, maf0.40 = 151:200)) {
	
	levelsList <- list()
	for (i in 1:length(mafIntervals)) {
		levelsList[[i]] <- unlist(sapply(cF, function(z) 
			sapply(z, function(y) y[5, mafIntervals[[i]]], USE.NAMES = FALSE), 
							USE.NAMES = FALSE), use.names = FALSE)
	}
	
	layersForest <- getLayerForest(getAncestryForest(rF))
	layersVector <- unlist(sapply(layersForest, 
							function(z) rep(z, each = 50)), use.names = FALSE)
	uniqueLayers <- unique(layersVector)
	
	tablesList <- lapply(levelsList, function(z) 
				tableLevelsVsLayer(z, layersVector, uniqueLayers))
	
	names(tablesList) <- names(mafIntervals)
	class(tablesList) <- c("LevelsVsLayerByMAF", "list")
	
	return(tablesList)
	
}

