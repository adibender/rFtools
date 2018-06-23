tableSplitVarLevelsVsLayerByMAF <- function(cFobject, rFobject) {
	
	splitVarsForest <- getSplitVariablesForest(rFobject = rFobject)
	MAFofSplitVars <- getMAFofSplitVariablesForest(splitVarsForest)
	levOfSplitVars <- getLevelsOfSplitVariablesForest(cFobject, splitVarsForest)
	layersForest <- getLayerForest(getAncestryForest(rFobject))
	
	LevLayMAF <- cbind(
			unlist(levOfSplitVars, use.names = FALSE), 
			unlist(layersForest, use.names = FALSE), 
			unlist(MAFofSplitVars, use.names = FALSE))
	
	maf.list <- list()
	
	for (i in 1:4) {
		ind_maf.i <- LevLayMAF[,3] == i
		maf.list[[i]] <- tableSplitVarLevelsVsLayer(LevLayMAF[ind_maf.i, 1], 
				LevLayMAF[ind_maf.i, 2], unique(LevLayMAF[ind_maf.i, 2]))
	}	
	
	names(maf.list) <- paste("maf", c("0.05", "0.10", "0.25", "0.40"), sep = "")
	class(maf.list) <- c("SplitVarLevelsVsLayerByMAF", "list")
	return(maf.list)

}

