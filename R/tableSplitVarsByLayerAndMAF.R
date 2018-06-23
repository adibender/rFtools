tableSplitVarsByLayerAndMAF <-
function(LevelsVsLayerByMAF, splitVarLevelsVsLayerByMAF) {
	
	max.levels <- sapply(splitVarLevelsVsLayerByMAF, ncol) 
	
	splitVarsByLayerAndMAF <- list()
	
	## new list 
	for (i in 1:length(max.levels)) {
		levs.i <- max.levels[i]
		splitVarsByLayerAndMAF[[i]] <- 
			lapply(seq_len(levs.i), 
				function(y) {
					split.iy <- splitVarLevelsVsLayerByMAF[[i]][,y]
					addmargins(as.table(
						cbind(split.iy, LevelsVsLayerByMAF[[i]][2:3,y] - split.iy)))	
				}
			)
		## change dimnames  	
		for (j in seq_len(levs.i)) {
			dimnames(splitVarsByLayerAndMAF[[i]][[j]])<- 
				list(c("2 categorical", "3 categorical", "Sum"), 
						c("split", "not split", "Sum"))
		}
	}
	
	names(splitVarsByLayerAndMAF) <- names(max.levels)
	class(splitVarsByLayerAndMAF) <- c("SplitVarsByLayerAndMAF", "list")
	
	return(splitVarsByLayerAndMAF)
	
}

