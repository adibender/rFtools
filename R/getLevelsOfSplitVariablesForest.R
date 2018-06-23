getLevelsOfSplitVariablesForest <- function(cF, splitVariablesForest) {
	
	levelsSplitVars <- list()
	
	for (i in 1:length(cF)) {
		splitVars.i <- splitVariablesForest[[i]]
		levels.i <- numeric(length(splitVars.i))
		for (j in 1:length(cF[[i]])) {
			levels.i[j] <- cF[[i]][[j]][5, splitVars.i[j]]
		}
		levelsSplitVars[[i]] <- levels.i
		names(levelsSplitVars[[i]]) <- splitVars.i
	}
	
	return(levelsSplitVars)
	
}

