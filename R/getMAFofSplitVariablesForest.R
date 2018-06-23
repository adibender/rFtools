getMAFofSplitVariablesForest <- function(splitVariablesForest = list(), ...) {
	
	sapply(splitVariablesForest, getMAFofSplitVariablesTree, ...)
	
}

