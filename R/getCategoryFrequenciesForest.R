getCategoryFrequenciesForest <- function(rFobject, X) {
	
	## workaround to transform data.frame with factors into numeric matrix
	X <- matrix(as.numeric(as.matrix(X)), ncol = ncol(X))
	
	splitInfoForest <- getSplitInformationForest(rfObject = rFobject, X = X)                                  
	nodeObservationsForest <- lapply(splitInfoForest, function(z) z[[1]])
	splitVariablesForest<- lapply(splitInfoForest, function(z) z[[3]])
	
	cF <- lapply(1:length(nodeObservationsForest), function(z) 
			getCategoryFrequenciesTree(
					nodeObservationsTree = nodeObservationsForest[[z]], X = X, 
								splitVariablesTree = splitVariablesForest[[z]]))
	
	class(cF) <- c("CategoryFrequenciesForest")
	
	return(cF)
	
}

