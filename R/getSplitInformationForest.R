getSplitInformationForest <- function(rfObject, X = matrix()) {
	
	inbag <- rfObject$inbag
	
	lapply(1:rfObject$ntree, function(z) 
				getSplitInformationTree(tree = getTree(rfObject, z), 
						inbagInd = as.logical(inbag[,z]), X = X))
	
}

