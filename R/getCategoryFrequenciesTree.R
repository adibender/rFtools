getCategoryFrequenciesTree <- 
		function(nodeObservationsTree, X, splitVariablesTree) {
	
	reducedX.list<- lapply(nodeObservationsTree, function(z) X[z, ])
	lapply(1:length(reducedX.list), function(z) 
			getCategoryFrequenciesNode(reducedX.list[[z]], splitVariablesTree[z]))
	
}

