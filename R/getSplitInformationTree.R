getSplitInformationTree <- function(tree = matrix(), inbagInd = logical(), 
		X = matrix()) {
	
	## index of not terminal nodes
	ntNodes <- as.numeric(which(tree[, "status"] == 1))
	
	## index of parent to the i-th ntNode
	pInd <- match(ntNodes, tree[, "left daughter"], nomatch = 0) + 
			match(ntNodes, tree[, "right daughter"], nomatch = 0)
	
	## index of the splitting variables to the i-th parent Node
	splitVars <- c(0, as.numeric(tree[pInd, "split var"]))
	
	## list of splits for the according ntNodes
	ntSplits <- sapply(tree[ntNodes, "split point"], function(z) 
				switch(z, 0, 1, {0:1}, 2, c(0,2), {1:2}))
	
	## initiate object to be returned
	resList <- list()
	lrl <- length(resList) <- length(ntNodes)
	resList[[1]] <- inbagInd## root node with all inbag observations
	
	## loop over all (not root) ntNodes to determine remaining observations
	indI <- match(pInd, ntNodes)
	rightD.ind <- c(0, tree[pInd, "right daughter"])
	
	for( i in 2:lrl ) {
		
		ind.i <- indI[i]
		indNode <- resList[[ind.i]]
		wi <- X[, splitVars[i]] %in% ntSplits[[ind.i]] & indNode														
		if( ntNodes[i] == rightD.ind[i] ) indNode[wi] <- FALSE
		else indNode[-which(wi)] <- FALSE ## if left daughter
		resList[[i]] <- indNode
		
	}
	
	names(resList) <- ntNodes
	list(indNodes = resList, ntSplits = ntSplits, ## ntSplits currently not used 
			splitVars = tree[ntNodes, "split var"])
	
}

