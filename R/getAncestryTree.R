getAncestryTree <- function (tree = matrix()) {
	
	ntNodes <- as.numeric(which(tree[, "status"] == 1))
	
	## index of parent to the i-th ntNode
	pInd <- match(ntNodes, tree[, "left daughter"], nomatch = 0) + 
			match(ntNodes, tree[, "right daughter"], nomatch = 0)
	
	ancestryList <- list(); ln <- length(ancestryList) <- length(ntNodes)
	ancestryList[[1]] <- NA
	
	
	p2 <- pInd[2]
	p3 <- pInd[3] 
	
	if (all(ntNodes[2:3] == c(2,3))) {
		ancestryList[[2]] <- ancestryList[[3]] <- tree[1, "split var"]
	}
	else {
		if (ntNodes[2] != 2) {
			ancestryList[[3]] <- tree[p3, "split var"] 
			ancestryList[[2]] <- c(tree[p2, "split var"], 
					ancestryList[[which(ntNodes == p2)]])
			
		}
		else {
			ancestryList[[2]] <- tree[p2, "split var"] 
			ancestryList[[3]] <- c(tree[p3, "split var"], 
					ancestryList[[which(ntNodes == p3)]])	
		}
	}
	
	for ( i in 4:ln ) {
		
		parent.i <- pInd[i]
		ancestryList[[i]] <- c(tree[parent.i, "split var"], 
									ancestryList[[which(ntNodes == parent.i)]])
		
	}
	
	names(ancestryList) <- ntNodes
	return(ancestryList)
	
}

