tableLevelsVsLayerCF <- function(BinTree, layer = 0) {
	
	layer <- layer + 1
	if (BinTree[[4]]) return(NULL)
	
	nl <- tableLevelsVsLayerCF(BinTree[[8]], layer = layer )
	nr <- tableLevelsVsLayerCF(BinTree[[9]], layer = layer )
	
	return(c(nl, nr, splitVar = BinTree[[5]][[1]], layer = layer))
}

