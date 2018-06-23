tableLevelsVsLayerCFbyMAF <- function(cforest,  
		mafIntervals =  list(1:50, 51:100, 101:150, 151:200)) {
	
	##load cforest object by filename
	if ( class(cforest) != "RandomForest" ) {
		if ( class(cforest) == "character" )
			cforest <- eval(parse(text = load(cforest)))
		else 
			stop("cforest object has to be either of class 'RandomForest' or 
					'character'")
	}
	
	splitVarsAndLayer <- unlist(lapply(cforest@ensemble, tableLevelsVsLayerCF))
	layInd <- names(splitVarsAndLayer) == "layer"
	layVarMaf <- data.frame(layer = splitVarsAndLayer[layInd], 
			splitVar = splitVarsAndLayer[!layInd])

	for ( i in 1:length(mafIntervals) ) 
		layVarMaf$maf[layVarMaf$splitVar %in% mafIntervals[[i]]] <- i
	
	tableLayerVsMaf <- table(layVarMaf[, -2])
	res <- list(VarCountByLayerAndMAF = tableLayerVsMaf, 
			n.nodes = table(layVarMaf[,1]))
	class(res) <- c("LevelsVsLayerCFbyMAF", "list")
	return(res)
}

