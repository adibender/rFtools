simulateAndTable <- function(seed = 123, n.obs = 500, n.snp = 200, 
		maf = rep(c(0.05, 0.10, 0.25, 0.40), each = 50), ntree = 500,
		mafIntervals = list(maf0.05 = 1:50, maf0.10 = 51:100, 
				maf0.25 = 101:150, maf0.40 = 151:200)) {
	
	
	# compute simulation objects					
	SNPdata <- simulateSNPdata(n.obs, n.snp, seed, maf)
	set.seed(seed)
	rF<- randomForest(x = SNPdata[, -1], y = SNPdata[, 1], ntree = ntree,  
			importance = TRUE, keep.forest = TRUE, keep.inbag = TRUE)
	cForest <- cforest(y ~ ., data = SNPdata, 
			controls = cforest_control(mtry = sqrt(ncol(SNPdata) - 1), replace = FALSE))
	cF <- getCategoryFrequenciesForest(rF, X = SNPdata[, -1])
	
	# save simulation objects
	tabAll <- tableLevelsVsLayerByMAF(cF, rF, mafIntervals = mafIntervals)
	tabSplitVars <- tableSplitVarLevelsVsLayerByMAF(cF, rF)
	tabCforest <- tableLevelsVsLayerCFbyMAF(cForest)
	
	return(list(tableAll = tabAll, tableSplitVars = tabSplitVars, 
					tableCforest = tabCforest))
}
