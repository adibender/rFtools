getMAFofSplitVariablesTree <- function(splitVariablesTree = numeric(),
		mafIntervals = list(maf0.05 = 1:50, maf0.10 = 51:100, 
				maf0.25 = 101:150, maf0.40 = 151:200)) {
	
	MAF <- numeric(length(splitVariablesTree))
	
	for (i in 1:length(mafIntervals)) {
		
		ind_maf.i <- splitVariablesTree %in% mafIntervals[[i]]
		MAF[ind_maf.i] <- i
		
	}
	
	names(MAF) <- splitVariablesTree
	
	return(MAF)
}

