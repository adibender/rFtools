simulateAndSafe <- function(seed = 123, n.obs = 500, n.snp = 200, 
		maf = rep(c(0.05, 0.10, 0.25, 0.40), each = 50), ntree = 500,
		path = "~/", subpaths = c("SNPdata/", "rF/", "cforest/", "cF/"), 
		outprefix = "") {
	
	data.path <- paste(path, subpaths[1], outprefix, sep = "")
	rF.path <- paste(path, subpaths[2], outprefix, sep = "")
	cforest.path <- paste(path, subpaths[3], outprefix, sep = "")
	catFreq.path <- paste(path, subpaths[4], outprefix, sep = "")
	
	# compute simulation objects					
	SNPdata <- simulateSNPdata(n.obs, n.snp, seed, maf)
	set.seed(seed)
	rf <- randomForest(x = SNPdata[,-1], y = SNPdata[,1], ntree = ntree,  
			importance = TRUE, keep.forest = TRUE, keep.inbag = TRUE)
	cforest <- cforest(y ~ ., data = SNPdata, 
			controls = cforest_control(mtry = sqrt(ncol(SNPdata) - 1), replace = FALSE))
	cF <- getCategoryFrequenciesForest(rf, X = SNPdata[,-1])
	
	# save simulation objects
	save(SNPdata, file = paste(data.path, "SNPdata", seed, ".Rda", sep = ""))
	save(rf, file = paste(rF.path, "rF", seed, ".Rda", sep = ""))
	save(cforest, file = paste(cforest.path, "cforest", seed, ".Rda", sep = ""))
	save(cF, file = paste(catFreq.path, "cF", seed, ".Rda", sep = ""))
	
}

