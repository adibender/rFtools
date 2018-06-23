simulateSNPdata <- function(n.obs = 500, n.snp = 200, seed = NA,  
		maf = rep(c(0.05, 0.10, 0.25, 0.40), each = 50), prob1 = 0.5) {
	
	set.seed(seed)                             
	
	#SNPs
	X <- data.frame(sapply(1:n.snp, function(z)
		as.factor(sample(0:2, n.obs, replace = TRUE, prob = mafToProb(maf[z])))))
	
	colnames(X) <- paste("SNP", 1:n.snp, sep = "")
	
	#response
	Y <- as.factor(sample(0:1, n.obs, replace = TRUE, prob = c(1-prob1, prob1)))
	
	return(data.frame(y = Y, X))
	
}

