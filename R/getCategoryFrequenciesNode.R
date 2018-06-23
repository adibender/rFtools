getCategoryFrequenciesNode <- 
		function(SNPmatrix = matrix(),  splitVar = numeric()) {

    cf <- .Call("getCategoryFrequencies", 
                 SNPmatrix, splitVar,
                 PACKAGE = "rFtools")

    return(cf)
	
}

