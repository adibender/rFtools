getSplitVariablesForest <-
function(rFobject) {
	
	lapply(seq_len(rFobject$ntree),
		function(z) getSplitVariablesTree(getTree(rFobject, z)))

}

