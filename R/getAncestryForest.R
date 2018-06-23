getAncestryForest <- function (rFobject) {
	
	lapply(1:rFobject$ntree, function(z) getAncestryTree(getTree(rFobject, z)))
	
}

