getAncestryNode <- function (indNode = integer(), tree = matrix()) {
	
	ancestryVector <- numeric()
	
	while (indNode != 1) {
		indNode <- getParentNode(indNode, tree)
		ancestryVector <- c(ancestryVector, indNode)
	}
	
	tree[ancestryVector, "split var"]
	
}

