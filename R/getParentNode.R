getParentNode <- function(childNode = integer(), tree = matrix()) {
	
	which(tree[,1] == childNode | tree[,2] == childNode)
	
}

