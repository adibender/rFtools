getSplitVariablesTree <-
function(tree = matrix()) {
	ntNodes <- tree[ , "status"] != -1
	return(tree[ntNodes, "split var"])
}

