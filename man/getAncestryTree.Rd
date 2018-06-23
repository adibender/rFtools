\name{getAncestryTree}
\alias{getAncestryTree}

\title{ retrieves the path from node to root for all nodes 
of a tree
}
\description{
For each (not terminal) node of a tree this function retrieves the path 
from the according node to the root of the considered tree.
}
\usage{
getAncestryTree(tree = matrix())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
 	A \code{matrix} containing tree information that is returned by 
 	the \code{getTree} function.
}
}

\value{
A \code{list} of the same length as number of not terminal nodes in the tree.
Each element contains the path from the according node to the root of the tree, 
by providing the index of the variables used for splitting. First value 
is the closest ancestor, last value equals the root.
}

\note{
	For reasons of efficiency this function does not apply 
	\code{\link{getAncestryNode}}. Therefore the result for a specific node 
	differs slightly regarding the layout, e.g. node indices are omitted.
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getAncestryNode}}, \code{\link{getAncestryForest}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## ancestry
getAncestryTree(getTree(rF, 1))[5:10]
getAncestryNode(10, getTree(rF,1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Ancestry }
\keyword{ Tree }% __ONLY ONE__ keyword per line
