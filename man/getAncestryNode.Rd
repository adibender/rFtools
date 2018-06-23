\name{getAncestryNode}
\alias{getAncestryNode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves path from node to root
}
\description{
 Extracts the path from the specified node to the root of the tree.
}
\usage{
getAncestryNode(indNode = integer(), tree = matrix())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{indNode}{
	An \code{integer}. Specifies the node for which to retrieve the path.
}
  \item{tree}{
	A tree of a \code{randomForest} that is returned by \code{getTree}.
}
}

\value{
A \code{numeric} vector. The names indicate the ancestor nodes, the values
the variables used to split the according node. First element of the vector 
is the closest relative, the last element represents the root.
}


\seealso{
\code{\link{getAncestryTree}}, \code{\link{getAncestryForest}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)

## ancestry		
getAncestryNode(indNode = 104, tree = getTree(rF, 1))

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Ancestry }
\keyword{ Node }% __ONLY ONE__ keyword per line
