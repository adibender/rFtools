\name{getLayerTree}
\alias{getLayerTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves the layer for all nodes of a tree
}
\description{
For a given tree this function returns the layer for each not terminal node. 

}
\usage{
getLayerTree(ancestryTreeObj = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ancestryTreeObj}{
	A \code{list} as it is returned by \code{\link{getAncestryTree}}.
}
}

\value{
A \code{vector} with each element indicating the layer of the according 
(not terminal) node.
}


\seealso{
\code{\link{getLayerForest}}, \code{\link{getAncestryTree}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## ancestry
aT1 <- getAncestryTree(getTree(rF, 1))

## layer
getLayerTree(aT1) ## layers of the not terminal nodes of tree 1
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Layer }
\keyword{ Tree }% __ONLY ONE__ keyword per line
