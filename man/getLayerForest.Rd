\name{getLayerForest}
\alias{getLayerForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves the layer of (not terminal) nodes
}
\description{
Returns the layer of every (not terminal) node in the forest, the 
root having layer 1. 
}
\usage{
getLayerForest(ancestryForestObj = list())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ancestryForestObj}{
	A \code{list} as it is returned by \code{\link{getAncestryForest}}.
}
}
\details{
Applies \code{\link{getLayerTree}} to every list element. 
}
\value{
 A \code{list} of length \code{ntree}. Each list element contains a vector where 
 the single elements indicate the layer of the according (not terminal) nodes. 
}


\seealso{
\code{\link{getLayerTree}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)

## ancestry		
aF <- getAncestryForest(rF)

## layer
lF <- getLayerForest(aF) 
length(lF) == rF$ntree
lF[[1]] ## layers of not terminal nodes for the first tree
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Layer }
\keyword{ Forest }% __ONLY ONE__ keyword per line
