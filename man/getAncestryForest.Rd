\name{getAncestryForest}
\alias{getAncestryForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{  retrieves the path from node to root for all (not terminal) nodes
}
\description{
Extracts the paths from node to root for each (not terminal) node. 
Applies \code{getAncestryTree} to all 
trees of a \code{randomForest}-object.
}
\usage{
getAncestryForest(rFobject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rFobject}{
	A \code{randomForest} object
}
}

\value{
 A \code{list} of length \code{rFobject$ntree}
}

\seealso{
\code{\link{getAncestryTree}}, \code{\link{getAncestryNode}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)

## ancestry		
aF <- getAncestryForest(rF)

aF[[1]]$"32"
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Ancestry }
\keyword{ Forest }% __ONLY ONE__ keyword per line
