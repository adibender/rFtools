\name{getSplitVariablesForest}
\alias{getSplitVariablesForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves split variables from forest
}
\description{
For a given \code{randomForest}-object this function returns the variables 
used for splitting in each not terminal node of the forest.
Basically applies \code{\link{getSplitVariablesTree}} to each tree in the forest.
}
\usage{
getSplitVariablesForest(rFobject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rFobject}{
	A \code{randomForest}-object that is returned by \code{randomForest}
}
}

\value{
A \code{list} of the same length as number of trees in the forest. Each element 
contains a \code{vector} that is returned by \code{\link{getSplitVariablesTree}}.
}


\seealso{
\code{\link{getSplitVariablesTree}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## split vars
splitVarsF <- getSplitVariablesForest(rF)
length(splitVarsF) == rF$ntree
splitVarsF[[1]] # first tree
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Split Variables }
\keyword{ Forest }% __ONLY ONE__ keyword per line
