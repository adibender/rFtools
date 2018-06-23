\name{getSplitInformationForest}
\alias{getSplitInformationForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ returns split information on forest
}
\description{
Given a \code{randomForest}-object this function returns split information 
for all trees in the according forest. Basically applies 
\code{\link{getSplitInformationTree}} to all trees in the forest. 
}
\usage{
getSplitInformationForest(rfObject, X = matrix())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rfObject}{
	A \code{randomForest}-object as returned by \code{randomForest}. 
}
  \item{X}{
	A \code{matrix} containing the (SNP-) variables used in the call 
	to \code{randomForest}. 
}
}

\value{
A \code{list} of the same length as number of trees in the forest, 
each element with 3 components as returned by \code{\link{getSplitInformationTree}}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getSplitInformationTree}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)
X <- matrix(as.numeric(as.matrix(YX[, -1])), ncol = ncol(YX[, -1]))

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## split info
splitInfoF <- getSplitInformationForest(rF, X)
length(splitInfoF) 
names(splitInfoF[[1]])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Split Info }
\keyword{ Forest }% __ONLY ONE__ keyword per line
