\name{getCategoryFrequenciesForest}
\alias{getCategoryFrequenciesForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves category frequency information of a forest
}
\description{
Given a \code{randomForest}-object this function retrieves the category frequencies
for each tree of the considered forest.
}
\usage{
getCategoryFrequenciesForest(rFobject, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rFobject}{
	A \code{randomForest} object
}
  \item{X}{
 	A \code{matrix} containing the SNPs used in the analysis 
}
}
\details{
	This function basically applies \code{\link{getCategoryFrequenciesTree}} 
	to each tree in the forest.
}
\value{
 A \code{list} of length \code{rFobject$ntree} containing objects as they are  
 returned by \code{\link{getCategoryFrequenciesTree}}.
}

\seealso{
\code{\link{getCategoryFrequenciesNode}}, \code{\link{getCategoryFrequenciesTree}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## category frequencies
cF <- getCategoryFrequenciesForest(rF, YX[ , -1])
}
\keyword{ Category Frequencies }
\keyword{ Forest }% __ONLY ONE__ keyword per line
