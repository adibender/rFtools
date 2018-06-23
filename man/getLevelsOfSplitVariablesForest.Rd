\name{getLevelsOfSplitVariablesForest}
\alias{getLevelsOfSplitVariablesForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ returns the number of different categories
}
\description{
For every not terminal node, i.e. for every variable used for splitting 
in the forest, this function returns the number of different levels/categories
the variable actually has in the according node. 
}
\usage{
getLevelsOfSplitVariablesForest(cF, splitVariablesForest)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cF}{
	A \code{CategoryFrequenciesForest}-object as it is returned by 
	\code{\link{getCategoryFrequenciesForest}}
}
  \item{splitVariablesForest}{
	A \code{list} containing the indices of the variables used for 
	splitting in the forest as they are returned by \code{\link{getSplitVariablesForest}}
	or \code{\link{getSplitInformationForest}}.
}
}

\value{
A \code{list} of the same length as number of trees in the forest. 
Each element contains a \code{vector} of the same length as number of not terminal
nodes in the according tree. Returns the categoricality of the variables used 
for splitting in the according node.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getSplitVariablesForest}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## category frequencies
cF <- getCategoryFrequenciesForest(rF, X = YX[ , -1])

## levels of split variables
splitVarsF <- getSplitVariablesForest(rF)
levelsF <- getLevelsOfSplitVariablesForest(cF, splitVarsF) 

length(levelsF) == rF$ntree
levelsF[[1]] # levels for first tree

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Split Variables }
\keyword{ Forest }% __ONLY ONE__ keyword per line
