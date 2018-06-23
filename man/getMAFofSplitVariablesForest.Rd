\name{getMAFofSplitVariablesForest}
\alias{getMAFofSplitVariablesForest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ returns the minor allel frequency of the split variables
}
\description{
For each not terminal node in the forest this functions returns the 
minor allel frequency (MAF) of the variable used for splitting in the 
according node.
}
\usage{
getMAFofSplitVariablesForest(splitVariablesForest = list(), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{splitVariablesForest}{
	A \code{list} containing the indices of the variables used for splitting 
	in the forest as returned by \code{\link{getSplitVariablesForest}}.
}
  \item{...}{
 	Further arguments passed to \code{\link{getMAFofSplitVariablesTree}}.  
}
}

\value{
A \code{list} of same length as number of trees in the forest. 
Each element contains a \code{vector} of same length as number of not terminal
nodes in the according tree. Returns the MAF of the variables used 
for splitting in the according node. 
Values range from \code{1} to \code{length(mafIntervals)} 
(See. \code{\link{getMAFofSplitVariablesTree}}), where \code{1} indicates the 
first element of \code{mafIntervals}.  
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getMAFofSplitVariablesTree}}, \code{\link{getSplitVariablesForest}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)

## MAF of split variables
splitVarsF <- getSplitVariablesForest(rF)
mafF <- getMAFofSplitVariablesForest(splitVarsF) 

length(mafF) == rF$ntree
mafF[[1]] # MAFs for first tree
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ MAF }
\keyword{ SplitVariables }% __ONLY ONE__ keyword per line
