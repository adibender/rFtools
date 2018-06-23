\name{getMAFofSplitVariablesTree}
\alias{getMAFofSplitVariablesTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ returns minor allele frequencies
}
\description{
For each not terminal node in the tree this functions returns the 
minor allele frequency (MAF) of the variable used for splitting in the 
according node.
}
\usage{
getMAFofSplitVariablesTree(splitVariablesTree = numeric(), 
	mafIntervals = list(maf0.05 = 1:50, maf0.10 = 51:100, 
	maf0.25 = 101:150, maf0.40 = 151:200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{splitVariablesTree}{
 	A \code{vector} containing the indices of the variables used for splitting
 	in the tree. 
}
  \item{mafIntervals}{
	A \code{list} with each element giving the indices of variables
	being simulated with the according MAF. Depends on how the data was simulated
	in \code{\link{simulateSNPdata}}. 
}
}

\value{
A \code{vector} of the same length as number of not terminal nodes in the tree. 
Contains the MAFs of the variables used for splitting in the according nodes. 
}



\seealso{
\code{\link{getMAFofSplitVariablesForest}}, \code{\link{getSplitVariablesForest}}
}

\keyword{ Split Variables }
\keyword{ Forest }% __ONLY ONE__ keyword per line
