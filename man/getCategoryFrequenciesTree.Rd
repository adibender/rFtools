\name{getCategoryFrequenciesTree}
\alias{getCategoryFrequenciesTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves category frequencies for one tree
}
\description{
Given a specific tree from a \code{randomForest}-object this function retrieves
the category frequencies for each SNP and for every 
(not terminal) node in the considered tree.
}
\usage{
getCategoryFrequenciesTree(nodeObservationsTree, X, splitVariablesTree)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nodeObservationsTree}{
	A \code{list} of the same length as number of not terminal nodes in 
	the tree. Each element being a \code{logical} vector indicating the 
	observations 'active' in the according nodes of the tree.  
}
  \item{X}{
	A \code{matrix} containing the SNP variables of the data set used for 
	the computation of the \code{randomForest}-object from which 
	the considered tree is extracted from.
}
  \item{splitVariablesTree}{
	A \code{numeric} vector containing the variables/SNPs used for splitting the 
	not terminal nodes in the considered tree.
}
}
\details{
 To obtain the \code{nodeObservationsTree}-object take a look at the 
 \code{getSplitInformationTree} and -\code{Forest} functions. The split 
 variables are also in the objects returned by these functions, but can also 
 be obtained by \code{getSplitVariablesTree} or -\code{Forest} functions. 
}
\value{
 A \code{list} with as many entries as not terminal nodes in the tree. 
 Each contains a 5 \code{x} n.snps \code{matrix} as it is returned by 
 \code{getCategoryFrequenciesNode}. 
}


\seealso{
\code{\link{getCategoryFrequenciesNode}}, 
\code{\link{getCategoryFrequenciesForest}}
}

\keyword{ Category Frequencies }
\keyword{ Tree }% __ONLY ONE__ keyword per line
