\name{getCategoryFrequenciesNode}
\alias{getCategoryFrequenciesNode}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ retrieves the category frequencies for one node
}
\description{
For a specified node of a tree this function retrieves 
the category frequencies for each SNP.
}
\usage{
getCategoryFrequenciesNode(SNPmatrix = matrix(), splitVar = numeric())
}

\arguments{
  \item{SNPmatrix}{
	A \code{matrix} containing the SNPs used in the analysis. 
}
  \item{splitVar}{
	A \code{numeric} indicating the variable used to split the 
	considered Node.
}
}

\details{
 The provided \code{matrix} could technically be any Matrix with categorical 
 variables. In context of this package it will usually be a subset of the 
 original SNP data set containing only the observations still 'active'  
 in the considered node. The \code{splitVar} argument also only makes sense 
 in this particular setting.
}

\value{
 A 5 \code{x} n.snps \code{matrix}, where \code{n.snps} is the number of 
 SNP variables in \code{SNPmatrix}. The rows contain in this order:
 \item{}{An indicator whether a variable was used for splitting in the 
 considered node; \code{0 = } no, \code{1 = } yes}
 \item{}{The number of category 0 ( = AA) observations}
 \item{}{The number of category 1 ( = Aa) observations}
 \item{}{The number of category 2 ( = aa) observations}
 \item{}{Number of levels/different categories a variable/SNP has in 
 the according node.}
}

\note{
	Calls the underlying Rcpp/C++ function with the same name.
}

\section{Warning }{
 This function only counts the instances of observations having one of the 
 values \code{0, 1} or \code{2}. If you provide a \code{SNPmatrix} with
 differently coded categories, you will get wrong results.
 }
\seealso{
\code{\link{getCategoryFrequenciesTree}}, 
\code{\link{getCategoryFrequenciesForest}}
}

\keyword{ Category Frequencies }
\keyword{ Node }% __ONLY ONE__ keyword per line
