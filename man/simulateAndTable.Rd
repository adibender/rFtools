\name{simulateAndTable}
\alias{simulateAndTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ simulate and table information in one step
}
\description{
A wrapper that performs tabulation of the \code{CategoryFrequencies}-objects
directly instead of saving them to the hard drive first as in 
\code{simulateAndSafe}.
}
\usage{
simulateAndTable(seed = 123, n.obs = 500, n.snp = 200, 
maf = rep(c(0.05, 0.1, 0.25, 0.4), each = 50), ntree = 500, 
mafIntervals = list(maf0.05 = 1:50, maf0.10 = 51:100, 
maf0.25 = 101:150, maf0.40 = 151:200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seed}{
	The seed to be used in data simulation. 
}
  \item{n.obs}{
	A \code{integer}. The number of observations.
}
  \item{n.snp}{
	A \code{integer}. The number of SNPs.
}
  \item{maf}{
	A \code{numeric} of length \code{n.snps} indicating the MAF with which 
	the according SNP should be simulated. 
}
  \item{ntree}{
	Passed to \code{randomForest}.
}
  \item{mafIntervals}{
	A \code{list} with each element giving the indices of variables
	being simulated with the according MAF. Depends on how the data was simulated
	in \code{\link{simulateSNPdata}}. 
}
}

\value{
 A \code{list} with components:
 \item{tableAll}{An object of class \code{LevelsVsLayerByMAF} as returned by 
 \code{\link{tableLevelsVsLayerByMAF}}.}
 \item{tableSplitVars}{An object of class \code{SplitVarLevelsVsLayerByMAF} as 
 returned by \code{\link{tableSplitVarLevelsVsLayerByMAF}}.}
 \item{tableCforest}{An object of class \code{LevelsVsLayerCFbyMAF} as returned 
 by \code{\link{tableLevelsVsLayerCFbyMAF}}.}
}


\seealso{
\code{\link{simulateAndSafe}}, \code{\link{tableSplitVarsByLayerAndMAF}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Simulate }
\keyword{ Table }% __ONLY ONE__ keyword per line
