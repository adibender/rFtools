\name{plot.LevelsVsLayerByMAF}
\alias{plot.LevelsVsLayerByMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plot levels per layer and minor allel frequency
}
\description{
A generic function to plot \code{LevelsVsLayerByMAF} objects. 
}
\usage{
plot.LevelsVsLayerByMAF(x = list(), 
	main_vec = paste("MAF =", c("0.05", "0.10", "0.25", "0.40")), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of \code{class} \emph{LevelsVsLayerByMAF} that is returned by 
	\code{\link{tableLevelsVsLayerByMAF}}.
}
  \item{main_vec}{
	A \code{vector} of the same length as \code{x}. Can be of any type accepted by 
	\code{main} in \code{\link{plot}}. 
}
  \item{...}{
  	Further arguments passed to \code{\link{plot}}.
}
}


\seealso{
	\code{\link{tableLevelsVsLayerByMAF}}, \code{\link{plot.LevelsVsLayer}} 
}
\examples{
YX <- simulateSNPdata(n.obs = 500, n.snp = 200, seed = 123, 
		maf = rep(c(0.05, 0.1, 0.25, 0.4), each = 50))
dim(YX)
X <- matrix(as.integer(as.matrix(YX[ ,-1])), ncol = ncol(YX[ ,-1]))

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)

## category frequencies
cF <- getCategoryFrequenciesForest(rF, X = YX[ , -1])

## table levels vs. layer by maf
tab.all <- tableLevelsVsLayerByMAF(cF, rF)

## do the plot
plot(tab.all)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ plot }
\keyword{ LevelsVsLayerByMAF }% __ONLY ONE__ keyword per line
