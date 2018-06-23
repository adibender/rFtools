\name{plot.LevelsVsLayerCFbyMAF}
\alias{plot.LevelsVsLayerCFbyMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ plot levels per layer and MAF for conditional forests
}
\description{
A generic function to plot \code{LevelVsLayerCFbyMAF} objects as returned 
by \code{tableLevelsVsLayerCFbyMAF}. 
}
\usage{
plot.LevelsVsLayerCFbyMAF(x, norm.by.nodes = TRUE, legPos = "topleft", 
col = 1:4, ylab = NULL, xlab = "layer", type = "o", pch = 4, 
legend = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	An object of \code{class} \emph{LevelsVsLayerCFbyMAF} that is returned by 
	\code{\link{tableLevelsVsLayerCFbyMAF}}
}
  \item{norm.by.nodes}{
	A \code{logical} indicating whether the absolute category frequencies 
	in each layer of the forest 
	should be normed by the number of nodes per layer. Defaults to \code{TRUE}.
}
  \item{legPos}{
	The position of the legend. Passed to \code{legend}.
}
  \item{col, ylab, xlab, type, pch}{
	Arguments passed to \code{plot}.
}
 
  \item{legend}{
	Passed to \code{legend}.
}
  \item{\dots}{
	Further arguments passed to \code{plot}.
}
}

\seealso{
\code{\link{tableLevelsVsLayerCFbyMAF}}
}
\examples{
YX <- simulateSNPdata(n.obs = 500, n.snp = 200, seed = 123, 
		maf = rep(c(0.05, 0.1, 0.25, 0.4), each = 50))

cForest <- cforest(y ~ ., data = YX, 
	controls = cforest_control(mtry = sqrt(ncol(YX) - 1), replace = FALSE))
	
cForestTab <- tableLevelsVsLayerCFbyMAF(cForest)

plot(cForestTab)
plot(cForestTab, norm.by.nodes = FALSE, legPos = "topright")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LevelsVsLayerByMAF }
\keyword{ cforest }% __ONLY ONE__ keyword per line
