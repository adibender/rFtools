\name{tableLevelsVsLayer}
\alias{tableLevelsVsLayer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ tables levels vs. layer
}
\description{
Basically a helper function called by \code{\link{tableLevelsVsLayerByMAF}}. 
Is itself just a wrapper for the underlying Rcpp/C++ function with the same name.
A low-level function that probably shouldn't be used by itself.
}
\usage{
tableLevelsVsLayer(nlevels = numeric(), layerVec = numeric(), 
	uniqueLayers = numeric())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nlevels}{
	A \code{numeric} vector.
}
  \item{layerVec}{
	A \code{numeric} vector.
}
  \item{uniqueLayers}{
  	A \code{numeric} vector containing unique values in \code{layerVec}.
}
}

\value{
	A \code{matrix} containing information on how many 2-/3-categorical nodes
	appear in a certain layer of the tree(s) of a \code{randomForest}.
}

\seealso{
\code{\link{tableLevelsVsLayerByMAF}}
}


\keyword{ table }
\keyword{ Levels }
\keyword{ Layer }% __ONLY ONE__ keyword per line
