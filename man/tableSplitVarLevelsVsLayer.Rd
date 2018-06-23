\name{tableSplitVarLevelsVsLayer}
\alias{tableSplitVarLevelsVsLayer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ table category frequency information for split variables
}
\description{
 Same as \code{\link{tableLevelsVsLayer}} except only variables/SNPs used 
 for splitting are considered.  A low-level function used in 
 \code{\link{tableSplitVarLevelsVsLayerByMAF}}.
}
\usage{
tableSplitVarLevelsVsLayer(nlevels = numeric(), layerVec = numeric(), 
	uniqueLayers = numeric())
}

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
	A \code{matrix} containing information on how often the variables used 
	for splitting are 2- or 3-categorical in the different layers of the tree.
}


\seealso{
\code{\link{tableSplitVarLevelsVsLayerByMAF}}, \code{\link{tableSplitVarsByLayerAndMAF}}
}

\keyword{ Split Variable }
\keyword{ Levels }
\keyword{ Layer }
\keyword{ table }% __ONLY ONE__ keyword per line
