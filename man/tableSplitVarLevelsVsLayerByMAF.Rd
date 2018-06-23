\name{tableSplitVarLevelsVsLayerByMAF}
\alias{tableSplitVarLevelsVsLayerByMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ table category frequencies per layer and MAF
}
\description{
Same as \code{\link{tableLevelsVsLayerByMAF}} except only variables 
used for splitting are considered. 
}
\usage{
tableSplitVarLevelsVsLayerByMAF(cFobject, rFobject)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cFobject}{
	A \code{CategoryFrequenciesForest}-object as returned by 
	\code{\link{getCategoryFrequenciesForest}}. 
}
  \item{rFobject}{
	A \code{randomForest}-object as returned by \code{randomForest}.
}
}

\value{
A \code{list} of the same length as number of different MAFs. 
Each element contains
tabled information for one MAF agglomerated over the whole forest as returned 
by \code{\link{tableLevelsVsLayer}}. 
}


\seealso{
\code{\link{tableSplitVarLevelsVsLayer}}
}

\keyword{ Levels }
\keyword{ table }
\keyword{ Layer }
\keyword{ Split Variables }% __ONLY ONE__ keyword per line
