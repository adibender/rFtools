\name{tableLevelsVsLayerByMAF}
\alias{tableLevelsVsLayerByMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ table category frequency information by MAF
}
\description{
Given a \code{CategoryFrequenciesForest}-object this function returns 
tabled information on how often 1-, 2- and 3-categorical variables are 
in the different layers stratified by MAF.
}
\usage{
tableLevelsVsLayerByMAF(cF, rF, 
	mafIntervals = list(maf0.05 = 1:50, maf0.10 = 51:100, 
	maf0.25 = 101:150, maf0.40 = 151:200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cF}{
	An \code{CategoryFrequenciesForest}-object as returned by 
	\code{\link{getCategoryFrequenciesForest}}.
}
  \item{rF}{
	An \code{randomForest}-object as returned by \code{randomForest}.
}
  \item{mafIntervals}{
	A \code{list} with each element giving the indices of variables
	being simulated with the according MAF. Depends on how the data was simulated
	in \code{\link{simulateSNPdata}}. 
}
}
\details{
This function splits the data set by MAF as indicated in \code{mafIntervals} 
and calls \code{\link{tableLevelsVsLayer}} which in turn calls the Rcpp/C++ 
function of the same name. 
	 
}

\value{
A \code{list} of same length as \code{mafIntervals}. Each element contains
tabled information for one MAF agglomerated over the whole forest as returned 
by \code{\link{tableLevelsVsLayer}}. 
}


\seealso{
\code{\link{tableLevelsVsLayer}}, \code{\link{getCategoryFrequenciesForest}}
}

\keyword{ Layer }
\keyword{ Levels }
\keyword{ table }% __ONLY ONE__ keyword per line
