\name{tableLevelsVsLayerCFbyMAF}
\alias{tableLevelsVsLayerCFbyMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
	table cforest category frequency information by MAF
}
\description{
Given a \code{RandomForest} object as returned by \code{cforest}
this function returns tabled information on the frequency of 1-, 2- and 
3-categorical variables in the different layers of the forest stratified by MAF. 
}
\usage{
tableLevelsVsLayerCFbyMAF(cforest, 
	mafIntervals = list(1:50, 51:100, 101:150, 151:200))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cforest}{
	Either a \code{RandomForest} object as returned by the \code{cforest}-function
	from the \code{party} package or a \code{character} indicating the path 
	to the according object. 
}
  \item{mafIntervals}{
	A \code{list} with each element giving the indices of variables
	being simulated with the according MAF. Depends on how the data was simulated
	in \code{\link{simulateSNPdata}}. 
}
}

\value{
A \code{list} of same length as \code{mafIntervals}. Each element contains
tabled information for one MAF agglomerated over the whole forest. 
}

\seealso{
\code{\link{plot.LevelsVsLayerCFbyMAF}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ LevelsVsLayer }
\keyword{ cforest }% __ONLY ONE__ keyword per line
