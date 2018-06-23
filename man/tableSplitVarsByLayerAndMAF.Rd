\name{tableSplitVarsByLayerAndMAF}
\alias{tableSplitVarsByLayerAndMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
table category frequency information stratified by layer and MAF
}
\description{
For each layer of a tree and every MAF this functions returns 
tables with information on how often 2- and 3-categorical variables 
were or were not used for splitting. 
}
\usage{
tableSplitVarsByLayerAndMAF(LevelsVsLayerByMAF, splitVarLevelsVsLayerByMAF)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LevelsVsLayerByMAF}{
	A \code{LevelsVsLayerByMAF}-object that is returned by 
	\code{\link{tableLevelsVsLayerByMAF}}
}
  \item{splitVarLevelsVsLayerByMAF}{
	A \code{splitVarLevelsVsLayerByMAF}-object that is returned by 
	\code{\link{tableSplitVarLevelsVsLayerByMAF}}.
}
}

\value{
A \code{list} of the same length number of MAFs. Each element contains 
a list of the same length as the number of layers in the according tree. 
}


\seealso{
\code{\link{tableLevelsVsLayerByMAF}}, \code{\link{tableSplitVarLevelsVsLayerByMAF}}
}

\keyword{ table }
\keyword{ Layer }
\keyword{ Levels }
\keyword{ Split Variables }% __ONLY ONE__ keyword per line
