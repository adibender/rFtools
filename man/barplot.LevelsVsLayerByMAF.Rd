\name{barplot.LevelsVsLayerByMAF}
\alias{barplot.LevelsVsLayerByMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
	barplot of category frequencies separated by MAF
}
\description{
Visualizes the number of 1-, 2- and 3-categorical nodes in each
layer agglomerated over the whole forest and separated by the 
minor allel frequency (MAF).
}
\usage{
barplot.LevelsVsLayerByMAF(height = list(), col = 1:3, beside = TRUE,  
	legend.text = 1:3, cex.axis = 0.8, names.arg = NULL, main = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{height}{
  	An object of \code{class} \emph{LevelsVsLayerByMAF} that is returned 
  	by \code{\link{tableLevelsVsLayerByMAF}}.
}
  \item{col}{
	A \code{vector} of the same length as the number of rows in each element of 
	the \code{height} argument. Can be of any type accepted for \code{col} 
	by \code{\link{plot}}. Specifies the colors of the plotted bars. 
	Defaults to \code{1:3}.
}
  \item{beside, 
  legend.text, 
  cex.axis, 
  names.arg}{
  	See \code{barplot}.
}
  \item{main}{
  A \code{vector} of the same length as \code{height}.
 }
  \item{...}{
  Further arguments that are passed to \code{barplot}.
}
}



\seealso{
\code{\link{tableLevelsVsLayerByMAF}}
}

\keyword{plot}
\keyword{LevelsVsLayer}% __ONLY ONE__ keyword per line
