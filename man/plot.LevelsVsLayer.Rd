\name{plot.LevelsVsLayer}
\alias{plot.LevelsVsLayer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ visualization of levels per layer 
}
\description{
This function visualizes frequencies of 1-, 2- and 3-categorical variables in 
each layer of the tree.

}
\usage{
plot.LevelsVsLayer(x, main = character(), col_vec = NULL, 
	type_vec = NULL, pch_vec = NULL, legend_text = NULL, xlab = "layer", 
	ylab = "relative frequency", ...)
}

\arguments{
  \item{x}{
		An object of \code{class} \emph{LevelsVsLayer} that is returned by 
		\code{\link{tableLevelsVsLayer}}.
}
  \item{main}{
	See \code{\link{plot}}.
}
  \item{col_vec}{
  	A \code{vector} with colors of length \code{nrow(x)}. Can be of any 
  	type accepted for \code{col} by \code{\link{plot}}. Specifies the colors of 
  	the plotted lines. Defaults to \code{1:3}.
}
  \item{type_vec}{
  	Same as \code{col}. Specifies the \code{type} of the plotted lines.
  	Defaults to \code{"o"}.
}
  \item{pch_vec}{
  Same as \code{col}. Specifies the \code{pch} of the points, if \code{type = "o"}.
  Defaults to \code{4}.
}
  \item{legend_text}{
  	The \code{legend} argument of the \code{\link{legend}} function.
}
  \item{xlab, ylab}{
  	Arguments passed to \code{\link{plot}}.
}

  \item{...}{
   Further arguments passed to \code{\link{plot}}.
}
}




%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{tableLevelsVsLayer}}, \code{\link{plot.LevelsVsLayerByMAF}}, 
\code{\link{tableLevelsVsLayerByMAF}}
}

\keyword{plot}
\keyword{LevelsVsLayer}% __ONLY ONE__ keyword per line
