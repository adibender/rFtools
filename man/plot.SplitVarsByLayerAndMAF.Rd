\name{plot.SplitVarsByLayerAndMAF}
\alias{plot.SplitVarsByLayerAndMAF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ visualize category frequencies of split variable }
\description{
 Plots (relative) frequencies of 2- and/or 3- categorical variables stratified 
 for Layer and MAF.  
}
\usage{
plot.SplitVarsByLayerAndMAF(x = list(), fun = relative,
	row.num = 1, col.num = 1, row.denom = NULL, col.denom = NULL,   
	col = 1:4, main = character(), ylab = character(), ylim, xlim, 
	legPos = "topright", norm.by.nodes = FALSE, layerForest, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
 A \code{list} of class \code{SplitVarsByLayerAndMAF} that is returned 
 by \code{\link{tableSplitVarsByLayerAndMAF}}.
}
  \item{fun}{
	The \code{function} to preprocess the data (\code{x}) to be plotted. 
	One of \code{c("relative", "absolute")}. Defaults to \code{"relative"}.
	See \code{details}.
}
  \item{row.num, col.num, row.denom, col.denom}{
  	Passed to \code{fun} (see above). See \code{details} section.

}
  \item{col}{
	A \code{vector} containing the colors to be used for the different MAFs.
	Passed to \code{plot}.
}

  \item{main, ylab, xlim, ylim}{
 	See \code{plot}. 
}

  \item{legPos}{
	The position of the legend. Passed to \code{legend}.
}
  \item{norm.by.nodes}{
	A \code{logical} indicating whether the absolute/relative frequencies 
	should be normed by the number of nodes per layer. Defaults to \code{FALSE}. 
}
  \item{layerForest}{
 	If \code{norm.by.nodes = TRUE} one has to provide the layer information 
 	for the according forest (see examples). 
} 

  \item{...}{
  	Further arguments passed to \code{plot}.
}
}

\details{
	If \code{ fun == "absolute" }, \code{row.denom} and \code{col.denom}
	are ignored, and fun simply extracts the frequencies from \code{x}
	for the according \code{row.num, col.num} combination in all list elements, 
	i.e. all layers. 
	
	If \code{fun == "relative"}, per default you get relative instead 
	of absolute frequencies when \code{row.denom} and \code{col.denom} are not 
	specified. Otherwise for all list-elements the  quotient 
	\code{tab[row.nom, col.nom]/tab[row.denom, col.denom]} is returned.
}

\seealso{
\code{\link{tableSplitVarsByLayerAndMAF}}
}
\examples{
## data
YX <- simulateSNPdata(seed = 123)

## forest
rF <- randomForest(x = YX[ , -1], y = YX[ , 1], keep.forest = TRUE, 
		keep.inbag = TRUE, importance = TRUE, ntree = 500)
		
## category frequencies
cF <- getCategoryFrequenciesForest(rF, YX[ , -1])

## table for all Levels vs. Layer
tab1 <- tableLevelsVsLayerByMAF(cF, rF)
tab2 <- tableSplitVarLevelsVsLayerByMAF(cF, rF)
tab3 <- tableSplitVarsByLayerAndMAF(tab1, tab2)

layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(tab3, row.num = 1, col.num = 1, legPos = "topleft", norm.by.nodes = FALSE)
plot(tab3, row.num = 2, col.num = 1, legPos = "topleft", norm.by.nodes = FALSE)
plot(tab3, row.num = 3, col.num = 1, legPos = "topleft", norm.by.nodes = FALSE)
plot(tab3, row.num = 1, col.num = 1, fun = "absolute", norm.by.nodes = FALSE)

## normed by number of nodes per layer
lF <- getLayerForest(getAncestryForest(rF))

layout(matrix(1:4, nrow = 2, byrow = TRUE))
plot(tab3, layerForest = lF, row.num = 1, col.num = 1, legPos = "topleft")
plot(tab3, layerForest = lF, row.num = 2, col.num = 1)
plot(tab3, layerForest = lF, row.num = 3, col.num = 1, legPos = "topleft")
plot(tab3, layerForest = lF, row.num = 1, col.num = 1, 
fun = "relative", legPos = "topleft")
}

\keyword{ plot }
\keyword{ Split Variables }% __ONLY ONE__ keyword per line
