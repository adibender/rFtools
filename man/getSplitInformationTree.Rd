\name{getSplitInformationTree}
\alias{getSplitInformationTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get split information for all not terminal nodes in tree
}
\description{
For a given tree-object this function returns the split information
for each not terminal node in the tree. 
}
\usage{
getSplitInformationTree(tree = matrix(), inbagInd = logical(), X = matrix())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
	A \code{matrix} as returned by \code{getTree}. 
}
  \item{inbagInd}{
	A \code{logical} indicating inbag observations. 
}
  \item{X}{
	A \code{matrix} containing the (SNP-) variables used in the call 
	to \code{randomForest}.
}
}

\value{
A \code{list} with 3 components:
\item{indNodes}{A \code{logical} indicating inbag variables in the according nodes.}
\item{ntSplits}{The actual split used in the according node. 
Gives the categories going to the left branch of the tree.}
\item{splitVars}{The variables used for splitting the according nodes.}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getSplitInformationForest}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Split Info }
\keyword{ Tree }% __ONLY ONE__ keyword per line
