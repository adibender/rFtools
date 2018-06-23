\name{getSplitVariablesTree}
\alias{getSplitVariablesTree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ extract split variables from tree
}
\description{
Given a tree of a \code{randomForest}-object this function extracts 
the variables used for splitting in the not terminal nodes of the 
according tree.
}
\usage{
getSplitVariablesTree(tree = matrix())
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tree}{
	A \code{matrix} that is returned by \code{getTree}. 
}
}

\value{
A \code{vector} of the same length as number of not terminal nodes in the tree.
}


\seealso{
\code{\link{getSplitVariablesForest}}
}
% R documentation directory.
\keyword{ Split Variables }
\keyword{ Tree }% __ONLY ONE__ keyword per line
