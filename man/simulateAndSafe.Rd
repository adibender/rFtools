\name{simulateAndSafe}
\alias{simulateAndSafe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ wraper to create all objects needed for the further (meta-) analysis 
of random forests. 
}
\description{
This wrapper simulates a data set, performs \code{randomForest} and 
extracts category frequency information and safes the according objects 
to the hard drive. 
}
\usage{
simulateAndSafe(seed = 123, n.obs = 500, n.snp = 200, 
	maf = rep(c(0.05, 0.1, 0.25, 0.4), each = 50), ntree = 500, 
	path = "~/", subpaths = c("SNPdata/", "rF/", "cforest/", "cF/"), 
	outprefix = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seed}{
	The seed to be used in data simulation. 
}
  \item{n.obs}{
	A \code{integer}. The number of observations.
}
  \item{n.snp}{
	A \code{integer}. The number of SNPs.
}
  \item{maf}{
	A \code{numeric} of length \code{n.snps} indicating the MAF with which 
	the according SNP should be simulated. 
}
  \item{ntree}{
	Passed to \code{randomForest}.
}
  \item{path}{
	The path in which to create the object sub-folders and save the objects to.
}
  \item{subpaths}{
  The names of the folders where the according objects will be saved to.
  
}
  \item{outprefix}{
  	A \code{character} to prefix all object names when they are saved. 
  	Usually not needed, because the \code{seed} used in the function call will
  	also be part of the names. 
}
}


\seealso{
\code{\link{simulateSNPdata}}, \code{\link{simulateAndTable}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Simulate }
