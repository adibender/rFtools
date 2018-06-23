\name{simulateSNPdata}
\alias{simulateSNPdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ simulate SNP data
}
\description{
This is a very simple function to simulate SNP data. For more sophisticated 
methods for simulating SNP data consider for example the \code{scrime} package.
}
\usage{
simulateSNPdata(n.obs = 500, n.snp = 200, seed = NA, 
	maf = rep(c(0.05, 0.1, 0.25, 0.4), each = 50), prob1 = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n.obs}{
	A \code{integer}. The number of observations.
}
  \item{n.snp}{
	A \code{integer}. The number of SNPs.
}
  \item{seed}{
	The seed to be used in data simulation. 
}
  \item{maf}{
	A \code{numeric} of length \code{n.snps} indicating the MAF with which 
	the according SNP should be simulated. 
}

  \item{prob1}{
 	The probability with which every observation of the response vector \code{y} 
 	has value 1. Defaults to \code{0.5}. 
}
}

\value{
A \code{data.frame} with dimensions \code{n.obs x (n.snps + 1)}. The 
response \code{y} is contained in the first column and is simulated binary (0,1), 
with equal probabilities. 
}

\note{
The user can provide his own data set and use the other functions in this package
as long as the first column contains the response and the other columns 
1-3 categorical variables coded (0,1,2). One could use the more advanced 
package \code{scrime} to simulate the SNP data for example. 
}


\examples{
YX <- simulateSNPdata(seed = 123)
dim(YX) 
class(YX)
colnames(YX)[1:10]
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Simulate }