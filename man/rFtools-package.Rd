\name{rFtools-package}
\alias{rFtools-package}
\alias{rFtools}
\docType{package}
\title{ Tools for the (meta-) analysis of Random Forests}
\description{
This package provides functions for the analysis of \code{randomForest} 
(and \code{cforest}) objects 
in general and to replicate simulations conducted for the paper
on "Random Forest Gini importance favors SNPs with large minor allele frequency"
(\url{http://bib.oxfordjournals.org/content/early/2011/09/10/bib.bbr053.abstract}) 
in specific. 
}
\details{
\tabular{ll}{
Package: \tab rFtools\cr
Type: \tab Package\cr
Version: \tab 0.5-6\cr
Date: \tab 2011-01-09\cr
License: \tab GPL (>= 2)\cr
LazyLoad: \tab yes\cr
}

This Package mainly facilitates the extraction of meta-information on 
Random Forests especially \code{randomForest} objects, i.e. ancestry, number 
of observations/levels, split information, etc. for individual nodes or 
all nodes in a tree or forest. While some functions can be used on 
\code{randomForest}-objects in general, others are designed specifically 
to compute information such as category frequencies for SNP-data. 

}
\author{ Andreas Bender

Maintainer: Andreas Bender <bender.at.R@googlemail.com>
}
\references{
Anne-Laure Boulesteix, Andreas Bender,
Justo Lorenzo Bermejo, Carolin Strobl: 
\emph{Random Forest Gini Importance favours SNPs with large minor allele 
frequency}
(\url{http://bib.oxfordjournals.org/content/early/2011/09/10/bib.bbr053.abstract})

Material: 
\url{http://www.ibe.med.uni-muenchen.de/organisation/mitarbeiter/020_professuren/boulesteix/ginibias/}
}
%~~ Optionally other standard keywords, one per line, from file KEYWORDS in ~~
%~~ the R documentation directory ~~
