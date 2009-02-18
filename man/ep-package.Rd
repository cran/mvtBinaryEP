\name{mvtBinaryEP-package}
\alias{mvtBinaryEP-package}
\alias{mvtBinaryEP-package}
\docType{package}
\title{
A Package For Generating Correlated Binary Data
}

\description{
Uses the algorithm of Emrich and Piedmonte (1991) to generate
correlated binary data for a given correlation matrix
}

\details{
\tabular{ll}{
Package: \tab mvtBinaryEP \cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-02-16\cr
License: \tab GPL(>=2) \cr
}
The most important function is \code{ep}.  Other functions include
\code{ranMVN}, \code{ranMVN2}, and \code{ranMvnXch}.  These are used
in the \code{ep} function to generate correlated binary responses.
The user may use these functions if multivariate normal data is desired.
We note that the \code{mvtnorm} package also avails the user of functions to
generate multivariate data which may be more efficient than our mutivariate normal
functions.
}
\author{
These routines were written by Dr. Bahjat Qaqish in SAS and translated to R by
Kunthel By.  Questions may be relayed to <kby@bios.unc.edu>
}
\references{
Emrich, L.J. and Piedmonte, M.R., A method for generating high-dimensional multivariate
binary variates, The American Statistician, 45:302-304, 1991.
}