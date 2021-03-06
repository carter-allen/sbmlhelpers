% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Lei_GOF.R
\name{Lei_GOF}
\alias{Lei_GOF}
\title{Implement Lei's goodness of fit test}
\usage{
Lei_GOF(A, z, alpha = 0.05)
}
\arguments{
\item{A}{A symmetric adjacency matrix with entries of 0 or 1}

\item{z}{A putative cluster labeling vector. The number of entries should equal the row and column dimension of A.}

\item{alpha}{The significance level of the goodness of fit test}
}
\description{
Function to test the fit of a putative stochastic block model labeling
using Lei's goodness of fit test (Lei, 2016)
}
\examples{
Lei_GOF(A,z)
}
\keyword{Lei's}
\keyword{SBM,}
\keyword{fit}
\keyword{goodness}
\keyword{of}
\keyword{test}
