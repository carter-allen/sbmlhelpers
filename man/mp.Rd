% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mp.R
\name{mp}
\alias{mp}
\title{The mp function}
\usage{
mp(vec, K)
}
\arguments{
\item{vec}{A vector MCMC cluster labelings for a single node}

\item{K}{the number of clusters specified in the SBM}
}
\description{
This function allows you to obtain the posterior
probability of membership in each cluster for one
node at a time.
}
\examples{
apply(t(apply(Sigma, 2, mp, K)), 1, which.max)
}
\keyword{SBM}
