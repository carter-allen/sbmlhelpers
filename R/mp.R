#' The mp function
#'
#' This function allows you to obtain the posterior
#' probability of membership in each cluster for one
#' node at a time.
#' @param vec A vector MCMC cluster labelings for a single node
#' @param K the number of clusters specified in the SBM
#' @keywords SBM
#' @export
#' @examples
#' apply(t(apply(Sigma, 2, mp, K)), 1, which.max)
mp <- function(vec, K)
{
  v = rep(1:K)
  l = length(vec)

  for (i in 1:K)
  {
    v[i] = sum(vec==i)/l
  }
  return(v)
}
