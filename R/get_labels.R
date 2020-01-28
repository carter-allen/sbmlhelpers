#' The get_labels function
#'
#' function to obtain remapped posterior centroid estimator of cluster labels
#' see Peng & Car. 2016.
#' @param fit A model object obtained with sbmlogit.mcmc()
#' @keywords SBM
#' @export
#' @examples
#' get_labels(fitK2)
get_labels <- function(fit)
{
    K = fit$ngroups # number of clusters
    Sigma = fit$sample # posterior samples
    sigma = apply(t(apply(Sigma, 2, mp, K)), 1, which.max) # posterior estimator
    scentroid = sbmlogit.remap(sigma) # remapped posterior estimator
    return(scentroid)
}
