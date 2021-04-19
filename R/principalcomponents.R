#' principalcomponents
#'
#' @description Computes relevant values and output related to the principal components of a sample of data
#' @details Computes the covariance matrix for the sample, the principal components, the lambda values, the total variance and the correlations between the variables and principal components
#'
#' @param data data set for which some principal component calculations are to be performed
#'
#' @return A list containing the sample covariance matrix, the principal components, the eigenvalues, the total variance, and a matrix of the correlations between the variables and the principal components
#' @export
#'
#' @examples \dontrun{ principalcomponents(data = MATH4793MOSER::welder)}
principalcomponents <- function(data){
  s <- cov(data)
  e <- eigen(s)
  p <- length(data[1,])
  correlations <- matrix(nrow = p, ncol = p)

  for(i in 1:p){
    for(j in 1:p){
      correlations[j,i] = e$vectors[j,i] * sqrt(e$values[i]) / sqrt(var(data[j]))
    }
  }

  list(S = s, PrincipalComponents = e$vectors, Eigenvalues = e$values, TotalVariance = sum(e$values), ryixk = correlations)
}
