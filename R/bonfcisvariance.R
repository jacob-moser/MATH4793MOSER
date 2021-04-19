#' bonfcisvariance
#'
#' @description Produces Bonferroni confidence intervals for the first q principal components of a sample
#'
#' @param data the data set to calculate the intervals for the variances
#' @param q the number of variances to estimate
#' @param alpha the confidence level for the intervals
#'
#' @return Bonferonni confidence intervals for the first q principal component variances
#' @export
#'
#' @examples \dontrun{ bonfcisvariance(data = MATH4793MOSER::stocks, q = 2, alpha = 0.1)}
bonfcisvariance <- function(data, q = 1, alpha = 0.05){
  S <- cov(data)
  e <- eigen(S)
  n <- length(data[,1])

  intervals <- matrix(nrow = q, ncol = 2)
  for(i in 1:q){
    intervals[i,1] = e$values[i] / (1 - qnorm(alpha/(2*q))*sqrt(2/n))
    intervals[i,2] = e$values[i] / (1 + qnorm(alpha/(2*q))*sqrt(2/n))
  }

  list(Intervals = intervals)

}
