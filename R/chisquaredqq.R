#' chisqauredqq
#'
#' @description Produces sorted data and theoretical quantiles for a chi-squared QQ plot for a provided data set
#' @details The quantiles and sorted data can be used by a plotting pacakge to create the QQ plot
#'
#' @param data the data set to compute the chi squared quantiles for
#'
#'
#' @return list output with theoretical chi squared quantiles and sorted calculated quantiles
#'
#' @export
#'
#' @examples
#' chisquaredqq(MATH4793MOSER::welder)
chisquaredqq <- function(data){

  xbar <- matrix(colMeans(data), nrow = length(data[1,]), ncol = 1)
  S <- cov(data)
  Sinv <- solve(S)
  d <- matrix(nrow = length(data[,1]), ncol = 1)
  for(i in 1:length(data[,1])){
    d[i,1] = t(t(data[i,])-xbar)%*%Sinv%*%(t(data[i,])-xbar)
  }
  d <- sort(d)
  chivals <- matrix(nrow = length(data[,1]),ncol = 1)
  for(i in 1: length(data[,1])){
    chivals[i,1] = qchisq((i-0.5)/length(data[,1]),df=3)
  }
  list(TheoQuants = chivals, CompQuants = d)
}
