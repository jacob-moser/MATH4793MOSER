#' univariateqq
#'
#' @description Checks the univariate normality for each column of a data matrix based on the expected proporiton of observations falling within 1 and 2 standard deviations of the mean
#'
#' @details User provides a multivariate data set and the function produces a list output calculating the propotion of data for each individual variable within one and two standard deviations. This is checked against the theoretical expectations of 0.638 and 0.954 within the theoretically expected range for the sampling distribution of the proportion
#'
#'
#' @return List ouput containing the proportions and output signalling whether the proportion is in the expected range for a normal variable or not
#'
#' @export
#'
#' @examples
#' proportionnormaltest(data = matrix(c(2,4,3,7,9,1,3,5,6), nrow = 3, ncol =3))
univariateqq <- function(x){
  normquants <- matrix(nrow = length(x), ncol = 1)
  sortx <- sort(x)
  for(i in 1:length(x)){
    normquants[i] = qnorm((i-0.5)/length(x))
  }
  list(TheoreticalQuantiles = normquants, Data = sortx)

}
