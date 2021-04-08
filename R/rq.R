#' rq
#'
#' @description Calculates the correlation coefficient between sorted data and theoretical quantiles in a univariate QQ plot
#' @details For a variable, univariateqq() is invoked to get quantiles, then the corelation coefficient is calculated
#'
#' @param x the vector of data to calculate rq for
#'
#' @return List ouput containing the rq value
#'
#' @export
#'
#' @examples
#' rq(x = c(2,4,3,7,9,1,3,5,6))
rq <- function(x){
    theoquant <- univariateqq(x)
    coeff <- cov(x,theoquant$TheoreticalQuantiles)

    list(RQval = coeff)
}
