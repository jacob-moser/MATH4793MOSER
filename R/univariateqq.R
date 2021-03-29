#' univariateqq
#'
#' @description Produces the quantiles and sorted data to produce a standard normal QQ plot for a single variable
#' @details User provides data as a vector and the quantiles and sorted data are returned
#'
#'
#' @return List ouput containing the quantiles and sorted data
#' @export
#'
#' @examples
#' univariateqq(x = c(2,4,3,7,9,1,3,5,6))
univariateqq <- function(x){
  normquants <- matrix(nrow = length(x), ncol = 1)
  sortx <- sort(x)
  for(i in 1:length(x)){
    normquants[i] = qnorm((i-0.5)/length(x))
  }
  list(TheoreticalQuantiles = normquants, Data = sortx)

}
