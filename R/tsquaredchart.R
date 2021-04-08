#' tsquaredchart
#'
#' @description Creates a t squared plot for a data set
#' @details The t squared chart helps the suer see abnormal observations based on 95 and 99% confidence threshholds
#'
#' @param data the data set to create the plot for
#' @param conflevel1 first confidence level for the chart. Decimal represents percentage NOT alpha level
#' @param conflevel2 second confidence level for the chart. Decimal representes percentage NOT alpha level
#'
#'
#' @return A ggplot object containing the plot
#'
#' @export
#'
#' @examples
#' \dontrun{ tsquaredchart(data = MATH4793MOSER::welder)}
tsquaredchart <- function(data, conflevel1 = 0.95, conflevel2 = 0.99){
  obsnum <- 1:length(data[,1])
  xbar <- as.numeric(matrix(colMeans(data)))
  S <- cov(data)
  Sinv <- solve(S)

  tsquared <- vector()

  for(i in 1:length(data[,1])){
    d <- t(as.numeric(matrix(data[i,])) - xbar) %*% Sinv %*% (as.numeric(matrix(data[i,]))-xbar)
    tsquared <- c(tsquared,d)
  }

  use <- matrix(c(obsnum,tsquared), ncol = 2)
  use <- data.frame(use)

  g <- ggplot2::ggplot(use, ggplot2::aes(x = X1, y = X2)) + ggplot2::geom_point() + ggplot2::xlab("Observation Number") +
    ggplot2::ylab("T-Squared Value") + ggplot2::ggtitle("T-Squared Chart") + ggplot2::geom_hline(ggplot2::aes(yintercept = qchisq(0.95,length(data[1,])))) +          ggplot2::geom_hline(ggplot2::aes(yintercept = qchisq(0.99,length(data[1,])))) +
    ggplot2::geom_label(ggplot2::aes(x = 2, y = qchisq(conflevel1,length(data[1,])), label = paste(conflevel1*100, "% Limit"))) +
    ggplot2::geom_label(ggplot2::aes(x = 2, y = qchisq(conflevel2,length(data[1,])), label = paste(conflevel2*100, "% Limit")))

  list(Plot = g)

}
