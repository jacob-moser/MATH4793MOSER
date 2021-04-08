#' confidenceellipse
#'
#' @description Generates a confidence ellipse for two variables at a given confidence level
#' @details User provides two data vectors to use as variables in question and alpha level
#'
#'
#' @return A ggplot object containing the scatterplot with confidence ellipse overlayed
#'
#' @param x1 the first varaible's data
#' @param x2 the second varaible's data
#' @param alpha the confidence level for the confidence ellipse
#'
#' @export
#'
#' @examples
#' data <- MATH4793MOSER::welder
#' \dontrun{ confidenceellipse(data$voltage,data$current,alpha=0.1)}
confidenceellipse <- function(x1,x2,alpha = 0.05){
  use <- matrix(c(x1,x2), nrow = length(x1), ncol = 2)
  xbar <- matrix(colMeans(use), nrow = 2, ncol = 1)
  S <- cov(use)
  Sinv <- solve(S)
  chiquant <- qchisq(1-alpha,2)
  e <- eigen(S)
  use2 <- data.frame(use)

  g <- ggplot2::ggplot(use2, ggplot2::aes(x = X1, y = X2)) + ggplot2::geom_point() + ggplot2::ggtitle("Confidence Ellipse") + ggplot2::xlab("First Variable") + ggplot2::ylab("Second Variable")
  g <- g + ggforce::geom_ellipse(ggplot2::aes(x0 = xbar[1,1], y0 = xbar[2,1], a = (e$values[1]*chiquant)^0.5, b = (e$values[2]*chiquant)^0.5, angle = atan(e$vectors[2,1]/e$vectors[1,1])))
  list(Plot = g)
}
