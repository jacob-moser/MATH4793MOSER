#' univariatexbarchart
#'
#' @description Creates a univariate quality control chart based on the variable mean
#' @details UThe quality control chart shows the observations in relation to the mean across a sample
#'
#' @param x the data vector to create the chart for
#'
#'
#' @return Quality control chart as a ggplot object
#'
#' @export
#'
#' @examples
#' \dontrun{ univariatexbarchart(x = c(3,6,7,7,9,2,4,5,5,6,7))}
univariatexbarchart <- function(x) {
  obsnumber <- 1:length(x)
  use <- data.frame(matrix(c(obsnumber,x), ncol = 2))

  mean <- mean(x)
  UCL <- mean(x) + 3 * var(x)^0.5
  LCL <- mean(x) - 3 * var(x)^0.5

  g <- ggplot2::ggplot(use, ggplot2::aes(x = X1, y = X2)) + ggplot2::geom_point() + ggplot2::geom_hline(ggplot2::aes(yintercept = mean)) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = UCL)) + ggplot2::geom_hline(ggplot2::aes(yintercept = LCL)) + ggplot2::geom_path() +
    ggplot2::xlab("Observation Number") + ggplot2::ylab("Observation Value") + ggplot2::ggtitle("Univariate Quality Control Chart") +
    ggplot2::geom_label(ggplot2::aes(x = 0, y = UCL, label = "UCL")) + ggplot2::geom_label(ggplot2::aes(x = 0, y = mean, label = "Mean")) +
    ggplot2::geom_label(ggplot2::aes(x = 0, y = LCL, label = "LCL"))

  list(Plot = g)
}
