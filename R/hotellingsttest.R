#' hotellingsttest
#'
#' @description This function performs calculations and makes a plot for a bivariate dataset based on Hotelling's T test
#'
#' @details User inputs a bivariate dataset, estimate for the mu vector, and alpha value corresponding to the desired confidence level and gets the outcome of the test, information about the data provided, and a plot of the confidence ellipse
#'
#'
#' @return List output of calculations from Hotelling's T Test and a plot of the confidence ellipse plot
#' @section List of outputs includes:
#' - Outcome of the test
#' - The calculated quadratic size
#' - The scaled quantile
#' - The eigenvectors of the variance covariance matrix
#' - The eigenvalues of the variance covariance matrix
#' - The half major and minor axis lengths of the confidence ellipse
#' - The ratio of the major and minor axes in for the confidence ellipse
#'
#' @section The plot:
#' The plot shows the confidence ellipse with lines marking the mean values and the axes of the ellipse
#'
#' @export
#'
#' @examples
#' hotellingsttest(data = matrix(c(1,2,3,4,5,6),nrow=3,ncol=2), mu = matrix(c(1,2),nrow=2, ncol=1), alpha=0.05)
hotellingsttest <- function(data, mu, alpha){

  ## Conditional stop for inappropriate alpha

  if(alpha > 1 | alpha < 0){
    stop("Alpha must be a numeric value between 0 and 1")
  }

  ## Calculation of relevant quantities

  means <- colMeans(data)
  S <- cov(data)
  Sinv <- solve(S)
  n <- length(data[,1])
  p <- length(data[1,])

  Tsq <- n * (t(means)-t(mu))%*%Sinv%*%(means-mu)

  f <- p*(n-1)/(n-p)*qf(alpha,p,n-p)

  b <- 1- alpha

  percent <- 100 * b

  outcome <- ""

  if(Tsq <= f){
    outcome <- "The the mean vector falls within the confidence region"
  }

  if(Tsq > f){
    outcome <- "The the mean vector does not fall within the confidence region"
  }

  eigenS <- eigen(S)

  halflength1 <- eigenS$values[1]^0.5 * f^0.5
  halflength2 <- eigenS$values[2]^0.5 * f^0.5

  majorhalflength <- max(halflength1,halflength2)
  minorhalflength <- min(halflength1,halflength2)

  axisratio <- majorhalflength/minorhalflength

  xeig <- eigenS$vectors[,1]

  xaxis <- matrix(c(1,0), nrow = 2, ncol = 1)

  ang <- acos((xeig%*%xaxis))

  mean1 <- mean(data[,1])
  mean2 <- mean(data[,2])

  ## Plot

  library(ggplot2)
  library(ggforce)

  g <- ggplot() + labs(x = "X1", y = "X2", title = "Confidence Ellipse")

  ## Produce plot of ellipse

  g <- g + geom_ellipse(aes(x0 = mean1, y0 = mean2, a = halflength1, b = halflength2, angle = ang)) +
    geom_segment(aes(x=0,xend = mean1,y=mean2,yend=mean2)) +
    geom_segment(aes(x=mean1,xend=mean1,y=mean2,yend=0)) +
    geom_label(aes(label = "Mean X1", x = mean1, y = 0)) +
    geom_label(aes(label = "Mean X2", x = 0, y = mean2)) +
    geom_segment(aes(x = mean1 - eigenS$vectors[1,1] * halflength1, y = mean2 - eigenS$vectors[2,1]*halflength1, xend = mean1 + eigenS$vectors[1,1] * halflength1 , yend = mean2 + eigenS$vectors[2,1]*halflength1)) +
    geom_segment(aes(x = mean1 - eigenS$vectors[1,2] * halflength2, y = mean2 - eigenS$vectors[2,2]*halflength2, xend = mean1 + eigenS$vectors[1,2] * halflength2 , yend = mean2 + eigenS$vectors[2,2]*halflength2))
  g

  ## List output

  list(TestResult = outcome, QuadraticSize = Tsq, ScaledQuantile = f, Eigenvectors = eigenS$vectors, Eigenvalues = eigenS$values,
       HalfMajorAxisLength = majorhalflength, HalfMinorAxisLength = minorhalflength, AxisRatio = axisratio)
}
