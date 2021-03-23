#' bivariatenormcheck
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
bivariatenormcheck <- function(x1,x2){
  xbar = matrix(c(mean(x1),mean(x2)), ncol=1 , nrow =2)
  xdata <- data.frame(x1,x2)
  S = cov(xdata)
  Sinv <- solve(S)
  count = 0
  d = 0
  obs = matrix(c(0,0), ncol = 1, nrow = 2)

  for(i in 1:length(x1)){
    obs[1,1] = x1[i]
    obs[2,1] = x2[i]
    d = t(obs-xbar)%*%Sinv%*%(obs-xbar)
    if(d <= qchisq(0.5,2)){
      count = count + 1
    }
  }

  percentin = count/length(x1)
  list(Outcome = cat(percentin," <- proportion of data falling within expected 50% contour in the Bivariate Normality Check",""))
}
