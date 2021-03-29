#' bivariatenormcheck
#'
#' @description Uses a bivariate normality check to calculate the proportion of bivariate data falling in an expected 50% confidence ellipse
#' @details User provides two data vectors for function to check the pairs of data against 50% confidence ellipse
#'
#'
#' @return Text output describing result of the test
#'
#' @export
#'
#' @examples
#' bivariatenormcheck(x1 = c(1,4,3,3,5,6,7), x2 = c(20,15,8,7,40,30,22))
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
