#' proportionnormaltest
#'
#' @description Checks the univariate normality for each column of a data matrix based on the expected proporiton of observations falling within 1 and 2 standard deviations of the mean
#'
#' @details User provides a multivariate data set and the function produces a list output calculating the propotion of data for each individual variable within one and two standard deviations. This is checked against the theoretical expectations of 0.638 and 0.954 within the theoretically expected range for the sampling distribution of the proportion
#'
#' @param data data set to conduct the test on
#'
#' @return List ouput containing the proportions and output signalling whether the proportion is in the expected range for a normal variable or not
#'
#' @export
#'
#' @examples
#' proportionnormaltest(data = matrix(c(2,4,3,7,9,1,3,5,6), nrow = 3, ncol =3))
proportionnormaltest <- function(data){

  vars = length(data[1,])
  obs = length(data[,1])
  sensitivity1 = 1.369/(obs^0.5)
  sensitivity2 = 0.628/(obs^0.5)
  outcomes = matrix(nrow = 4, ncol = vars)
  for (i in 1:vars){
    outcomes[1,i] = round(length(which(data[,i] >= mean(data[,i]) - var(data[,i])^0.5 & data[,i] <= mean(data[,i]) + var(data[,i])^0.5)) / obs,3)
    }

  for (i in 1:vars){
    if(outcomes[1,i] < 0.638 + sensitivity1 & outcomes[1,i] > 0.638 - sensitivity1){
      outcomes[2,i] = "In Expected Range"
    }
    if(outcomes[1,i] >= 0.638 + sensitivity1 | outcomes[1,i] <= 0.638 - sensitivity1){
      outcomes[2,i] = "Not In Expected Range"
    }
  }

  for (i in 1:vars){
    outcomes[3,i] = round(length(which(data[,i] >= mean(data[,i]) - 2 * var(data[,i])^0.5 & data[,i] <= mean(data[,i]) + 2 * var(data[,i])^0.5)) / obs,3)
  }

  for (i in 1:vars){
    if(outcomes[3,i] < 0.954 + sensitivity2 & outcomes[3,i] > 0.954 - sensitivity2){
      outcomes[4,i] = "In Expected Range"
    }
    if(outcomes[3,i] >= 0.954 + sensitivity2 | outcomes[3,i] <= 0.954 - sensitivity2){
      outcomes[4,i] = "Not In Expected Range"
    }
  }

  list(WithinOneStandardDeviationProportion = outcomes[1,], WithinOneExpectation = outcomes[2,], WithinTwoStandardDeviationsProportion = outcomes[3,], WithinTwoExpectation = outcomes[4,])
}
