#' pcabootAG
#'
#' @description this function produces confidence intervals for the eigenvector components and eigenvalues of a given data set
#' @details This function produces confidence intervals using bootstrap methods as well as theoretical large sample results from Anderson and Girshick
#'
#' @param data the data set to run the principal component analysis on
#' @param alpha decimal value to express the desired 100(1-alpha) percent confidence level
#' @param iter the number of iterations to be used in the bootstrap sampling
#' @param cov True or false value to indicate use of covariance or correlation matrix where true uses the covariance matrix
#'
#' @return the function automatically plots density histograms for each eigenvalue and a boxplot of estimates for the first two principal components (eigenvectors). It also returns bootstrap confidence intervals for all eigenvector components and eigenvalues as well as Anderson Girshick theoretical confidence intervals for the eigenvalues. The intervals are listed in order on the command line for each eigenvalue. Components are listed by vector in order.
#' @export
#'
#' @examples \dontrun{ pcabootAG(MATH4793MOSER::welder, alpha = 0.15, iter = 2000, cov = TRUE)}
pcabootAG <- function (data, alpha = 0.05, iter = 1000, cov = FALSE){
  p <- length(data[1,])

  vals <-eigestimate(mat = data, iter = iter, cov = cov)

  for(i in 1:p){
    h <- hist(vals$lambdas[,i], plot = FALSE)
    d <- h$density
    den <- d/max(d)
    hist(vals$lambdas[,i], freq = FALSE,
         col = rgb(0,den,1-den^2, 0.7),
         main = expression(paste("Bootstrap distribution of ", widehat(lambda)[i])), xlab = bquote(widehat(lambda)[.(i)]))
  }

  labs <- vector()
  one <- vector()
  for(i in 1:p){
    labs <- c(labs,paste("e1",i))
    one <- c(one,1)
  }
  for(i in 1:p){
    labs <- c(labs,paste("e2",i))
    one <- c(one,1)
  }

  labs <- rep(labs, iter*one)
  cat <- matrix(labs,nr = iter, nc = 2*p, byrow = FALSE)

  boxplot(vals$vectors ~ cat,
          xlab = "Eigenvector Components",
          ylab = "Value of Component")

  lambcis <- vector()
  for(i in 1:p){
    ci <- quantile(vals$lambdas[,i], c(alpha/2, 1-alpha/2))
    lambcis <- c(lambcis, ci)
  }

  vectcis <- vector()
  for(i in 1:p^2){
    ci <- quantile(vals$vectors[,i], c(alpha/2, 1-alpha/2))
    vectcis <- c(vectcis, ci)
  }

  aglambcis <- vector()
  for(i in 1:p){
    ci <- c(mean(vals$lambdas[,i])/(1 + qnorm(1-alpha/2)*sqrt(2/length(vals$lambdas[,i]))),mean(vals$lambdas[,i])/(1 -
                                                                                                                     qnorm(1-alpha/2)*sqrt(2/length(vals$lambdas[,i]))))
    aglambcis <- c(aglambcis,ci)
  }

  list(BootLambdas = lambcis, BootVectors = vectcis, AGLambdas = aglambcis)

}

lambdaestimatecov <- function(mat, indices){
  S <- cov(mat[indices,])
  E <- eigen(S)

  lambda <-E$values
  return(c(lambda))
}

vectorestimatecov <- function(mat, indices){
  S <- cov(mat[indices,])
  E <- eigen(S)

  e <- E$vectors
  return(e)
}

lambdaestimatecor <- function(mat, indices){
  R <- cor(mat[indices,])
  E <- eigen(R)

  lambda <-E$values
  return(c(lambda))
}

vectorestimatecor <- function(mat, indices){
  R <- cor(mat[indices,])
  E <- eigen(R)

  e <- E$vectors
  return(e)
}

eigestimate <- function(mat, iter, cov){
  dat <- mat
  if(cov == TRUE){
    out <-boot::boot(dat, lambdaestimatecov, R = iter)$t

    out2 <- boot::boot(dat,vectorestimatecov, R = iter)$t
  }

  if(cov == FALSE){
    out <- boot::boot(dat, lambdaestimatecor, R = iter)$t

    out2 <- boot::boot(dat,vectorestimatecor, R = iter)$t
  }

  list(lambdas = out, vectors = out2)
}
