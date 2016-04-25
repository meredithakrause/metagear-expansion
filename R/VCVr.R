#' @title Correlation Coefficent for common control
#' 
#' @param table
#' @param r.ij Column label for the correlation coefficent (r) of two variables i and j
#' @param r.ik Column label for the correlation coefficient of two variables i and k
#' @param r.jk Column label for the correlation coefficient of two variables j and k
#' @param n Column label for the sample size
#' 
#' @return Variance and covariance of r 
#' 
#' @description VCVr calculates the variance and covariance of r for two data inputs that share the X, SD, and N of a control group and returns these values as a vector.
#' 
#' @example #Note that "VCVr.test" is an imported table
#' VCVr(VCVr.test)
#' 
#' @references Olkin, Ingram and Jeremy D. Finn. 1995. Correlations Redux. Psycological Bulletin. 118(1): 155-164.     


VCVr <- function (x) {
  r.df <- data.frame(
    SOURCE = c("r.ij", "r.ik", "r.jk", "n")
  )  
  #r.ij = r of variables i and j
  #r.ik = r of variables i and k
  #r.jk = r of variables j and k
  #n = sample size
  
  var.r.ij <- (1 - r.ij^2)^2 / n
  var.r.ik <- (1 - r.ik^2)^2 / n
  
  cov.r1 <- ((2 * r.jk) - r.ij * r.ik) * (1 - r.ij^2 - r.ik^2 - r.jk^2)
  cov.r2 <- ((1/2) * cov.r1) + r.jk^3
  cov.r3 <- cov.r2/n
  
  return(c(var.r.ij, var.r.ik, cov.r3))
}