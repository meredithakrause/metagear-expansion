#' @title Correlation Coefficent for common control
#' 
#' @param table
#' @param r_ij Column label for the correlation coefficent (r) of two variables i and j
#' @param r_ik Column label for the correlation coefficient of two variables i and k
#' @param r_jk Column label for the correlation coefficient of two variables j and k
#' @param n Column label for the sample size
#' 
#' @return Variance and covariance of r 
#' 
#' @description VCVr calculates the variance and covariance of r for two data inputs that share the X, SD, and N of a control group and returns these values as a vector.  


VCVr <- function (x) {
  r.df <- data.frame(
    SOURCE = c("r_ij", "r_ik", "r_jk", "n")
  )  
  #r_ij = r of variables i and j
  #r_ik = r of variables i and k
  #r_jk = r of variables j and k
  #n = sample size
  
  var_r_ij <- (1 - r_ij^2)^2 / n
  var_r_ik <- (1 - r_ik^2)^2 / n
  
  cov_r1 <- ((2 * r_jk) - r_ij * r_ik) * (1 - r_ij^2 - r_ik^2 - r_jk^2)
  cov_r2 <- ((1/2) * cov_r1) + r_jk^3
  cov_r3 <- cov_r2/n
  
  return(c(var_r_ij, var_r_ik, cov_r3))
}