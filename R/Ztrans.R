#' @title Z transformation or r for common control
#' 
#' @param table
#' @param r.a Column label for the correlation coefficients (r) of group a
#' @param n.a Column label for the sample sizes (n) of group a
#' @param sd.a Column label for the standard deviations (sd) of group a
#' @param r.b Column label for the correaltion coefficients of group b
#' @param n.b Column label for the sample sizes of group b
#' @param sd.b Column label for the standard deviations of group b
#' 
#' @return Z transformations, Variance, and Covariance
#' 
#' @description Z.trans calculates the Z transformation, the variance,and the covariance of two r values and returns these values as a vector
#' 
#' @example #Note that "Ztrans.test" is the name of an imported table
#' Z.trans(Ztrans.test)
 
Z.trans <- function(x) {
z.df <- data.frame(
  SOURCE = c("r.a", "n.a", "sd.a", "r.b", "n.b", "sd.b")
)  
#r.a = correlation coefficient of group a
#n.a = sample size of group a
#sd.a = standard deviation of group a
#r.b = correaltion coefficient of group b
#n.b = sample size of group b
#sd.b = standard deviation of group b
z.a <- 0.5 * log ((1 + r.a) / (1 - r.a))
var.z.a <- 1 / (n.a - 3)
z.b <- 0.5 * log ((1 + r.b) / (1 - r.b))
var.z.b <- 1 / (n.b - 3)
cov.z <- (sd.a + sd.b) / ((1 - var.z.a) * (1 - var.z.b))

return(c(z.a, var.z.a, z.b, var.z.b, cov.z))
}
