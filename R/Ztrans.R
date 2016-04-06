#' @title Z transformation or r for common control
#' 
#' @param table
#' @param r_a Column label for the correlation coefficients (r) of group a
#' @param n_a Column label for the sample sizes (n) of group a
#' @param sd_a Column label for the standard deviations (sd) of group a
#' @param r_b Column label for the correaltion coefficients of group b
#' @param n_b Column label for the sample sizes of group b
#' @param sd_b Column label for the standard deviations of group b
#' 
#' @return Z transformations, Variance, and Covariance
#' 
#' @description Z.trans calculates the Z transformation, the variance,and the covariance of two r values and returns these values as a vector
#' 
 
Z.trans <- function(x) {
z.df <- data.frame(
  SOURCE = c("r_a", "n_a", "sd_a", "r_b", "n_b", "sd_b")
)  
#r_a = correlation coefficient of group a
#n_a = sample size of group a
#sd_a = standard deviation of group a
#r_b = correaltion coefficient of group b
#n_b = sample size of group b
#sd_b = standard deviation of group b
z_a <- 0.5 * log ((1 + r_a) / (1 - r_a))
var.z_a <- 1 / (n_a - 3)
z_b <- 0.5 * log ((1 + r_b) / (1 - r_b))
var.z_b <- 1 / (n_b - 3)
cov.z <- (sd_a + sd_b) / ((1 - var.z_a) * (1 - var.z_b))

return(c(z_a, var.z_a, z_b, var.z_b, cov.z))
}
