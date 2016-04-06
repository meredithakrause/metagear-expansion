#' @title Cohen's G for common control
#' 
#' @param tables
#' @param X_t_a Column label for the means (X) of the treatment group (t) in group a
#' @param SD_t_a Column label for the standard deviations (SD) of the treatmet group in group a
#' @param N_t_a Column label for the sample sizes (N) of the treatment group in group a
#' @param X_c_a Column label for the means of the control group (c) in group a
#' @param SD_c_a Column label for the standard deviations of the control group in group a
#' @param N_c_a Column label for the sample sizes of the control group in group a
#' @param X_t_b Column label for the means of the treatment group in group b
#' @param SD_t_b Column label for the standard deviations of the treatment group in group b
#' @param N_t_b Column label for the sample sizes of the treatment group in group b
#' @param X_c_b Column label for the means of the control group in group b
#' @param SD_c_b Column label for the standard deviations of the control group in group b
#' @param N_c_b Column label for the sample sizes of the control group in group b
#' 
#' @return Cohen's D, variance, and covariance 
#' 
#' @description CohensG calculates the Cohen's G, the variance, and the covariance for two data inputs that share the X, SD, and N of a control group and returns these values as a vector
#' 
#' 

CohensG <- function (x) {
G.df <- data.frame(
  SOURCE = c("X_t_a", "SD_t_a", "N_t_a", "X_c_a", "SD_c_a", "N_c_a", 
             "X_t_b", "SD_t_b", "N_t_b", "X_c_b", "SD_c_b", "N_c_b")
)
#X_t_a = mean of treatment in group a
#SD_t_a = standard deviation of treatmet in group a
#N_t_a = sample size of treatment in group a
#X_c_a = mean of control in group a
#SD_c_a = standard deviation of control in group a
#N_c_a = sample size of control in group a
#X_t_b = mean of treatment in group b
#SD_t_b = standard deviation of treatmet in group b
#N_t_b = sample size of treatment in group b
#X_c_b = mean of control in group b
#SD_c_b = standard deviation of control in group b
#N_c_b = sample size of control in group b

num.1a <- (N_c_a - 1) * (SD_c_a^2)
num.2a <- (N_t_a - 1) * (SD_t_a^2)
num_a <- num.1a + num.2a
dem_a <- (N_c_a + N_t_a) - 2
S.tot_a <- sqrt(num_a/dem_a)
g_a <- (X_t_a - X_c_a)/S.tot_a 

v.1a <- (1 / N_t_a) + (1 / N_c_a)
v.2a <- g_a^2 / (2 * (N_t_a + N_c_a))
v.3a <- v.1a + v.2a

num.1b <- (N_c_b - 1) * (SD_c_b^2)
num.2b <- (N_t_b - 1) * (SD_t_b^2)
num_b <- num.1b + num.2b
dem_b <- (N_c_b + N_t_a) - 2
S.tot_b <- sqrt(num_b/dem_b)
g_b <- (X_t_b - X_c_b)/S.tot_b

v.1b <- (1 / N_t_b) + (1 / N_c_b)
v.2b <- g_b^2 / (2 * (N_t_b + N_c_b))
v.3b <- v.1b + v.2b

N_c <- N_c_a + N_c_b
cov.g1 <- (g_a * g_b) / (2 * (N_c + N_t_a + N_t_b))
cov.g <- cov.g1 + (1 / N_c)

return(c(g_a, v.3a, g_b, v.3b, cov.g))
}
