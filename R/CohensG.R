#' @title Cohen's G for common control
#' 
#' @param tables
#' @param X.t.a Column label for the means (X) of the treatment group (t) in group a
#' @param SD.t.a Column label for the standard deviations (SD) of the treatmet group in group a
#' @param N.t.a Column label for the sample sizes (N) of the treatment group in group a
#' @param X.c.a Column label for the means of the control group (c) in group a
#' @param SD.c.a Column label for the standard deviations of the control group in group a
#' @param N.c.a Column label for the sample sizes of the control group in group a
#' @param X.t.b Column label for the means of the treatment group in group b
#' @param SD.t.b Column label for the standard deviations of the treatment group in group b
#' @param N.t.b Column label for the sample sizes of the treatment group in group b
#' @param X.c.b Column label for the means of the control group in group b
#' @param SD.c.b Column label for the standard deviations of the control group in group b
#' @param N.c.b Column label for the sample sizes of the control group in group b
#' 
#' @return Cohen's D, variance, and covariance 
#' 
#' @description CohensG calculates the Cohen's G, the variance, and the covariance for two data inputs that share the X, SD, and N of a control group and returns these values as a vector
#' 
#' @example #Note that "Gtest" is the name of an imported table
#' CohensG(Gtest)

CohensG <- function (x) {
G.df <- data.frame(
  SOURCE = c("X.t.a", "SD.t.a", "N.t.a", "X.c.a", "SD.c.a", "N.c.a", 
             "X.t.b", "SD.t.b", "N.t.b", "X.c.b", "SD.c.b", "N.c.b")
)
#X.t.a = mean of treatment in group a
#SD.t.a = standard deviation of treatmet in group a
#N.t.a = sample size of treatment in group a
#X.c.a = mean of control in group a
#SD.c.a = standard deviation of control in group a
#N.c.a = sample size of control in group a
#X.t.b = mean of treatment in group b
#SD.t.b = standard deviation of treatmet in group b
#N.t.b = sample size of treatment in group b
#X.c.b = mean of control in group b
#SD.c.b = standard deviation of control in group b
#N.c.b = sample size of control in group b

num.1a <- (N.c.a - 1) * (SD.c.a^2)
num.2a <- (N.t.a - 1) * (SD.t.a^2)
num.a <- num.1a + num.2a
dem.a <- (N.c.a + N.t.a) - 2
S.tot.a <- sqrt(num.a/dem.a)
g.a <- (X.t.a - X.c.a)/S.tot.a 

v.1a <- (1 / N.t.a) + (1 / N.c.a)
v.2a <- g.a^2 / (2 * (N.t.a + N.c.a))
v.3a <- v.1a + v.2a

num.1b <- (N.c.b - 1) * (SD.c.b^2)
num.2b <- (N.t.b - 1) * (SD.t.b^2)
num.b <- num.1b + num.2b
dem.b <- (N.c.b + N.t.a) - 2
S.tot.b <- sqrt(num.b/dem.b)
g.b <- (X.t.b - X.c.b)/S.tot.b

v.1b <- (1 / N.t.b) + (1 / N.c.b)
v.2b <- g.b^2 / (2 * (N.t.b + N.c.b))
v.3b <- v.1b + v.2b

N.c <- N.c.a + N.c.b
cov.g1 <- (g.a * g.b) / (2 * (N.c + N.t.a + N.t.b))
cov.g <- cov.g1 + (1 / N.c)

return(c(g.a, v.3a, g.b, v.3b, cov.g))
}
