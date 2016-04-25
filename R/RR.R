#' @title Response Ratio for common control
#' 
#' @param table
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
#' @return Response ratios, variance, and covariance
#' 
#' @description RR calculates the Response Ratios, the variance and the covariance for two data inputs that share a X, SD, and N of a control group and returns these values as a vector
#' 
#' @example #Note that "RRtest" is the name of a imported table
#' RR(test)
#' 
#' @references Lajeunesse, Marc. 2011. On the meta-analysis of reponse ratios for the studies with correlated and multigroup designs. Ecology. 92(11): 2049-2055.

RR <- function(x){
  RR.df <- data.frame(
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
  
  RR.a <- log(X.t.a / X.c.a)
  
  var.rr1.a <- SD.c.a^2 / (N.c.a * X.c.a^2) 
  var.rr2.a <- SD.t.a^2 / (N.t.a * X.t.a^2)
  var.rr.a <- var.rr1.a + var.rr2.a
  
  RR.b <- log(X.t.b / X.c.b)
  
  var.rr1.b <- SD.c.b^2 / (N.c.b * X.c.b^2) 
  var.rr2.b <- SD.t.b^2 / (N.t.b * X.t.b^2)
  var.rr.b <- var.rr1.b + var.rr2.b
  
  #cov.rr1 <- (r * SD.t.a * SD.t.b) / ((X.t.a * X.t.b) * sqrt(N.t.a * N.t.b))
  #cov.rr2 <- (r * SD.c.a * SD.c.b) / ((X.c.a * X.c.b) * sqrt(N.c.a * N.c.b))
  
  cov.rr <- SD.c.a^2 / (N.c.a * X.c.a^2)
  
  return(c(RR.a, var.rr.a, cov.rr, RR.b, var.rr.b))
  
}

