#' @title Response Ratio for common control
#' 
#' @param table
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
#' @return Response ratios, variance, and covariance
#' 
#' @description RR calculates the Response Ratios, the variance and the covariance for two data inputs that share a X, SD, and N of a control group and returns these values as a vector
#' 

RR <- function(x){
  RR.df <- data.frame(
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
  
  RR_a <- log(X_t_a / X_c_a)
  
  var.rr1_a <- SD_c_a^2 / (N_c_a * X_c_a^2) 
  var.rr2_a <- SD_t_a^2 / (N_t_a * X_t_a^2)
  var.rr_a <- var.rr1_a + var.rr2_a
  
  RR_b <- log(X_t_b / X_c_b)
  
  var.rr1_b <- SD_c_b^2 / (N_c_b * X_c_b^2) 
  var.rr2_b <- SD_t_b^2 / (N_t_b * X_t_b^2)
  var.rr_b <- var.rr1_b + var.rr2_b
  
  #cov.rr1 <- (r * SD_t_a * SD_t_b) / ((X_t_a * X_t_b) * sqrt(N_t_a * N_t_b))
  #cov.rr2 <- (r * SD_c_a * SD_c_b) / ((X_c_a * X_c_b) * sqrt(N_c_a * N_c_b))
  
  cov.rr <- SD_c_a^2 / (N_c_a * X_c_a^2)
  
  return(c(RR_a, var.rr_a, cov.rr, RR_b, var.rr_b))
  
}

