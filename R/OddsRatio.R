#' @title Odd's Ratio
#' 
#' @param table
#' @param na_t Column label for the frequency (n) of the treatment group (t) in group a
#' @param nb_t Column label for the frequency of the treatment group in group b
#' @param na_c Column label for the frequency of the control group (c) in group a
#' @param nb_c Column label for the frequency of the control group in group b
#' 
#' @return Odd's ratio value
#' 
#' @description OddsRatio calculates the Odd's Ratio of the frequency of the treatment and control that occur in two groups
#' 

OddsRatio <- function(x) {
  OR.df <- data.frame(
    SOURCE = c("na_t", "nb_t", "na_c", "nb_c")
  )  
  #na_t = frequency of treatment in group a
  #nb_t = frequency of treatment in group b
  #na_c = frequency of control in group a
  #nb_c = frequency of control in group b
  OR <- (na_t * nb_c)/(na_c * nb_t)
  return(c(OR))
}