#' @title Odd's Ratio
#' 
#' @param table
#' @param na.t Column label for the frequency (n) of the treatment group (t) in group a
#' @param nb.t Column label for the frequency of the treatment group in group b
#' @param na.c Column label for the frequency of the control group (c) in group a
#' @param nb.c Column label for the frequency of the control group in group b
#' 
#' @return Odd's ratio value
#' 
#' @description OddsRatio calculates the Odd's Ratio of the frequency of the treatment and control that occur in two groups
#' 
#' @example #note that "OR.test" is the name of an imported table
#' OddsRatio(OR.test)

OddsRatio <- function(x) {
  OR.df <- data.frame(
    SOURCE = c("na.t", "nb.t", "na.c", "nb.c")
  )  
  #na.t = frequency of treatment in group a
  #nb.t = frequency of treatment in group b
  #na.c = frequency of control in group a
  #nb.c = frequency of control in group b
  OR <- (na.t * nb.c)/(na.c * nb.t)
  return(c(OR))
}