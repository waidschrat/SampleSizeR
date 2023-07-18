#' @title Sequence Generation (Log-Scale)
#' 
#' @description Generates a sequence with equidistant steps on a log-scale 
#' based on the minimum and maximum value of any numeric vector.
#' 
#' @param x Numeric vector.  
#' @param by Integer. Difference between elements on log-scale. Defaults to 1.
#' @param base Integer. Base of logarithm. Defaults to 2.
#' @param digits Integer. Number of decimal places to be returned. Negative values are allowed.
#' 
#' @return Return numeric
#' 
#' @noRd
seqb <- function(x, by = 1, base = 2, digits = 1){
  if(any(x == Inf)){
    x <- x[x != Inf]
    rx <- log(range(x), base = base)
    ret <- c(seq(from = rx[1], to = rx[2], by = by),Inf)
  }else{
    rx <- log(range(x), base = base)
    ret <- seq(from = rx[1], to = rx[2], by = by)
  }
  return(round(base^ret, digits))
}

#' @title Log Odds Ratio
#' 
#' @description Calculates log-transformed Odds Ratio 
#' 
#' @param R1 Numeric. Event Risk in Treatment Arm.  
#' @param R2 Numeric. Event Risk in Comparator Arm.
#' 
#' @return Return numeric
#' 
#' @noRd
OddsRatio <- Vectorize(function(R1,R2){
  OR <- (R1 * (1-R2)) / (R2 * (1-R1)) 
  log(OR)
}, vectorize.args = c("R1","R2"))


#' @title Standard Error of Log Odds Ratio
#' 
#' @description Calculates Standard Error of log-transformed Odds Ratio
#' 
#' @param R1 Numeric. Event Risk in Treatment Arm.  
#' @param R2 Numeric. Event Risk in Comparator Arm.
#' @param N1 Integer. Sample Size of Treatment Arm.
#' @param N2 Integer. Sample Size of Comparator Arm.
#' 
#' @return Return numeric
#' 
#' @noRd
OddsRatioSE <- Vectorize(function(R1,R2,N1,N2){
  k1 <- R1*N1 #number of responses in treatment group 
  k2 <- R2*N2 #number of responses in control group
  (1/(N1-k1) + 1/(N2-k2) + 1/k1 + 1/k2)^0.5
}, vectorize.args = c("R1","R2","N1","N2"))
