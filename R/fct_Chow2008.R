#' @title Power 1 Mean: 1-Sample, 2-Sided Equality
#'
#' @description Calculate Sample Size Needed to Test 1 Mean: 1-Sample, 2-Sided Equality
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 51.
#'
#' @noRd
pow_1m_equality <- function(mu=2,
                            mu0=1.5,
                            sd=1,
                            alpha=0.05,
                            beta=0.20){
  (n=(sd*(qnorm(1-alpha/2)+qnorm(1-beta))/(mu-mu0))^2)
  ceiling(n)# 32
  z=(mu-mu0)/sd*sqrt(n)
  (Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))  
}


#' @title Power 1 Mean: 1-Sample Non-Inferiority or Superiority
#'
#' @description Calculate Sample Size Needed to Test 1 Mean: 1-Sample Non-Inferiority or Superiority
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 52.
#'
#' @noRd
pow_1m_superiority <- function(  mu=2,
                                 mu0=1.5,
                                 delta=-0.5,
                                 sd=1,
                                 alpha=0.05,
                                 beta=0.20){
  (n=(sd*(qnorm(1-alpha)+qnorm(1-beta))/(mu-mu0-delta))^2)
  ceiling(n)# 7
  z=(mu-mu0-delta)/sd*sqrt(n)
  (Power=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))
}

#' @title Power 1 Mean: 1-Sample Equivalence
#'
#' @description Calculate Sample Size Needed to Test 1 Mean: 1-Sample Equivalence
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 54.
#'
#' @noRd
pow_1m_equivalence <- function(mu=2,
                               mu0=2,
                               delta=0.05,
                               sd=0.10,
                               alpha=0.05,
                               beta=0.20){
  
  (n=(sd*(qnorm(1-alpha)+qnorm(1-beta/2))/(delta-abs(mu-mu0)))^2)
  ceiling(n) # 35
  z=(abs(mu-mu0)-delta)/sd*sqrt(n)
  (Power=2*(pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))-1)  
}




#' @title Power 1 Proportion: 1-Sample, 2-Sided Equality
#'
#' @description Calculate Sample Size Needed to Test 1 Proportion: 1-Sample, 2-Sided Equality
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 85.
#'
#' @noRd
pow_1p_equality <- function(p=0.5,
                            p0=0.3,
                            alpha=0.05,
                            beta=0.20){
  (n=p*(1-p)*((qnorm(1-alpha/2)+qnorm(1-beta))/(p-p0))^2)
  ceiling(n) # 50
  z=(p-p0)/sqrt(p*(1-p)/n)
  (Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))
}


#' @title Power 1 Proportion: 1-Sample Non-Inferiority or Superiority
#'
#' @description Calculate Sample Size Needed to Test 1 Proportion: 1-Sample Non-Inferiority or Superiority
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 86.
#'
#' @noRd
pow_1p_superiority <- function(p=0.5,
                               p0=0.3,
                               delta=-0.1,
                               alpha=0.05,
                               beta=0.20){
  (n=p*(1-p)*((qnorm(1-alpha)+qnorm(1-beta))/(p-p0-delta))^2)
  ceiling(n) # 18
  z=(p-p0-delta)/sqrt(p*(1-p)/n)
  (Power=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))
}

#' @title Power 1 Proportion: 1-Sample Equivalence
#'
#' @description Calculate Sample Size Needed to Test 1 Proportion: 1-Sample Equivalence
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 87.
#'
#' @noRd
pow_1p_equivalence <- function(p=0.6,
                               p0=0.6,
                               delta=0.2,
                               alpha=0.05,
                               beta=0.20){
  (n=p*(1-p)*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(p-p0)-delta))^2)
  ceiling(n) # 52
  z=(abs(p-p0)-delta)/sqrt(p*(1-p)/n)
  (Power=2*(pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))-1)
  
}




#' @title Power Odds Ratio: Equality
#'
#' @description Calculate Sample Size Needed to Test Odds Ratio: Equality
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 106.
#'
#' @noRd
pow_OR_equality <- function(pA=0.40,
                            pB=0.25,
                            kappa=1,
                            alpha=0.05,
                            beta=0.20){
  (OR=pA*(1-pB)/pB/(1-pA)) # 2
  (nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha/2)+qnorm(1-beta))/log(OR))^2)
  ceiling(nB) # 156
  z=log(OR)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))
  (Power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2)))  
}


#' @title Power Odds Ratio: Non-Inferiority or Superiority
#'
#' @description Calculate Sample Size Needed to Test Odds Ratio: Non-Inferiority or Superiority
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 107.
#'
#' @noRd
pow_OR_superiority <- function(pA=0.40,
                               pB=0.25,
                               delta=0.20,
                               kappa=1,
                               alpha=0.05,
                               beta=0.20){
  (OR=pA*(1-pB)/pB/(1-pA)) # 2
  (nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta))/(log(OR)-delta))^2)
  ceiling(nB) # 242
  z=(log(OR)-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))
  (Power=pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))  
}


#' @title Power Odds Ratio: Equivalence
#'
#' @description Calculate Sample Size Needed to Test Odds Ratio: Equivalence
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 107.
#'
#' @noRd
pow_OR_equivalence <- function(pA=0.25,
                               pB=0.25,
                               delta=0.50,
                               kappa=1,
                               alpha=0.05,
                               beta=0.20){
  (OR=pA*(1-pB)/pB/(1-pA)) # 1
  (nB=(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))*((qnorm(1-alpha)+qnorm(1-beta/2))/(abs(log(OR))-delta))^2)
  ceiling(nB) # 366
  z=(abs(log(OR))-delta)*sqrt(nB)/sqrt(1/(kappa*pA*(1-pA))+1/(pB*(1-pB)))
  (Power=2*(pnorm(z-qnorm(1-alpha))+pnorm(-z-qnorm(1-alpha)))-1)  
}





#' @title Power Hazard Ratio: Equality
#'
#' @description Calculate Sample Size Needed to Test Time-To-Event Data: Cox PH, 2-Sided Equality
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 177.
#'
#' @noRd
pow_HR_equality <- function(hr=2,
                            hr0=1,
                            pE=0.8,
                            pA=0.5,
                            alpha=0.05,
                            beta=0.20){
  (n=((qnorm(1-alpha/2)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE))
  ceiling(n) # 82
  (Power=pnorm((log(hr)-log(hr0))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha/2)))
}


#' @title Power Hazard Ratio: Non-Inferiority or Superiority
#'
#' @description Calculate Sample Size Needed to Test Time-To-Event Data: Cox PH 1-Sided, non-inferiority, or superiority
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 177.
#'
#' @noRd
pow_HR_superiority <- function(hr=2,
                               hr0=1,
                               pE=0.8,
                               pA=0.5,
                               alpha=0.05/2,
                               beta=0.20){
  (n=((qnorm(1-alpha)+qnorm(1-beta))/(log(hr)-log(hr0)))^2/(pA*(1-pA)*pE))
  ceiling(n) # 82
  (Power=pnorm((log(hr)-log(hr0))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha)))
  ## Note: divide alpha by 2 to get 2-sided test of the cited example
}


#' @title Power Hazard Ratio: Equivalence
#'
#' @description Calculate Sample Size Needed to Test Time-To-Event Data: Cox PH, Equivalence
#'
#' @return The return value, if any, from executing the function.
#'
#' @references Chow S, Shao J, Wang H. 2008. Sample Size Calculations in Clinical Research. 2nd Ed. Chapman & Hall/CRC Biostatistics Series. page 177.
#'
#' @noRd
pow_HR_equivalence <- function(hr=1,
                               delta=0.5,
                               pE=0.8,
                               pA=0.5,
                               alpha=0.05,
                               beta=0.20){
  (n=((qnorm(1-alpha)+qnorm(1-beta/2))/(delta-abs(log(hr))))^2/(pA*(1-pA)*pE))
  ceiling(n) # 172
  (Power=2*pnorm((delta-abs(log(hr)))*sqrt(n*pA*(1-pA)*pE)-qnorm(1-alpha))-1)
}