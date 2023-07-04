#' @title Calculate Odds Ratio (Balance)
#' 
#' @description Same Sample Size in Treatment Arm and Comparator Arm
#' 
#' @param BR Base Rate (Comparator Arm).
#' @param RR Risk Ratio (Treatment Arm).
#' @param N Sample Size per Arm.
#' @param log_e Boolean. If true, log-scaled Odds Ratio is returned
#' 
#' @return Return (Log) Odds Ratio
#' 
#' @noRd
OddsRatio <- Vectorize(function(BR, RR, N, log_e = FALSE){
  k1 <- (BR/RR)*N #number of responses in treatment arm 
  k2 <- BR*N #number of responses in comparator arm
  OR <- (k1 * (N-k2)) / (k2 * (N-k1)) 
  if(log_e) OR <- log(OR)
  return(OR)
}, vectorize.args = "RR")


#' @title Calculate Odds Ratio (Imbalance)
#' 
#' @description Different Sample Size in Treatment Arm and Control Arm
#' 
#' @param BR Base Rate (Comparator Arm).
#' @param RR Risk Ratio (Treatment Arm).
#' @param N1 Sample Size in Treatment Arm.
#' @param N2 Sample Size in Comparator Arm.
#' @param log_e Boolean. If true, log-scaled Odds Ratio is returned
#' 
#' @return Return (Log) Odds Ratio
#' 
#' @noRd
OddsRatio2 <- Vectorize(function(BR, RR, N1, N2, log_e = FALSE){
  k1 <- (BR/RR)*N1 #number of responses in treatment arm 
  k2 <- BR*N2 #number of responses in comparator arm
  OR <- (k1 * (N2-k2)) / (k2 * (N1-k1))
  if(log_e) OR <- log(OR)
  return(OR)
}, vectorize.args = "RR")



#' @title Calculate Standard Error of log-scaled Odds Ratio (Balance)
#' 
#' @description Same Sample Size in Treatment Arm and Control Arm
#' 
#' @param BR Base Rate (Comparator Arm).
#' @param RR Risk Ratio (Treatment Arm).
#' @param N Sample Size per Arm.
#' 
#' @return Return Standard Error of Log Odds Ratio
#' 
#' @noRd
OddsRatioSE <- Vectorize(function(BR,RR,N){
  k1 <- BR*N #number of responses in treatment group 
  k2 <- (BR/RR)*N #number of responses in control group
  (1/(N-k1) + 1/(N-k2) + 1/k1 + 1/k2)^0.5
}, vectorize.args = "RR")


#' @title Calculate Standard Error of log-scaled Odds Ratio (Imbalance)
#' 
#' @description Different Sample Size in Treatment Arm and Control Arm
#' 
#' @param BR Base Rate (Comparator Arm).
#' @param RR Risk Ratio (Treatment Arm).
#' @param N1 Sample Size in Treatment Arm.
#' @param N2 Sample Size in Comparator Arm.
#' 
#' @return Return Standard Error of Log Odds Ratio
#' 
#' @noRd
OddsRatio2SE <- Vectorize(function(BR,RR,N1,N2){
  k1 <- BR*N1 #number of responses in treatment group 
  k2 <- (BR/RR)*N2 #number of responses in control group
  (1/(N1-k1) + 1/(N2-k2) + 1/k1 + 1/k2)^0.5
}, vectorize.args = "RR")


#' @title Power of Odds Ratio (Balance)
#' 
#' @description Shiny Reactive
#' 
#' 
#' @return Power
#' 
#' @importFrom shiny reactive
#' @noRd
Pow <- reactive({
  lOR <- OddsRatio(input$BR,input$RR,input$Np) #calculate log OR
  lOR_se <- OddsRatioSE(input$BR,input$RR,input$Np) #calculate standard error of log OR
  round((1-pnorm(qnorm(1-input$Alpha,0,lOR_se), lOR, lOR_se))*100,1)
})

#' @title Power of Odds Ratio (Imbalance)
#' 
#' @description Shiny Reactive
#'
#' @return Power
#' 
#' @importFrom shiny reactive
#' @noRd
Pow2 <- reactive({
  lOR1 <- OddsRatio(input$BR,input$RR,input$Np) #calculate log OR
  lOR1_se <- OddsRatioSE(input$BR,input$RR,input$Np) #calculate standard error of log OR
  
  lOR2 <- OddsRatio2(input$BR,input$RR,input$NpE1,input$NpE2) #calculate log OR, external
  lOR2_se <- OddsRatio2SE(input$BR,input$RR,input$NpE1,input$NpE2) #calculate standard error of log OR
  
  mod <- rma.uni(c(lOR1,lOR2), c(lOR1_se,lOR2_se)^2, method="FE")
  lOR <- mod$b[,1]
  lOR_se <- mod$se
  
  round((1-pnorm(qnorm(1-input$Alpha,0,lOR_se), lOR, lOR_se))*100,1)
})
