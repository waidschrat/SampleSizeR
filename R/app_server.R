#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny ggplot2 shinyBS
#' @importFrom golem get_golem_options
#' @noRd
app_server <- function(input, output, session) {
  
  # application server logic
  
  mod_RCT_binary_server("RCT_binary_1")
  #mod_bayesROE_server("bayesROE_1")
  #mod_RCT_continuous_server("RCT_continuous_1")
  #mod_VaccCoverage_server("VaccCoverage_1")

}
