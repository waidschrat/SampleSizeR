#' RCT_continuous UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_RCT_continuous_ui <- function(id){
  ns <- NS(id)
  
  tagList(
    h2("metric endpoint")
    
  )
}
    
#' RCT_continuous Server Functions
#'
#' @noRd 
mod_RCT_continuous_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
