#' @title Shiny Application
#'
#' @description Initialize and execute a local Shiny session.
#'     
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @param init Named list containing the arguments that are passed to function
#' @inheritParams shiny::shinyApp
#' 
#' @return Start Shiny server and access application using browser
#' 
#' @examples
#' inits <- list("Risk_T"=0.5, "Risk_C"=0.3)
#' if(interactive()){
#'    run_app(init = init)
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(
    launch.browser = TRUE
  ),
  enableBookmarking = NULL,
  uiPattern = "/",
  init = NULL,
  ...) {
  
  #set default golem_opts and update based on user input
  inits <- list("Risk_T"=0.2, "Risk_C"=0.1, "Sample"=c(32,512), "Prop_T"=0.5, "Alpha"=0.025, "Delta"=1) #Default Parameter Set
  if(!is.null(init)) inits[match.arg(names(init),names(inits), several.ok = TRUE)] <- init
  
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(inits = inits, ...)
  )
}
