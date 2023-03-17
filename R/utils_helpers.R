#' Print method for bayesROE object
#' 
#' @description Print bayesROE
#' 
#' @return Return ggplot2 object
#' 
#' @noRd
print.bayesROE <- function(x, ...) {
  print(x$plot)
  invisible(x)
}