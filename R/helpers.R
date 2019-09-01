#' Constants
#' 
#' Wraps a constant in a JavaScript function as most
#' underlying JavaScript functions expect it.
#' 
#' @param x A constant.
#' 
#' @export
constant <- function(x){
  htmlwidgets::JS(paste0("() => '", x, "'"))
}