#' Shared options
#' 
#' Options that will be shared with all subsequent visualisation.
#' This is ideal when using Rmarkdown or Shiny, e.g.: only set
#' the background color once, instead of multiple times.
#' 
#' @param value Value to set globally.
#' 
#' @export
shared_background <- function(value){
  assert_that(not_missing(value))
  options(backgroundColor = value)
}