not_missing <- function(x) {
  !missing(x)
}

on_failure(not_missing) <- function(call, env) {
  paste0(
    "Missing `",
    crayon::red(deparse(call$x)),
    "`."
  )
}

has_data <- function(x){
  if(missing(x))
    return(FALSE)
  !is.null(x)
}

on_failure(has_data) <- function(call, env) {
  "Missing data."
}

has_coords <- function(x){
  if(is.null(x))
    return(FALSE)
  if(!length(x))
    return(FALSE)
  return(TRUE)
}

on_failure(has_coords) <- function(call, env) {
  paste0(
    "No coordinates found, see `", 
    crayon::red("coords") ,
    "`."
  )
}

is_sf <- function(x) {
  inherits(x, "sf")
}

on_failure(is_sf) <- function(call, env) {
  paste0(
    "Data must be of class `",
    crayon::red("sf"),
    "`."
  )
}