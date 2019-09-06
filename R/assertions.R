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
  "No coordinates found, see `coords`."
}