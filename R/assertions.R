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
