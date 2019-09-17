.onAttach <- function(libname, pkgname) {
  shiny::registerInputHandler("globe4rParseJS", function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)

  packageStartupMessage(
    "Welcome to globe4r\n\n",
    "Docs: globe4r.john-coene.com"
  )
}