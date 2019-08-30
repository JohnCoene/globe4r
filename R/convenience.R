#' Image URLs
#' 
#' Convenience function to fetch URLs for \code{\link{globe_img_url}}.
#' 
#' @param name Name of image to return.
#' 
#' @return URL to \url{https://jsdelivr.net} image.
#' 
#' @export
image_url <- function(name = c("earth-night", "earth-blue-marble", "earth-dark", "earth-topology")) {
  name <- match.arg(name)
  paste0(
    "https://cdn.jsdelivr.net/npm/three-globe/example/img/",
    name,
    ".jpg"
  )
}