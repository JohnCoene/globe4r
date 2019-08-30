#' Image URLs
#' 
#' Convenience function to fetch URLs for \code{\link{globe_img_url}}, 
#' and \code{\link{bump_img_url}}.
#' 
#' @param name Name of image to return.
#' 
#' @examples
#' create_globe() %>% 
#'   globe_img_url(image_url("earth-dark"))
#' 
#' @return URL to \url{https://jsdelivr.net} image.
#' 
#' @export
image_url <- function(name = c("earth-night", "earth-blue-marble", "earth-dark", "earth-topology")) {
  name <- match.arg(name)
  ext <- ifelse(name == "earth-topology", "png", "jpg")
  paste0(
    "https://cdn.jsdelivr.net/npm/three-globe/example/img/",
    name, ".", ext
  )
}