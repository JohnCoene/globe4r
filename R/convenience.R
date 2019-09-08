#' Image URLs
#' 
#' Convenience function to fetch URLs for \code{\link{globe_img_url}}, 
#' and \code{\link{bump_img_url}}.
#' 
#' @param name Name of image to return.
#' 
#' @examples
#' create_globe() %>% 
#'   globe_img_url(image_url("dark"))
#' 
#' @return URL to \url{https://jsdelivr.net} image.
#' 
#' @export
image_url <- function(name = c("night", "blue-marble", "dark", "topology")) {
  name <- match.arg(name)
  ext <- ifelse(name == "topology", "png", "jpg")
  paste0(
    "https://cdn.jsdelivr.net/npm/three-globe/example/img/earth-",
    name, ".", ext
  )
}

#' Demo Shiny App
#' 
#' Run demo Shiny app.
#' 
#' @examples
#' if(interactive()) demo_app()
#' 
#' @export
demo_app <- function(){
  demo <- system.file("shiny", package = "globe4r")
  shiny::shinyApp(demo)
}