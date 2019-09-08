#' Globe Material
#' 
#' Getter/setter for the URL of the image used in the material 
#' that wraps the globe. If no image is provided, the globe is 
#' represented as a black sphere. Also provides API to create a 
#' bump map in the material, to represent the globe's terrain.
#' 
#' @param globe An object of class \code{globe} as returned by
#' \code{\link{create_globe}}, or an object of class \code{globeProxy}
#' as returned by \code{\link{globeProxy}}.
#' @param url URL of the image layer to use.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   bump_img_url()
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("btn", "Add img"),
#'   globeOutput("globe")
#' )
#' 
#' server <- function(input, output){
#'   output$globe <- renderGlobe({
#'     create_globe()
#'   })
#' 
#'   observeEvent(input$btn, {
#'     globeProxy("globe") %>% 
#'       bump_img_url()
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
#' @name globe_img
#' @export
globe_img_url <- function(globe, url = image_url()) UseMethod("globe_img_url")

#' @export
#' @method globe_img_url globe
globe_img_url.globe <- function(globe, url = image_url()){
  globe$x$globeImageUrl <- url
  return(globe)
}

#' @export
#' @method globe_img_url globeProxy
globe_img_url.globeProxy <- function(globe, url = image_url()){
  data <- list(id = globe$id, url = url)
  globe$session$sendCustomMessage("globeImageUrl", data)
}

#' @rdname globe_img
#' @export
bump_img_url <- function(globe, url = image_url("topology")) UseMethod("bump_img_url")

#' @export
#' @method bump_img_url globe
bump_img_url.globe <- function(globe, url = image_url("topology")){
  globe$x$bumpImageUrl <- url
  return(globe)
}

#' @export
#' @method bump_img_url globeProxy
bump_img_url.globeProxy <- function(globe, url = image_url("topology")){
  data <- list(id = globe$id, url = url)
  globe$session$sendCustomMessage("bumpImageUrl", data)
}

#' Atmosphere & Grid
#' 
#' Customise atmosphere (bright halo surrounding the globe) 
#' and graticules grid demarking latitude and longitude lines 
#' at every 10 degrees.
#' 
#' @inheritParams globe_img
#' @param show Whether to show the atmosphere or graticules.
#' 
#' @examples
#' # basic use case
#' img <- image_url("blue-marble")
#' create_globe() %>% 
#'   globe_img_url(img) %>% 
#'   show_atmosphere(FALSE) %>% 
#'   show_graticules(TRUE)
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   radioButtons("show", "Atmosphere", choices = c(TRUE, FALSE)),
#'   globeOutput("globe")
#' )
#' 
#' server <- function(input, output){
#'   output$globe <- renderGlobe({
#'     create_globe() %>% 
#'       globe_img_url()
#'   })
#' 
#'   observeEvent(input$show, {
#'     globeProxy("globe") %>% 
#'       show_atmosphere(input$show)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
#' @name atmosphere
#' @export
show_atmosphere <- function(globe, show = TRUE) UseMethod("show_atmosphere")

#' @export
#' @method show_atmosphere globe
show_atmosphere.globe <- function(globe, show = TRUE){
  globe$x$showAtmosphere <- show
  return(globe)
}

#' @export
#' @method show_atmosphere globeProxy
show_atmosphere.globeProxy <- function(globe, show = TRUE){
  data <- list(id = globe$id, show = show)
  globe$session$sendCustomMessage("showAtmosphere", data)
}

#' @name atmosphere
#' @export
show_graticules <- function(globe, show = TRUE) UseMethod("show_graticules")

#' @export
#' @method show_graticules globe
show_graticules.globe <- function(globe, show = TRUE){
  globe$x$showGraticules <- show
  return(globe)
}

#' @export
#' @method show_graticules globeProxy
show_graticules.globeProxy <- function(globe, show = TRUE){
  data <- list(id = globe$id, show = show)
  globe$session$sendCustomMessage("showGraticules", data)
}

#' Dimensions & Background
#' 
#' Customise the dimensions and background color of the visualisation.
#' 
#' @inheritParams globe_img
#' @param width,height An integer defining the number of pixels.
#' @param color A valid hex code or color name.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_dimensions(250, 250) %>% 
#'   globe_background("#000")
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   sliderInput(
#'     "width", 
#'     "Change width", 
#'     min = 300, 
#'     max = 900, 
#'     step = 50, 
#'     value = 250
#'   ), 
#'   globeOutput("globe")
#' )
#' 
#' server <- function(input, output){
#'   output$globe <- renderGlobe({
#'     create_globe() %>% 
#'       globe_img_url()
#'   })
#' 
#'   observeEvent(input$width, {
#'     globeProxy("globe") %>% 
#'       globe_dimensions(width = input$width)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
#' @name container
#' @export
globe_dimensions <- function(globe, width = NULL, height = NULL) UseMethod("globe_dimensions")

#' @export
#' @method globe_dimensions globe
globe_dimensions.globe <- function(globe, width = NULL, height = NULL){
  # force integer conversion
  globe$x$width <- if(!is.null(width)) as.integer(width)
  globe$x$height <- if(!is.null(height)) as.integer(height)
  return(globe)
}

#' @export
#' @method globe_dimensions globeProxy
globe_dimensions.globeProxy <- function(globe, width = NULL, height = NULL){
  data <- list(id = globe$id)
  # force integer conversion
  if(!is.null(width)) data$width <- as.integer(width)
  if(!is.null(height)) data$height <- as.integer(height)
  globe$session$sendCustomMessage("dimensions", data)
}

#' @name container
#' @export
globe_background <- function(globe, color = "#000011") UseMethod("globe_background")

#' @export
#' @method globe_background globe
globe_background.globe <- function(globe, color = "#000011"){
  globe$x$backgroundColor <- color
  return(globe)
}

#' @export
#' @method globe_background globeProxy
globe_background.globeProxy <- function(globe, color = "#000011"){
  data <- list(id = globe$id, color = color)
  globe$session$sendCustomMessage("backgroundColor", data)
}