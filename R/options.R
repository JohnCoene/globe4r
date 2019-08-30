#' Globe Material
#' 
#' Getter/setter for the URL of the image used in the material 
#' that wraps the globe. If no image is provided, the globe is 
#' represented as a black sphere.
#' 
#' @param globe An object of class \code{globe} as returned by
#' \code{\link{create_globe}}, or an object of class \code{globeProxy}
#' as returned by \code{\link{globeProxy}}.
#' @param url URL of the image layer to use.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url()
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("btn", "Add Image"),
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
#'       globe_img_url()
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
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
