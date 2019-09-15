#' Hex
#' 
#' Add hexbin to a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame of points to draw.
#' @param on_click,on_right_click,on_hover JavaScript functions as strings.
#' @param inherit_coords Whether to inherit the coordinates (\code{\link{coords}})
#' from \code{\link{create_globe}}. Only applies to method applied to object of class
#' \code{globe4r} not on objects of class \code{globeProxy}.
#' @param ... Coordinates, as specified by \code{\link{coords}}.
#' 
#' @section Coordinates:
#' Valid coordinates (depending on layer).
#' \itemize{
#'   \item{\code{lat}, \code{lon}},
#'   \item{\code{altitude}}
#'   \item{\code{margin}}
#'   \item{\code{weight}}
#'   \item{\code{resolution}}
#'   \item{\code{merge}}
#'   \item{\code{transition}}
#' }
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_hex(coords(lat, long, altitude = mag, label = stations), data = quakes)
#' 
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("add", "Add hex"),
#'   globeOutput("globe")
#' )
#' 
#' server <- function(input, output){
#'   output$globe <- renderGlobe({
#'     create_globe() 
#'   })
#' 
#'   observeEvent(input$add, {
#'     globeProxy("globe") %>% 
#'       globe_hex(coords(lat, long, weight = mag), data = quakes) %>% 
#'       globe_pov(-21, 179)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
#' @export
globe_hex <- function(globe, ..., data = NULL, inherit_coords = TRUE, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL) UseMethod("globe_hex")

#' @export
#' @method globe_hex globe
globe_hex.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){


  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  coords <- combine_coords(globe$x$coords, coords, inherit_coords)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  # create points array
  globe$x$hexBinPointsData <- dplyr::select(data, columns)

  # set options
  globe$x$hexLabel <- coords_to_opts(coords, "label")
  globe$x$hexBinPointLat <- coords_to_opts(coords, "lat")
  globe$x$hexBinPointLng <- coords_to_opts(coords, "lon")
  globe$x$hexBinPointWeight <- coords_to_opts(coords, "weight")
  globe$x$hexBinResolution <- coords_to_opts(coords, "resolution")
  globe$x$hexAltitude <- coords_to_opts(coords, "altitude")
  globe$x$hexMargin <-  coords_to_opts(coords, "margin")
  globe$x$hexTopColor <- coords_to_opts(coords, "cap_color")
  globe$x$hexSideColor <- coords_to_opts(coords, "side_color")
  globe$x$hexBinMerge <-  coords_to_opts(coords, "merge")
  globe$x$hexTransitionDuration <- coords_to_opts(coords, "transition")
  globe$x$onHexClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  globe$x$onHexRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  globe$x$onHexHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)
  
  return(globe)

}


#' @export
#' @method globe_hex globeProxy
globe_hex.globeProxy <- function(globe, ..., data = NULL, inherit_coords = FALSE, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){

  assert_that(!missing(data), msg = "Stop missing `data`")

  # extract & process coordinates
  coords <- get_coords(...)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  msg <- list(id = globe$id)

  # create points array
  msg$hexBinPointsData <- dplyr::select(data, columns)

  # set options
  msg$hexLabel <- coords_to_opts(coords, "label")
  msg$hexBinPointLat <- coords_to_opts(coords, "lat")
  msg$hexBinPointLng <- coords_to_opts(coords, "lon")
  msg$hexBinPointWeight <- coords_to_opts(coords, "weight")
  msg$hexBinResolution <- coords_to_opts(coords, "resolution")
  msg$hexAltitude <- coords_to_opts(coords, "altitude")
  msg$hexMargin <-  coords_to_opts(coords, "margin")
  msg$hexTopColor <- coords_to_opts(coords, "cap_color")
  msg$hexSideColor <- coords_to_opts(coords, "side_color")
  msg$hexBinMerge <-  coords_to_opts(coords, "merge")
  msg$hexTransitionDuration <- coords_to_opts(coords, "transition")
  msg$onHexClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  msg$onHexRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  msg$onHexHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)

  globe$session$sendCustomMessage("globe_hex", msg)
  
  return(globe)

}
