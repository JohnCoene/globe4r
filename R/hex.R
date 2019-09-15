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


#' Hex Raw API
#' 
#' Functional API to add and customise hex on globe.
#' 
#' @inheritParams globe_hex
#' @param lat,lon Column names or numeric value indicating coordinates.
#' @param color Column name or character vector indicating color of points.
#' @param label Column name or constant of label.
#' @param altitude Column name or character vector indicating altitude of points 
#' in terms of globe radius units (0 = 0 altitude (flat circle), 1 = globe radius).
#' @param radius Column name of radius a numeric constant for the cylinder's 
#' radius, in angular degrees.
#' @param resolution Numeric value defining the geometric resolution of each 
#' cylinder, expressed in how many slice segments to divide the circumference. 
#' Higher values yield smoother cylinders.
#' @param merge Whether to merge all the point meshes 
#' into a single ThreeJS object, for improved rendering performance. 
#' Visually both options are equivalent, setting this option only affects 
#' the internal organization of the ThreeJS objects.
#' @param transition Duration (ms) of the transition 
#' to animate point changes involving geometry modifications. A value of 
#' \code{0} will move the objects immediately to their final position. 
#' New objects are animated by scaling them from the ground up. Only works 
#' if \code{merge} is disabled.
#' @param func JavaScript function as character vector.
#' 
#' @examples
#' # use data
#' create_globe() %>% 
#'   hex_data(quakes) %>% 
#'   hex_lat("lat") %>% 
#'   hex_lon("long") %>% 
#'   hex_weight("mag")
#' 
#' @name hex_data
#' @export
hex_data <- function(globe, data) UseMethod("hex_data")

#' @export
#' @method hex_data globe
hex_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  globe$x$hexBinPointsData <- data
  return(globe)
}

#' @export
#' @method hex_data globeProxy
hex_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  msg <- list(id = globe$id)
  msg$hexBinPointsData <- apply(data, 1, as.list)
  globe$session$sendCustomMessage("hex_data", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_lat <- function(globe, lat = "lat") UseMethod("hex_lat")

#' @export
#' @method hex_lat globe
hex_lat.globe <- function(globe, lat = "lat"){
  assert_that(not_missing(lat))
  globe$x$hexBinPointLat <- lat
  return(globe)
}

#' @export
#' @method hex_lat globeProxy
hex_lat.globeProxy <- function(globe, lat = "lat"){
  msg <- list(id = globe$id)
  msg$hexBinPointLat <- lat
  globe$session$sendCustomMessage("hex_lat", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_lon <- function(globe, lon = "lng") UseMethod("hex_lon")

#' @export
#' @method hex_lon globe
hex_lon.globe <- function(globe, lon = "lng"){
  globe$x$hexBinPointLng <- lon
  return(globe)
}

#' @export
#' @method hex_lon globeProxy
hex_lon.globeProxy <- function(globe, lon = "lng"){
  msg <- list(id = globe$id)
  msg$hexBinPointLng <- lon
  globe$session$sendCustomMessage("hex_lon", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_weight <- function(globe, weight = 1L) UseMethod("hex_weight")

#' @export
#' @method hex_weight globe
hex_weight.globe <- function(globe, weight = 1L){
  globe$x$hexBinPointWeight <- weight
  return(globe)
}

#' @export
#' @method hex_weight globeProxy
hex_weight.globeProxy <- function(globe, weight = 1L){
  msg <- list(id = globe$id)
  msg$hexBinPointWeight <- weight
  globe$session$sendCustomMessage("hex_weight", msg)
  return(globe)
} 