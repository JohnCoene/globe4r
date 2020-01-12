#' Hex
#' 
#' Add hexbin to a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame of points to draw.
#' @param on_click,on_right_click,on_hover JavaScript functions as \link[htmlwidgets]{JS} 
#' or \code{TRUE} to pick up the event from Shiny server side, see example in \code{\link{globe_bars}}.
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
#'   globe_hex(coords(lat, long, weight = 1L), data = quakes) %>% 
#'   scale_hex_side_color(max = 5) %>% 
#'   scale_hex_cap_color(max = 5)
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
#' if(interactive()) shinyApp(ui, server)
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

  w <- coords_to_opts(coords, "weight")

  # set options
  globe$x$hexLabel <- coords_to_opts(coords, "label")
  globe$x$hexBinPointLat <- coords_to_opts(coords, "lat")
  globe$x$hexBinPointLng <- coords_to_opts(coords, "lon")
  globe$x$hexBinPointWeight <- if(is.null(w)) 1L else w
  globe$x$hexBinResolution <- coords_to_opts(coords, "resolution")
  globe$x$hexAltitude <- coords_to_opts(coords, "altitude")
  globe$x$hexMargin <-  coords_to_opts(coords, "margin")
  globe$x$hexTopColor <- coords_to_opts(coords, "cap_color")
  globe$x$hexSideColor <- coords_to_opts(coords, "side_color")
  globe$x$hexBinMerge <-  coords_to_opts(coords, "merge")
  globe$x$hexTransitionDuration <- coords_to_opts(coords, "transition")
  globe$x$onHexClick <- if(!is.null(on_click)) on_click
  globe$x$onHexRightClick <- if(!is.null(on_right_click)) on_right_click
  globe$x$onHexHover <- if(!is.null(on_hover)) on_hover
  
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
  msg$onHexClick <- if(!is.null(on_click)) on_click
  msg$onHexRightClick <- if(!is.null(on_right_click)) on_right_click
  msg$onHexHover <- if(!is.null(on_hover)) on_hover

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
#' @param weight Column name or JavaScript function defining height of hex.
#' @param margin The radial margin of each hexagon. 
#' Margins above 0 will create gaps between adjacent hexagons and serve only 
#' a visual purpose, as the data points within the margin still contribute to 
#' the hexagon's data. The margin is specified in terms of fraction of the 
#' hexagon's surface diameter. Values below 0 or above 1 are disadvised. This 
#' property also supports using an accessor method based on the hexagon's 
#' aggregated data, following the syntax: 
#' \code{htmlwidgets::JS('hexMargin(({ points, sumWeight, center: { lat, lng }}) => ...)')}. 
#' This method should return a numeric constant.
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
#'   hex_weight(1L)
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

#' @rdname hex_data
#' @export
hex_label <- function(globe, label) UseMethod("hex_label")

#' @export
#' @method hex_label globe
hex_label.globe <- function(globe, label){
  assert_that(not_missing(label))
  globe$x$hexLabel <- label
  return(globe)
}

#' @export
#' @method hex_label globeProxy
hex_label.globeProxy <- function(globe, label){
  assert_that(not_missing(label))
  msg <- list(id = globe$id)
  msg$hexLabel <- label
  globe$session$sendCustomMessage("hex_label", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_resolution <- function(globe, resolution = 4L) UseMethod("hex_resolution")

#' @export
#' @method hex_resolution globe
hex_resolution.globe <- function(globe, resolution = 4L){
  globe$x$hexBinResolution <- resolution
  return(globe)
}

#' @export
#' @method hex_resolution globeProxy
hex_resolution.globeProxy <- function(globe, resolution = 4L){
  msg <- list(id = globe$id)
  msg$hexBinResolution <- resolution
  globe$session$sendCustomMessage("hex_resolution", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_margin <- function(globe, margin = .2) UseMethod("hex_margin")

#' @export
#' @method hex_margin globe
hex_margin.globe <- function(globe, margin = .2){
  globe$x$hexMargin <- margin
  return(globe)
}

#' @export
#' @method hex_margin globeProxy
hex_margin.globeProxy <- function(globe, margin = .2){
  msg <- list(id = globe$id)
  msg$hexMargin <- margin
  globe$session$sendCustomMessage("hex_margin", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_cap_color <- function(globe, color = constant("#ffffaa")) UseMethod("hex_cap_color")

#' @export
#' @method hex_cap_color globe
hex_cap_color.globe <- function(globe, color = constant("#ffffaa")){
  globe$x$hexTopColor <- color
  return(globe)
}

#' @export
#' @method hex_cap_color globeProxy
hex_cap_color.globeProxy <- function(globe, color = constant("#ffffaa")){
  msg <- list(id = globe$id)
  msg$hexTopColor <- color
  globe$session$sendCustomMessage("hex_cap_color", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_side_color <- function(globe, color = constant("#ffffaa")) UseMethod("hex_side_color")

#' @export
#' @method hex_side_color globe
hex_side_color.globe <- function(globe, color = constant("#ffffaa")){
  globe$x$hexSideColor <- color
  return(globe)
}

#' @export
#' @method hex_side_color globeProxy
hex_side_color.globeProxy <- function(globe, color = constant("#ffffaa")){
  msg <- list(id = globe$id)
  msg$hexSideColor <- color
  globe$session$sendCustomMessage("hex_side_color", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_merge <- function(globe, merge = TRUE) UseMethod("hex_merge")

#' @export
#' @method hex_merge globe
hex_merge.globe <- function(globe, merge = TRUE){
  globe$x$hexBinMerge <- merge
  return(globe)
}

#' @export
#' @method hex_merge globeProxy
hex_merge.globeProxy <- function(globe, merge = TRUE){
  msg <- list(id = globe$id)
  msg$hexBinMerge <- merge
  globe$session$sendCustomMessage("hex_merge", msg)
  return(globe)
}

#' @rdname hex_data
#' @export
hex_transition <- function(globe, transition = 1000L) UseMethod("hex_transition")

#' @export
#' @method hex_transition globe
hex_transition.globe <- function(globe, transition = 1000L){
  globe$x$hexTransitionDuration <- transition
  return(globe)
}

#' @export
#' @method hex_transition globeProxy
hex_transition.globeProxy <- function(globe, transition = 1000L){
  msg <- list(id = globe$id)
  msg$hexTransitionDuration <- transition
  globe$session$sendCustomMessage("hex_transition", msg)
  return(globe)
}

#' @rdname hex_data
#' @export
hex_on_click <- function(globe, func) UseMethod("hex_on_click")

#' @export
#' @method hex_on_click globe
hex_on_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method hex_on_click globeProxy
hex_on_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_click", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_on_right_click <- function(globe, func) UseMethod("hex_on_right_click")

#' @export
#' @method hex_on_right_click globe
hex_on_right_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointRightClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method hex_on_right_click globeProxy
hex_on_right_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointRightClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("hex_on_right_click", msg)
  return(globe)
} 

#' @rdname hex_data
#' @export
hex_on_hover <- function(globe, func) UseMethod("hex_on_hover")

#' @export
#' @method hex_on_hover globe
hex_on_hover.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointHover <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method hex_on_hover globeProxy
hex_on_hover.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointHover <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("hex_on_hover", msg)
  return(globe)
} 