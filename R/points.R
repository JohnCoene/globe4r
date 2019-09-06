#' Points
#' 
#' Add points to a globe.
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
#'   \item{\code{radius}}
#'   \item{\code{color}}
#'   \item{\code{label}}
#'   \item{\code{resolution}}
#'   \item{\code{merge}}
#'   \item{\code{transition}}
#' }
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_points(coords(lat, long, label = stations), data = quakes)
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("add", "Add points"),
#'   globeOutput("globe")
#' )
#' 
#' server <- function(input, output){
#'   output$globe <- renderGlobe({
#'     create_globe() %>% globe_img_url()
#'   })
#' 
#'   observeEvent(input$add, {
#'     globeProxy("globe") %>% 
#'       globe_points(coords(lat, long), data = quakes) %>% 
#'       globe_pov(-21, 179)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' @export
globe_points <- function(globe, ..., data = NULL, inherit_coords = TRUE, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL) UseMethod("globe_points")

#' @export
#' @method globe_points globe
globe_points.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE, on_click = NULL, on_right_click = NULL, 
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
  globe$x$pointsData <- dplyr::select(data, columns)

  # set options
  globe$x$pointLat <- coords_to_opts(coords, "lat")
  globe$x$pointLng <- coords_to_opts(coords, "lon")
  globe$x$pointLabel <- coords_to_opts(coords, "label")
  globe$x$pointColor <- coords_to_opts(coords, "color")
  globe$x$pointAltitude <- coords_to_opts(coords, "altitude")
  globe$x$pointRadius <-  coords_to_opts(coords, "radius")
  globe$x$pointResolution <- coords_to_opts(coords, "resolution")
  globe$x$pointsMerge <- coords_to_opts(coords, "merge")
  globe$x$pointsTransitionDuration <- coords_to_opts(coords, "transition")
  globe$x$onPointClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  globe$x$onPointRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  globe$x$onPointHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)
  
  return(globe)
}


#' @export
#' @method globe_points globeProxy
globe_points.globeProxy <- function(globe, ..., data = NULL, inherit_coords = FALSE, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){

  assert_that(has_data(data))

  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  msg <- list(id = globe$id)

  # create points array
  msg$pointsData <- dplyr::select(data, columns) %>% 
    apply(1, as.list)

  msg$pointLat <- coords_to_opts(coords, "lat")
  msg$pointLng <- coords_to_opts(coords, "lon")
  msg$pointLabel <- coords_to_opts(coords, "label")
  msg$pointColor <- coords_to_opts(coords, "color")
  msg$pointAltitude <- coords_to_opts(coords, "altitude")
  msg$pointRadius <-  coords_to_opts(coords, "radius")
  msg$pointResolution <- coords_to_opts(coords, "resolution")
  msg$pointsMerge <- coords_to_opts(coords, "merge")
  msg$pointsTransitionDuration <- coords_to_opts(coords, "transition")
  msg$onPointClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  msg$onPointRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  msg$onPointHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)
  
  globe$session$sendCustomMessage("globe_points", msg)

  return(globe)
}

#' Points Functional API
#' 
#' Functional API to add and customise points on globe.
#' 
#' @inheritParams globe_points
#' @param lat,lon Column names or numeric value indicating coordinates.
#' @param color Column name or character vector indicating color of points.
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
#'   globe_img_url() %>% 
#'   points_data(quakes) %>% 
#'   points_lat("lat") %>% 
#'   points_lon("long")
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("draw", "draw points"),
#'   globeOutput("globe")
#' )
#' 
#' server <- function(input, output) {
#'   output$globe <- renderGlobe({
#'     create_globe() %>% 
#'       globe_img_url() %>% 
#'       points_color(htmlwidgets::JS("() => '#ffffff'")) 
#'   })
#' 
#'   observeEvent(input$draw, {
#'     globeProxy("globe") %>% 
#'       points_data(quakes) %>% 
#'       points_lon("long") %>% 
#'       globe_pov(-21, 179)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
#' @name points_data
#' @export
points_data <- function(globe, data) UseMethod("points_data")

#' @export
#' @method points_data globe
points_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  globe$x$pointsData <- data
  return(globe)
}

#' @export
#' @method points_data globeProxy
points_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  msg <- list(id = globe$id)
  msg$pointsData <- apply(data, 1, as.list)
  globe$session$sendCustomMessage("points_data", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_lat <- function(globe, lat = "lat") UseMethod("points_lat")

#' @export
#' @method points_lat globe
points_lat.globe <- function(globe, lat = "lat"){
  assert_that(not_missing(lat))
  globe$x$pointLat <- lat
  return(globe)
}

#' @export
#' @method points_data globeProxy
points_lat.globeProxy <- function(globe, lat = "lat"){
  assert_that(not_missing(lat))
  msg <- list(id = globe$id)
  msg$pointLat <- lat
  globe$session$sendCustomMessage("points_lat", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_lon <- function(globe, lon = "lng") UseMethod("points_lon")

#' @export
#' @method points_lon globe
points_lon.globe <- function(globe, lon = "lng"){
  assert_that(not_missing(lon))
  globe$x$pointLng <- lon
  return(globe)
}

#' @export
#' @method points_lon globeProxy
points_lon.globeProxy <- function(globe, lon = "lng"){
  assert_that(not_missing(lon))
  msg <- list(id = globe$id)
  msg$pointLng <- lon
  globe$session$sendCustomMessage("points_lon", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_color <- function(globe, color = constant("ffffaa")) UseMethod("points_color")

#' @export
#' @method points_color globe
points_color.globe <- function(globe, color = constant("ffffaa")){
  assert_that(not_missing(color))
  globe$x$pointColor <- color
  return(globe)
}

#' @export
#' @method points_color globeProxy
points_color.globeProxy <- function(globe, color = constant("ffffaa")){
  assert_that(not_missing(color))
  msg <- list(id = globe$id)
  msg$pointColor <- color
  globe$session$sendCustomMessage("points_color", msg)
  return(globe)
}

#' @rdname points_data
#' @export
points_altitude <- function(globe, altitude = .1) UseMethod("points_altitude")

#' @export
#' @method points_altitude globe
points_altitude.globe <- function(globe, altitude = .1){
  globe$x$pointAltitude <- altitude
  return(globe)
}

#' @export
#' @method points_altitude globeProxy
points_altitude.globeProxy <- function(globe, altitude = .1){
  msg <- list(id = globe$id)
  msg$pointAltitude <- altitude
  globe$session$sendCustomMessage("points_altitude", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_radius <- function(globe, radius = .25) UseMethod("points_radius")

#' @export
#' @method points_radius globe
points_radius.globe <- function(globe, radius = .25){
  globe$x$pointRadius <- radius
  return(globe)
}

#' @export
#' @method points_radius globeProxy
points_radius.globeProxy <- function(globe, radius = .25){
  msg <- list(id = globe$id)
  msg$pointRadius <- radius
  globe$session$sendCustomMessage("points_radius", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_resolution <- function(globe, resolution = 12L) UseMethod("points_resolution")

#' @export
#' @method points_resolution globe
points_resolution.globe <- function(globe, resolution = 12L){
  globe$x$pointResolution <- resolution
  return(globe)
}

#' @export
#' @method points_resolution globeProxy
points_resolution.globeProxy <- function(globe, resolution = 12L){
  msg <- list(id = globe$id)
  msg$pointResolution <- resolution
  globe$session$sendCustomMessage("points_resolution", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_merge <- function(globe, merge = TRUE) UseMethod("points_merge")

#' @export
#' @method points_merge globe
points_merge.globe <- function(globe, merge = TRUE){
  globe$x$pointMerge <- merge
  return(globe)
}

#' @export
#' @method points_merge globeProxy
points_merge.globeProxy <- function(globe, merge = TRUE){
  msg <- list(id = globe$id)
  msg$pointMerge <- merge
  globe$session$sendCustomMessage("points_merge", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_transition <- function(globe, transition = 1000L) UseMethod("points_transition")

#' @export
#' @method points_transition globe
points_transition.globe <- function(globe, transition = 1000L){
  globe$x$pointsTransitionDuration <- transition
  return(globe)
}

#' @export
#' @method points_transition globeProxy
points_transition.globeProxy <- function(globe, transition = 1000L){
  msg <- list(id = globe$id)
  msg$pointsTransitionDuration <- transition
  globe$session$sendCustomMessage("points_transition", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_on_click <- function(globe, func) UseMethod("points_on_click")

#' @export
#' @method points_on_click globe
points_on_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method points_on_click globeProxy
points_on_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_click", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_on_right_click <- function(globe, func) UseMethod("points_on_right_click")

#' @export
#' @method points_on_right_click globe
points_on_right_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointRightClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method points_on_right_click globeProxy
points_on_right_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointRightClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_right_click", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_on_hover <- function(globe, func) UseMethod("points_on_hover")

#' @export
#' @method points_on_hover globe
points_on_hover.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointHover <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method points_on_hover globeProxy
points_on_hover.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointHover <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_hover", msg)
  return(globe)
} 