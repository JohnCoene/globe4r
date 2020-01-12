#' Bars
#' 
#' Add bars to a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame of points to draw.
#' @param on_click,on_right_click,on_hover JavaScript functions as \link[htmlwidgets]{JS} 
#' or \code{TRUE} to pick up the event from Shiny server side, see example.
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
#'   globe_pov(-21, 179) %>% 
#'   globe_bars(coords(lat, long, altitude = mag), data = quakes)
#' 
#' # use in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("add", "Add points"),
#'   globeOutput("globe"),
#'   verbatimTextOutput("clicked")
#' )
#' 
#' server <- function(input, output){
#'   output$globe <- renderGlobe({
#'     create_globe() %>% globe_img_url()
#'   })
#' 
#'   observeEvent(input$add, {
#'     globeProxy("globe") %>% 
#'       globe_bars(coords(lat, long), data = quakes, on_click = TRUE) %>% 
#'       globe_pov(-21, 179)
#'   })
#' 
#'   output$clicked <- renderPrint({
#'     input$globe_click_bar
#'   })
#' }
#' 
#' if(interactive()) shinyApp(ui, server)
#' @export
globe_bars <- function(globe, ..., data = NULL, inherit_coords = TRUE, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL) UseMethod("globe_bars")

#' @export
#' @method globe_bars globe
globe_bars.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE, on_click = NULL, on_right_click = NULL, 
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
  globe$x$onPointClick <- if(!is.null(on_click)) on_click
  globe$x$onPointRightClick <- if(!is.null(on_right_click)) on_right_click
  globe$x$onPointHover <- if(!is.null(on_hover)) on_hover
  
  return(globe)
}


#' @export
#' @method globe_bars globeProxy
globe_bars.globeProxy <- function(globe, ..., data = NULL, inherit_coords = FALSE, on_click = NULL, on_right_click = NULL, 
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
  msg$onPointClick <- if(!is.null(on_click)) on_click
  msg$onPointRightClick <- if(!is.null(on_right_click)) on_right_click
  msg$onPointHover <- if(!is.null(on_hover)) on_hover
  
  globe$session$sendCustomMessage("globe_points", msg)

  return(globe)
}

#' Bars Raw API
#' 
#' Functional API to add and customise bars on globe.
#' 
#' @inheritParams globe_bars
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
#'   bars_data(quakes) %>% 
#'   bars_lat("lat") %>% 
#'   bars_lon("long")
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
#'       bars_color(constant("#ffffff")) 
#'   })
#' 
#'   observeEvent(input$draw, {
#'     globeProxy("globe") %>% 
#'       bars_data(quakes) %>% 
#'       bars_lon("long") %>% 
#'       globe_pov(-21, 179)
#'   })
#' }
#' 
#' if(interactive()) shinyApp(ui, server)
#' 
#' @name bars_data
#' @export
bars_data <- function(globe, data) UseMethod("bars_data")

#' @export
#' @method bars_data globe
bars_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  globe$x$pointsData <- data
  return(globe)
}

#' @export
#' @method bars_data globeProxy
bars_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  msg <- list(id = globe$id)
  msg$pointsData <- apply(data, 1, as.list)
  globe$session$sendCustomMessage("points_data", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_lat <- function(globe, lat = "lat") UseMethod("bars_lat")

#' @export
#' @method bars_lat globe
bars_lat.globe <- function(globe, lat = "lat"){
  assert_that(not_missing(lat))
  globe$x$pointLat <- lat
  return(globe)
}

#' @export
#' @method bars_data globeProxy
bars_lat.globeProxy <- function(globe, lat = "lat"){
  assert_that(not_missing(lat))
  msg <- list(id = globe$id)
  msg$pointLat <- lat
  globe$session$sendCustomMessage("points_lat", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_lon <- function(globe, lon = "lng") UseMethod("bars_lon")

#' @export
#' @method bars_lon globe
bars_lon.globe <- function(globe, lon = "lng"){
  assert_that(not_missing(lon))
  globe$x$pointLng <- lon
  return(globe)
}

#' @export
#' @method bars_lon globeProxy
bars_lon.globeProxy <- function(globe, lon = "lng"){
  assert_that(not_missing(lon))
  msg <- list(id = globe$id)
  msg$pointLng <- lon
  globe$session$sendCustomMessage("points_lon", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_color <- function(globe, color = constant("ffffaa")) UseMethod("bars_color")

#' @export
#' @method bars_color globe
bars_color.globe <- function(globe, color = constant("ffffaa")){
  assert_that(not_missing(color))
  globe$x$pointColor <- color
  return(globe)
}

#' @export
#' @method bars_color globeProxy
bars_color.globeProxy <- function(globe, color = constant("ffffaa")){
  assert_that(not_missing(color))
  msg <- list(id = globe$id)
  msg$pointColor <- color
  globe$session$sendCustomMessage("points_color", msg)
  return(globe)
}


#' @rdname bars_data
#' @export
bars_label <- function(globe, label) UseMethod("bars_label")

#' @export
#' @method bars_label globe
bars_label.globe <- function(globe, label){
  assert_that(not_missing(label))
  globe$x$pointLabel <- label
  return(globe)
}

#' @export
#' @method bars_label globeProxy
bars_label.globeProxy <- function(globe, label){
  assert_that(not_missing(label))
  msg <- list(id = globe$id)
  msg$pointLabel <- label
  globe$session$sendCustomMessage("points_labels", msg)
  return(globe)
}

#' @rdname bars_data
#' @export
bars_altitude <- function(globe, altitude = .1) UseMethod("bars_altitude")

#' @export
#' @method bars_altitude globe
bars_altitude.globe <- function(globe, altitude = .1){
  globe$x$pointAltitude <- altitude
  return(globe)
}

#' @export
#' @method bars_altitude globeProxy
bars_altitude.globeProxy <- function(globe, altitude = .1){
  msg <- list(id = globe$id)
  msg$pointAltitude <- altitude
  globe$session$sendCustomMessage("points_altitude", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_radius <- function(globe, radius = .25) UseMethod("bars_radius")

#' @export
#' @method bars_radius globe
bars_radius.globe <- function(globe, radius = .25){
  globe$x$pointRadius <- radius
  return(globe)
}

#' @export
#' @method bars_radius globeProxy
bars_radius.globeProxy <- function(globe, radius = .25){
  msg <- list(id = globe$id)
  msg$pointRadius <- radius
  globe$session$sendCustomMessage("points_radius", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_resolution <- function(globe, resolution = 12L) UseMethod("bars_resolution")

#' @export
#' @method bars_resolution globe
bars_resolution.globe <- function(globe, resolution = 12L){
  globe$x$pointResolution <- resolution
  return(globe)
}

#' @export
#' @method bars_resolution globeProxy
bars_resolution.globeProxy <- function(globe, resolution = 12L){
  msg <- list(id = globe$id)
  msg$pointResolution <- resolution
  globe$session$sendCustomMessage("points_resolution", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_merge <- function(globe, merge = TRUE) UseMethod("bars_merge")

#' @export
#' @method bars_merge globe
bars_merge.globe <- function(globe, merge = TRUE){
  globe$x$pointMerge <- merge
  return(globe)
}

#' @export
#' @method bars_merge globeProxy
bars_merge.globeProxy <- function(globe, merge = TRUE){
  msg <- list(id = globe$id)
  msg$pointMerge <- merge
  globe$session$sendCustomMessage("points_merge", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_transition <- function(globe, transition = 1000L) UseMethod("bars_transition")

#' @export
#' @method bars_transition globe
bars_transition.globe <- function(globe, transition = 1000L){
  globe$x$pointsTransitionDuration <- transition
  return(globe)
}

#' @export
#' @method bars_transition globeProxy
bars_transition.globeProxy <- function(globe, transition = 1000L){
  msg <- list(id = globe$id)
  msg$pointsTransitionDuration <- transition
  globe$session$sendCustomMessage("points_transition", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_on_click <- function(globe, func) UseMethod("bars_on_click")

#' @export
#' @method bars_on_click globe
bars_on_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method bars_on_click globeProxy
bars_on_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_click", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_on_right_click <- function(globe, func) UseMethod("bars_on_right_click")

#' @export
#' @method bars_on_right_click globe
bars_on_right_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointRightClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method bars_on_right_click globeProxy
bars_on_right_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointRightClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_right_click", msg)
  return(globe)
} 

#' @rdname bars_data
#' @export
bars_on_hover <- function(globe, func) UseMethod("bars_on_hover")

#' @export
#' @method bars_on_hover globe
bars_on_hover.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointHover <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method bars_on_hover globeProxy
bars_on_hover.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointHover <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("points_on_hover", msg)
  return(globe)
} 