#' Arcs
#' 
#' Add arcs to a globe.
#' 
#' @inheritParams globe_bars
#' 
#' @section Coordinates:
#' Valid coordinates.
#' \itemize{
#'   \item{\code{start_lat}, \code{start_lon}}
#'   \item{\code{end_lat}, \code{end_lon}}
#'   \item{\code{altitude}}
#'   \item{\code{color}}
#'   \item{\code{label}}
#'   \item{\code{transition}}
#'   \item{\code{altitude_scale}}
#'   \item{\code{stroke}}
#'   \item{\code{curve_resolution}}
#'   \item{\code{circular_resolution}}
#'   \item{\code{dash_length}}
#'   \item{\code{dash_gap}}
#'   \item{\code{dash_initial_gap}}
#'   \item{\code{dash_animate_time}}
#' }
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_arcs(
#'     data= usflights, 
#'     coords(
#'       start_lat = start_lat, 
#'       start_lon = start_lon, 
#'       end_lat = end_lat, 
#'       end_lon = end_lon,
#'       color = cnt
#'     )
#'   ) %>% 
#'   scale_arc_color()
#' 
#' # in shiny
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("add", "Add arcs"),
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
#'       globe_arcs(
#'         coords(start_lat, start_lon, end_lat, end_lon),
#'         data = usflights 
#'       )
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' @export
globe_arcs <- function(globe, ..., data = NULL, inherit_coords = TRUE,
  on_click = NULL, on_right_click = NULL, on_hover = NULL) UseMethod("globe_arcs")

#' @export
#' @method globe_arcs globe
globe_arcs.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE,
  on_click = NULL, on_right_click = NULL, on_hover = NULL){

  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  coords <- combine_coords(globe$x$coords, coords, inherit_coords)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  # create points array
  globe$x$arcsData <- dplyr::select(data, columns)

  globe$x$arcStartLat <- coords_to_opts(coords, "start_lat")
  globe$x$arcStartLng <- coords_to_opts(coords, "start_lon")
  globe$x$arcEndLat <- coords_to_opts(coords, "end_lat")
  globe$x$arcEndLng <- coords_to_opts(coords, "end_lon")

  globe$x$arcLabel <- coords_to_opts(coords, "label")
  globe$x$arcColor <- coords_to_opts(coords, "color")
  globe$x$arcAltitude <- coords_to_opts(coords, "altitude")
  globe$x$arcAltitudeAutoScale <- coords_to_opts(coords, "altitude_scale")
  globe$x$arcStroke <- coords_to_opts(coords, "stroke")
  globe$x$arcCurveResolution <- coords_to_opts(coords, "curve_resolution")
  globe$x$arcCircularResolution <- coords_to_opts(coords, "circular_resolution")
  globe$x$arcDashLength <- coords_to_opts(coords, "dash_length")
  globe$x$arcDashGap <- coords_to_opts(coords, "dash_gap")
  globe$x$arcDashInitialGap <- coords_to_opts(coords, "dash_initial_gap")
  globe$x$arcDashAnimateTime <- coords_to_opts(coords, "dash_animate_time")
  globe$x$arcsTransitionDuration <- coords_to_opts(coords, "transition")
  globe$x$onArcClick <- if(!is.null(on_click)) on_click
  globe$x$onArcRightClick <- if(!is.null(on_right_click)) on_right_click
  globe$x$onArcHover <- if(!is.null(on_hover)) on_hover

  return(globe)
}

#' @export
#' @method globe_arcs globeProxy
globe_arcs.globeProxy <- function(globe, ..., data = NULL, inherit_coords = TRUE,
  on_click = NULL, on_right_click = NULL, on_hover = NULL){

  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  msg <- list(id = globe$id)

  msg$arcsData <- dplyr::select(data, columns)

  msg$arcStartLat <- coords_to_opts(coords, "start_lat")
  msg$arcStartLng <- coords_to_opts(coords, "start_lon")
  msg$arcEndLat <- coords_to_opts(coords, "end_lat")
  msg$arcEndLng <- coords_to_opts(coords, "end_lon")

  msg$arcLabel <- coords_to_opts(coords, "label")
  msg$arcColor <- coords_to_opts(coords, "color")
  msg$arcAltitude <- coords_to_opts(coords, "altitude")
  msg$arcAltitudeAutoScale <- coords_to_opts(coords, "altitude_scale")
  msg$arcStroke <- coords_to_opts(coords, "stroke")
  msg$arcCurveResolution <- coords_to_opts(coords, "curve_resolution")
  msg$arcCircularResolution <- coords_to_opts(coords, "circular_resolution")
  msg$arcDashLength <- coords_to_opts(coords, "dash_length")
  msg$arcDashGap <- coords_to_opts(coords, "dash_gap")
  msg$arcDashInitialGap <- coords_to_opts(coords, "dash_initial_gap")
  msg$arcDashAnimateTime <- coords_to_opts(coords, "dash_animate_time")
  msg$arcsTransitionDuration <- coords_to_opts(coords, "transition")
  msg$onArcClick <- if(!is.null(on_click)) on_click
  msg$onArcRightClick <- if(!is.null(on_right_click)) on_right_click
  msg$onArcHover <- if(!is.null(on_hover)) on_hover

  globe$session$sendCustomMessage("globe_arcs", msg)

  return(globe)
}

#' Arcs Raw API
#' 
#' Custimise arcs on a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame containing arcs data.
#' @param lat,lon Column names or numeric value indicating 
#' starting and ending coordinates of arc(s).
#' @param color Column name or character vector indicating color of points.
#' @param label Column name containing label or a charcater vector. Supports html.
#' @param altitude Column name or a numeric constant for the arc's maximum altitude
#' (ocurring at the half-way distance between the two points) in terms of globe radius
#' units (0 = a numeric constant for the scale of the arc's automatic altitude, in terms of units of the great-arc distance between the two points. A value of 1 indicates the arc should be as high as its length on the ground. Only applicable if arcAltitude is not set.0 altitude (ground line), 1 = globe radius). If a value of null or
#' undefined is used, the altitude is automatically set proportionally to the distance
#' between the two points, according to the scale set in \code{arcs_altitude_scale}.
#' @param stroke Column name indicating the line's diameter, 
#' in angular degrees. A value of null or undefined will render a 
#' \href{https://threejs.org/docs/#api/objects/Line}{ThreeJS} Line 
#' whose width is constant (1px) regardless of the camera distance. 
#' Otherwise, a 
#' \href{https://threejs.org/docs/#api/en/geometries/TubeGeometry}{TubeGeometry}
#' is used.
#' @param scale Column name or a numeric constant for the scale of the arc's automatic
#' altitude, in terms of units of the great-arc distance between the two points. A 
#' value of 1 indicates the arc should be as high as its length on the ground. 
#' Only applicable if \code{arcs_altitude} is not set.
#' @param curve Resolution, expressed in how many straight line 
#' segments to divide the curve by. Higher values yield smoother curves.
#' @param circular Radial geometric resolution of each line, 
#' expressed in how many slice segments to divide the tube's circumference. 
#' Only applicable when using Tube geometries (defined \code{stroke}).
#' @param length Column name or numeric giving the length of the dashed 
#' segments in the arc, in terms of relative length of the whole line 
#' (\code{1} = full line length).
#' @param gap Column name or numeric giving the length of the gap between 
#' dash segments, in terms of relative line length.
#' @param ms Colum name or numeric for the time duration (in ms) to animate 
#' the motion of dash positions from the start to the end point for a full line 
#' length. A value of 0 disables the animation.
#' @param func JavaScript function as character vector.
#' 
#' @examples
#' create_globe() %>% 
#'   arcs_data(usflights) %>% 
#'   arcs_start_lat("start_lat") %>% 
#'   arcs_start_lon("start_lon") %>% 
#'   arcs_end_lat("end_lat") %>% 
#'   arcs_end_lon("end_lon")
#' 
#' @name arcs_data
#' @export
arcs_data <- function(globe, data) UseMethod("arcs_data")

#' @export
#' @method arcs_data globe
arcs_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  globe$x$arcsData <- data
  return(globe)
}

#' @export
#' @method arcs_data globeProxy
arcs_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  msg <- list(id = globe$id)
  msg$arcsData <- apply(data, 1, as.list)
  globe$session$sendCustomMessage("arcs_data", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_label <- function(globe, label) UseMethod("arcs_label")

#' @export
#' @method arcs_label globe
arcs_label.globe <- function(globe, label){
  assert_that(not_missing(label))
  globe$x$arcLabel <- label
  return(globe)
}

#' @export
#' @method arcs_label globeProxy
arcs_label.globeProxy <- function(globe, label){
  assert_that(not_missing(label))
  msg <- list(id = globe$id)
  msg$arcLabel <- label
  globe$session$sendCustomMessage("arcs_label", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_start_lat <- function(globe, lat = "startLat") UseMethod("arcs_start_lat")

#' @export
#' @method arcs_start_lat globe
arcs_start_lat.globe <- function(globe, lat = "startLat"){
  assert_that(not_missing(lat))
  globe$x$arcStartLat <- lat
  return(globe)
}

#' @export
#' @method arcs_start_lat globeProxy
arcs_start_lat.globeProxy <- function(globe, lat = "startLat"){
  assert_that(not_missing(lat))
  msg <- list(id = globe$id)
  msg$arcStartLat <- lat
  globe$session$sendCustomMessage("arcs_start_lat", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_start_lon <- function(globe, lon = "startLng") UseMethod("arcs_start_lon")

#' @export
#' @method arcs_start_lon globe
arcs_start_lon.globe <- function(globe, lon = "startLng"){
  assert_that(not_missing(lon))
  globe$x$arcStartLng <- lon
  return(globe)
}

#' @export
#' @method arcs_start_lon globeProxy
arcs_start_lon.globeProxy <- function(globe, lon = "startLng"){
  assert_that(not_missing(lon))
  msg <- list(id = globe$id)
  msg$arcStartLng <- lon
  globe$session$sendCustomMessage("arcs_start_lon", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_end_lat <- function(globe, lat = "endLat") UseMethod("arcs_end_lat")

#' @export
#' @method arcs_end_lat globe
arcs_end_lat.globe <- function(globe, lat = "endLat"){
  assert_that(not_missing(lat))
  globe$x$arcEndLat <- lat
  return(globe)
}

#' @export
#' @method arcs_end_lat globeProxy
arcs_end_lat.globeProxy <- function(globe, lat = "endLat"){
  assert_that(not_missing(lat))
  msg <- list(id = globe$id)
  msg$arcEndLat <- lat
  globe$session$sendCustomMessage("arcs_end_lat", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_end_lon <- function(globe, lon = "endLng") UseMethod("arcs_end_lon")

#' @export
#' @method arcs_end_lon globe
arcs_end_lon.globe <- function(globe, lon = "endLng"){
  assert_that(not_missing(lon))
  globe$x$arcEndLng <- lon
  return(globe)
}

#' @export
#' @method arcs_end_lon globeProxy
arcs_end_lon.globeProxy <- function(globe, lon = "endLng"){
  assert_that(not_missing(lon))
  msg <- list(id = globe$id)
  msg$arcEndLng <- lon
  globe$session$sendCustomMessage("arcs_end_lon", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_color <- function(globe, color = constant("#ffffaa")) UseMethod("arcs_color")

#' @export
#' @method arcs_color globe
arcs_color.globe <- function(globe, color = constant("#ffffaa")){
  globe$x$arcColor <- color
  return(globe)
}

#' @export
#' @method arcs_color globeProxy
arcs_color.globeProxy <- function(globe, color = constant("#ffffaa")){
  msg <- list(id = globe$id)
  msg$arcColor <- color
  globe$session$sendCustomMessage("arcs_color", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_altitude <- function(globe, altitude) UseMethod("arcs_altitude")

#' @export
#' @method arcs_altitude globe
arcs_altitude.globe <- function(globe, altitude){
  assert_that(not_missing(altitude))
  globe$x$arcAltitude <- altitude
  return(globe)
}

#' @export
#' @method arcs_altitude globeProxy
arcs_altitude.globeProxy <- function(globe, altitude){
  assert_that(not_missing(altitude))
  msg <- list(id = globe$id)
  msg$arcAltitude <- altitude
  globe$session$sendCustomMessage("arcs_altitude", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_altitude_scale <- function(globe, scale = .5) UseMethod("arcs_altitude_scale")

#' @export
#' @method arcs_altitude_scale globe
arcs_altitude_scale.globe <- function(globe, scale = .5){
  globe$x$arcAltitudeAutoScale <- scale
  return(globe)
}

#' @export
#' @method arcs_altitude_scale globeProxy
arcs_altitude_scale.globeProxy <- function(globe, scale = .5){
  msg <- list(id = globe$id)
  msg$arcAltitudeAutoScale <- scale
  globe$session$sendCustomMessage("arcs_altitude_scale", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_stroke <- function(globe, stroke) UseMethod("arcs_stroke")

#' @export
#' @method arcs_stroke globe
arcs_stroke.globe <- function(globe, stroke){
  assert_that(not_missing(stroke))
  globe$x$arcStroke <- scale
  return(globe)
}

#' @export
#' @method arcs_stroke globeProxy
arcs_stroke.globeProxy <- function(globe, stroke){
  assert_that(not_missing(stroke))
  msg <- list(id = globe$id)
  msg$arcStroke <- scale
  globe$session$sendCustomMessage("arcs_stroke", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_curve_resolution <- function(globe, curve = 64L) UseMethod("arcs_curve_resolution")

#' @export
#' @method arcs_curve_resolution globe
arcs_curve_resolution.globe <- function(globe, curve = 64L){
  globe$x$arcCurveResolution <- curve
  return(globe)
}

#' @export
#' @method arcs_curve_resolution globeProxy
arcs_curve_resolution.globeProxy <- function(globe, curve = 64L){
  msg <- list(id = globe$id)
  msg$arcCurveResolution <- curve
  globe$session$sendCustomMessage("arcs_curve_resolution", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_circular_resolution <- function(globe, circular = 6L) UseMethod("arcs_circular_resolution")

#' @export
#' @method arcs_circular_resolution globe
arcs_circular_resolution.globe <- function(globe, circular = 6L){
  globe$x$arcCircularResolution <- circular
  return(globe)
}

#' @export
#' @method arcs_circular_resolution globeProxy
arcs_circular_resolution.globeProxy <- function(globe, circular = 6L){
  msg <- list(id = globe$id)
  msg$arcCircularResolution <- circular
  globe$session$sendCustomMessage("arcs_circular_resolution", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_dash_length <- function(globe, length = 1L) UseMethod("arcs_dash_length")

#' @export
#' @method arcs_dash_length globe
arcs_dash_length.globe <- function(globe, length = 1L){
  globe$x$arcDashLength <- length
  return(globe)
}

#' @export
#' @method arcs_dash_length globeProxy
arcs_dash_length.globeProxy <- function(globe, length = 1L){
  msg <- list(id = globe$id)
  msg$arcDashLength <- length
  globe$session$sendCustomMessage("arcs_dash_length", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_dash_gap <- function(globe, gap = 0L) UseMethod("arcs_dash_gap")

#' @export
#' @method arcs_dash_gap globe
arcs_dash_gap.globe <- function(globe, gap = 0L){
  globe$x$arcDashGap <- gap
  return(globe)
}

#' @export
#' @method arcs_dash_gap globeProxy
arcs_dash_gap.globeProxy <- function(globe, gap = 0L){
  msg <- list(id = globe$id)
  msg$arcDashGap <- gap
  globe$session$sendCustomMessage("arcs_dash_gap", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_dash_initial_gap <- function(globe, gap = 0L) UseMethod("arcs_dash_initial_gap")

#' @export
#' @method arcs_dash_initial_gap globe
arcs_dash_initial_gap.globe <- function(globe, gap = 0L){
  globe$x$arcDashInitialGap <- gap
  return(globe)
}

#' @export
#' @method arcs_dash_initial_gap globeProxy
arcs_dash_initial_gap.globeProxy <- function(globe, gap = 0L){
  msg <- list(id = globe$id)
  msg$arcDashInitialGap <- gap
  globe$session$sendCustomMessage("arcs_dash_initial_gap", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_dash_animate <- function(globe, ms = 0L) UseMethod("arcs_dash_animate")

#' @export
#' @method arcs_dash_animate globe
arcs_dash_animate.globe <- function(globe, ms = 0L){
  globe$x$arcDashAnimateTime <- ms
  return(globe)
}

#' @export
#' @method arcs_dash_animate globeProxy
arcs_dash_animate.globeProxy <- function(globe, ms = 0L){
  msg <- list(id = globe$id)
  msg$arcDashAnimateTime <- ms
  globe$session$sendCustomMessage("arcs_dash_animate", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_transition <- function(globe, ms = 0L) UseMethod("arcs_transition")

#' @export
#' @method arcs_transition globe
arcs_transition.globe <- function(globe, ms = 0L){
  globe$x$arcsTransitionDuration <- ms
  return(globe)
}

#' @export
#' @method arcs_transition globeProxy
arcs_transition.globeProxy <- function(globe, ms = 0L){
  msg <- list(id = globe$id)
  msg$arcsTransitionDuration <- ms
  globe$session$sendCustomMessage("arcs_transition", msg)
  return(globe)
}

#' @rdname arcs_data
#' @export
arcs_on_click <- function(globe, func) UseMethod("arcs_on_click")

#' @export
#' @method arcs_on_click globe
arcs_on_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method arcs_on_click globeProxy
arcs_on_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("arcs_on_click", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_on_right_click <- function(globe, func) UseMethod("arcs_on_right_click")

#' @export
#' @method arcs_on_right_click globe
arcs_on_right_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointRightClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method arcs_on_right_click globeProxy
arcs_on_right_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointRightClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("arcs_on_right_click", msg)
  return(globe)
} 

#' @rdname arcs_data
#' @export
arcs_on_hover <- function(globe, func) UseMethod("arcs_on_hover")

#' @export
#' @method arcs_on_hover globe
arcs_on_hover.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointHover <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method arcs_on_hover globeProxy
arcs_on_hover.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointHover <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("arcs_on_hover", msg)
  return(globe)
} 