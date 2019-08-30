#' Arcs
#' 
#' Add arcs to a globe.
#' 
#' @inheritParams globe_points
#' @param start_lat,start_lon,end_lat,end_lon Bare column names 
#' giving start and end coordinates of arcs. 
#' @param label Bare column name of arc labels.
#' @param color Bare column name of arc color.
#' @param altitude Bare column name of the arc's maximum altitude 
#' (ocurring at the half-way distance between the two points) in 
#' terms of globe radius units (\code{0} = 0 altitude (ground line), 
#' \code{1} = globe radius). If a value of null or undefined is used, 
#' the altitude is automatically set proportionally to the distance 
#' between the two points, according to the scale set in 
#' \code{altitude_scale}.
#' @param altitude_scale Bare column name of he scale of the arc's 
#' automatic altitude, in terms of units of the great-arc distance 
#' between the two points. A value of 1 indicates the arc should be 
#' as high as its length on the ground. Only applicable if 
#' \code{altitude} is not set.
#' @param stroke Bare column name indicating the line's diameter, 
#' in angular degrees. A value of null or undefined will render a 
#' \href{https://threejs.org/docs/#api/objects/Line}{ThreeJS} Line 
#' whose width is constant (1px) regardless of the camera distance. 
#' Otherwise, a 
#' \href{https://threejs.org/docs/#api/en/geometries/TubeGeometry}{TubeGeometry}
#' is used.
#' @param curve_resolution Resolution, expressed in how many straight line 
#' segments to divide the curve by. Higher values yield smoother curves.
#' @param circular_resolution Radial geometric resolution of each line, 
#' expressed in how many slice segments to divide the tube's circumference. 
#' Only applicable when using Tube geometries (defined \code{stroke}).
#' @param dash_length The length of the dashed segments in the arc, in terms
#' of relative length of the whole line (\code{1} = full line length).
#' @param dash_gap The length of the gap between dash segments, in terms of 
#' relative line length.
#' @param dash_initial_gap The length of the initial gap before the first 
#' dash segment, in terms of relative line length.
#' @param dash_animate_time The time duration (in ms) to animate the motion 
#' of dash positions from the start to the end point for a full line length. 
#' A value of 0 disables the animation.
#' @param transition Duration (ms) of the transition to animate arc changes 
#' involving geometry modifications. A value of 0 will move the arcs immediately 
#' to their final position. New arcs are animated by rising them from the ground up.
#' @param on_click,on_right_click,on_hover JavaScript functions as strings.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_arcs(usflights, start_lat, start_lon, end_lat, end_lon)
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
#'     create_globe() %>% 
#'       globe_img_url()
#'   })
#' 
#'   observeEvent(input$add, {
#'     globeProxy("globe") %>% 
#'       globe_arcs(usflights, start_lat, start_lon, end_lat, end_lon)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' @export
globe_arcs <- function(globe, data, start_lat, start_lon, end_lat, end_lon,
  label = NULL, color = NULL, altitude = NULL, altitude_scale = NULL, stroke = NULL,
  curve_resolution = 64L, circular_resolution = 6L, dash_length = 1L, dash_gap = 0L,
  dash_initial_gap = 0L, dash_animate_time = 0L, transition = 1000L,
  on_click = NULL, on_right_click = NULL, on_hover = NULL) UseMethod("globe_arcs")

#' @export
#' @method globe_arcs globe
globe_arcs.globe <- function(globe, data, start_lat, start_lon, end_lat, end_lon,
  label = NULL, color = NULL, altitude = NULL, altitude_scale = NULL, stroke = NULL,
  curve_resolution = 64L, circular_resolution = 6L, dash_length = 1L, dash_gap = 0L,
  dash_initial_gap = 0L, dash_animate_time = 0L, transition = 1000L,
  on_click = NULL, on_right_click = NULL, on_hover = NULL){

  # check inputs
  assert_that(not_missing(data))
  assert_that(not_missing(start_lat))
  assert_that(not_missing(start_lon))
  assert_that(not_missing(end_lat))
  assert_that(not_missing(end_lon))

  # enquo all things
  start_lat_enquo <- rlang::enquo(start_lat)
  start_lon_enquo <- rlang::enquo(start_lon)
  end_lat_enquo <- rlang::enquo(end_lat)
  end_lon_enquo <- rlang::enquo(end_lon)
  label_enquo <- rlang::enquo(label)
  color_enquo <- rlang::enquo(color)
  altitude_enquo <- rlang::enquo(altitude)
  altitude_scale_enquo <- rlang::enquo(altitude_scale)
  stroke_enquo <- rlang::enquo(stroke)

  # create points array
  globe$x$arcsData <- data %>% 
    dplyr::select(
      startLat = !!start_lat_enquo,
      startLng = !!start_lon_enquo,
      endLat = !!end_lat_enquo,
      endLng = !!end_lon_enquo,
      name = !!label_enquo,
      color = !!color_enquo,
      altitude = !!altitude_enquo,
      altitude_scale = !!altitude_scale_enquo,
      stroke = !!stroke_enquo
    )

  globe$x$arcColor <- if(!rlang::quo_is_null(color_enquo)) "color"
  globe$x$arcAltitude <- if(!rlang::quo_is_null(altitude_enquo)) "altitude"
  globe$x$arcAltitudeAutoScale <- if(!rlang::quo_is_null(altitude_scale_enquo)) "altitude_scale"
  globe$x$arcStroke <- if(!rlang::quo_is_null(stroke_enquo)) "stroke"
  globe$x$arcCurveResolution <- curve_resolution
  globe$x$arcCircularResolution <- circular_resolution
  globe$x$arcDashLength <- dash_length
  globe$x$arcDashGap <- dash_gap
  globe$x$arcDashInitialGap <- dash_initial_gap
  globe$x$arcDashAnimateTime <- dash_animate_time
  globe$x$arcsTransitionDuration <- transition
  globe$x$onArcClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  globe$x$onArcRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  globe$x$onArcHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)

  return(globe)
}

#' @export
#' @method globe_arcs globeProxy
globe_arcs.globeProxy <- function(globe, data, start_lat, start_lon, end_lat, end_lon,
  label = NULL, color = NULL, altitude = NULL, altitude_scale = NULL, stroke = NULL,
  curve_resolution = 64L, circular_resolution = 6L, dash_length = 1L, dash_gap = 0L,
  dash_initial_gap = 0L, dash_animate_time = 0L, transition = 1000L,
  on_click = NULL, on_right_click = NULL, on_hover = NULL){

  # check inputs
  assert_that(not_missing(data))
  assert_that(not_missing(start_lat))
  assert_that(not_missing(start_lon))
  assert_that(not_missing(end_lat))
  assert_that(not_missing(end_lon))

  # enquo all things
  start_lat_enquo <- rlang::enquo(start_lat)
  start_lon_enquo <- rlang::enquo(start_lon)
  end_lat_enquo <- rlang::enquo(end_lat)
  end_lon_enquo <- rlang::enquo(end_lon)
  label_enquo <- rlang::enquo(label)
  color_enquo <- rlang::enquo(color)
  altitude_enquo <- rlang::enquo(altitude)
  altitude_scale_enquo <- rlang::enquo(altitude_scale)
  stroke_enquo <- rlang::enquo(stroke)

  msg <- list(id = globe$id)

  msg$arcsData <- data %>% 
    dplyr::select(
      startLat = !!start_lat_enquo,
      startLng = !!start_lon_enquo,
      endLat = !!end_lat_enquo,
      endLng = !!end_lon_enquo,
      name = !!label_enquo,
      color = !!color_enquo,
      altitude = !!altitude_enquo,
      altitude_scale = !!altitude_scale_enquo,
      stroke = !!stroke_enquo
    ) %>% 
    apply(1, as.list)

  msg$arcColor <- if(!rlang::quo_is_null(color_enquo)) "color"
  msg$arcAltitude <- if(!rlang::quo_is_null(altitude_enquo)) "altitude"
  msg$arcAltitudeAutoScale <- if(!rlang::quo_is_null(altitude_scale_enquo)) "altitude_scale"
  msg$arcStroke <- if(!rlang::quo_is_null(stroke_enquo)) "stroke"
  msg$arcCurveResolution <- curve_resolution
  msg$arcCircularResolution <- circular_resolution
  msg$arcDashLength <- dash_length
  msg$arcDashGap <- dash_gap
  msg$arcDashInitialGap <- dash_initial_gap
  msg$arcDashAnimateTime <- dash_animate_time
  msg$arcsTransitionDuration <- transition
  msg$onArcClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  msg$onArcRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  msg$onArcHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)

  globe$session$sendCustomMessage("globe_arcs", msg)

  return(globe)
}
