#' Points
#' 
#' Add points to a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame of points to draw.
#' @param lat,lon Bare column names of points coordinates.
#' @param color Bare column name of points color.
#' @param label Bare column name of points label. 
#' Supports plain text or HTML content.
#' @param altitude Bare column name of points
#' defining the cylinder's altitude in terms of globe radius units 
#' (0 = 0 altitude (flat circle), 1 = globe radius).
#' @param radius Bare column name of points for the cylinder's radius, 
#' in angular degrees.
#' @param resolution Getter/setter for the radial geometric resolution 
#' of each cylinder, expressed in how many slice segments to divide the
#'  circumference. Higher values yield smoother cylinders.
#' @param merge Getter/setter for whether to merge all the point meshes 
#' into a single ThreeJS object, for improved rendering performance. 
#' Visually both options are equivalent, setting this option only affects 
#' the internal organization of the ThreeJS objects.
#' @param transition Getter/setter for duration (ms) of the transition 
#' to animate point changes involving geometry modifications. A value of 
#' \code{0} will move the objects immediately to their final position. 
#' New objects are animated by scaling them from the ground up. Only works 
#' if \code{merge} is disabled.
#' @param on_click,on_right_click,on_hover JavaScript functions as strings.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_points(quakes, lat, long, label = stations)
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
#'       globe_points(quakes, lat, long)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' @export
globe_points <- function(globe, data, lat, lon, color = NULL, 
  label = NULL, altitude = NULL, radius = NULL, resolution = 12L, merge = FALSE, 
  transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL) UseMethod("globe_points")

#' @export
#' @method globe_points globe
globe_points.globe <- function(globe, data, lat, lon, color = NULL, 
  label = NULL, altitude = NULL, radius = NULL, resolution = 12L, merge = FALSE, 
  transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){

  # check inputs
  assert_that(not_missing(data))
  assert_that(not_missing(lat))
  assert_that(not_missing(lon))

  # enquo all things
  lat_enquo <- rlang::enquo(lat)
  lon_enquo <- rlang::enquo(lon)
  label_enquo <- rlang::enquo(label)
  color_enquo <- rlang::enquo(color)
  altitude_enquo <- rlang::enquo(altitude)
  radius_enquo <- rlang::enquo(radius)

  # create points array
  globe$x$pointsData <- data %>% 
    dplyr::select(
      lat = !!lat_enquo,
      lng = !!lon_enquo,
      name = !!label_enquo,
      color = !!color_enquo,
      altitude = !!altitude_enquo,
      radius = !!radius_enquo
    )
  
  globe$x$pointColor <- if(!rlang::quo_is_null(color_enquo)) "color"
  globe$x$pointAltitude <- if(!rlang::quo_is_null(altitude_enquo)) "altitude"
  globe$x$pointRadius <- if(!rlang::quo_is_null(radius_enquo)) "radius"
  globe$x$pointResolution <- resolution
  globe$x$pointsMerge <- merge
  globe$x$pointsTransitionDuration <- transition
  globe$x$onPointClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  globe$x$onPointRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  globe$x$onPointHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)
  
  return(globe)
}


#' @export
#' @method globe_points globeProxy
globe_points.globeProxy <- function(globe, data, lat, lon, color = NULL, 
  label = NULL, altitude = NULL, radius = NULL, resolution = 12L, merge = FALSE, 
  transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){

  # check inputs
  assert_that(not_missing(data))
  assert_that(not_missing(lat))
  assert_that(not_missing(lon))

  # enquo all things
  lat_enquo <- enquo(lat)
  lon_enquo <- enquo(lon)
  label_enquo <- enquo(label)
  color_enquo <- enquo(color)
  altitude_enquo <- enquo(altitude)
  radius_enquo <- enquo(radius)

  # create points array
  msg <- list(id = globe$id)
  msg$pointsData <- data %>% 
    select(
      lat = !!lat_enquo,
      lon = !!lon_enquo,
      label = !!label_enquo,
      color = !!color_enquo,
      altitude = !!altitude_enquo,
      radius = !!radius_enquo
    ) %>% 
    apply(1, as.list)

  msg$pointLat <- if(!rlang::quo_is_null(lat_enquo)) "lat"
  msg$pointLng <- if(!rlang::quo_is_null(lon_enquo)) "lon"
  msg$pointColor <- if(!rlang::quo_is_null(color_enquo)) "color"
  msg$pointLabel <- if(!rlang::quo_is_null(label_enquo)) "label"
  msg$pointAltitude <- if(!rlang::quo_is_null(altitude_enquo)) "altitude"
  msg$pointRadius <- if(!rlang::quo_is_null(radius_enquo)) "radius"
  msg$pointResolution <- resolution
  msg$pointsMerge <- merge
  msg$pointsTransitionDuration <- transition
  msg$onPointClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  msg$onPointRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  msg$onPointHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)
  globe$session$sendCustomMessage("globe_points", msg)

  return(globe)
}
