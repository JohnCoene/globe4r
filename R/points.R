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
#'       globe_points(quakes, lat, long) %>% 
#'       globe_pov(-21, 179)
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' @export
globe_points <- function(globe, data, lat = NULL, lon = NULL, color = NULL, 
  label = NULL, altitude = NULL, radius = NULL, resolution = 12L, merge = FALSE, 
  transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL) UseMethod("globe_points")

#' @export
#' @method globe_points globe
globe_points.globe <- function(globe, data, lat = NULL, lon = NULL, color = NULL, 
  label = NULL, altitude = NULL, radius = NULL, resolution = 12L, merge = FALSE, 
  transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){

  # check inputs
  assert_that(not_missing(data))

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
globe_points.globeProxy <- function(globe, data, lat = NULL, lon = NULL, color = NULL, 
  label = NULL, altitude = NULL, radius = NULL, resolution = 12L, merge = FALSE, 
  transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL){

  # check inputs
  assert_that(not_missing(data))

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
points_lat <- function(globe, lat) UseMethod("points_lat")

#' @export
#' @method points_lat globe
points_lat.globe <- function(globe, lat){
  assert_that(not_missing(lat))
  globe$x$pointLat <- lat
  return(globe)
}

#' @export
#' @method points_data globeProxy
points_lat.globeProxy <- function(globe, lat){
  assert_that(not_missing(lat))
  msg <- list(id = globe$id)
  msg$pointLat <- lat
  globe$session$sendCustomMessage("points_lat", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_lon <- function(globe, lon) UseMethod("points_lon")

#' @export
#' @method points_lon globe
points_lon.globe <- function(globe, lon){
  assert_that(not_missing(lon))
  globe$x$pointLng <- lon
  return(globe)
}

#' @export
#' @method points_lon globeProxy
points_lon.globeProxy <- function(globe, lon){
  assert_that(not_missing(lon))
  msg <- list(id = globe$id)
  msg$pointLng <- lon
  globe$session$sendCustomMessage("points_lon", msg)
  return(globe)
} 

#' @rdname points_data
#' @export
points_color <- function(globe, color) UseMethod("points_color")

#' @export
#' @method points_color globe
points_color.globe <- function(globe, color){
  assert_that(not_missing(color))
  globe$x$pointColor <- color
  return(globe)
}

#' @export
#' @method points_color globeProxy
points_color.globeProxy <- function(globe, color){
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