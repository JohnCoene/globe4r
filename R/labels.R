#' Labels
#' 
#' Add labels to a globe.
#' 
#' @inheritParams globe_bars
#' 
#' @section Coordinates:
#' Valid coordinates.
#' \itemize{
#'   \item{\code{lat}, \code{lon}},
#'   \item{\code{altitude}}
#'   \item{\code{color}}
#'   \item{\code{label}}
#'   \item{\code{resolution}}
#'   \item{\code{transition}}
#'   \item{\code{size}}
#'   \item{\code{text}}
#'   \item{\code{rotation}}
#'   \item{\code{include_dot}}
#'   \item{\code{dot_radius}}
#'   \item{\code{dot_orientation}}
#'   \item{\code{type_face}}
#' }
#' 
#' @examples
#' quakes %>% 
#'   create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_labels(
#'     coords(lat, long, text = stations)
#'   )
#' 
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   actionButton("add", "add quakes labels"),
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
#'       globe_labels(
#'         coords(lat, long, text = stations),
#'         data = quakes
#'       )
#'   })
#' }
#' 
#' if(interactive()) shinyApp(ui, server)
#' 
#' @export
globe_labels <- function(globe, ...) UseMethod("globe_labels")

#' @export
#' @method globe_labels globe
globe_labels.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE, 
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
  globe$x$labelsData <- dplyr::select(data, columns)

  # force character conversion
  if(!is.null(coords_to_opts(coords, "text")))
    if(coords_to_opts(coords, "text") %in% names(globe$x$labelsData))
      globe$x$labelsData[[coords_to_opts(coords, "text")]] <- as.character(globe$x$labelsData[[coords_to_opts(coords, "text")]])

  globe$x$labelLat <- coords_to_opts(coords, "lat")
  globe$x$labelLng <- coords_to_opts(coords, "lon")
  globe$x$labelColor <- coords_to_opts(coords, "color")
  globe$x$labelAltitude <- coords_to_opts(coords, "altitude")
  globe$x$labelSize <- coords_to_opts(coords, "size")
  globe$x$labelText <- coords_to_opts(coords, "text")
  globe$x$labelLabel <- coords_to_opts(coords, "label")
  globe$x$labelRotation <- coords_to_opts(coords, "rotation")
  globe$x$labelIncludeDot <- coords_to_opts(coords, "include_dot")
  globe$x$labelDotRadius <- coords_to_opts(coords, "dot_radius")
  globe$x$labelDotOrientation <- coords_to_opts(coords, "dot_orientation")
  globe$x$labelResolution <- coords_to_opts(coords, "resolution")
  globe$x$labelTypeFace <- coords_to_opts(coords, "type_face")
  globe$x$labelTransitionDuration <- coords_to_opts(coords, "transition")
  globe$x$onLabelClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  globe$x$onLabelRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  globe$x$onLabelHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)

  return(globe)
}

#' @export
#' @method globe_labels globeProxy
globe_labels.globeProxy <- function(globe, ..., data = NULL, 
  on_click = NULL, on_right_click = NULL, on_hover = NULL){
  
    # check inputs
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  msg <- list(id = globe$id)
  data <- dplyr::select(data, columns)

  # force character conversion
  if(!is.null(coords_to_opts(coords, "text")))
    if(coords_to_opts(coords, "text") %in% names(data))
      data[[coords_to_opts(coords, "text")]] <- as.character(data[[coords_to_opts(coords, "text")]])

  msg$labelsData <- apply(data, 1, as.list)

  msg$labelLat <- coords_to_opts(coords, "lat")
  msg$labelLng <- coords_to_opts(coords, "lon")
  msg$labelColor <- coords_to_opts(coords, "color")
  msg$labelAltitude <- coords_to_opts(coords, "altitude")
  msg$labelSize <- coords_to_opts(coords, "size")
  msg$labelText <- coords_to_opts(coords, "text")
  msg$labelLabel <- coords_to_opts(coords, "label")
  msg$labelRotation <- coords_to_opts(coords, "rotation")
  msg$labelIncludeDot <- coords_to_opts(coords, "include_dot")
  msg$labelDotRadius <- coords_to_opts(coords, "dot_radius")
  msg$labelDotOrientation <- coords_to_opts(coords, "dot_orientation")
  msg$labelResolution <- coords_to_opts(coords, "resolution")
  msg$labelTypeFace <- coords_to_opts(coords, "type_face")
  msg$labelTransitionDuration <- coords_to_opts(coords, "transition")
  msg$onLabelClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  msg$onLabelRightClick <- if(!is.null(on_right_click)) htmlwidgets::JS(on_right_click)
  msg$onLabelHover <- if(!is.null(on_hover)) htmlwidgets::JS(on_hover)

  globe$session$sendCustomMessage("globe_labels", msg)

  return(globe)
}

#' Points Functional API
#' 
#' Functional API to add and customise points on globe.
#' 
#' @inheritParams bars_data
#' @param typeface Font of \code{text}. Supports any typeface font generated by 
#' \href{http://gero3.github.io/facetype.js/}{Facetype.js}.
#' @param size Column name or constant of the size of the label text height, in angular degrees. 
#' @param rotation Column name or constant of the label rotation in degrees. 
#' The rotation is performed clockwise along the axis of its latitude parallel plane.
#' @param text Column name or constant of text.
#' @param include Boolean or colum name indicating whether to show the dot.
#' @param orientation Orientation of label, \code{right}, \code{top}, or \code{bottom}.
#' 
#' @examples
#' # use data
#' create_globe() %>% 
#'   labels_data(quakes) %>% 
#'   labels_lat("lat") %>% 
#'   labels_lon("long")
#' 
#' @name labels_data
#' @export
labels_data <- function(globe, data) UseMethod("labels_data")

#' @export
#' @method labels_data globe
labels_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  globe$x$labelsData <- data
  return(globe)
}

#' @export
#' @method labels_data globeProxy
labels_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  msg <- list(id = globe$id)
  msg$labelsData <- apply(data, 1, as.list)
  globe$session$sendCustomMessage("labels_data", msg)
  return(globe)
} 


#' @rdname labels_data
#' @export
labels_lat <- function(globe, lat = "lat") UseMethod("labels_lat")

#' @export
#' @method labels_lat globe
labels_lat.globe <- function(globe, lat = "lat"){
  globe$x$labelLat <- lat
  return(globe)
}

#' @export
#' @method labels_data globeProxy
labels_lat.globeProxy <- function(globe, lat = "lat"){
  msg <- list(id = globe$id)
  msg$labelLat <- lat
  globe$session$sendCustomMessage("labels_lat", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_lon <- function(globe, lon = "lng") UseMethod("labels_lon")

#' @export
#' @method labels_lon globe
labels_lon.globe <- function(globe, lon = "lng"){
  globe$x$labelLng <- lon
  return(globe)
}

#' @export
#' @method labels_lon globeProxy
labels_lon.globeProxy <- function(globe, lon = "lng"){
  msg <- list(id = globe$id)
  msg$labelLng <- lon
  globe$session$sendCustomMessage("labels_lon", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_text <- function(globe, text = "text") UseMethod("labels_text")

#' @export
#' @method labels_text globe
labels_text.globe <- function(globe, text = "text"){
  globe$x$labelText <- text
  return(globe)
}

#' @export
#' @method labels_text globeProxy
labels_text.globeProxy <- function(globe, text = "text"){
  msg <- list(id = globe$id)
  msg$labelText <- text
  globe$session$sendCustomMessage("labels_text", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_color <- function(globe, color = constant("#ffffaa")) UseMethod("labels_color")

#' @export
#' @method labels_color globe
labels_color.globe <- function(globe, color = constant("#ffffaa")){
  globe$x$labelColor <- color
  return(globe)
}

#' @export
#' @method labels_color globeProxy
labels_color.globeProxy <- function(globe, color = constant("#ffffaa")){
  msg <- list(id = globe$id)
  msg$labelColor <- color
  globe$session$sendCustomMessage("labels_color", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_altitude <- function(globe, altitude = 0L) UseMethod("labels_altitude")

#' @export
#' @method labels_altitude globe
labels_altitude.globe <- function(globe, altitude = 0L){
  globe$x$pointAltitude <- altitude
  return(globe)
}

#' @export
#' @method labels_altitude globeProxy
labels_altitude.globeProxy <- function(globe, altitude = 0L){
  msg <- list(id = globe$id)
  msg$pointAltitude <- altitude
  globe$session$sendCustomMessage("labels_altitude", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_size <- function(globe, size = .5) UseMethod("labels_size")

#' @export
#' @method labels_size globe
labels_size.globe <- function(globe, size = .5){
  globe$x$pointSize <- size
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_size globeProxy
labels_size.globeProxy <- function(globe, size = .5){
  msg <- list(id = globe$id)
  msg$pointSize <- size
  globe$session$sendCustomMessage("labels_size", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_typeface <- function(globe, typeface) UseMethod("labels_typeface")

#' @export
#' @method labels_typeface globe
labels_typeface.globe <- function(globe, typeface){
  globe$x$labelTypeFace <- typeface
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_typeface globeProxy
labels_typeface.globeProxy <- function(globe, typeface){
  msg <- list(id = globe$id)
  msg$labelTypeFace <- typeface
  globe$session$sendCustomMessage("labels_typeface", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_rotation <- function(globe, rotation = 0L) UseMethod("labels_rotation")

#' @export
#' @method labels_rotation globe
labels_rotation.globe <- function(globe, rotation = 0L){
  globe$x$labelRotation <- rotation
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_rotation globeProxy
labels_rotation.globeProxy <- function(globe, rotation = 0L){
  msg <- list(id = globe$id)
  msg$labelRotation <- rotation
  globe$session$sendCustomMessage("labels_rotation", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_resolution <- function(globe, resolution = 3L) UseMethod("labels_resolution")

#' @export
#' @method labels_resolution globe
labels_resolution.globe <- function(globe, resolution = 3L){
  globe$x$labelResolution <- resolution
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_resolution globeProxy
labels_resolution.globeProxy <- function(globe, resolution = 3L){
  msg <- list(id = globe$id)
  msg$labelResolution <- resolution
  globe$session$sendCustomMessage("labels_resolution", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_include_dot <- function(globe, include = FALSE) UseMethod("labels_include_dot")

#' @export
#' @method labels_include_dot globe
labels_include_dot.globe <- function(globe, include = FALSE){
  globe$x$labelIncludeDot <- include
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_include_dot globeProxy
labels_include_dot.globeProxy <- function(globe, include = FALSE){
  msg <- list(id = globe$id)
  msg$labelIncludeDot <- include
  globe$session$sendCustomMessage("labels_include_dot", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_dot_radius <- function(globe, radius = .1) UseMethod("labels_dot_radius")

#' @export
#' @method labels_dot_radius globe
labels_dot_radius.globe <- function(globe, radius = .1){
  globe$x$labelDotRadius <- radius
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_dot_radius globeProxy
labels_dot_radius.globeProxy <- function(globe, radius = .1){
  msg <- list(id = globe$id)
  msg$labelDotRadius <- radius
  globe$session$sendCustomMessage("labels_dot_radius", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_dot_orientation <- function(globe, orientation = constant("bottom")) UseMethod("labels_dot_orientation")

#' @export
#' @method labels_dot_orientation globe
labels_dot_orientation.globe <- function(globe, orientation = constant("bottom")){
  globe$x$labelDotOrientation <- orientation
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_dot_orientation globeProxy
labels_dot_orientation.globeProxy <- function(globe, orientation = constant("bottom")){
  msg <- list(id = globe$id)
  msg$labelDotOrientation <- orientation
  globe$session$sendCustomMessage("labels_dot_orientation", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_transition <- function(globe, transition = 1000L) UseMethod("labels_transition")

#' @export
#' @method labels_transition globe
labels_transition.globe <- function(globe, transition = 1000L){
  globe$x$labelsTransitionDuration <- transition
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_transition globeProxy
labels_transition.globeProxy <- function(globe, transition = 1000L){
  msg <- list(id = globe$id)
  msg$labelsTransitionDuration <- transition
  globe$session$sendCustomMessage("labels_transition", msg)
  return(globe)
} 

#' @rdname labels_data
#' @export
labels_on_click <- function(globe, func) UseMethod("labels_on_click")

#' @export
#' @method labels_on_click globe
labels_on_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onLabelClick <- func
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_on_click globeProxy
labels_on_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onLabelClick <- func
  globe$session$sendCustomMessage("labels_on_click", msg)
  return(globe)
}

#' @rdname labels_data
#' @export
labels_on_right_click <- function(globe, func) UseMethod("labels_on_right_click")

#' @export
#' @method labels_on_right_click globe
labels_on_right_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onLabelRightClick <- func
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_on_right_click globeProxy
labels_on_right_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onLabelRightClick <- func
  globe$session$sendCustomMessage("labels_on_right_click", msg)
  return(globe)
}

#' @rdname labels_data
#' @export
labels_on_hover <- function(globe, func) UseMethod("labels_on_hover")

#' @export
#' @method labels_on_hover globe
labels_on_hover.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onLabelHover <- func
  return(globe)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             
}

#' @export
#' @method labels_on_hover globeProxy
labels_on_hover.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onLabelHover <- func
  globe$session$sendCustomMessage("labels_on_hover", msg)
  return(globe)
}