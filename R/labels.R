#' Points
#' 
#' Add points to a globe.
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
#'   globe_points(
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
#'       globe_points(
#'         coords(lat, long, text = stations),
#'         data = quakes
#'       )
#'   })
#' }
#' 
#' \dontrun{shinyApp(ui, server)}
#' 
#' @export
globe_points <- function(globe, ...) UseMethod("globe_points")

#' @export
#' @method globe_points globe
globe_points.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE, 
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
#' @method globe_points globeProxy
globe_points.globeProxy <- function(globe, ..., data = NULL, 
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
#' @inheritParams globe_points
#' @param func JavaScript function as character vector.
#' 
#' @examples
#' # use data
#' create_globe() %>% 
#'   points_data(quakes) %>% 
#'   points_lat("lat") %>% 
#'   points_lon("long")
#' 
#' @name points_data
#' @export
points_data <- function(globe, data) UseMethod("points_data")

#' @export
#' @method points_data globe
points_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  globe$x$labelsData <- data
  return(globe)
}

#' @export
#' @method points_data globeProxy
points_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  msg <- list(id = globe$id)
  msg$labelsData <- apply(data, 1, as.list)
  globe$session$sendCustomMessage("labels_data", msg)
  return(globe)
} 
