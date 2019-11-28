#' paths
#' 
#' Add paths to a globe.
#' 
#' @inheritParams globe_bars
#' 
#' @section Coordinates:
#' Valid coordinates.
#' \itemize{
#'   \item{\code{label}}
#'   \item{\code{altitude}}
#'   \item{\code{color}}
#'   \item{\code{resolution}}
#'   \item{\code{stroke}}
#'   \item{\code{dash_length}}
#'   \item{\code{initial_dash_length}}
#'   \item{\code{dash_animate_time}}
#'   \item{\code{transition}}
#' }
#' 
#' @examples 
#' create_globe(cables) %>% 
#'  globe_paths(
#'    coords(
#'      color = color, 
#'      dash_length = .1, 
#'      dash_gap = .008, 
#'      dash_animate_time = 12000
#'    )
#'  )
#' 
#' @export
globe_paths <- function(globe, ..., data = NULL, inherit_coords = TRUE,
  on_click = NULL, on_right_click = NULL, on_hover = NULL) UseMethod("globe_paths")

#' @export
#' @method globe_paths globe
globe_paths.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE,
  on_click = NULL, on_right_click = NULL, on_hover = NULL){

  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))
  assert_that(is_sf(data))

  # extract & process coordinates
  coords <- get_coords(...)
  coords <- combine_coords(globe$x$coords, coords, inherit_coords)

  # create line array
  coordinates <- sf::st_coordinates(data) %>% 
    tibble::as_tibble() %>% 
    purrr::set_names(c("lat", "lon", "L")) %>% 
    tidyr::nest(coords = c("lat", "lon"))

  globe$x$pathsData <- data %>% 
    sf::st_drop_geometry() %>% 
    mutate(L = 1:dplyr::n()) %>% 
    left_join(coordinates, by = "L")

  globe$x$pathLabel <- coords_to_opts(coords, "label")
  globe$x$pathColor <- coords_to_opts(coords, "color")
  globe$x$pathPointAlt <- coords_to_opts(coords, "altitude")
  globe$x$pathResolution <- coords_to_opts(coords, "resolution")
  globe$x$pathStroke <- coords_to_opts(coords, "stroke")
  globe$x$pathDashLength <- coords_to_opts(coords, "dash_length")
  globe$x$pathDashGap <- coords_to_opts(coords, "dash_gap")
  globe$x$pathDashInitialGap <- coords_to_opts(coords, "initial_dash_length")
  globe$x$pathDashAnimateTime <- coords_to_opts(coords, "dash_animate_time")
  globe$x$pathTransitionDuration <- coords_to_opts(coords, "transition")

  globe$x$onPathClick <- if(!is.null(on_click)) on_click
  globe$x$onPathRightClick <- if(!is.null(on_right_click)) on_right_click
  globe$x$onPathHover <- if(!is.null(on_hover)) on_hover

  return(globe)
}
