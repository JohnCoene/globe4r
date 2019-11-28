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
#' }
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

  globe$x$onPathClick <- if(!is.null(on_click)) on_click
  globe$x$onPathRightClick <- if(!is.null(on_right_click)) on_right_click
  globe$x$onPathHover <- if(!is.null(on_hover)) on_hover

  return(globe)
}
