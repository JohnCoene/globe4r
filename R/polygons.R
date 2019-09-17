#' Choropleth
#' 
#' Add points to a globe.
#' 
#' @inheritParams globe_bars
#' @param match The type of \code{country} identifier, 
#' \code{auto} attempts to infer the type, \code{iso2}
#' (e.g.: "US"), iso3 (e.g.: "USA"), or the country name.
#' 
#' @section Coordinates:
#' Valid coordinates.
#' \itemize{
#'   \item{\code{country}},
#'   \item{\code{altitude}}
#'   \item{\code{label}}
#'   \item{\code{side_color}, \code{cap_color}}
#'   \item{\code{transition}}
#' }
#' 
#' @examples
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_choropleth(
#'     data = agriland, 
#'     coords(country = country_code, cap_color = percent)
#'   )
#' 
#' @export
globe_choropleth <- function(globe, ..., data = NULL, inherit_coords = TRUE, 
  on_click = NULL, on_right_click = NULL, on_hover = NULL, 
  match = c("auto", "iso2", "iso3", "name")) UseMethod("globe_choropleth")

#' @method globe_choropleth globe
#' @export
globe_choropleth.globe <- function(globe, ..., data = NULL, inherit_coords = TRUE, 
  on_click = NULL, on_right_click = NULL, on_hover = NULL, 
  match = c("auto", "iso2", "iso3", "name")){

  match <- match.arg(match)

  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  coords <- combine_coords(globe$x$coords, coords, inherit_coords)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  # create points array
  data <- dplyr::select(data, columns)

  # match
  country_column <- coords_to_opts(coords, "country")
  if(match == "auto"){
    N <- nchar(data[[country_column]][1])
    validate_that(N > 1, msg = "Cannot correctly infer `match`")
    match <- "name"
    if(N < 4)
      match <- paste0("iso", N)
  }
  match <- paste0("country_", match)

  # by arg
  byarg <- match
  names(byarg) <- country_column
  data <- inner_join(data, country_polygons, by = byarg)
  
  globe$x$polygonsData <- data
  globe$x$polygonTransitionDuration <- coords_to_opts(coords, "transition")
  globe$x$polygonGeoJsonGeometry <- coords_to_opts(coords, "geometry")
  globe$x$polygonCapColor <- coords_to_opts(coords, "cap_color")
  globe$x$polygonSideColor <- coords_to_opts(coords, "side_color")
  globe$x$polygonAltitude <- coords_to_opts(coords, "altitude")
  globe$x$polygonLabel <- coords_to_opts(coords, "label")
  globe$x$onPolygonClick <- if(!is.null(on_click)) on_click
  globe$x$onPolygonRightClick <- if(!is.null(on_click)) on_right_click
  globe$x$onPolygonHover <- if(!is.null(on_click)) on_hover

  return(globe)
}

#' @method globe_choropleth globeProxy
#' @export
globe_choropleth.globeProxy <- function(globe, ..., data = NULL, inherit_coords = FALSE, 
  on_click = NULL, on_right_click = NULL, on_hover = NULL, 
  match = c("auto", "iso2", "iso3", "name")){

  match <- match.arg(match)

  # check inputs
  data <- .get_data(globe$x$data, data)
  assert_that(has_data(data))

  # extract & process coordinates
  coords <- get_coords(...)
  coords <- combine_coords(globe$x$coords, coords, inherit_coords)
  assert_that(has_coords(coords))
  columns <- coords_to_columns(coords)

  # create points array
  data <- dplyr::select(data, columns)

  # match
  country_column <- coords_to_opts(coords, "country")
  if(match == "auto"){
    N <- nchar(data[[country_column]][1])
    validate_that(N > 1, msg = "Cannot correctly infer `match`")
    match <- "name"
    if(N < 4)
      match <- paste0("iso", N)
  }
  match <- paste0("country_", match)

  # by arg
  byarg <- match
  names(byarg) <- country_column
  data <- inner_join(data, country_polygons, by = byarg)
  
  meta <- data %>% 
    apply(1, as.list) %>% 
    map(function(x){
      x$features <- NULL
      return(x)
    })

  features <- data$features
  features <- map2(features, meta, function(x, y){
    append(x, y)
  })
  
  msg <- list(id = globe$id, polygonsData = features)
  msg$polygonTransitionDuration <- coords_to_opts(coords, "transition")
  msg$polygonGeoJsonGeometry <- coords_to_opts(coords, "geometry")
  msg$polygonCapColor <- coords_to_opts(coords, "cap_color")
  msg$polygonSideColor <- coords_to_opts(coords, "side_color")
  msg$polygonAltitude <- coords_to_opts(coords, "altitude")
  msg$polygonLabel <- coords_to_opts(coords, "label")
  msg$onPolygonClick <- if(!is.null(on_click)) on_click
  msg$onPolygonRightClick <- if(!is.null(on_click)) on_right_click
  msg$onPolygonHover <- if(!is.null(on_click)) on_hover

  globe$session$sendCustomMessage("globe_choropleth", msg)

  return(globe)
}

#' Polygons Raw API
#' 
#' Raw API to polygons layer.
#' 
#' @inheritParams globe_img
#' @param data A data.frame containing arcs data.
#' @param label Name containing label in \code{data} list
#' or JavaScript function.
#' @param geometry Name containing geometry in \code{data} list 
#' or JavaScript function.
#' @param color A JavaScript function or \code{\link{constant}}, or
#' name containing color in \code{data} list.
#' @param altitude Name containing altitude in \code{data} list,
#' a numeric value, or JavaScript function.
#' @param ms Number of milliseconds. A value of 0 will size the 
#' cone immediately to their final altitude. New polygons are 
#' animated by rising them from the ground up.
#' @param func JavaScript function as character vector.
#' 
#' @name polygons_data
#' @export
polygons_data <- function(globe, data) UseMethod("polygons_data")

#' @export
#' @method polygons_data globe
polygons_data.globe <- function(globe, data){
  assert_that(not_missing(data))
  has_features <- "features" %in% names(data) 
  validate_that(has_features, msg = "Do not pass the entire GeoJSON, only the `features`")
  globe$x$polygonsData <- data
  return(globe)
}

#' @export
#' @method polygons_data globeProxy
polygons_data.globeProxy <- function(globe, data){
  assert_that(not_missing(data))
  has_features <- "features" %in% names(data) 
  validate_that(has_features, msg = "Do not pass the entire GeoJSON, only the `features`")
  msg <- list(id = globe$id)
  msg$polygonsData <- data
  globe$session$sendCustomMessage("polygons_data", msg)
  return(globe)
}

#' @rdname polygons_data
#' @export
polygons_label <- function(globe, label = "name") UseMethod("polygons_label")

#' @export
#' @method polygons_label globe
polygons_label.globe <- function(globe, label = "name"){
  globe$x$polygonLabel <- label
  return(globe)
}

#' @export
#' @method polygons_label globeProxy
polygons_label.globeProxy <- function(globe, label = "name"){
  msg <- list(id = globe$id)
  msg$polygonLabel <- label
  globe$session$sendCustomMessage("polygons_label", msg)
  return(globe)
}

#' @rdname polygons_data
#' @export
polygons_geometry <- function(globe, geometry = "geometry") UseMethod("polygons_geometry")

#' @export
#' @method polygons_geometry globe
polygons_geometry.globe <- function(globe, geometry = "geometry"){
  globe$x$polygonGeoJsonGeometry <- geometry
  return(globe)
}

#' @export
#' @method polygons_geometry globeProxy
polygons_geometry.globeProxy <- function(globe, geometry = "geometry"){
  msg <- list(id = globe$id)
  msg$polygonGeoJsonGeometry <- geometry
  globe$session$sendCustomMessage("polygons_geometry", msg)
  return(globe)
}

#' @rdname polygons_data
#' @export
polygons_cap_color <- function(globe, color = constant("ffffaa")) UseMethod("polygons_cap_color")

#' @export
#' @method polygons_cap_color globe
polygons_cap_color.globe <- function(globe, color = constant("ffffaa")){
  globe$x$polygonCapColor <- color
  return(globe)
}

#' @export
#' @method polygons_cap_color globeProxy
polygons_cap_color.globeProxy <- function(globe, color = constant("ffffaa")){
  msg <- list(id = globe$id)
  msg$polygonCapColor <- color
  globe$session$sendCustomMessage("polygons_cap_color", msg)
  return(globe)
}

#' @rdname polygons_data
#' @export
polygons_side_color <- function(globe, color = constant("ffffaa")) UseMethod("polygons_side_color")

#' @export
#' @method polygons_side_color globe
polygons_side_color.globe <- function(globe, color = constant("ffffaa")){
  globe$x$polygonCapColor <- color
  return(globe)
}

#' @export
#' @method polygons_side_color globeProxy
polygons_side_color.globeProxy <- function(globe, color = constant("ffffaa")){
  msg <- list(id = globe$id)
  msg$polygonCapColor <- color
  globe$session$sendCustomMessage("polygons_side_color", msg)
  return(globe)
}

#' @rdname polygons_data
#' @export
polygons_altitude <- function(globe, altitude = .1) UseMethod("polygons_altitude")

#' @export
#' @method polygons_altitude globe
polygons_altitude.globe <- function(globe, altitude = .1){
  globe$x$polygonAltitude <- altitude
  return(globe)
}

#' @export
#' @method polygons_altitude globeProxy
polygons_altitude.globeProxy <- function(globe, altitude = .1){
  msg <- list(id = globe$id)
  msg$polygonAltitude <- altitude
  globe$session$sendCustomMessage("polygons_altitude", msg)
  return(globe)
}

#' @rdname polygons_data
#' @export
polygons_transition <- function(globe, ms = 1000L) UseMethod("polygons_transition")

#' @export
#' @method polygons_transition globe
polygons_transition.globe <- function(globe, ms = 1000L){
  globe$x$polygonsTransitionDuration <- ms
  return(globe)
}

#' @export
#' @method polygons_transition globeProxy
polygons_transition.globeProxy <- function(globe, ms = 1000L){
  msg <- list(id = globe$id)
  msg$polygonsTransitionDuration <- ms
  globe$session$sendCustomMessage("polygons_transition", msg)
  return(globe)
}


#' @rdname polygons_data
#' @export
polygons_on_click <- function(globe, func) UseMethod("polygons_on_click")

#' @export
#' @method polygons_on_click globe
polygons_on_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method polygons_on_click globeProxy
polygons_on_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("polygons_on_click", msg)
  return(globe)
} 

#' @rdname polygons_data
#' @export
polygons_on_right_click <- function(globe, func) UseMethod("polygons_on_right_click")

#' @export
#' @method polygons_on_right_click globe
polygons_on_right_click.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointRightClick <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method polygons_on_right_click globeProxy
polygons_on_right_click.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointRightClick <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("polygons_on_right_click", msg)
  return(globe)
} 

#' @rdname polygons_data
#' @export
polygons_on_hover <- function(globe, func) UseMethod("polygons_on_hover")

#' @export
#' @method polygons_on_hover globe
polygons_on_hover.globe <- function(globe, func){
  assert_that(not_missing(func))
  globe$x$onPointHover <- htmlwidgets::JS(func)
  return(globe)
}

#' @export
#' @method polygons_on_hover globeProxy
polygons_on_hover.globeProxy <- function(globe, func){
  assert_that(not_missing(func))
  msg <- list(id = globe$id)
  msg$onPointHover <- htmlwidgets::JS(func)
  globe$session$sendCustomMessage("polygons_on_hover", msg)
  return(globe)
} 