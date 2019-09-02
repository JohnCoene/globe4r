#' Choropleth
#' 
#' Add points to a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame of points to draw.
#' @param country The Bare column name containing either 
#' the ISO2, ISO3, or country name (see \code{match} argument).
#' @param cap_color Bare column name containing the color of 
#' the surface.
#' @param label Bare column name containing label.
#' @param side_color Bare column name containing color of 
#' the sides.
#' @param altitude Altitude of countries where (0 = 0 altitude 
#' (flat polygon), 1 = globe radius).
#' @param transition A value of 0 will size the cone immediatel
#' to their final altitude. New polygons are animated by rising
#' them from the ground up.
#' @param on_click,on_right_click,on_hover JavaScript functions as strings.
#' @param match The type of \code{country} identifier, 
#' \code{auto} attempts to infer the type, \code{iso2}
#' (e.g.: "US"), iso3 (e.g.: "USA"), or the country name.
#' 
#' @examples
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_choropleth(agriland, country_code, cap_color = percent)
#' 
#' @export
globe_choropleth <- function(globe, data, country, cap_color = NULL, side_color = NULL,
  altitude = NULL, label = NULL, transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL, match = c("auto", "iso2", "iso3", "name")) UseMethod("globe_choropleth")

#' @method globe_choropleth globe
#' @export
globe_choropleth.globe <- function(globe, data, country, cap_color = NULL, side_color = NULL, 
  altitude = NULL, label = NULL, transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL, match = c("auto", "iso2", "iso3", "name")){

  match <- match.arg(match)

  # check inputs
  assert_that(not_missing(data))
  assert_that(not_missing(country))

  # enquo all things
  country_enquo <- enquo(country)
  cap_color_enquo <- enquo(cap_color)
  side_color_enquo <- enquo(side_color)
  altitude_enquo <- enquo(altitude)
  label_enquo <- enquo(label)

  # select data
  data <- data %>% 
    select(
      country = !!country_enquo,
      cap_color = !!cap_color_enquo,
      side_color = !!side_color_enquo,
      altitude = !!altitude_enquo,
      label = !!label_enquo
    ) %>% 
    mutate(
      country = as.character(country)
    )

  # match
  if(match == "auto"){
    N <- nchar(data$country[1])
    validate_that(N > 1, msg = "Cannot correctly infer `match`")
    match <- "name"
    if(N < 4)
      match <- paste0("iso", N)
  }
  match <- paste0("country_", match)

  data <- inner_join(data, country_polygons, by = c("country" = match))
  
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
  
  globe$x$polygonsData <- features
  globe$x$polygonCapColor <- if(!rlang::quo_is_null(cap_color_enquo)) "cap_color"
  globe$x$polygonSideColor <- if(!rlang::quo_is_null(side_color_enquo)) "side_color"
  globe$x$polygonAltitude <- if(!rlang::quo_is_null(altitude_enquo)) "altitude"
  globe$x$polygonLabel <- if(!rlang::quo_is_null(label_enquo)) "label"
  globe$x$polygonsTransitionDuration <- transition
  globe$x$onPolygonClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  globe$x$onPolygonRightClick <- if(!is.null(on_click)) htmlwidgets::JS(on_right_click)
  globe$x$onPolygonHover <- if(!is.null(on_click)) htmlwidgets::JS(on_hover)

  return(globe)
}

#' @method globe_choropleth globeProxy
#' @export
globe_choropleth.globeProxy <- function(globe, data, country, cap_color = NULL, side_color = NULL, 
  altitude = NULL, label = NULL, transition = 1000L, on_click = NULL, on_right_click = NULL, 
  on_hover = NULL, match = c("auto", "iso2", "iso3", "name")){

  match <- match.arg(match)

  # check inputs
  assert_that(not_missing(data))
  assert_that(not_missing(country))

  # enquo all things
  country_enquo <- enquo(country)
  cap_color_enquo <- enquo(cap_color)
  side_color_enquo <- enquo(side_color)
  altitude_enquo <- enquo(altitude)
  label_enquo <- enquo(label)

  # select data
  data <- data %>% 
    select(
      country = !!country_enquo,
      cap_color = !!cap_color_enquo,
      side_color = !!side_color_enquo,
      altitude = !!altitude_enquo,
      label = !!label_enquo
    ) %>% 
    mutate(
      country = as.character(country)
    )

  # match
  if(match == "auto"){
    N <- nchar(data$country[1])
    validate_that(N > 1, msg = "Cannot correctly infer `match`")
    match <- "name"
    if(N < 4)
      match <- paste0("iso", N)
  }
  match <- paste0("country_", match)

  data <- inner_join(data, country_polygons, by = c("country" = match))
  
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
  msg$polygonCapColor <- if(!rlang::quo_is_null(cap_color_enquo)) "cap_color"
  msg$polygonSideColor <- if(!rlang::quo_is_null(side_color_enquo)) "side_color"
  msg$polygonAltitude <- if(!rlang::quo_is_null(altitude_enquo)) "altitude"
  msg$polygonLabel <- if(!rlang::quo_is_null(label_enquo)) "label"
  msg$polygonsTransitionDuration <- transition
  msg$onPolygonClick <- if(!is.null(on_click)) htmlwidgets::JS(on_click)
  msg$onPolygonRightClick <- if(!is.null(on_click)) htmlwidgets::JS(on_right_click)
  msg$onPolygonHover <- if(!is.null(on_click)) htmlwidgets::JS(on_hover)

  globe$session$sendCustomMessage("globe_choropleth", msg)

  return(globe)
}

#' Polygons Data
#' 
#' Add polygons data to a globe.
#' 
#' @inheritParams globe_img
#' @param data A data.frame containing arcs data.
#' @param label Column name containing label.
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
