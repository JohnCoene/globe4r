#' Constants
#' 
#' Wraps a constant in a JavaScript function as most
#' underlying JavaScript functions expect it.
#' 
#' @param x A constant.
#' 
#' @examples
#' # create plot
#' g <- create_globe() %>% 
#'   globe_img_url() %>% 
#'   points_data(quakes) %>% 
#'   points_lat("lat") %>% 
#'   points_lon("long")
#' 
#' # passing a constant straight does not work
#' \dontrun{points_color(g, "red")}
#' 
#' # using `constant` it works
#' RED <- constant("red")
#' points_color(g, RED)
#' 
#' @export
constant <- function(x){
  htmlwidgets::JS(paste0("() => '", x, "'"))
}

#' Scale Colour
#' 
#' Pass a numeric value as color and use this function 
#' 
#' @inheritParams globe_img
#' @param palette A vector of colors.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_points(quakes, lat, long, color = mag) %>% 
#'   scale_points_color()
#' 
#' @name scaling_color
#' @export
scale_points_color <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")) UseMethod("scale_points_color") 

#' @export
#' @method scale_points_color globe
scale_points_color.globe <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")){
  assert_that(length(globe$x$pointsData$color) >= 1, msg = "No color specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$pointsData$color <- scale(globe$x$pointsData$color)

  return(globe)
}

#' @rdname scaling_color
#' @export
scale_arc_color <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")) UseMethod("scale_arc_color") 

#' @export
#' @method scale_arc_color globe
scale_arc_color.globe <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")){
  assert_that(length(globe$x$arcData$color) >= 1, msg = "No color specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$arcData$color <- scale(globe$x$arcData$color)

  return(globe)
}

#' @rdname scaling_color
#' @export
scale_choropleth_cap_color <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")) UseMethod("scale_choropleth_cap_color") 

#' @export
#' @method scale_choropleth_cap_color globe
scale_choropleth_cap_color.globe <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")){
  assert_that(length(globe$x$polygonsData[[1]]$cap_color) >= 1, msg = "No color specified.")
  colors <- purrr::map(globe$x$polygonsData, "cap_color") %>% unlist()

  scale <- scales::col_numeric(palette, NULL)
  scaled_colors <- scale(colors)
  globe$x$polygonsData <- purrr::map2(globe$x$polygonsData, scaled_colors, function(x, y){
    x$cap_color <- y
    return(x)
  })

  return(globe)
}

#' @rdname scaling_color
#' @export
scale_choropleth_side_color <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")) UseMethod("scale_choropleth_side_color") 

#' @export
#' @method scale_choropleth_side_color globe
scale_choropleth_side_color.globe <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")){
  assert_that(length(globe$x$polygonsData[[1]]$side_color) >= 1, msg = "No color specified.")
  colors <- purrr::map(globe$x$polygonsData, "side_color") %>% unlist()

  scale <- scales::col_numeric(palette, NULL)
  scaled_colors <- scale(colors)
  globe$x$polygonsData <- purrr::map2(globe$x$polygonsData, scaled_colors, function(x, y){
    x$side_color <- y
    return(x)
  })

  return(globe)
}

#' @name scaling_color
#' @export
scale_arcs_color <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")) UseMethod("scale_arcs_color") 

#' @export
#' @method scale_arcs_color globe
scale_arcs_color.globe <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")){
  assert_that(length(globe$x$arcData$color) >= 1, msg = "No color specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$arcData$color <- scale(globe$x$arcData$color)

  return(globe)
}

#' @name scaling_color
#' @export
scale_label_color <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")) UseMethod("scale_label_color") 

#' @export
#' @method scale_label_color globe
scale_label_color.globe <- function(globe, palette = c("#edf8b1", "#7fcdbb", "#2c7fb8")){
  assert_that(length(globe$x$labelData$color) >= 1, msg = "No color specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$labelData$color <- scale(globe$x$labelData$color)

  return(globe)
}

#' Scale Altitude
#' 
#' Rescale altitude to a more appropriate range, where 0 if flat
#' and 1 is the globe radius.
#' 
#' @inheritParams globe_img
#' @param min,max Target minimum and maximum values of altitude.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_points(quakes, lat, long, altitude = mag) %>% 
#'   scale_points_altitude()
#' 
#' @name scaling_altitude
#' @export
scale_points_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_points_altitude")

#' @export
#' @method scale_points_altitude globe 
scale_points_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$pointsData$altitude) >= 1, msg = "No altitude specified.")
  globe$x$pointsData$altitude <- scales::rescale(globe$x$pointsData$altitude, to = c(min, max))
  return(globe)
}

#' @rdname scaling_altitude
#' @export
scale_arcs_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_arcs_altitude")

#' @export
#' @method scale_arcs_altitude globe 
scale_arcs_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$arcsData$altitude) >= 1, msg = "No altitude specified.")
  globe$x$arcsData$altitude <- scales::rescale(globe$x$arcsData$altitude, to = c(min, max))
  return(globe)
}

#' @rdname scaling_altitude
#' @export
scale_labels_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_labels_altitude")

#' @export
#' @method scale_labels_altitude globe 
scale_labels_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$labelsData$altitude) >= 1, msg = "No altitude specified.")
  globe$x$labelsData$altitude <- scales::rescale(globe$x$labelsData$altitude, to = c(min, max))
  return(globe)
}


#' @rdname scaling_altitude
#' @export
scale_choropleth_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_choropleth_altitude") 

#' @export
#' @method scale_choropleth_altitude globe
scale_choropleth_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$polygonsData[[1]]$altitude) >= 1, msg = "No altitude specified.")
  altitude <- purrr::map(globe$x$polygonsData, "altitude") %>% unlist()

  altitude <- scales::rescale(globe$x$arcsData$altitude, to = c(min, max))
  globe$x$polygonsData <- purrr::map2(globe$x$polygonsData, altitude, function(x, y){
    x$altitude <- y
    return(x)
  })

  return(globe)
}

#' Scale Arc Stroke
#' 
#' Rescale arc stroke to a more appropriate pixel values.
#' 
#' @inheritParams globe_img
#' @param min,max Target minimum and maximum values of pixels.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_arcs(
#'     usflights, start_lat, start_lon, end_lat, end_lon,
#'     stroke = cnt
#'   ) %>% 
#'   scale_arc_stroke()
#' 
#' @name scaling_stroke
#' @export
scale_arc_stroke <- function(globe, min = .1, max = 1) UseMethod("scale_arc_stroke")

#' @export
#' @method scale_arc_stroke globe 
scale_arc_stroke.globe <- function(globe, min = .1, max = 1){
  assert_that(length(globe$x$arcsData$stroke) >= 1, msg = "No altitude specified.")
  globe$x$arcsData$stroke <- scales::rescale(globe$x$arcsData$stroke, to = c(min, max))
  return(globe)
}