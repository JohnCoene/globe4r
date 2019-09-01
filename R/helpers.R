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