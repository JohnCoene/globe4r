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
#'   bars_data(quakes) %>% 
#'   bars_lat("lat") %>% 
#'   bars_lon("long")
#' 
#' # passing a constant straight does not work
#' \dontrun{bars_color(g, "red")}
#' 
#' # using `constant` it works
#' RED <- constant("red")
#' bars_color(g, RED)
#' 
#' @export
constant <- function(x){
  if(is.logical(x))
    x <- tolower(x)
  htmlwidgets::JS(paste0("() => '", x, "'"))
}

#' Scale Colour
#' 
#' Pass a numeric value as color and use this function 
#' 
#' @inheritParams globe_img
#' @param palette A vector of colors.
#' @param min,max Domain to scale color with \href{https://gka.github.io/chroma.js/}{Chroma.js}.
#' @param ... \code{var}, the variable used to scale color as well as \code{min} and \code{max} arguments.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_bars(coords(lat, long, color = mag), data = quakes) %>% 
#'   scale_bars_color()
#' 
#' @name scaling_color
#' @export
scale_bars_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), ...) UseMethod("scale_bars_color") 

#' @export
#' @method scale_bars_color globe
scale_bars_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), ...){
  assert_that(length(globe$x$pointColor) >= 1, msg = "No `color` specified.")

  colors <- globe$x$pointsData[[globe$x$pointColor]]
  scale <- scales::col_numeric(palette, range(colors))
  globe$x$pointsData$GLOBE4pointColor <- scale(colors)
  globe$x$pointColor <- "GLOBE4pointColor"

  return(globe)
}

#' @export
#' @method scale_bars_color globeProxy
scale_bars_color.globeProxy <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), ..., var, min = 0, max = 1){
  assert_that(not_missing(var))
  scl <- color_scale(var, palette, min, max)
  msg <- list(id = globe$id, pointColor = scl)
  globe$session$sendCustomMessage("scale_bars_color", msg)
  return(globe)
}

#' @rdname scaling_color
#' @export
scale_arc_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")) UseMethod("scale_arc_color") 

#' @export
#' @method scale_arc_color globe
scale_arc_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")){
  assert_that(length(globe$x$arcColor) >= 1, msg = "No `color` specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$arcsData$GLOBE4RarcColor <- scale(globe$x$arcsData[[globe$x$arcColor]])
  globe$x$arcColor <- "GLOBE4RarcColor"

  return(globe)
}

#' @rdname scaling_color
#' @export
scale_labels_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")) UseMethod("scale_labels_color") 

#' @export
#' @method scale_labels_color globe
scale_labels_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")){
  assert_that(length(globe$x$labelColor) >= 1, msg = "No `color` specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$labelsData$GLOBE4RlabelColor <- scale(globe$x$labelsData[[globe$x$labelColor]])
  globe$x$labelColor <- "GLOBE4RlabelColor"

  return(globe)
}

#' @rdname scaling_color
#' @export
scale_choropleth_cap_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")) UseMethod("scale_choropleth_cap_color") 

#' @export
#' @method scale_choropleth_cap_color globe
scale_choropleth_cap_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")){
  assert_that(length(globe$x$polygonCapColor) >= 1, msg = "No `cap_color` specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$polygonsData$GLOBE4RcapColor <- scale(globe$x$polygonsData[[globe$x$polygonCapColor]])
  globe$x$polygonCapColor <- "GLOBE4RcapColor"

  return(globe)
}

#' @rdname scaling_color
#' @export
scale_hex_cap_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), min = NULL, max = NULL) UseMethod("scale_hex_cap_color") 

#' @export
#' @method scale_hex_cap_color globe
scale_hex_cap_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), min = NULL, max = NULL){
  palette <- jsonlite::toJSON(palette)
  rng <- NULL
  if(is.null(min)){
    validate_that(
      is.character(globe$x$hexBinPointWeight),
      msg = "Cannot scale `cap_color` when `weight` is set to an integer in `coords`: only works if hexagons are pre-computed."
    )
    rng <- range(globe$x$hexBinPointsData[[globe$x$hexBinPointWeight]])
  } else 
    rng <- c(min, max)

  assert_that(!is.null(rng), msg = "Cannot determine scale, pass `min` and `max`.")
  
  domain <- jsonlite::toJSON(rng)
  color_scale <- htmlwidgets::JS(
    "function(d){
      fnc = chroma.scale(", palette, ").domain(", domain, ").mode('lab');
      return fnc(d.sumWeight).hex();
    }"
  )  

  hex_cap_color(globe, color_scale)
}

#' @rdname scaling_color
#' @export
scale_hex_side_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), min = NULL, max = NULL) UseMethod("scale_hex_side_color") 

#' @export
#' @method scale_hex_side_color globe
scale_hex_side_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1"), min = NULL, max = NULL){
  palette <- jsonlite::toJSON(palette)
  rng <- NULL
  if(is.null(min)){
    validate_that(
      is.character(globe$x$hexBinPointWeight),
      msg = "Cannot scale `cap_color` when `weight` is set to an integer in `coords`: only works if hexagons are pre-computed."
    )
    rng <- range(globe$x$hexBinPointsData[[globe$x$hexBinPointWeight]])
  } else 
    rng <- c(min, max)

  assert_that(!is.null(rng), msg = "Cannot determine scale, pass `min` and `max`.")
  
  domain <- jsonlite::toJSON(rng)
  color_scale <- htmlwidgets::JS(
    "function(d){
      fnc = chroma.scale(", palette, ").domain(", domain, ").mode('lab');
      return fnc(d.sumWeight).hex();
    }"
  )  

  hex_side_color(globe, color_scale)
}

#' @rdname scaling_color
#' @export
scale_choropleth_side_color <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")) UseMethod("scale_choropleth_side_color") 

#' @export
#' @method scale_choropleth_side_color globe
scale_choropleth_side_color.globe <- function(globe, palette = c("#2c7fb8", "#7fcdbb", "#edf8b1")){
  assert_that(length(globe$x$polygonSideColor) >= 1, msg = "No `side_color` specified.")

  scale <- scales::col_numeric(palette, NULL)
  globe$x$polygonsData$GLOBE4RpolygonColor <- scale(globe$x$polygonsData[[globe$x$polygonSideColor]])
  globe$x$polygonSideColor <- "GLOBE4RpolygonColor"

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
#'   globe_bars(data = quakes, coords(lat, long, altitude = mag)) %>% 
#'   scale_bars_altitude()
#' 
#' @name scaling_altitude
#' @export
scale_bars_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_bars_altitude")

#' @export
#' @method scale_bars_altitude globe 
scale_bars_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$pointAltitude) >= 1, msg = "No `altitude` specified.")
  globe$x$pointsData$GLOBE4RpointAltitude <- scales::rescale(globe$x$pointsData[[globe$x$pointAltitude]], to = c(min, max))
  globe$x$pointAltitude <- "GLOBE4RpointAltitude"
  return(globe)
}

#' @rdname scaling_altitude
#' @export
scale_arcs_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_arcs_altitude")

#' @export
#' @method scale_arcs_altitude globe 
scale_arcs_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$arcAltitude) >= 1, msg = "No `altitude` specified.")
  globe$x$arcsData$GLOBE4RarcAltitude <- scales::rescale(globe$x$arcsData[[globe$x$arcAltitude]], to = c(min, max))
  globe$x$arcAltitude <- "GLOBE4RarcAltitude"
  return(globe)
}

#' @rdname scaling_altitude
#' @export
scale_labels_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_labels_altitude")

#' @export
#' @method scale_labels_altitude globe 
scale_labels_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$labelsData$altitude) >= 1, msg = "No `altitude` specified.")
  globe$x$labelsData[[globe$x$labelAltitude]] <- scales::rescale(globe$x$labelsData[[globe$x$labelAltitude]], to = c(min, max))
  return(globe)
}


#' @rdname scaling_altitude
#' @export
scale_choropleth_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_choropleth_altitude") 

#' @export
#' @method scale_choropleth_altitude globe
scale_choropleth_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$polygonAltitude) >= 1, msg = "No `altitude` specified.")

  globe$x$polygonsData$GLOBE4RpolygonAltitude <- scales::rescale(globe$x$polygonsData[[globe$x$polygonAltitude]], to = c(min, max))
  globe$x$polygonAltitude <- "GLOBE4RpolygonAltitude"

  return(globe)
}

#' @rdname scaling_altitude
#' @export
scale_hex_altitude <- function(globe, min = 0, max = .5) UseMethod("scale_hex_altitude") 

#' @export
#' @method scale_hex_altitude globe
scale_hex_altitude.globe <- function(globe, min = 0, max = .5){
  assert_that(length(globe$x$hexAltitude) >= 1, msg = "No `altitude` specified.")

  globe$x$hexBinPointsData$GLOBE4RhexAltitude <- scales::rescale(globe$x$hexBinPointsData[[globe$x$hexAltitude]], to = c(min, max))
  globe$x$hexAltitude <- "GLOBE4RhexAltitude"

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
#'     data = usflights, 
#'     coords (
#'       start_lat, start_lon, end_lat, end_lon,
#'       stroke = cnt
#'     )
#'   ) %>% 
#'   scale_arc_stroke()
#' 
#' @name scaling_stroke
#' @export
scale_arc_stroke <- function(globe, min = .1, max = 1) UseMethod("scale_arc_stroke")

#' @export
#' @method scale_arc_stroke globe 
scale_arc_stroke.globe <- function(globe, min = .1, max = 1){
  assert_that(length(globe$x$arcStroke) >= 1, msg = "No `stroke` specified.")
  globe$x$arcsData$GLOBE4RarcStroke <- scales::rescale(globe$x$arcsData[[globe$x$arcStroke]], to = c(min, max))
  globe$x$arcStroke <- "GLOBE4RarcStroke"
  return(globe)
}

#' Scale Arc Dash
#' 
#' Rescale various aspects of arcs dashes.
#' 
#' @inheritParams globe_img
#' @param min,max Target minimum and maximum values of pixels.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_arcs(
#'     data = usflights, 
#'     coords (
#'       start_lat, start_lon, end_lat, end_lon,
#'       dash_length = cnt
#'     )
#'   ) %>% 
#'   scale_arc_dash_length()
#' 
#' @section Scales:
#' \itemize{
#'   \item{Dash length: where \code{1} is the entire length of the arc.}
#' }
#' 
#' @name scaling_dash
#' @export
scale_arc_dash_length <- function(globe, min = .1, max = 1) UseMethod("scale_arc_dash_length")

#' @export
#' @method scale_arc_dash_length globe 
scale_arc_dash_length.globe <- function(globe, min = .1, max = 1){
  assert_that(length(globe$x$arcDashLength) >= 1, msg = "No `dash_length` specified.")
  globe$x$arcsData[[globe$x$arcDashLength]] <- scales::rescale(globe$x$arcsData[[globe$x$arcDashLength]], to = c(min, max))
  return(globe)
}

#' Scale Dots
#' 
#' Rescale various aspects of dots from \code{\link{globe_labels}}.
#' 
#' @inheritParams globe_img
#' @param min,max Target minimum and maximum values.
#' 
#' @name scale_labels_radius
#' @export
scale_labels_radius <- function(globe, min = .1, max = 1) UseMethod("scale_labels_radius")

#' @export
#' @method scale_labels_radius globe 
scale_labels_radius.globe <- function(globe, min = .1, max = 1){
  assert_that(length(globe$x$labelDotRadius) >= 1, msg = "No `dot_radius` specified.")
  globe$x$labelsData$GLOBE4RdotRadius <- scales::rescale(globe$x$labelsData[[globe$x$labelDotRadius]], to = c(min, max))
  globe$x$labelDotRadius <- "GLOBE4RdotRadius"
  return(globe)
}

#' Scale Labels SIze
#' 
#' Rescale \code{\link{globe_labels}} text size.
#' 
#' @inheritParams globe_img
#' @param min,max Target minimum and maximum values.
#' 
#' @name scale_labels_size
#' @export
scale_labels_size <- function(globe, min = .1, max = 1) UseMethod("scale_labels_size")

#' @export
#' @method scale_labels_size globe 
scale_labels_size.globe <- function(globe, min = .1, max = 1){
  assert_that(length(globe$x$labelSize) >= 1, msg = "No `size` specified.")
  globe$x$labelsData$GLOBE4Rsize <- scales::rescale(globe$x$labelsData[[globe$x$labelSize]], to = c(min, max))
  globe$x$labelSize <- "GLOBE4Rsize"
  return(globe)
}


#' Scale Hex Weight
#' 
#' Rescale hex weight to more appropriate values.
#' 
#' @inheritParams globe_img
#' @param min,max Target minimum and maximum values of pixels.
#' 
#' @name scale_hex_weight
#' @export
scale_hex_weight <- function(globe, min = .1, max = 1) UseMethod("scale_hex_weight")

#' @export
#' @method scale_hex_weight globe 
scale_hex_weight.globe <- function(globe, min = .1, max = 1){
  assert_that(length(globe$x$hexBinPointWeight) >= 1, msg = "No `weight` specified.")
  globe$x$hexBinPointsData$GLOBE4Rweight <- scales::rescale(globe$x$hexBinPointsData[[globe$x$hexBinPointWeight]], to = c(min, max))
  globe$x$arcStroke <- "GLOBE4Rweight"
  return(globe)
}