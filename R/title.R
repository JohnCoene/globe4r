#' Title
#' 
#' Add a title to a globe.
#' 
#' @inheritParams globe_img
#' @param text The text to display as title or subtitle.
#' @param tag The html tag to use, from \link[shiny]{tags}.
#' @param vertical,horizontal Alignment of title.
#' @param color Color of title or title.
#' @param margin Margin from \code{horizontal} and \code{vertical}.
#' 
#' @examples
#' create_globe() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_bars(
#'     coords(lat, long, label = stations), 
#'     data = quakes
#'   ) %>% 
#'   globe_title("A Globe!")
#' 
#' @export
globe_title <- function(globe, text, tag = shiny::tags$h1, vertical = c("top", "bottom"), horizontal = c("left", "right"), color = "#fff", margin = "3%") UseMethod("globe_title")

#' @export
#' @method globe_title globe
globe_title.globe <- function(globe, text, tag = shiny::tags$h1, vertical = c("top", "bottom"), horizontal = c("left", "right"), color = "#fff", margin = "3%"){
  vertical <- match.arg(vertical)
  horizontal <- match.arg(horizontal)
  
  content <- tag(text)

  args <- list(
    color = color,
    `z-index` = 9000,
    position = "absolute"
  )
  args[[vertical]] <- margin
  args[[horizontal]] <- margin
  args <- paste0(names(args), ":", args, collapse =";")
  content <- do.call(shiny::tagAppendAttributes, list(content, style = args))
  htmlwidgets::prependContent(globe, content)
}