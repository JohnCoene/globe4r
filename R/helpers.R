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