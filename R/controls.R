#' Controls
#' 
#' Controls globe.
#' 
#' @inheritParams globe_img
#' @param speed How fast to rotate around the globe. 
#' @param rotate Whether to autorotate the globe.
#' 
#' @examples
#' # basic
#' create_globe() %>% 
#'   globe_pov(-21, 179) %>% 
#'   globe_bars(
#'     coords(lat, long), 
#'     data = quakes
#'   ) %>% 
#'   globe_rotate()
#' 
#' @export
globe_rotate <- function(globe, speed = 1L, rotate = TRUE) UseMethod("globe_rotate")

#' @export
#' @method globe_rotate globe
globe_rotate.globe <- function(globe, speed = 1L, rotate = TRUE){
  globe$x$autoRotate <- rotate
  globe$x$autoRotateSpeed <- speed
  return(globe)
}

#' @export
#' @method globe_rotate globeProxy
globe_rotate.globeProxy <- function(globe, speed = 1L, rotate = TRUE){
  msg <- list(id = globe$id)
  msg$autoRotate <- rotate
  msg$autoRotateSpeed <- speed
  globe$session$sendCustomMessage("globe_rotate", msg)
  return(globe)
}