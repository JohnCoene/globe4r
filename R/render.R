#' Point of View
#' 
#' Change the camera point of view.
#' 
#' @inheritParams globe_img
#' @param lat,lon Coordinates of camera position.
#' @param altitude Altitude of camera, defaults to \code{2.5} globe radii.
#' 
#' @examples
#' create_globe() %>% 
#'   globe_img_url() %>% 
#'   globe_pov(38, -77)
#' 
#' @export
globe_pov <- function(globe, lat, lon, altitude = 2.5) UseMethod("globe_pov")

#' @export
#' @method globe_pov globe
globe_pov.globe <- function(globe, lat, lon, altitude = 2.5){

  assert_that(not_missing(lat))
  assert_that(not_missing(lon))

  globe$x$pointOfView <- list(
    lat = lat,
    lng = lon,
    altitude = altitude
  )

  return(globe)
}

#' @export
#' @method globe_pov globeProxy
globe_pov.globeProxy <- function(globe, lat, lon, altitude = 2.5){

  assert_that(not_missing(lat))
  assert_that(not_missing(lon))

  msg <- list(
    id = globe$id,
    pointOfView = list(
      lat = lat,
      lng = lon,
      altitude = altitude
    )
  )

  globe$session$sendCustomMessage("globe_pov", msg)

  return(globe)
}

#' Point of View
#' 
#' Change the camera point of view.
#' 
#' @inheritParams globe_img
#' @name animation
#' @export
pause_animation <- function(globe) UseMethod("pause_animation")

#' @export
#' @method globe_pov globeProxy
pause_animation.globeProxy <- function(globe){

  msg <- list(id = globe$id)

  globe$session$sendCustomMessage("pause_animation", msg)

  return(globe)
}

#' @rdname animation
#' @export
resume_animation <- function(globe) UseMethod("resume_animation")

#' @export
#' @method globe_pov globeProxy
resume_animation.globeProxy <- function(globe){

  msg <- list(id = globe$id)

  globe$session$sendCustomMessage("resume_animation", msg)

  return(globe)
}