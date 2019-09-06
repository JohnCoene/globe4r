#' Create a globe
#'
#' Create a globe object.
#'
#' @param data A data.frame containing coordinates.
#' @param mapping Coordiantes as specified by \code{\link{coords}}.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Id of element.
#' @param antialias,alpha Configuration parameters to pass to the 
#' \href{https://threejs.org/docs/#api/en/renderers/WebGLRenderer}{ThreeJS WebGLRenderer} constructor.
#' @param animate_in Whether to animate the globe initialization, by scaling 
#' and rotating the globe into its inital position.
#'
#' @import purrr 
#' @import dplyr
#' @import assertthat
#' @import htmlwidgets
#'
#' @export
create_globe <- function(data = NULL, mapping = NULL, antialias = TRUE, alpha = TRUE, animate_in = TRUE, 
  width = NULL, height = NULL, elementId = NULL) {

  x = list(
    init = list(
      rendererConfig = list(
        antialias = antialias, 
        alpha = alpha
      ),
      animateIn = animate_in
    ),
    data = data,
    mapping = mapping,
    globals = list()
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'globe',
    x,
    width = width,
    height = height,
    package = 'globe4r',
    elementId = elementId,
    preRenderHook = build_globe,
    sizingPolicy = htmlwidgets::sizingPolicy(
      padding = 0,
      browser.fill = TRUE
    )
  )
}

build_globe <- function(globe){
  if(length(globe$x$pointsData))
    globe$x$pointsData <- apply(globe$x$pointsData, 1, as.list)
  if(length(globe$x$arcsData))
    globe$x$arcsData <- apply(globe$x$arcsData, 1, as.list)
  if(length(globe$x$labelsData))
    globe$x$labelsData <- apply(globe$x$labelsData, 1, as.list)

  if(!length(globe$x$backgroundColor) && length(getOption("backgroundColor")))
    globe$x$backgroundColor <- getOption("backgroundColor")

  globe$x$data <- NULL
  globe$x$mapping <- NULL
  return(globe)
}

#' Shiny bindings for globe
#'
#' Output and render functions for using globe within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a globe
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @param id Target chart id.
#' @param session Shiny session.
#'
#' @name globe-shiny
#'
#' @export
globeOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'globe', width, height, package = 'globe4r')
}

#' @rdname globe-shiny
#' @export
renderGlobe <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, globeOutput, env, quoted = TRUE)
}

#' @rdname globe-shiny
#' @export
globeProxy <- function(id, session = shiny::getDefaultReactiveDomain()){
  
  proxy <- list(id = id, session = session)
  class(proxy) <- "globeProxy"
  
  return(proxy)
}