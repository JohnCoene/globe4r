.get_data <- function(x, y){
  if(!is.null(y))
    return(y)
  return(x)
}

color_scale <- function(x, palette, min , max){
  palette <- jsonlite::toJSON(palette)
  domain <- jsonlite::toJSON(c(min, max))
  func <- paste0(
    "function(d){
      fnc = chroma.scale(", palette, ").domain(", domain, ");
      return(fnc(d.", x, ").hex());
    }"
  )
  htmlwidgets::JS(func)
}