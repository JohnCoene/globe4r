#' U.S. Flights
#'
#' A small data.frame of flight routes in the U.S.
#'
#' @format A data.frame object with 178 rows and 8 columns
#' \describe{
#'   \item{\code{start_lat}, \code{start_lon}, \code{end_lat}, \code{end_lon}}{: geographical coordinates.}
#'   \item{\code{airline}}{: name of airline.}
#'   \item{\code{airport_start}, \code{airport_end}}{: names of airports.}
#'   \item{\code{cnt}}{: number of flights.}
#' }
"usflights"

#' Agricultural Lands
#'
#' A datatset of the available agricultural land (as percentage of land area) by country in 2016.
#'
#' @format A data.frame object with 254 rows and 3 columns
#' \describe{
#'   \item{\code{country_name}}{: name of the country.}
#'   \item{\code{country_code}}{: the iso3 code identifying the country.}
#'   \item{\code{percent}}{: percentage of lang.}
#' }
#' @source \url{https://data.worldbank.org/indicator/AG.LND.AGRI.ZS}
"agriland"

#' World Population
#'
#' A datatset of world population by tiles.
#'
#' @format A data.frame object with 38,654 rows and 3 columns
#' \describe{
#'   \item{\code{lat}, \code{lon}}{: Coordinates.}
#'   \item{\code{country_code}}{: Population.}
#' }
#' @source \url{https://ecomfe.github.io/echarts-examples/public/data-gl/asset/data/population.json}
"population"

#' Cables
#'
#' Underwater cables.
#'
#' @format An object of class sp containing LINESTRINGS.
"cables"
