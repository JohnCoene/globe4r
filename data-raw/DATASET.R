# sample dataset for arcs
usflights <- read.csv(
  paste0("https://raw.githubusercontent.com/plotly/datasets/",
         "master/2011_february_aa_flight_paths.csv")
)

names(usflights) <- c("start_lat", "start_lon", "end_lat", "end_lon", "airline", 
  "airport_start", "airport_end", "cnt")

# countries polygons
library(purrr)

# read json to list
countries_geojson <- geojsonio::geojson_read("https://raw.githubusercontent.com/vasturiano/globe.gl/master/example/datasets/ne_110m_admin_0_countries.geojson")
countries_features <- countries_geojson$features # get features only

# get iso3
country_iso3 <- map(countries_features, "properties") %>% 
  map("ISO_A3") %>% 
  unlist()

country_iso2 <- map(countries_features, "properties") %>% 
  map("ISO_A2") %>% 
  unlist()

country_name <- map(countries_features, "properties") %>% 
  map("NAME_SORT") %>% 
  unlist()

country_polygons <- tibble::tibble(
  country_name = country_name,
  country_iso3 = country_iso3,
  country_iso2 = country_iso2,
  features = countries_features
)

# agricultural lands
library(dplyr)

agriland <- read.csv("./data-raw/agricultural_land_area.csv", skip = 4, stringsAsFactors = FALSE)
agriland <- agriland %>% 
  select(
    country_name = `Country.Name`,
    country_code = `Country.Code`,
    percent = X2016
  ) %>% 
  mutate(
    percent = percent / 100
  ) %>% 
  filter(!is.na(percent))

url <- paste0("https://ecomfe.github.io/echarts-examples/",
              "public/data-gl/asset/data/population.json")
population <- jsonlite::fromJSON(url)
population <- as.data.frame(population)
names(population) <- c("lon", "lat", "value")

usethis::use_data(usflights, agriland, population, overwrite = TRUE)
usethis::use_data(country_polygons, internal = TRUE, overwrite = TRUE)
