usflights <- read.csv(
  paste0("https://raw.githubusercontent.com/plotly/datasets/",
         "master/2011_february_aa_flight_paths.csv")
)

names(usflights) <- c("start_lat", "start_lon", "end_lat", "end_lon", "airline", 
  "airport_start", "airport_end", "cnt")

usethis::use_data(usflights, overwrite = TRUE)
