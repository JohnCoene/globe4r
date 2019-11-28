wd <- getwd()
setwd("./pkgdown")

library(globe4r)
library(htmltools)
# shared_background("#060606")

# viz
# hex
color_scale <- htmlwidgets::JS(
  "function(d){
    fnc = chroma.scale(['yellow', 'lightgreen', '008ae5']).domain([0, 5]);
    return fnc(d.sumWeight).hex();
  }"
)  

create_globe(height = "100vh") %>% 
  globe_img_url(image_url("blue")) %>% 
  bump_img_url() %>% 
  globe_pov(-16, 172, 1) %>%  
  hex_data(quakes) %>% 
  hex_lat("lat") %>% 
  hex_lon("long") %>% 
  hex_weight(1L) %>% 
  hex_side_color(color_scale) %>% 
  hex_cap_color(color_scale) %>%  
  htmlwidgets::saveWidget(file = "hex.html")

# polygons
create_globe(height = "100vh") %>% 
  globe_choropleth(
    coords(
      country = country_code,
      altitude = percent,
      cap_color = percent,
    ), 
    data = agriland
  ) %>% 
  scale_choropleth_cap_color() %>% 
  polygons_side_color(constant("rgba(255,255,255,.1)")) %>% 
  scale_choropleth_altitude(0.06, 0.2) %>% 
  htmlwidgets::saveWidget(file = "choropleth.html")

# arcs
create_globe(height = "100vh") %>% 
  globe_arcs(
    data = usflights,
    coords(
      start_lat = start_lat,
      start_lon = start_lon,
      end_lat = end_lat,
      end_lon = end_lon,
      color = cnt
    )
  ) %>% 
  globe_pov(
    34, -93, 1.5, ms = 4500
  ) %>% 
  scale_arc_color() %>% 
  htmlwidgets::saveWidget(file = "arcs.html")

# labels
create_globe(height = "100vh") %>% 
  globe_img_url(image_url("blue")) %>% 
  globe_labels(
    data = quakes,
    coords(
      lat, long, 
      text = stations, 
      color = mag, 
      dot_radius = mag,
      size = mag,
      label = stations
    )
  ) %>% 
  globe_pov(
    -25, 181, .5, ms = 4500
  ) %>% 
  scale_labels_radius(.05, .1) %>% 
  scale_labels_color() %>% 
  scale_labels_size(.05, .35) %>% 
  htmlwidgets::saveWidget(file = "labels.html")

create_globe(cables, height = "100vh") %>% 
  globe_img_url(image_url("dark")) %>% 
  bump_img_url() %>% 
  globe_paths(
    coords(
      color = color, 
      dash_length = .1, 
      dash_gap = .008, 
      dash_animate_time = 12000
    )
  ) %>% 
  htmlwidgets::saveWidget(file = "paths.html")

setwd(wd)