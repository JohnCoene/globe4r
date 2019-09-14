
library(globe4r)
library(htmltools)
# shared_background("#060606")

# viz
# bars
create_globe(height = "100vh") %>% 
  globe_hex(
    coords(
      lat, lon, 
      weight = value,
      color = value,
      cap_color = value
    ), 
    data = population
  ) %>% 
  globe_rotate(1L) %>% 
  scale_hex_cap_color() %>% 
  scale_hex_altitude() %>% 
  htmlwidgets::saveWidget(file = "hex.html")

# polygons
create_globe(height = "100vh") %>% 
  globe_choropleth(
    coords(
      country = country_code,
      altitude = percent,
      cap_color = percent
    ), 
    data = agriland
  ) %>% 
  scale_choropleth_cap_color() %>% 
  scale_choropleth_altitude() %>% 
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