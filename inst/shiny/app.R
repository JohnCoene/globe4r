library(shiny)
library(globe4r)

ui <- fluidPage(
  fluidRow(
    column(
      8, globeOutput("globe", height = "100vh")
    ),
    column(
      4,
      selectInput(
        "background",
        "Select background:",
        choices = list(
          "Night" = "night", 
          "Blue" = "blue-marble", 
          "Dark" = "dark"
        )
      ),
      actionButton("pov", "Focus Camera"),
      actionButton("animate", "Animate")
    )
  )
)

server <- function(input, output, session) {
  output$globe <- render_globe({
    create_globe() %>% 
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
      scale_arc_color()
  })

  # background
  observeEvent(input$background, {
    globe_proxy("globe") %>% 
      globe_img_url(image_url(input$background))
  })

  # draw arcs
  observeEvent(input$pov, {
    globe_proxy("globe") %>% 
      globe_pov(
        34, -93, 2, ms = 2500
      )
  })

  # animate
  observeEvent(input$animate, {
    globe_proxy("globe") %>% 
      globe_pov(
        34, -93, 2, ms = 2500
      ) %>% 
      arcs_dash_animate(10000L) %>% 
      arcs_dash_length(.4) %>% 
      arcs_dash_gap(.2)
  })
}

shinyApp(ui, server)