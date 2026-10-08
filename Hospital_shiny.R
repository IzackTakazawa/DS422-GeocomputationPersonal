library(shiny)
library(mapboxapi)
library(leaflet)
library(sf)
library(raster)
library(fasterize)

# Pick the UTM zone that fits the location, so this works in Hawaii, Oregon, anywhere.
# (Your .qmd used 32618, which is the East Coast zone. Oahu is 32604.)
utm_epsg <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  if (lat >= 0) 32600 + zone else 32700 + zone
}

# ---------------------------------------------------------------- UI
ui <- fluidPage(
  titlePanel("Isochrone Accessibility Explorer"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("address", "Starting location",
                value = "3140 Waialae Ave, Honolulu, HI 96816"),
      selectInput("profile", "Travel mode",
                  choices = c("Driving" = "driving",
                              "Driving (traffic)" = "driving-traffic",
                              "Walking" = "walking",
                              "Cycling" = "cycling"),
                  selected = "driving"),
      sliderInput("max_time", "Maximum travel time (minutes)",
                  min = 5, max = 60, value = 45, step = 5),
      selectInput("step", "Isochrone interval (minutes)",
                  choices = c(1, 2, 5), selected = 2),
      sliderInput("res", "Surface resolution (meters)",
                  min = 50, max = 500, value = 100, step = 50),
      selectInput("palette", "Color palette",
                  choices = c("plasma", "viridis", "magma", "inferno"),
                  selected = "plasma"),
      sliderInput("opacity", "Surface opacity",
                  min = 0.1, max = 1, value = 0.5, step = 0.1),
      actionButton("go", "Create isochrones", class = "btn-primary"),
      helpText("Smaller intervals make a smoother surface but need more API calls.")
    ),
    mainPanel(
      width = 9,
      leafletOutput("map", height = "80vh")
    )
  )
)

# ---------------------------------------------------------------- Server
server <- function(input, output, session) {
  
  # Calculate everything only when the button is clicked (and once on startup)
  iso_data <- eventReactive(input$go, {
    req(nzchar(input$address))
    
    withProgress(message = "Building isochrones...", value = 0.1, {
      result <- tryCatch({
        loc <- mb_geocode(input$address)               # c(lon, lat)
        incProgress(0.2, detail = "Requesting drive-time polygons")
        
        times <- seq(as.numeric(input$step), input$max_time, by = as.numeric(input$step))
        isos <- mb_isochrone(location = loc,
                             profile = input$profile,
                             time = times)
        incProgress(0.4, detail = "Building surface")
        
        isos_proj <- st_transform(isos, utm_epsg(loc[1], loc[2]))
        template <- raster(isos_proj, resolution = input$res)
        surface <- fasterize(isos_proj, template, field = "time", fun = "min")
        
        list(loc = loc, isos = isos, surface = surface)
      }, error = function(e) {
        showNotification(paste("Something went wrong:", conditionMessage(e)),
                         type = "error", duration = 10)
        NULL
      })
      result
    })
  }, ignoreNULL = FALSE)
  
  # Base map, drawn once. Starts centered on Oahu.
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = "light-v9",
                     username = "mapbox",
                     scaling_factor = "0.5x") %>%
      setView(lng = -157.86, lat = 21.31, zoom = 10)
  })
  
  # Update the map whenever new results (or palette/opacity) change
  observe({
    res <- iso_data()
    req(res)
    
    pal <- colorNumeric(input$palette, res$isos$time, na.color = "transparent")
    mode_label <- c(driving = "Drive", "driving-traffic" = "Drive (traffic)",
                    walking = "Walk", cycling = "Bike")[[input$profile]]
    
    leafletProxy("map") %>%
      clearImages() %>%
      clearMarkers() %>%
      clearControls() %>%
      addRasterImage(res$surface, colors = pal, opacity = input$opacity) %>%
      addLegend(values = res$isos$time, pal = pal,
                title = paste0(mode_label, "-time<br>(minutes)"),
                position = "bottomright") %>%
      addMarkers(lng = res$loc[1], lat = res$loc[2],
                 popup = input$address) %>%
      flyToBounds(lng1 = min(st_bbox(res$isos)[c(1, 3)]),
                  lat1 = min(st_bbox(res$isos)[c(2, 4)]),
                  lng2 = max(st_bbox(res$isos)[c(1, 3)]),
                  lat2 = max(st_bbox(res$isos)[c(2, 4)]))
  })
}

shinyApp(ui = ui, server = server)
