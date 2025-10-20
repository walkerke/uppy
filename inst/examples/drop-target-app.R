# Uppy Drop Target Example
# Demonstrates drag-and-drop on specific outputs (map, plot, etc.)

library(shiny)
library(bslib)
library(leaflet)
library(uppy)

ui <- page_fluid(
  theme = bs_theme(version = 5),

  titlePanel("Drag & Drop Files onto Outputs"),

  p(strong("Instructions:"), "Try dragging files onto the map or plot below!"),
  p("Map accepts: GeoJSON files | Plot accepts: CSV files"),
  p(em("Need a test file? Download ",
       a("sample-data.csv", href = "#", onclick = "downloadSampleCSV(); return false;"),
       " to try the plot drop zone.")),

  # JavaScript to create downloadable sample CSV
  tags$script(HTML("
    function downloadSampleCSV() {
      const csv = 'x,y\\n1,2\\n2,4\\n3,6\\n4,8\\n5,10\\n6,12\\n7,14\\n8,16\\n9,18\\n10,20';
      const blob = new Blob([csv], { type: 'text/csv' });
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = 'sample-data.csv';
      a.click();
      window.URL.revokeObjectURL(url);
    }
  ")),

  # Drop target for the map (accepts geographic files)
  uppy_drop_target(
    input_id = "map_files",
    target = "my_map",  # Just the output ID - uppy handles the rest!
    allowed_file_types = c(".geojson", ".json"),
    max_file_size = 10 * 1024 * 1024,  # 10MB
    on_drop_message = "Loading map data..."
  ),

  # Drop target for the plot (accepts CSV)
  uppy_drop_target(
    input_id = "plot_files",
    target = "my_plot",
    allowed_file_types = c(".csv", ".txt"),
    max_file_size = 5 * 1024 * 1024,  # 5MB
    on_drop_message = "Loading CSV data...",
    style = uppy_style(
      primary_color = "#2563eb",
      accent_color = "#3b82f6"
    )
  ),

  fluidRow(
    column(
      width = 6,
      card(
        card_header("Map - Drop GeoJSON files here"),
        leafletOutput("my_map", height = 400),
        card_footer(textOutput("map_status"))
      )
    ),
    column(
      width = 6,
      card(
        card_header("Plot - Drop CSV files here"),
        plotOutput("my_plot", height = 400),
        card_footer(textOutput("plot_status"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Initialize map
  output$my_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })

  # Initialize map status
  output$map_status <- renderText({
    "Drop a GeoJSON file here to display it on the map"
  })

  # Initialize plot
  output$my_plot <- renderPlot({
    plot(1:10, 1:10, type = "n",
         xlab = "X axis",
         ylab = "Y axis",
         main = "Drop a CSV file here")
    text(5.5, 5.5, "Drag & Drop CSV Here", cex = 2, col = "gray70")
  })

  # Initialize plot status
  output$plot_status <- renderText({
    "Drop a CSV file here to plot it (needs 'x' and 'y' columns)"
  })

  # Handle map file drops
  observeEvent(input$map_files, {
    req(input$map_files)

    files <- uppy_process_files(input$map_files$files)

    tryCatch({
      # Read GeoJSON
      geojson_text <- readLines(files$datapath[1], warn = FALSE)
      geojson_data <- jsonlite::fromJSON(paste(geojson_text, collapse = ""))

      # Update map with GeoJSON
      output$my_map <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addGeoJSON(geojson_data) %>%
          fitBounds(
            lng1 = min(sapply(geojson_data$features$geometry$coordinates, function(x) min(x[[1]][,1]))),
            lat1 = min(sapply(geojson_data$features$geometry$coordinates, function(x) min(x[[1]][,2]))),
            lng2 = max(sapply(geojson_data$features$geometry$coordinates, function(x) max(x[[1]][,1]))),
            lat2 = max(sapply(geojson_data$features$geometry$coordinates, function(x) max(x[[1]][,2])))
          )
      })

      output$map_status <- renderText({
        sprintf("✓ Loaded: %s (%s features)",
                files$name[1],
                length(geojson_data$features))
      })

      showNotification(
        paste("GeoJSON loaded:", files$name[1]),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      output$map_status <- renderText({
        paste("Error loading file:", e$message)
      })
      showNotification(
        paste("Error reading GeoJSON:", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  # Handle plot file drops
  observeEvent(input$plot_files, {
    req(input$plot_files)

    files <- uppy_process_files(input$plot_files$files)

    tryCatch({
      # Read CSV
      data <- read.csv(files$datapath[1])

      # Check for x and y columns
      if (!all(c("x", "y") %in% names(data))) {
        stop("CSV must have 'x' and 'y' columns")
      }

      # Update plot
      output$my_plot <- renderPlot({
        plot(data$x, data$y,
             main = paste("Data from", files$name[1]),
             xlab = "x",
             ylab = "y",
             pch = 19,
             col = "#2563eb",
             cex = 1.5)
        grid()
        lines(data$x, data$y, col = "#3b82f6", lwd = 2)
      })

      output$plot_status <- renderText({
        sprintf("✓ Loaded: %s (%d rows)", files$name[1], nrow(data))
      })

      showNotification(
        paste("CSV loaded:", files$name[1]),
        type = "message",
        duration = 3
      )
    }, error = function(e) {
      output$plot_status <- renderText({
        paste("Error:", e$message)
      })
      showNotification(
        paste("Error reading CSV:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
}

shinyApp(ui, server)
