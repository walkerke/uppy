# Styled Uppy Example
# Demonstrates custom styling and branding

library(shiny)
library(bslib)
library(uppy)

ui <- page_fluid(
  theme = bs_theme(version = 5),

  titlePanel("Custom Uppy Styling Demo"),

  p(strong("Try adding some files to see the styled buttons and progress bars!")),
  p("Notice how each dashboard has different colors for borders, buttons, and progress indicators."),

  fluidRow(
    column(
      width = 6,
      card(
        card_header(
          "Professional Blue",
          class = "bg-primary text-white"
        ),
        uppy_input(
          "files_blue",
          max_file_size = 25 * 1024 * 1024,
          allowed_file_types = c(".jpg", ".jpeg", ".png", ".gif", ".pdf"),
          height = 350,
          note = "Professional blue theme",
          style = uppy_style(
            primary_color = "#0066cc",      # Professional blue
            accent_color = "#0052a3",       # Darker blue
            success_color = "#10b981",      # Green
            border_color = "#0066cc",
            border_radius = "8px"
          )
        ),
        card_footer(
          textOutput("blue_status")
        )
      )
    ),

    column(
      width = 6,
      card(
        card_header(
          "Vibrant Orange/Purple",
          class = "text-white",
          style = "background-color: #F77F00;"
        ),
        uppy_input(
          "files_orange",
          max_file_size = 25 * 1024 * 1024,
          allowed_file_types = c(".jpg", ".jpeg", ".png", ".gif", ".pdf"),
          height = 350,
          note = "Bold orange & purple theme",
          style = uppy_style(
            primary_color = "#6f42c1",      # Purple
            accent_color = "#F77F00",       # Orange
            success_color = "#20c997",      # Teal
            border_color = "#6f42c1",
            border_radius = "16px",
            custom_css = "
              .uppy-Dashboard-browse {
                font-weight: 600;
                text-transform: uppercase;
                letter-spacing: 0.5px;
              }
              .uppy-Dashboard-AddFiles {
                font-size: 1.1rem;
              }
            "
          )
        ),
        card_footer(
          textOutput("orange_status")
        )
      )
    )
  ),

  hr(),

  fluidRow(
    column(
      width = 12,
      card(
        card_header(
          "Minimal Dark Theme",
          class = "bg-dark text-white"
        ),
        uppy_input(
          "files_dark",
          max_file_size = 50 * 1024 * 1024,
          height = 300,
          note = "Sleek dark minimalist design",
          style = uppy_style(
            primary_color = "#1a1a1a",
            accent_color = "#ff4757",      # Red accent
            success_color = "#2ed573",     # Bright green
            background_color = "#f8f9fa",
            border_color = "#495057",
            text_color = "#212529",
            border_radius = "4px"
          )
        ),
        card_footer(
          textOutput("dark_status")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Blue theme status
  output$blue_status <- renderText({
    if (is.null(input$files_blue)) {
      "No files uploaded yet"
    } else {
      files <- uppy_process_files(input$files_blue)
      sprintf("✓ %d file(s) uploaded - Total: %.2f MB",
              nrow(files),
              sum(files$size) / (1024 * 1024))
    }
  })

  # Orange theme status
  output$orange_status <- renderText({
    if (is.null(input$files_orange)) {
      "No files uploaded yet"
    } else {
      files <- uppy_process_files(input$files_orange)
      sprintf("✓ %d file(s) uploaded - Total: %.2f MB",
              nrow(files),
              sum(files$size) / (1024 * 1024))
    }
  })

  # Dark theme status
  output$dark_status <- renderText({
    if (is.null(input$files_dark)) {
      "No files uploaded yet"
    } else {
      files <- uppy_process_files(input$files_dark)
      sprintf("✓ %d file(s) uploaded - Total: %.2f MB",
              nrow(files),
              sum(files$size) / (1024 * 1024))
    }
  })
}

shinyApp(ui, server)
