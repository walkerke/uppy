# Basic Uppy Example App
# Demonstrates simple file upload with Uppy

library(shiny)
library(bslib)
library(uppy)

ui <- page_fluid(
  titlePanel("Basic Uppy File Upload"),

  p(strong("Batch Mode (Recommended):"), "Add files, review them, then click the Upload button.",
    "This mode handles large batches (100s of files) reliably."),

  sidebarLayout(
    sidebarPanel(
      uppy_input(
        "files",
        max_file_size = 50 * 1024 * 1024,  # 50MB
        allowed_file_types = c(".pdf", ".docx", ".txt"),
        note = "Upload PDF, Word, or text files (max 50MB)",
        height = 300
      ),
      hr(),
      actionButton("reset", "Clear Uploads")
    ),

    mainPanel(
      h3("Uploaded Files"),
      tableOutput("files_table"),
      hr(),
      verbatimTextOutput("files_info")
    )
  )
)

server <- function(input, output, session) {

  # Debug: observe what input$files contains
  observe({
    if (!is.null(input$files)) {
      cat("\n=== DEBUG: input$files received ===\n")
      cat("Class:", class(input$files), "\n")
      cat("Length:", length(input$files), "\n")
      cat("Names:", names(input$files), "\n")
      cat("Structure:\n")
      str(input$files, max.level = 2)
      cat("====================================\n\n")
    }
  })

  # Display uploaded files info
  output$files_table <- renderTable({
    req(input$files)
    # Files are now a dataframe, just like fileInput()!
    data.frame(
      Filename = input$files$name,
      Size = sprintf("%.2f KB", input$files$size / 1024),
      Type = input$files$type
    )
  })

  # Display raw file information
  output$files_info <- renderPrint({
    req(input$files)
    # Files are a dataframe with name, size, type, datapath columns
    cat("Files uploaded:\n")
    str(input$files)
    cat("\nDatapaths:\n")
    print(input$files$datapath)
  })

  # Reset button
  observeEvent(input$reset, {
    uppy_reset("files", session)
    showNotification("Uploads cleared", type = "message")
  })
}

shinyApp(ui, server)
