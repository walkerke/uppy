# Basic Uppy Example App
# Demonstrates simple file upload with Uppy

library(shiny)
library(bslib)
library(uppy)

ui <- page_fluid(
  titlePanel("Basic Uppy File Upload"),

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

  # Display uploaded files info
  output$files_table <- renderTable({
    req(input$files)
    files <- uppy_process_files(input$files)
    data.frame(
      Filename = files$name,
      Size = sprintf("%.2f KB", files$size / 1024),
      Type = files$type
    )
  })

  # Display raw file information
  output$files_info <- renderPrint({
    req(input$files)
    files <- uppy_process_files(input$files)
    cat("Files uploaded:\n")
    str(files)
  })

  # Reset button
  observeEvent(input$reset, {
    uppy_reset("files", session)
    showNotification("Uploads cleared", type = "message")
  })
}

shinyApp(ui, server)
