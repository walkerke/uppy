# Uppy with Dropbox Integration Example
# Demonstrates cloud storage integration via Transloadit

library(shiny)
library(bslib)
library(uppy)

# IMPORTANT: Replace these with your actual Transloadit credentials
# Get them from: https://transloadit.com/
TRANSLOADIT_KEY <- Sys.getenv("TRANSLOADIT_KEY", "YOUR_KEY_HERE")
DROPBOX_CREDS <- Sys.getenv("DROPBOX_CREDENTIALS_NAME", "YOUR_CREDENTIALS_NAME")

ui <- page_fluid(
  titlePanel("Uppy with Dropbox Integration"),

  fluidRow(
    column(
      width = 8,
      offset = 2,
      wellPanel(
        h3("Upload from Multiple Sources"),
        p("Upload files from your computer or directly from Dropbox"),
        uppy_input(
          "files",
          max_file_size = 100 * 1024 * 1024,  # 100MB
          allowed_file_types = c(".pdf", ".docx", ".xlsx", ".pptx"),
          height = 400,
          note = "Upload files from your computer or Dropbox (max 100MB)",
          plugins = uppy_plugins(
            dropbox = uppy_transloadit(
              key = TRANSLOADIT_KEY,
              credentials_name = DROPBOX_CREDS
            )
          )
        )
      ),
      hr(),
      h4("Uploaded Files"),
      tableOutput("files_table")
    )
  )
)

server <- function(input, output, session) {

  output$files_table <- renderTable({
    req(input$files)
    files <- uppy_process_files(input$files)

    if (nrow(files) == 0) {
      return(data.frame(Message = "No files uploaded yet"))
    }

    data.frame(
      Filename = files$name,
      Size = sprintf("%.2f MB", files$size / (1024 * 1024)),
      Type = files$type,
      Location = files$datapath
    )
  })
}

shinyApp(ui, server)
