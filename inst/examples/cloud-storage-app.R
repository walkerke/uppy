# Cloud Storage Integration Example
# Demonstrates Box and Dropbox integration via Transloadit Companion

library(shiny)
library(bslib)
library(uppy)

# IMPORTANT: You need to set up Transloadit credentials for this to work
# 1. Sign up at https://transloadit.com/
# 2. Get your API key from your account
# 3. Set up Template Credentials for Box and Dropbox in your Transloadit account
# 4. Replace the placeholders below with your actual credentials

# Set your Transloadit credentials here
TRANSLOADIT_KEY <- Sys.getenv("TRANSLOADIT_KEY", "YOUR_TRANSLOADIT_KEY_HERE")
BOX_CREDENTIALS <- Sys.getenv("BOX_CREDENTIALS", "your-box-credentials")
DROPBOX_CREDENTIALS <- Sys.getenv("DROPBOX_CREDENTIALS", "your-dropbox-credentials")

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  titlePanel("Cloud Storage File Upload with Uppy"),

  layout_columns(
    col_widths = c(12, 12),

    # Information Card
    card(
      card_header("Setup Instructions"),
      markdown("
### Getting Started with Cloud Storage

This example demonstrates how to integrate **Box** and **Dropbox** with Uppy using Transloadit Companion.

#### Requirements:

1. **Transloadit Account**: Sign up at [transloadit.com](https://transloadit.com/)
2. **API Key**: Get your key from your Transloadit dashboard
3. **Template Credentials**: Configure credentials for each provider:
   - Go to your Transloadit account → Credentials → Template Credentials
   - Add credentials for Box and Dropbox
   - Note the credential names (e.g., 'my-box-creds', 'my-dropbox-creds')

#### Environment Variables (Recommended):

```r
# Set these in your .Renviron file:
TRANSLOADIT_KEY=your_actual_key_here
BOX_CREDENTIALS=my-box-creds
DROPBOX_CREDENTIALS=my-dropbox-creds
```

**Note**: Without valid credentials, the cloud storage buttons will appear but authentication will fail.
      ")
    ),

    # Upload Interface
    card(
      card_header("Upload Files from Cloud Storage or Device"),
      uppy_input(
        "cloud_files",
        plugins = uppy_plugins(
          box = uppy_transloadit(
            key = TRANSLOADIT_KEY,
            credentials_name = BOX_CREDENTIALS
          ),
          dropbox = uppy_transloadit(
            key = TRANSLOADIT_KEY,
            credentials_name = DROPBOX_CREDENTIALS
          )
        ),
        max_file_size = 100 * 1024 * 1024,  # 100MB
        height = 400,
        note = "Select files from your device, Box, or Dropbox"
      ),
      hr(),
      actionButton("reset", "Clear Uploads", class = "btn-outline-secondary")
    )
  ),

  # Results Section
  card(
    card_header("Uploaded Files"),

    layout_columns(
      col_widths = c(6, 6),

      # File Table
      card(
        card_header("File Information"),
        tableOutput("files_table")
      ),

      # Raw Data
      card(
        card_header("Raw File Data"),
        verbatimTextOutput("files_info")
      )
    )
  )
)

server <- function(input, output, session) {

  # Display uploaded files info
  output$files_table <- renderTable({
    req(input$cloud_files)

    data.frame(
      Filename = input$cloud_files$name,
      Size = sprintf("%.2f KB", input$cloud_files$size / 1024),
      Type = input$cloud_files$type,
      stringsAsFactors = FALSE
    )
  })

  # Display raw file information
  output$files_info <- renderPrint({
    req(input$cloud_files)

    cat("Files uploaded:\n")
    cat("Count:", nrow(input$cloud_files), "\n\n")

    cat("File paths:\n")
    for(i in seq_len(nrow(input$cloud_files))) {
      cat(sprintf("[%d] %s\n    -> %s\n",
                  i,
                  input$cloud_files$name[i],
                  input$cloud_files$datapath[i]))
    }

    cat("\nTotal size:",
        sprintf("%.2f MB", sum(input$cloud_files$size) / 1024 / 1024),
        "\n")
  })

  # Reset button
  observeEvent(input$reset, {
    uppy_reset("cloud_files", session)
    showNotification("Cloud uploads cleared", type = "message")
  })

  # Check credentials on startup
  observe({
    if (TRANSLOADIT_KEY == "YOUR_TRANSLOADIT_KEY_HERE") {
      showNotification(
        "⚠️ Transloadit credentials not configured. Cloud storage will not work.",
        type = "warning",
        duration = 10
      )
    }
  })
}

shinyApp(ui, server)
