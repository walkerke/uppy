# Tus Large File Upload Example
# Demonstrates resumable, chunked uploads for files >500MB

library(shiny)
library(bslib)
library(uppy)

# IMPORTANT: This example requires a running Tus server (tusd)
#
# Quick Start with Docker:
# docker run -p 1080:1080 -v $(pwd)/tus-data:/data tusproject/tusd:latest \
#   -upload-dir=/data -behind-proxy -hooks-dir=/data/hooks
#
# Or install tusd locally:
# https://github.com/tus/tusd/releases
#
# The tusd server will run on: http://localhost:1080/files/

TUS_ENDPOINT <- Sys.getenv("TUS_ENDPOINT", "http://localhost:1080/files/")

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),

  titlePanel("Large File Uploads with Tus Protocol"),

  layout_columns(
    col_widths = c(12, 12),

    # Setup Instructions
    card(
      card_header("🚀 Setup: Running a Tus Server"),
      markdown(paste0("
### Quick Start with Docker

The easiest way to run a Tus server for testing:

```bash
docker run -p 1080:1080 -v $(pwd)/tus-data:/data tusproject/tusd:latest \\
  -upload-dir=/data -behind-proxy
```

### Alternative: Install tusd

Download from: [github.com/tus/tusd/releases](https://github.com/tus/tusd/releases)

```bash
tusd -upload-dir=./tus-data -port=1080 -behind-proxy
```

### Current Configuration

**Tus Endpoint:** `", TUS_ENDPOINT, "`

", if (TUS_ENDPOINT == "http://localhost:1080/files/") {
  "⚠️ **Make sure tusd is running before uploading files!**"
} else {
  "✅ Using custom TUS_ENDPOINT from environment variable"
}, "

---

### Why Tus?

- ✅ **Resumable**: Survives connection loss, browser crashes
- ✅ **Chunked**: Files split into 5MB chunks (constant memory usage)
- ✅ **Fast**: Efficient for large files (tested with multi-GB files)
- ✅ **Production-ready**: Battle-tested protocol used by Vimeo, Transloadit, etc.
      "))
    ),

    # Upload Interface
    card(
      card_header("Upload Large Files"),
      uppy_input(
        "large_files",
        upload_mode = "tus",
        tus_endpoint = TUS_ENDPOINT,
        max_file_size = 10 * 1024 * 1024 * 1024,  # 10GB limit
        height = 450,
        note = "Tus mode: Files are chunked and resumable"
      ),
      hr(),
      layout_column_wrap(
        width = "200px",
        actionButton("reset", "Clear Uploads", class = "btn-outline-secondary"),
        actionButton("info", "How it Works", class = "btn-outline-info")
      )
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

      # Tus Details
      card(
        card_header("Tus Upload Details"),
        verbatimTextOutput("tus_info")
      )
    )
  )
)

server <- function(input, output, session) {

  # Check if tusd is accessible
  observe({
    tusd_check <- tryCatch({
      req <- httr2::request(TUS_ENDPOINT) |>
        httr2::req_timeout(5)
      resp <- httr2::req_perform(req)
      httr2::resp_is_error(resp)
    }, error = function(e) TRUE)

    if (tusd_check) {
      showNotification(
        HTML(paste0(
          "⚠️ Cannot connect to Tus server at ", TUS_ENDPOINT, "<br>",
          "Make sure tusd is running!<br>",
          "<code>docker run -p 1080:1080 tusproject/tusd:latest</code>"
        )),
        type = "warning",
        duration = NULL,
        id = "tusd_warning"
      )
    } else {
      removeNotification("tusd_warning")
      showNotification(
        paste0("✅ Connected to Tus server at ", TUS_ENDPOINT),
        type = "message",
        duration = 3
      )
    }
  })

  # Display uploaded files info
  output$files_table <- renderTable({
    req(input$large_files)

    data.frame(
      Filename = input$large_files$name,
      Size = sprintf("%.2f MB", input$large_files$size / 1024 / 1024),
      Type = input$large_files$type,
      stringsAsFactors = FALSE
    )
  })

  # Display Tus upload details
  output$tus_info <- renderPrint({
    req(input$large_files)

    cat("Files uploaded via Tus protocol:\n")
    cat("Count:", nrow(input$large_files), "\n\n")

    cat("How Tus worked:\n")
    cat("1. Files chunked into 5MB pieces\n")
    cat("2. Uploaded to tusd server\n")
    cat("3. Downloaded back to R\n")
    cat("4. Available as temp files\n\n")

    cat("Local file paths:\n")
    for(i in seq_len(nrow(input$large_files))) {
      cat(sprintf("[%d] %s\n    -> %s\n    (%s)\n",
                  i,
                  input$large_files$name[i],
                  input$large_files$datapath[i],
                  if (file.exists(input$large_files$datapath[i])) "✓ exists" else "✗ missing"))
    }

    if ("tusURL" %in% names(input$large_files)) {
      cat("\nTus URLs:\n")
      for(i in seq_len(nrow(input$large_files))) {
        cat(sprintf("  %s\n", input$large_files$tusURL[i]))
      }
    }

    cat("\nTotal size:",
        sprintf("%.2f MB", sum(input$large_files$size) / 1024 / 1024),
        "\n")
  })

  # Reset button
  observeEvent(input$reset, {
    uppy_reset("large_files", session)
    showNotification("Uploads cleared", type = "message")
  })

  # Info button
  observeEvent(input$info, {
    showModal(modalDialog(
      title = "How Tus Upload Works",
      markdown("
### Upload Flow

1. **Browser**: Uppy chunks your files into 5MB pieces
2. **Tus Server**: Each chunk uploaded individually (resumable)
3. **R/Shiny**: Downloads completed file from Tus server
4. **Your App**: File available as temp file (`datapath`)

### Benefits

- **Large Files**: Tested with multi-GB files
- **Resumable**: Connection lost? Pick up where you left off
- **Memory Efficient**: Constant memory usage (5MB chunks)
- **Progress Tracking**: See real-time upload progress

### Production Use

For production, you can:
- Run tusd on a dedicated server
- Use Transloadit (hosted Tus + file processing)
- Configure tusd to save directly to S3, GCS, or Azure

See: [tus.io](https://tus.io/)
      "),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

shinyApp(ui, server)
