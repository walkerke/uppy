#' Create an Uppy file input
#'
#' @description
#' Creates a modern file upload widget using Uppy.js. Provides drag-and-drop,
#' progress tracking, and optional cloud storage integration. Acts as a drop-in
#' replacement for `shiny::fileInput()` with enhanced capabilities.
#'
#' @param input_id Character. The input slot that will be used to access the value.
#' @param config Configuration object created by `uppy_config()`. If NULL, uses
#'   default settings. Can also pass individual config parameters directly (see below).
#' @param plugins Plugin configuration created by `uppy_plugins()`. Default is NULL
#'   (no cloud storage plugins).
#' @param style Styling configuration created by `uppy_style()`. Default is NULL
#'   (uses default Uppy styling).
#' @param ... Additional arguments passed to `uppy_config()` if `config` is NULL.
#'   Allows for simplified configuration without explicitly calling `uppy_config()`.
#'
#' @details
#' The uploaded files are available as a reactive value in the server function
#' via `input$<input_id>`. The value is a data frame with columns: name, size,
#' type, and datapath, matching the format of `shiny::fileInput()`.
#'
#' ## Cloud Storage Integration
#'
#' To enable cloud storage sources like Dropbox or Google Drive, you'll need:
#' 1. A Transloadit account and API key
#' 2. Configured Template Credentials in Transloadit for each provider
#' 3. Pass the configuration via the `plugins` argument
#'
#' See `vignette("cloud-storage", package = "uppy")` for detailed setup instructions.
#'
#' @return A Shiny UI element (tagList) containing the Uppy file upload widget
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(uppy)
#'
#' # Basic usage
#' ui <- fluidPage(
#'   uppy_input(
#'     "files",
#'     max_file_size = 50 * 1024 * 1024,
#'     allowed_file_types = c(".pdf", ".docx")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   observeEvent(input$files, {
#'     files <- input$files
#'     print(files)
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' # With configuration objects
#' ui <- fluidPage(
#'   uppy_input(
#'     "files",
#'     config = uppy_config(
#'       max_file_size = 100 * 1024 * 1024,
#'       allowed_file_types = c(".pdf"),
#'       height = 400,
#'       note = "Upload PDF files only"
#'     ),
#'     style = uppy_style(
#'       primary_color = "#2563eb",
#'       accent_color = "#dc2626"
#'     )
#'   )
#' )
#'
#' # With Dropbox integration
#' ui <- fluidPage(
#'   uppy_input(
#'     "files",
#'     plugins = uppy_plugins(
#'       dropbox = uppy_transloadit(
#'         key = "YOUR_TRANSLOADIT_KEY",
#'         credentials_name = "your-dropbox-creds"
#'       )
#'     )
#'   )
#' )
#' }
uppy_input <- function(input_id,
                       config = NULL,
                       plugins = NULL,
                       style = NULL,
                       ...) {

  # If config is NULL, create from ... arguments
  if (is.null(config)) {
    dots <- list(...)
    if (length(dots) > 0) {
      config <- do.call(uppy_config, dots)
    } else {
      config <- uppy_config()
    }
  }

  # Generate unique instance name
  instance_name <- paste0("uppy_", gsub("[^a-zA-Z0-9]", "_", input_id))

  # Build Uppy configuration JavaScript
  uppy_opts <- list(
    debug = FALSE,
    autoProceed = if (!is.null(config$autoProceed)) config$autoProceed else FALSE
  )

  if (!is.null(config$restrictions)) {
    uppy_opts$restrictions <- config$restrictions
  }

  # Build dashboard configuration
  dashboard_opts <- config$dashboard
  dashboard_opts$target <- paste0("#", input_id, "_uppy_container")

  # Build plugins configuration
  plugins_js <- ""
  if (!is.null(plugins) && inherits(plugins, "uppy_plugins")) {
    for (plugin_name in names(plugins)) {
      plugin_config <- plugins[[plugin_name]]
      plugin_config$target <- "Uppy.Dashboard"

      plugins_js <- paste0(
        plugins_js,
        sprintf(
          "uppy.use(Uppy.%s, %s);\n",
          plugin_name,
          jsonlite::toJSON(plugin_config, auto_unbox = TRUE)
        )
      )
    }
  }

  # Generate custom CSS if style is provided
  # Scope CSS to this specific instance using the container ID
  custom_css <- ""
  if (!is.null(style) && inherits(style, "uppy_style")) {
    scope_selector <- paste0("#", input_id)
    custom_css <- generate_uppy_css(style, scope = scope_selector)
  }

  # Build JavaScript initialization code
  js_code <- sprintf(
    "
    $(document).ready(function() {
      // Initialize Uppy
      const uppy = new Uppy.Uppy(%s);

      // Add Dashboard UI
      uppy.use(Uppy.Dashboard, %s);

      // Add Progress Bar
      uppy.use(Uppy.ProgressBar, {
        target: '#%s_uppy_progress',
        fixed: false,
        hideAfterFinish: true
      });

      %s

      // Store uppy instance globally
      window['%s'] = uppy;

      // Handle complete event
      uppy.on('complete', (result) => {
        console.log('Upload complete:', result);

        if (result.successful.length > 0) {
          let processedCount = 0;
          const totalFiles = result.successful.length;
          const filesData = [];

          result.successful.forEach(file => {
            const reader = new FileReader();
            reader.onload = function(e) {
              filesData.push({
                name: file.name,
                size: file.size,
                type: file.type || 'application/octet-stream',
                datapath: file.id,
                data: e.target.result
              });

              processedCount++;
              if (processedCount === totalFiles) {
                // Update Shiny input
                const container = document.getElementById('%s');
                $(container).data('uppy-files', filesData);
                $(container).trigger('change');
              }
            };
            reader.readAsDataURL(file.data);
          });
        }
      });

      // Handle upload errors
      uppy.on('upload-error', (file, error, response) => {
        console.error('Upload error:', file.name, error);
      });
    });
    ",
    jsonlite::toJSON(uppy_opts, auto_unbox = TRUE),
    jsonlite::toJSON(dashboard_opts, auto_unbox = TRUE),
    input_id,
    plugins_js,
    instance_name,
    input_id
  )

  # Build UI
  htmltools::tagList(
    uppy_dependencies(),
    if (nchar(custom_css) > 0) {
      htmltools::tags$style(htmltools::HTML(custom_css))
    },
    htmltools::div(
      id = input_id,
      class = "uppy-input-container",
      `data-uppy-instance` = instance_name,
      htmltools::div(id = paste0(input_id, "_uppy_container")),
      htmltools::div(
        id = paste0(input_id, "_uppy_progress"),
        style = "margin-top: 1rem;"
      ),
      htmltools::tags$script(htmltools::HTML(js_code))
    )
  )
}
