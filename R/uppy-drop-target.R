#' Create an Uppy drop target for drag-and-drop file uploads
#'
#' @description
#' Enables drag-and-drop file upload functionality on specific elements in your
#' Shiny app, such as maps, plots, or any output element. Files dropped on the
#' target element will be captured and made available via a reactive input.
#'
#' This is perfect for intuitive workflows like:
#' - Dragging a GeoJSON file onto a Leaflet map
#' - Dragging a CSV onto a data table
#' - Dragging images onto a gallery area
#'
#' @param input_id Character. The input slot that will be used to access dropped files.
#' @param target Character. The CSS selector or Shiny output ID for the drop target.
#'   Can be:
#'   - A Shiny output ID: `"my_leaflet_map"` (automatically converted to `#my_leaflet_map`)
#'   - A CSS selector: `"#my_map"`, `".drop-zone"`, etc.
#'   - Special value `"body"` to target the entire page
#' @param config Configuration object created by `uppy_config()`. If NULL, uses
#'   default settings. Can also pass individual config parameters via `...`.
#' @param style Styling configuration created by `uppy_style()`. Default is NULL.
#' @param on_drop_message Character. Message to show when files are dropped (optional).
#' @param ... Additional arguments passed to `uppy_config()` if `config` is NULL.
#'
#' @details
#' Unlike `uppy_input()` which creates a visible Dashboard UI, `uppy_drop_target()`
#' creates an invisible Uppy instance that only activates when files are dropped
#' on the specified target element.
#'
#' The target element will show visual feedback when files are dragged over it
#' (configurable via CSS class `.uppy-is-drag-over`).
#'
#' Dropped files are available as a reactive value via `input$<input_id>` in the
#' same format as `uppy_input()` and `fileInput()`.
#'
#' @return A Shiny UI element (tagList) containing the Uppy drop target configuration
#' @export
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(bslib)
#' library(leaflet)
#' library(uppy)
#'
#' ui <- page_fluid(
#'   titlePanel("Drag & Drop Files onto the Map"),
#'
#'   # Add drop target for the map
#'   uppy_drop_target(
#'     input_id = "map_files",
#'     target = "my_map",  # Just the output ID!
#'     allowed_file_types = c(".geojson", ".kml", ".gpx"),
#'     on_drop_message = "Processing your geographic file..."
#'   ),
#'
#'   leafletOutput("my_map", height = 600),
#'   verbatimTextOutput("file_info")
#' )
#'
#' server <- function(input, output, session) {
#'   output$my_map <- renderLeaflet({
#'     leaflet() %>% addTiles()
#'   })
#'
#'   observeEvent(input$map_files, {
#'     files <- uppy_process_files(input$map_files)
#'     print(files)
#'     # Load and display the file on the map...
#'   })
#' }
#'
#' shinyApp(ui, server)
#'
#' # Multiple drop targets
#' ui <- page_fluid(
#'   uppy_drop_target("csv_files", target = "data_table",
#'                    allowed_file_types = ".csv"),
#'   uppy_drop_target("image_files", target = "gallery",
#'                    allowed_file_types = c(".jpg", ".png")),
#'   # ... outputs ...
#' )
#' }
uppy_drop_target <- function(input_id,
                              target,
                              config = NULL,
                              style = NULL,
                              on_drop_message = NULL,
                              ...) {

  # Normalize target to CSS selector
  target_selector <- if (target == "body") {
    "body"
  } else if (grepl("^[#.]", target)) {
    # Already a CSS selector (starts with # or .)
    target
  } else {
    # Assume it's a Shiny output ID, prepend #
    paste0("#", target)
  }

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
  instance_name <- paste0("uppy_drop_", gsub("[^a-zA-Z0-9]", "_", input_id))

  # Build Uppy configuration JavaScript
  uppy_opts <- list(
    debug = FALSE,
    autoProceed = TRUE  # Auto-upload when files are dropped
  )

  if (!is.null(config$restrictions)) {
    uppy_opts$restrictions <- config$restrictions
  }

  # Generate custom CSS if style is provided
  custom_css <- ""
  if (!is.null(style) && inherits(style, "uppy_style")) {
    # Add drop target specific styling
    custom_css <- sprintf(
      "
      %s {
        transition: all 0.3s ease;
        position: relative;
      }
      %s.uppy-is-drag-over {
        outline: 3px dashed %s !important;
        outline-offset: -3px;
        background-color: rgba(%s, 0.05) !important;
        z-index: 1;
      }
      %s.uppy-is-drag-over * {
        pointer-events: none;
      }
      ",
      target_selector,
      target_selector,
      if (!is.null(style$primary_color)) style$primary_color else "#1B4332",
      if (!is.null(style$primary_color)) {
        # Convert hex to RGB for transparency
        paste(as.integer(strtoi(substring(gsub("#", "", style$primary_color), c(1, 3, 5), c(2, 4, 6)), 16)), collapse = ", ")
      } else "27, 67, 50",
      target_selector
    )
  } else {
    # Default drop target styling
    custom_css <- sprintf(
      "
      %s {
        transition: all 0.3s ease;
        position: relative;
      }
      %s.uppy-is-drag-over {
        outline: 3px dashed #1B4332 !important;
        outline-offset: -3px;
        background-color: rgba(27, 67, 50, 0.05) !important;
        z-index: 1;
      }
      %s.uppy-is-drag-over * {
        pointer-events: none;
      }
      ",
      target_selector,
      target_selector,
      target_selector
    )
  }

  # Build JavaScript initialization code
  js_code <- sprintf(
    "
    $(document).ready(function() {
      // Wait for target element to exist
      function initDropTarget() {
        const targetEl = document.querySelector('%s');
        if (!targetEl) {
          // Target not ready yet, try again in 100ms
          setTimeout(initDropTarget, 100);
          return;
        }

        // Initialize Uppy
        const uppy = new Uppy.Uppy(%s);

        // Add DropTarget plugin
        uppy.use(Uppy.DropTarget, {
          target: targetEl,
          onDragOver: (event) => {
            event.stopPropagation();
            event.preventDefault();
            targetEl.classList.add('uppy-is-drag-over');
          },
          onDragLeave: (event) => {
            event.stopPropagation();
            event.preventDefault();
            // Only remove if we're actually leaving the target element
            if (!targetEl.contains(event.relatedTarget)) {
              targetEl.classList.remove('uppy-is-drag-over');
            }
          },
          onDrop: (event) => {
            event.stopPropagation();
            event.preventDefault();
            targetEl.classList.remove('uppy-is-drag-over');
            %s
          }
        });

        // Store uppy instance globally
        window['%s'] = uppy;

        // Handle complete event
        uppy.on('complete', (result) => {
          console.log('Drop upload complete:', result);

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
                  Shiny.setInputValue('%s', {
                    files: filesData,
                    timestamp: Date.now()
                  }, {priority: 'event'});
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
      }

      initDropTarget();
    });
    ",
    target_selector,
    jsonlite::toJSON(uppy_opts, auto_unbox = TRUE),
    if (!is.null(on_drop_message)) {
      sprintf("Shiny.notifications.show({message: '%s', type: 'default', duration: 2000});",
              gsub("'", "\\\\'", on_drop_message))
    } else "",
    instance_name,
    input_id
  )

  # Build UI
  htmltools::tagList(
    uppy_dependencies(),
    if (nchar(custom_css) > 0) {
      htmltools::tags$style(htmltools::HTML(custom_css))
    },
    htmltools::tags$script(htmltools::HTML(js_code))
  )
}
