#' Process Uppy uploaded files on the server
#'
#' @description
#' Helper function to process files uploaded via Uppy and convert them to
#' a format compatible with standard Shiny file handling. This function
#' decodes base64-encoded file data and writes files to temporary locations.
#'
#' @param files_data List of file data from Uppy (typically from `input$<id>`)
#' @param temp_dir Character. Directory to write temporary files. Default is `tempdir()`.
#'
#' @return A data frame with columns: name, size, type, datapath (matching `fileInput()` format)
#' @export
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   observeEvent(input$files, {
#'     # Files are automatically processed by the input binding
#'     # but you can use this function if you need custom processing
#'     files_df <- uppy_process_files(input$files)
#'     print(files_df)
#'   })
#' }
#' }
uppy_process_files <- function(files_data, temp_dir = tempdir()) {
  if (is.null(files_data) || length(files_data) == 0) {
    return(data.frame(
      name = character(),
      size = numeric(),
      type = character(),
      datapath = character(),
      stringsAsFactors = FALSE
    ))
  }

  # Debug: check what we received
  if (!is.list(files_data)) {
    stop(sprintf(
      "files_data must be a list of file objects. Received: %s (class: %s, length: %d). Structure: %s",
      paste(head(as.character(files_data), 3), collapse = ", "),
      class(files_data)[1],
      length(files_data),
      paste(capture.output(str(files_data, max.level = 2)), collapse = "\n")
    ))
  }

  # Process each file
  files_list <- lapply(files_data, function(file) {
    tryCatch(
      {
        # Validate file structure
        if (!is.list(file)) {
          warning("Skipping non-list file object")
          return(NULL)
        }

        # Get file name and extension
        file_name <- file$name
        if (is.null(file_name) || !is.character(file_name) || length(file_name) == 0) {
          warning("File missing name property")
          return(NULL)
        }

        # Get extension safely
        ext <- tools::file_ext(file_name)
        if (nchar(ext) > 0) {
          ext <- paste0(".", ext)
        }

        # Create temporary file
        temp_path <- tempfile(tmpdir = temp_dir, fileext = ext)

        # Decode base64 data and write to file
        if (!is.null(file$data) && is.character(file$data) && grepl("^data:", file$data)) {
          # Remove data URL prefix
          base64_data <- sub("^data:[^,]+,", "", file$data)
          # Decode and write to temp file
          writeBin(base64enc::base64decode(base64_data), temp_path)

          data.frame(
            name = file_name,
            size = if (is.null(file$size)) 0 else as.numeric(file$size),
            type = if (is.null(file$type)) "application/octet-stream" else as.character(file$type),
            datapath = temp_path,
            stringsAsFactors = FALSE
          )
        } else {
          warning("File ", file_name, " missing valid data property")
          NULL
        }
      },
      error = function(e) {
        warning("Error processing file: ", e$message)
        NULL
      }
    )
  })

  # Combine into data frame
  files_df <- do.call(rbind, Filter(Negate(is.null), files_list))

  if (is.null(files_df) || nrow(files_df) == 0) {
    return(data.frame(
      name = character(),
      size = numeric(),
      type = character(),
      datapath = character(),
      stringsAsFactors = FALSE
    ))
  }

  return(files_df)
}

#' Reset/clear an Uppy input
#'
#' @description
#' Programmatically reset an Uppy file input from the server, clearing
#' all selected files.
#'
#' @param session Shiny session object
#' @param input_id Character. The input ID of the Uppy input to reset
#'
#' @return None (invisible NULL)
#' @export
#'
#' @examples
#' \dontrun{
#' server <- function(input, output, session) {
#'   observeEvent(input$reset_button, {
#'     uppy_reset("files", session)
#'   })
#' }
#' }
uppy_reset <- function(input_id, session = shiny::getDefaultReactiveDomain()) {
  instance_name <- paste0("uppy_", gsub("[^a-zA-Z0-9]", "_", input_id))

  # Clear via custom message handler
  session$sendCustomMessage(
    type = "clearUppy",
    message = list(instance = instance_name)
  )

  # Clear the Shiny input value directly
  session$sendInputMessage(
    input_id,
    list(reset = TRUE)
  )

  # Also set the input to NULL via JavaScript
  session$sendCustomMessage(
    type = "clearUppyInput",
    message = list(inputId = input_id)
  )

  invisible(NULL)
}
