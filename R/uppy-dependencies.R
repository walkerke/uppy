#' Uppy HTML dependencies
#'
#' @description
#' Creates the HTML dependencies for Uppy.js. This function is called
#' automatically by `uppy_input()` and typically doesn't need to be
#' called directly.
#'
#' @return An [htmltools::htmlDependency()] object
#' @keywords internal
#' @export
uppy_dependency <- function() {
  htmltools::htmlDependency(
    name = "uppy",
    version = "3.21.0",
    src = c(file = system.file("www/uppy", package = "uppy")),
    script = "uppy.min.js",
    stylesheet = "uppy.min.css"
  )
}

#' Uppy Shiny binding dependencies
#'
#' @description
#' Creates the HTML dependencies for the Uppy Shiny input binding.
#' This function is called automatically by `uppy_input()` and typically
#' doesn't need to be called directly.
#'
#' @return An [htmltools::htmlDependency()] object
#' @keywords internal
#' @export
uppy_shiny_dependency <- function() {
  htmltools::htmlDependency(
    name = "uppy-shiny",
    version = "0.1.0",
    src = c(file = system.file("www", package = "uppy")),
    script = "uppy-shiny.js"
  )
}

#' All Uppy dependencies
#'
#' @description
#' Returns a list of all HTML dependencies required for Uppy.
#'
#' @return A list of [htmltools::htmlDependency()] objects
#' @keywords internal
uppy_dependencies <- function() {
  # Register input handler for processing uploaded files
  .onLoad_uppy()

  list(
    uppy_dependency(),
    uppy_shiny_dependency()
  )
}

#' Register Shiny input handler for Uppy
#' @keywords internal
.onLoad_uppy <- function() {
  # Only register once
  if (!"uppy.files" %in% names(shiny:::.globals$inputHandlers)) {
    shiny::registerInputHandler("uppy.files", function(data, shinysession, name) {
      if (is.null(data) || length(data) == 0) {
        return(NULL)
      }

      # Process each file's base64 data
      files_list <- lapply(data, function(file) {
        tryCatch({
          # Decode base64 and write to temp file
          if (!is.null(file$datapath) && grepl("^data:", file$datapath)) {
            base64_data <- sub("^data:[^,]+,", "", file$datapath)

            # Create temp file
            ext <- tools::file_ext(file$name)
            if (nchar(ext) > 0) ext <- paste0(".", ext)
            temp_path <- tempfile(fileext = ext)

            # Write decoded data
            writeBin(base64enc::base64decode(base64_data), temp_path)

            # Return fileInput-compatible structure
            data.frame(
              name = file$name,
              size = as.numeric(file$size),
              type = file$type,
              datapath = temp_path,
              stringsAsFactors = FALSE
            )
          } else {
            NULL
          }
        }, error = function(e) {
          warning("Error processing file ", file$name, ": ", e$message)
          NULL
        })
      })

      # Combine into dataframe
      files_df <- do.call(rbind, Filter(Negate(is.null), files_list))

      if (is.null(files_df) || nrow(files_df) == 0) {
        return(NULL)
      }

      return(files_df)
    }, force = TRUE)
  }
}
