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
    version = "5.1.7",
    src = c(file = system.file("www/uppy", package = "uppy")),
    script = "uppy.min.mjs",
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

      # Check if this is Tus mode (files have tusURL field)
      is_tus_mode <- !is.null(data[[1]]$tusURL) && !is.null(data[[1]]$uploadMode) &&
                     data[[1]]$uploadMode == "tus"

      if (is_tus_mode) {
        # Tus mode: download files from Tus server
        tus_urls <- sapply(data, function(f) f$tusURL)
        file_names <- sapply(data, function(f) f$name)
        file_sizes <- sapply(data, function(f) as.numeric(f$size))
        file_types <- sapply(data, function(f) f$type)

        files_df <- download_tus_files(tus_urls, file_names, file_sizes, file_types)
        return(files_df)
      }

      # Base64 mode: process each file's base64 data
      files_list <- lapply(data, function(file) {
        tryCatch({
          # Decode base64 and write to temp file
          if (!is.null(file$data) && grepl("^data:", file$data)) {
            base64_data <- sub("^data:[^,]+,", "", file$data)

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
