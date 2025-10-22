#' Download files from Tus server
#'
#' @description
#' Downloads files from a Tus server to temporary local files.
#' Used internally when files are uploaded via Tus protocol.
#'
#' @param tus_urls Character vector of Tus upload URLs
#' @param file_names Character vector of original file names
#' @param file_sizes Numeric vector of file sizes
#' @param file_types Character vector of file MIME types
#' @param temp_dir Character. Directory for temporary files. Default is `tempdir()`.
#'
#' @return A data frame with columns: name, size, type, datapath, tusURL
#' @keywords internal
download_tus_files <- function(tus_urls, file_names, file_sizes, file_types, temp_dir = tempdir()) {
  if (length(tus_urls) == 0) {
    return(data.frame(
      name = character(),
      size = numeric(),
      type = character(),
      datapath = character(),
      tusURL = character(),
      stringsAsFactors = FALSE
    ))
  }

  files_list <- list()

  for (i in seq_along(tus_urls)) {
    tryCatch({
      # Create temp file with appropriate extension
      ext <- tools::file_ext(file_names[i])
      if (nchar(ext) > 0) ext <- paste0(".", ext)
      temp_path <- tempfile(tmpdir = temp_dir, fileext = ext)

      # Download file from Tus server using httr2
      req <- httr2::request(tus_urls[i]) |>
        httr2::req_timeout(300)  # 5 minute timeout

      resp <- httr2::req_perform(req, path = temp_path)

      # Add to files list
      files_list[[i]] <- data.frame(
        name = file_names[i],
        size = file_sizes[i],
        type = file_types[i],
        datapath = temp_path,
        tusURL = tus_urls[i],
        stringsAsFactors = FALSE
      )

    }, error = function(e) {
      warning("Error downloading ", file_names[i], ": ", e$message)
    })
  }

  # Combine into data frame
  if (length(files_list) == 0) {
    return(data.frame(
      name = character(),
      size = numeric(),
      type = character(),
      datapath = character(),
      tusURL = character(),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, Filter(Negate(is.null), files_list))
}
