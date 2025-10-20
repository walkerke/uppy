#' Configure Uppy file restrictions and dashboard settings
#'
#' @description
#' Helper function to configure Uppy's core settings including file restrictions,
#' dashboard appearance, and behavior.
#'
#' @param max_file_size Maximum file size in bytes. Default is NULL (no limit).
#' @param max_number_of_files Maximum number of files that can be uploaded.
#'   Default is NULL (no limit).
#' @param min_number_of_files Minimum number of files required. Default is NULL.
#' @param allowed_file_types Character vector of allowed file types/extensions
#'   (e.g., `c(".pdf", ".docx", "image/*")`). Default is NULL (all types allowed).
#' @param height Dashboard height in pixels. Default is 350.
#' @param width Dashboard width. Can be numeric (pixels) or character ("100%").
#'   Default is "100%".
#' @param inline Logical. Whether to show the dashboard inline (TRUE) or as a
#'   modal (FALSE). Default is TRUE.
#' @param theme Character. Dashboard theme: "light", "dark", or "auto". Default is "light".
#' @param note Character. Note text to display in the dashboard. Default is NULL.
#' @param auto_proceed Logical. Whether to automatically process files after they
#'   are selected. When TRUE, files are immediately available in Shiny.
#'   When FALSE (default, recommended), user must click the "Upload" button to batch-process files.
#'   Default is FALSE for stability with large file counts.
#' @param show_progress_details Logical. Show detailed progress information.
#'   Default is TRUE.
#' @param show_remove_button_after_complete Logical. Show remove button after
#'   upload completes. Default is TRUE.
#' @param hide_upload_button Logical. Hide the upload button. Default is FALSE.
#' @param proudly_display_powered_by_uppy Logical. Show "Powered by Uppy" badge.
#'   Default is FALSE.
#'
#' @return A list of configuration options for Uppy
#' @export
#'
#' @examples
#' \dontrun{
#' uppy_config(
#'   max_file_size = 50 * 1024 * 1024,  # 50MB
#'   allowed_file_types = c(".pdf", ".docx"),
#'   height = 400,
#'   note = "Upload your documents here"
#' )
#' }
uppy_config <- function(max_file_size = NULL,
                        max_number_of_files = NULL,
                        min_number_of_files = NULL,
                        allowed_file_types = NULL,
                        height = 350,
                        width = "100%",
                        inline = TRUE,
                        theme = c("light", "dark", "auto"),
                        note = NULL,
                        auto_proceed = FALSE,
                        show_progress_details = TRUE,
                        show_remove_button_after_complete = TRUE,
                        hide_upload_button = NULL,
                        proudly_display_powered_by_uppy = FALSE) {

  theme <- match.arg(theme)

  # Auto-hide upload button when auto_proceed is TRUE (immediate mode)
  # Keep button visible by default (FALSE) for stable batch uploads
  if (is.null(hide_upload_button)) {
    hide_upload_button <- auto_proceed
  }

  # Build restrictions object
  restrictions <- list()
  if (!is.null(max_file_size)) restrictions$maxFileSize <- max_file_size
  if (!is.null(max_number_of_files)) restrictions$maxNumberOfFiles <- max_number_of_files
  if (!is.null(min_number_of_files)) restrictions$minNumberOfFiles <- min_number_of_files
  if (!is.null(allowed_file_types)) restrictions$allowedFileTypes <- as.list(allowed_file_types)

  # Build dashboard options
  dashboard <- list(
    height = height,
    width = width,
    inline = inline,
    theme = theme,
    showProgressDetails = show_progress_details,
    showRemoveButtonAfterComplete = show_remove_button_after_complete,
    hideUploadButton = hide_upload_button,
    proudlyDisplayPoweredByUppy = proudly_display_powered_by_uppy
  )

  if (!is.null(note)) dashboard$note <- note

  structure(
    list(
      restrictions = if (length(restrictions) > 0) restrictions else NULL,
      dashboard = dashboard,
      autoProceed = auto_proceed
    ),
    class = "uppy_config"
  )
}
