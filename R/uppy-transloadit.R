#' Configure Transloadit Companion for cloud storage sources
#'
#' @description
#' Helper function to configure Transloadit Companion authentication for
#' cloud storage providers like Dropbox, Google Drive, and OneDrive.
#'
#' @param key Character. Your Transloadit API key. Required.
#' @param credentials_name Character. The name of your stored credentials in
#'   Transloadit. Required for Dropbox.
#' @param companion_url Character. URL of the Companion server. Default is
#'   Transloadit's hosted Companion at "https://api2.transloadit.com/companion".
#'
#' @return A list of Transloadit configuration options
#' @export
#'
#' @examples
#' \dontrun{
#' # Configure for Dropbox
#' uppy_transloadit(
#'   key = "YOUR_TRANSLOADIT_KEY",
#'   credentials_name = "my-dropbox-creds"
#' )
#' }
uppy_transloadit <- function(key,
                             credentials_name = NULL,
                             companion_url = "https://api2.transloadit.com/companion") {
  if (missing(key) || is.null(key) || key == "") {
    stop("Transloadit 'key' is required", call. = FALSE)
  }

  structure(
    list(
      key = key,
      credentialsName = credentials_name,
      companionUrl = companion_url,
      companionAllowedHosts = "https://api2[-.]transloadit\\\\.com/"
    ),
    class = "uppy_transloadit"
  )
}

#' Configure Uppy plugins for cloud storage and other sources
#'
#' @description
#' Helper function to enable and configure Uppy plugins for various file sources
#' including cloud storage providers (Dropbox, Google Drive, OneDrive) and
#' other input methods (webcam, URL import, etc.).
#'
#' @param dropbox Configuration for Dropbox plugin. Use `uppy_transloadit()` to
#'   configure with Transloadit Companion, or FALSE to disable. Default is NULL (disabled).
#' @param google_drive Configuration for Google Drive plugin. Use `uppy_transloadit()`
#'   or a custom config list, or FALSE to disable. Default is NULL (disabled).
#' @param onedrive Configuration for OneDrive plugin. Use `uppy_transloadit()`
#'   or a custom config list, or FALSE to disable. Default is NULL (disabled).
#' @param box Configuration for Box plugin. Use `uppy_transloadit()` to
#'   configure with Transloadit Companion, or FALSE to disable. Default is NULL (disabled).
#' @param url Logical or list. Enable URL import plugin. Default is FALSE.
#' @param webcam Logical or list. Enable webcam capture. Default is FALSE.
#' @param audio Logical or list. Enable audio recording. Default is FALSE.
#' @param screen_capture Logical or list. Enable screen capture. Default is FALSE.
#'
#' @return A list of plugin configurations
#' @export
#'
#' @examples
#' \dontrun{
#' # Enable Dropbox with Transloadit
#' uppy_plugins(
#'   dropbox = uppy_transloadit(
#'     key = "YOUR_KEY",
#'     credentials_name = "my-dropbox"
#'   )
#' )
#'
#' # Enable multiple plugins
#' uppy_plugins(
#'   dropbox = uppy_transloadit(key = "KEY1", credentials_name = "dropbox-creds"),
#'   google_drive = uppy_transloadit(key = "KEY1", credentials_name = "gdrive-creds"),
#'   box = uppy_transloadit(key = "KEY1", credentials_name = "box-creds"),
#'   url = TRUE,
#'   webcam = TRUE
#' )
#' }
uppy_plugins <- function(dropbox = NULL,
                         google_drive = NULL,
                         onedrive = NULL,
                         box = NULL,
                         url = FALSE,
                         webcam = FALSE,
                         audio = FALSE,
                         screen_capture = FALSE) {

  plugins <- list()

  # Cloud storage plugins
  if (!is.null(dropbox) && !isFALSE(dropbox)) {
    plugins$Dropbox <- if (inherits(dropbox, "uppy_transloadit")) {
      list(
        companionUrl = dropbox$companionUrl,
        companionAllowedHosts = dropbox$companionAllowedHosts,
        companionKeysParams = list(
          key = dropbox$key,
          credentialsName = dropbox$credentialsName
        )
      )
    } else {
      dropbox
    }
  }

  if (!is.null(google_drive) && !isFALSE(google_drive)) {
    plugins$GoogleDrive <- if (inherits(google_drive, "uppy_transloadit")) {
      list(
        companionUrl = google_drive$companionUrl,
        companionAllowedHosts = google_drive$companionAllowedHosts,
        companionKeysParams = list(
          key = google_drive$key,
          credentialsName = google_drive$credentialsName
        )
      )
    } else {
      google_drive
    }
  }

  if (!is.null(onedrive) && !isFALSE(onedrive)) {
    plugins$OneDrive <- if (inherits(onedrive, "uppy_transloadit")) {
      list(
        companionUrl = onedrive$companionUrl,
        companionAllowedHosts = onedrive$companionAllowedHosts,
        companionKeysParams = list(
          key = onedrive$key,
          credentialsName = onedrive$credentialsName
        )
      )
    } else {
      onedrive
    }
  }

  if (!is.null(box) && !isFALSE(box)) {
    plugins$Box <- if (inherits(box, "uppy_transloadit")) {
      list(
        companionUrl = box$companionUrl,
        companionAllowedHosts = box$companionAllowedHosts,
        companionKeysParams = list(
          key = box$key,
          credentialsName = box$credentialsName
        )
      )
    } else {
      box
    }
  }

  # Other plugins
  if (!isFALSE(url)) {
    plugins$Url <- if (is.list(url)) url else list()
  }

  if (!isFALSE(webcam)) {
    plugins$Webcam <- if (is.list(webcam)) webcam else list()
  }

  if (!isFALSE(audio)) {
    plugins$Audio <- if (is.list(audio)) audio else list()
  }

  if (!isFALSE(screen_capture)) {
    plugins$ScreenCapture <- if (is.list(screen_capture)) screen_capture else list()
  }

  structure(plugins, class = "uppy_plugins")
}
