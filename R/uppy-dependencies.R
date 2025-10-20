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
  list(
    uppy_dependency(),
    uppy_shiny_dependency()
  )
}
