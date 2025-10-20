#' Customize Uppy dashboard styling
#'
#' @description
#' Helper function to customize the appearance of the Uppy dashboard with
#' custom colors and styling. Generates CSS to brand the Uppy interface.
#'
#' @param primary_color Character. Primary brand color (hex code). Used for
#'   borders, progress bars, and primary UI elements. Default is "#1B4332" (dark green).
#' @param accent_color Character. Accent/action color (hex code). Used for
#'   buttons and interactive elements. Default is "#F77F00" (orange).
#' @param success_color Character. Success state color (hex code). Default is "#16A34A" (green).
#' @param background_color Character. Background color for dashboard. Default is "#ffffff" (white).
#' @param border_color Character. Border color for dashboard and file items.
#'   Default is "#e5e7eb" (light gray).
#' @param text_color Character. Primary text color. Default is "#374151" (dark gray).
#' @param secondary_text_color Character. Secondary text color. Default is "#6b7280" (gray).
#' @param border_radius Character or numeric. Border radius for rounded corners.
#'   Default is "8px".
#' @param box_shadow Character. CSS box-shadow property. Default is
#'   "0 2px 4px rgba(0, 0, 0, 0.1)".
#' @param custom_css Character. Additional custom CSS to inject. Default is NULL.
#'
#' @return A list with class "uppy_style" containing CSS styling configuration
#' @export
#'
#' @examples
#' \dontrun{
#' # Custom brand colors
#' uppy_style(
#'   primary_color = "#1B4332",
#'   accent_color = "#F77F00"
#' )
#'
#' # With additional custom CSS
#' uppy_style(
#'   primary_color = "#2563eb",
#'   accent_color = "#dc2626",
#'   custom_css = "
#'     .uppy-Dashboard-AddFiles {
#'       font-size: 1.2rem;
#'     }
#'   "
#' )
#' }
uppy_style <- function(primary_color = "#1B4332",
                       accent_color = "#F77F00",
                       success_color = "#16A34A",
                       background_color = "#ffffff",
                       border_color = "#e5e7eb",
                       text_color = "#374151",
                       secondary_text_color = "#6b7280",
                       border_radius = "8px",
                       box_shadow = "0 2px 4px rgba(0, 0, 0, 0.1)",
                       custom_css = NULL) {

  structure(
    list(
      primary_color = primary_color,
      accent_color = accent_color,
      success_color = success_color,
      background_color = background_color,
      border_color = border_color,
      text_color = text_color,
      secondary_text_color = secondary_text_color,
      border_radius = border_radius,
      box_shadow = box_shadow,
      custom_css = custom_css
    ),
    class = "uppy_style"
  )
}

#' Generate CSS from uppy_style configuration
#'
#' @param style An uppy_style object created by `uppy_style()`
#' @param scope Optional CSS selector to scope the styles to a specific instance
#' @return Character string containing CSS
#' @keywords internal
generate_uppy_css <- function(style, scope = NULL) {
  if (is.null(style) || !inherits(style, "uppy_style")) {
    return("")
  }

  # Helper to lighten color for hover effects
  lighten_color <- function(color, amount = 0.1) {
    # Simple approach - just use a slightly different shade
    # In production, you might want a more sophisticated color manipulation
    color
  }

  # Create scoped selector
  scoped <- function(selector) {
    if (!is.null(scope)) {
      paste0(scope, " ", selector)
    } else {
      selector
    }
  }

  css <- sprintf(
    "
    /* Dashboard container */
    %s {
      border-radius: %s;
      box-shadow: %s;
      border: 1px solid %s;
    }

    %s {
      background: %s;
    }

    /* Browse/Add Files button */
    %s {
      color: %s !important;
      border-color: %s !important;
    }

    %s:hover {
      background-color: %s !important;
      color: white !important;
    }

    /* Upload button */
    %s,
    %s {
      background-color: %s !important;
      border-color: %s !important;
      color: #fff !important;
    }

    %s:hover,
    %s:hover {
      background-color: %s !important;
      opacity: 0.9;
    }

    /* Progress indicators */
    %s {
      background: #f8f9fa;
      border-top: 2px solid %s;
    }

    /* File items */
    %s {
      border-radius: 6px;
      border: 1px solid %s;
    }

    %s {
      border-color: %s;
    }

    %s {
      background-color: %s !important;
    }

    /* Success state */
    %s {
      background-color: %s !important;
    }

    /* Drag and drop area */
    %s {
      border: 2px dashed %s;
      background: #fafafa;
    }

    %s {
      border-color: %s;
      background: rgba(%s, 0.05);
    }

    /* Tabs for cloud providers */
    %s {
      color: %s;
    }

    %s:hover {
      color: %s;
    }

    %s {
      color: %s;
      border-bottom-color: %s;
    }

    /* Progress bar */
    %s {
      background: linear-gradient(to right, %s, %s) !important;
    }

    %s {
      color: %s;
      font-weight: 600;
    }

    /* Status text */
    %s {
      color: %s;
      font-size: 0.875rem;
    }

    /* Note text */
    %s {
      color: %s;
      font-size: 0.875rem;
      margin-top: 0.5rem;
    }

    %s
    ",
    scoped(".uppy-Dashboard"),
    style$border_radius,
    style$box_shadow,
    style$border_color,
    scoped(".uppy-Dashboard-inner"),
    style$background_color,
    scoped(".uppy-Dashboard-browse"),
    style$primary_color,
    style$primary_color,
    scoped(".uppy-Dashboard-browse"),
    style$primary_color,
    scoped(".uppy-Dashboard-upload"),
    scoped(".uppy-StatusBar.is-waiting .uppy-StatusBar-actionBtn--upload"),
    style$accent_color,
    style$accent_color,
    scoped(".uppy-Dashboard-upload"),
    scoped(".uppy-StatusBar.is-waiting .uppy-StatusBar-actionBtn--upload"),
    style$accent_color,
    scoped(".uppy-Dashboard-progressindicators"),
    style$primary_color,
    scoped(".uppy-Dashboard-Item"),
    style$border_color,
    scoped(".uppy-Dashboard-Item.is-inprogress"),
    style$accent_color,
    scoped(".uppy-Dashboard-Item-progress"),
    style$primary_color,
    scoped(".uppy-Dashboard-Item.is-complete .uppy-Dashboard-Item-progress"),
    style$success_color,
    scoped(".uppy-Dashboard-AddFiles"),
    style$border_color,
    scoped(".uppy-Dashboard.uppy-Dashboard--isDraggingOver .uppy-Dashboard-AddFiles"),
    style$primary_color,
    style$primary_color,
    scoped(".uppy-DashboardTab"),
    style$secondary_text_color,
    scoped(".uppy-DashboardTab"),
    style$primary_color,
    scoped(".uppy-DashboardTab.is-active"),
    style$primary_color,
    style$primary_color,
    scoped(".uppy-ProgressBar-inner"),
    style$primary_color,
    style$success_color,
    scoped(".uppy-ProgressBar-percentage"),
    style$primary_color,
    scoped(".uppy-Dashboard-Item-status"),
    style$secondary_text_color,
    scoped(".uppy-Dashboard-note"),
    style$secondary_text_color,
    if (!is.null(style$custom_css)) style$custom_css else ""
  )

  return(css)
}
