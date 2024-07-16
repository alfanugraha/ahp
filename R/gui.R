#' Shows a Shiny Application that lets you specify AHP models and view the results.
#'
#' @inheritParams shiny::runApp
#'
#' @export
ahp_app <- function(port = getOption("shiny.port")) {
  appDir <- system.file("gui", "shiny", package = "ahp")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `ahp`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", port = port)
}
