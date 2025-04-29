#’ Launch the RAND-12 Shiny App
#’
#’ @param launch.browser Whether to open the app in your default web browser (default `TRUE`).
#’ @export
runRAND12App <- function(launch.browser = TRUE) {
  app_dir <- system.file("app", package = "Shiny.RAND12")
  if (app_dir == "" || !dir.exists(app_dir)) {
    stop("Could not find the Shiny app directory. Try reinstalling Shiny.RAND12.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser)
}
