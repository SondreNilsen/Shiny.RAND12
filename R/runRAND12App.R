#' Launch the RAND-12 Shiny App
#'
#' This function starts the Shiny application bundled with the package.
#'
#' @export
runRAND12App <- function() {
  app_dir <- system.file("app", package = "Shiny.RAND12")
  if (app_dir == "") {
    stop("Could not find Shiny app directory. Re-install the Shiny.RAND12 package.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
