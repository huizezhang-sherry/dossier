#' Edit Decisions in Shiny App
#'
#' This function launches a Shiny app that allows users to edit decisions.
#'
#' @return No return value, called for side effects.
#' @export
edit_decisions <- function(){
  appDir <- system.file("shiny", package = "dossier")
  if (appDir == "") {
    stop("Could not find app directory. Try re-installing `dossier`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
