#' Launch the easybreedeR Studio (suite launcher)
#'
#' Main entry point for the easybreedeR Shiny application suite. Provides
#' access to all sub-applications (easyblup, pedivieweR, dataprevieweR, RCW).
#'
#' @name run_easybreedeR
#' @export
#' @return A Shiny app object (invisibly returned by `shiny::runApp`).
#' @examples
#' \dontrun{
#'   run_easybreedeR()            # launch suite
#' }
run_easybreedeR <- function() {
  # Try to find app in installed package first
  app_dir <- system.file("easybreedeR_Studio", package = "easybreedeR")
  # Fallback to development location
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "easybreedeR_Studio")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("easybreedeR_Studio application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}

#' Launch easyblup application
#'
#' @description
#' Launches the easyblup application for BLUP/REML parameter generation
#' with AI assistant features.
#'
#' @export
#' @examples
#' \dontrun{
#'   run_easyblup()
#' }
run_easyblup <- function() {
  app_dir <- system.file("easyblup", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "easyblup")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("easyblup application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}

#' Launch pedivieweR application
#'
#' @description
#' Launches the pedivieweR application for pedigree visualization
#' and quality control.
#'
#' @export
#' @examples
#' \dontrun{
#'   run_pedivieweR()
#' }
run_pedivieweR <- function() {
  app_dir <- system.file("pedivieweR", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "pedivieweR")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("pedivieweR application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}

#' Launch dataprevieweR application
#'
#' @description
#' Launches the dataprevieweR application for data preview and exploration.
#'
#' @export
#' @examples
#' \dontrun{
#'   run_dataprevieweR()
#' }
run_dataprevieweR <- function() {
  app_dir <- system.file("dataprevieweR", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "dataprevieweR")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("dataprevieweR application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}

#' Launch RCW (R Canvas Workflow) application
#'
#' @description
#' Launches the RCW application for visual R script pipeline management.
#'
#' @export
#' @examples
#' \dontrun{
#'   run_rcw()
#' }
run_rcw <- function() {
  app_dir <- system.file("RCW", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "RCW")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("RCW application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, launch.browser = TRUE)
}

