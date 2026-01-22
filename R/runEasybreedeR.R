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
run_easybreedeR <- function(host = "0.0.0.0", port = NULL) {
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
  # Get local IP for display
  local_ip <- tryCatch({
    env_host <- Sys.getenv("EASYBREEDER_HOST", "")
    if (nzchar(env_host)) {
      env_host
    } else if (.Platform$OS.type == "unix" && Sys.info()["sysname"] == "Darwin") {
      system("ipconfig getifaddr en0 2>/dev/null || ipconfig getifaddr en1 2>/dev/null || echo '127.0.0.1'", intern = TRUE)[1]
    } else if (.Platform$OS.type == "unix") {
      system("hostname -I 2>/dev/null | awk '{print $1}' || echo '127.0.0.1'", intern = TRUE)[1]
    } else {
      "127.0.0.1"
    }
  }, error = function(e) "127.0.0.1")
  local_ip <- trimws(local_ip)
  
  shiny::runApp(app_dir, host = host, port = port, launch.browser = TRUE)
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
run_easyblup <- function(host = "0.0.0.0", port = NULL) {
  app_dir <- system.file("easyblup", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "easyblup")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("easyblup application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, host = host, port = port, launch.browser = TRUE)
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
run_pedivieweR <- function(host = "0.0.0.0", port = NULL) {
  app_dir <- system.file("pedivieweR", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "pedivieweR")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("pedivieweR application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, host = host, port = port, launch.browser = TRUE)
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
run_dataprevieweR <- function(host = "0.0.0.0", port = NULL) {
  app_dir <- system.file("dataprevieweR", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "dataprevieweR")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("dataprevieweR application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, host = host, port = port, launch.browser = TRUE)
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
run_rcw <- function(host = "0.0.0.0", port = NULL) {
  app_dir <- system.file("RCW", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "RCW")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("RCW application not found. Please reinstall the package or run from package root.")
    }
  }
  shiny::runApp(app_dir, host = host, port = port, launch.browser = TRUE)
}

