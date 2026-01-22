#' Launch the easybreedeR Studio (suite launcher)
#'
#' Main entry point for the easybreedeR Shiny application suite. Provides
#' access to all sub-applications (easyblup, pedivieweR, dataprevieweR, RCW).
#'
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
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
  # Get local IP using the shared function from Global.R
  # Note: This will be available when the app loads Global.R
  local_ip <- tryCatch({
    env_host <- Sys.getenv("EASYBREEDER_HOST", "")
    if (nzchar(env_host)) {
      env_host
    } else {
      # Use the same cross-platform detection logic
      sys_name <- Sys.info()["sysname"]
      ip <- NULL
      
      if (sys_name == "Darwin") {
        for (iface in c("en0", "en1")) {
          result <- suppressWarnings(
            system2("ipconfig", c("getifaddr", iface), stdout = TRUE, stderr = FALSE)
          )
          if (length(result) > 0 && nzchar(result[1])) {
            ip <- trimws(result[1])
            if (grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ip)) break
          }
        }
      } else if (sys_name == "Linux") {
        result <- suppressWarnings(
          system2("hostname", c("-I"), stdout = TRUE, stderr = FALSE)
        )
        if (length(result) > 0 && nzchar(result[1])) {
          ip <- trimws(strsplit(result[1], "\\s+")[[1]][1])
          if (!grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ip)) ip <- NULL
        }
      } else if (sys_name == "Windows") {
        # Try PowerShell first
        ps_cmd <- "Get-NetIPAddress -AddressFamily IPv4 | Where-Object {$_.IPAddress -notlike '127.*' -and $_.IPAddress -notlike '169.254.*'} | Select-Object -First 1 -ExpandProperty IPAddress"
        result <- suppressWarnings(
          system2("powershell", c("-Command", ps_cmd), stdout = TRUE, stderr = FALSE)
        )
        if (length(result) > 0 && nzchar(result[1])) {
          ip <- trimws(result[1])
          if (!grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ip)) ip <- NULL
        }
        # Fallback to ipconfig
        if (is.null(ip) || !nzchar(ip)) {
          result <- suppressWarnings(
            system2("ipconfig", stdout = TRUE, stderr = FALSE)
          )
          if (length(result) > 0) {
            ip_lines <- grep("IPv4", result, value = TRUE, ignore.case = TRUE)
            for (line in ip_lines) {
              matches <- regmatches(line, regexpr("([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})", line))
              if (length(matches) > 0) {
                candidate <- matches[1]
                if (!grepl("^127\\.|^169\\.254\\.", candidate)) {
                  ip <- candidate
                  break
                }
              }
            }
          }
        }
      }
      
      if (is.null(ip) || !nzchar(ip)) "127.0.0.1" else ip
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
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
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
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
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
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
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
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
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

