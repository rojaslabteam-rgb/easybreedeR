.resolve_port <- function(port) {
  if (!is.null(port)) {
    return(as.integer(port))
  }
  opt_port <- getOption("shiny.port")
  if (!is.null(opt_port)) {
    return(as.integer(opt_port))
  }
  if (requireNamespace("httpuv", quietly = TRUE)) {
    return(httpuv::randomPort())
  }
  warning("httpuv not available; falling back to port 0.")
  0L
}

.get_local_ip <- function() {
  # Use the same cross-platform detection logic
  tryCatch({
    env_host <- Sys.getenv("EASYBREEDER_HOST", "")
    if (nzchar(env_host)) {
      return(trimws(env_host))
    }
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
  }, error = function(e) "127.0.0.1")
}

.announce_urls <- function(host, port, local_ip) {
  effective_host <- if (is.null(host) || !nzchar(host)) "0.0.0.0" else host
  effective_port <- as.integer(port)
  host_url <- sprintf("http://%s:%d", effective_host, effective_port)
  message("App running at: ", host_url)
  if (!is.null(local_ip) && nzchar(local_ip)) {
    local_url <- sprintf("http://%s:%d", trimws(local_ip), effective_port)
    if (!identical(local_url, host_url)) {
      message("Local network: ", local_url)
    }
  }
}

#' Launch the easybreedeR Studio (suite launcher)
#'
#' Main entry point for the easybreedeR Shiny application suite. Provides
#' access to all sub-applications (easyblup, pedivieweR, datavieweR, genovieweR).
#'
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
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
  resolved_port <- .resolve_port(port)
  .announce_urls(host, resolved_port, .get_local_ip())
  shiny::runApp(app_dir, host = host, port = resolved_port, launch.browser = TRUE)
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
  resolved_port <- .resolve_port(port)
  .announce_urls(host, resolved_port, .get_local_ip())
  shiny::runApp(app_dir, host = host, port = resolved_port, launch.browser = TRUE)
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
  resolved_port <- .resolve_port(port)
  .announce_urls(host, resolved_port, .get_local_ip())
  shiny::runApp(app_dir, host = host, port = resolved_port, launch.browser = TRUE)
}

#' Launch genovieweR application
#'
#' @description
#' Launches the genovieweR application for genotype visualization and analysis.
#'
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
#' @export
#' @examples
#' \dontrun{
#'   run_genovieweR()
#' }
run_genovieweR <- function(host = "0.0.0.0", port = NULL) {
  app_dir <- system.file("genovieweR", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "genovieweR")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("genovieweR application not found. Please reinstall the package or run from package root.")
    }
  }
  resolved_port <- .resolve_port(port)
  .announce_urls(host, resolved_port, .get_local_ip())
  shiny::runApp(app_dir, host = host, port = resolved_port, launch.browser = TRUE)
}

#' Launch datavieweR application
#'
#' @description
#' Launches the datavieweR application for data preview and exploration.
#'
#' @param host The IPv4 address that the application should listen on. Defaults to "0.0.0.0" to allow access from other devices on the network.
#' @param port The TCP port that the application should listen on. Defaults to NULL, which will use a random available port.
#' @export
#' @examples
#' \dontrun{
#'   run_datavieweR()
#' }
run_datavieweR <- function(host = "0.0.0.0", port = NULL) {
  app_dir <- system.file("datavieweR", package = "easybreedeR")
  if (app_dir == "") {
    dev_dir <- file.path(getwd(), "inst", "datavieweR")
    if (dir.exists(dev_dir)) {
      app_dir <- dev_dir
    } else {
      stop("datavieweR application not found. Please reinstall the package or run from package root.")
    }
  }
  resolved_port <- .resolve_port(port)
  .announce_urls(host, resolved_port, .get_local_ip())
  shiny::runApp(app_dir, host = host, port = resolved_port, launch.browser = TRUE)
}
