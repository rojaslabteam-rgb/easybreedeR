suppressPackageStartupMessages({
  # Do not attempt runtime package installs on hosted platforms.
  # shinyapps.io builds dependencies from DESCRIPTION at deploy time.
  if (!requireNamespace("bslib", quietly = TRUE)) {
    warning("Package 'bslib' is not installed. Theme features may be limited.")
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    warning("Package 'shinyjs' is not installed. Some UI interactions may be limited.")
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    warning("Package 'curl' is not installed. Health checks will use slower fallback.")
  }

  library(shiny)
  if (requireNamespace("bslib", quietly = TRUE)) {
    library(bslib)
  } else {
    warning("bslib not available. Theme features may be limited.")
  }
  if (requireNamespace("shinyjs", quietly = TRUE)) {
    library(shinyjs)
  }
})

# Theme (Purdue gold + Crimson Text) - only if bslib is available and loaded
suite_theme <- NULL
if (requireNamespace("bslib", quietly = TRUE)) {
  # Ensure bslib is actually loaded (not just available)
  if (!("package:bslib" %in% search())) {
    try(library(bslib), silent = TRUE)
  }
  # Check if bs_theme function exists and callable
  if (exists("bs_theme", envir = asNamespace("bslib"), inherits = FALSE) || 
      exists("bs_theme", envir = globalenv())) {
    suite_theme <- tryCatch({
      bslib::bs_theme(
        version = 5,
        bg = "#ffffff",
        fg = "#333333",
        primary = "#CEB888",
        base_font = bslib::font_google("Crimson Text")
      )
    }, error = function(e) {
      warning("Failed to create bs_theme: ", e$message, ". Running without theme.")
      NULL
    })
  }
}
if (is.null(suite_theme)) {
  cat("Note: bslib theme not available. App will run with default theme.\n")
}

# ===== Load Language.R if available =====
# Try to load shared language utilities from multiple runtime layouts.
.language_candidates <- function(extra_dirs = character(0)) {
  app_dir <- if (exists("APP_DIR", inherits = TRUE)) get("APP_DIR", inherits = TRUE) else getwd()
  base_dirs <- unique(c(
    app_dir,
    dirname(app_dir),
    getwd(),
    file.path(getwd(), "inst"),
    extra_dirs
  ))
  unique(vapply(base_dirs, function(d) {
    normalizePath(file.path(d, "Language.R"), winslash = "/", mustWork = FALSE)
  }, character(1)))
}

.load_shared_language <- function(target_env = .GlobalEnv, extra_dirs = character(0)) {
  cands <- .language_candidates(extra_dirs = extra_dirs)
  for (cand in cands) {
    if (file.exists(cand)) {
      ok <- tryCatch({
        source(cand, local = target_env)
        TRUE
      }, error = function(e) FALSE)
      if (isTRUE(ok)) return(cand)
    }
  }
  ""
}

shared_language_file <- .load_shared_language(target_env = .GlobalEnv)
if (!nzchar(shared_language_file)) {
  warning("Shared Language.R was not found. UI may show translation keys.")
}

# ===== Global Language State =====
# Shared across the whole Suite; values: "English", "中文", "Português"
suite_language <- shiny::reactiveVal("English")
# Timestamp to force iframe refresh when language changes
suite_lang_timestamp <- shiny::reactiveVal(as.numeric(Sys.time()))

# Helper: map display name to URL code
language_code <- function(name) {
  if (identical(name, "中文")) return("zh")
  if (identical(name, "Português") || identical(name, "葡萄牙语")) return("pt")
  "en"
}

# Get local IP address for LAN access
.get_local_ip <- function() {
  # Check environment variable first (user can set EASYBREEDER_HOST)
  env_host <- Sys.getenv("EASYBREEDER_HOST", "")
  if (nzchar(env_host)) {
    return(trimws(env_host))
  }
  
  # Try to detect IP automatically using cross-platform approach
  ip <- NULL
  sys_name <- Sys.info()["sysname"]
  
  if (sys_name == "Darwin") {
    # macOS
    try({
      # Try en0 first, then en1
      for (iface in c("en0", "en1")) {
        result <- suppressWarnings(
          system2("ipconfig", c("getifaddr", iface), stdout = TRUE, stderr = FALSE)
        )
        if (length(result) > 0 && nzchar(result[1])) {
          ip <- trimws(result[1])
          if (grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ip)) {
            break
          }
        }
      }
    }, silent = TRUE)
  } else if (sys_name == "Linux") {
    # Linux
    try({
      result <- suppressWarnings(
        system2("hostname", c("-I"), stdout = TRUE, stderr = FALSE)
      )
      if (length(result) > 0 && nzchar(result[1])) {
        # Get first IP address
        ip <- trimws(strsplit(result[1], "\\s+")[[1]][1])
        if (!grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ip)) {
          ip <- NULL
        }
      }
    }, silent = TRUE)
  } else if (sys_name == "Windows") {
    # Windows - use PowerShell for more reliable detection
    try({
      # Try PowerShell first (more reliable)
      ps_cmd <- "Get-NetIPAddress -AddressFamily IPv4 | Where-Object {$_.IPAddress -notlike '127.*' -and $_.IPAddress -notlike '169.254.*'} | Select-Object -First 1 -ExpandProperty IPAddress"
      result <- suppressWarnings(
        system2("powershell", c("-Command", ps_cmd), stdout = TRUE, stderr = FALSE)
      )
      if (length(result) > 0 && nzchar(result[1])) {
        ip <- trimws(result[1])
        if (!grepl("^[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}$", ip)) {
          ip <- NULL
        }
      }
      
      # Fallback to ipconfig if PowerShell fails
      if (is.null(ip) || !nzchar(ip)) {
        result <- suppressWarnings(
          system2("ipconfig", stdout = TRUE, stderr = FALSE)
        )
        if (length(result) > 0) {
          # Find IPv4 addresses, exclude loopback and link-local
          ip_lines <- grep("IPv4", result, value = TRUE, ignore.case = TRUE)
          for (line in ip_lines) {
            # Extract IP address using regex
            matches <- regmatches(line, regexpr("([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})", line))
            if (length(matches) > 0) {
              candidate <- matches[1]
              # Exclude loopback and link-local addresses
              if (!grepl("^127\\.|^169\\.254\\.", candidate)) {
                ip <- candidate
                break
              }
            }
          }
        }
      }
    }, silent = TRUE)
  }
  
  # Fallback to localhost if detection fails
  if (is.null(ip) || !nzchar(ip)) {
    return("127.0.0.1")
  }
  ip
}

# Get local IP (cached per session)
.local_ip <- .get_local_ip()

# ===== Deployment / child app configuration =====
.detect_deploy_mode <- function() {
  override <- tolower(trimws(Sys.getenv("EASYBREEDER_DEPLOY_MODE", "")))
  if (override %in% c("local", "hosted")) return(override)
  hosted_markers <- c("RSTUDIO_CONTENT_GUID", "RS_CONNECT_START_TIME", "RSC_VERSION", "RSC_INSTANCE")
  if (any(vapply(hosted_markers, function(var) nzchar(Sys.getenv(var, "")), logical(1)))) {
    return("hosted")
  }
  "local"
}

.normalize_remote_url <- function(url) {
  candidate <- trimws(url)
  if (!nzchar(candidate)) {
    return("")
  }
  if (!grepl("^https?://", candidate, ignore.case = TRUE)) {
    candidate <- paste0("https://", candidate)
  }
  candidate
}

suite_deploy_mode <- .detect_deploy_mode()

# Child metadata reused across diagnostics / runtime
child_specs <- list(
  dataviewer = list(label = "dataviewR", port = 8001L, env_var = "EASYBREEDER_DATAVIEWER_URL"),
  pediviewer = list(label = "PedivieweR", port = 8002L, env_var = "EASYBREEDER_PEDIVIEWER_URL"),
  genoviewer = list(label = "genovieweR", port = 8005L, env_var = "EASYBREEDER_GENOVIEWER_URL"),
  easyblupf90 = list(label = "easyBLUP", port = 8003L, env_var = "EASYBREEDER_EASYBLUP_URL")
)

# Resolve app directory from the calling context (works when sourced by app.R)
.resolve_suite_dir <- function() {
  # app.R sets APP_DIR in parent env; fall back to cwd if missing
  if (exists("APP_DIR", inherits = TRUE)) {
    app_dir <- get("APP_DIR", inherits = TRUE)
    if (dir.exists(app_dir)) return(app_dir)
  }
  cwd <- getwd()
  candidates <- c(
    cwd,
    file.path(cwd, "inst", "easybreedeR_Studio"),
    file.path(dirname(cwd), "inst", "easybreedeR_Studio")
  )
  for (cand in unique(candidates)) {
    if (dir.exists(cand) && file.exists(file.path(cand, "R", "Global.R"))) {
      return(normalizePath(cand, winslash = "/", mustWork = FALSE))
    }
  }
  cwd
}

# Child app folders relative to the Suite app directory
suite_dir <- .resolve_suite_dir()
apps_base_dir <- normalizePath(file.path(suite_dir, ".."), winslash = "/", mustWork = FALSE)
suite_log_path <- normalizePath(file.path(suite_dir, "suite_diagnostics.log"), winslash = "/", mustWork = FALSE)

child_paths <- list(
  dataviewer = normalizePath(file.path(apps_base_dir, "datavieweR"), winslash = "/", mustWork = FALSE),
  pediviewer = normalizePath(file.path(apps_base_dir, "pedivieweR"), winslash = "/", mustWork = FALSE),
  genoviewer = normalizePath(file.path(apps_base_dir, "genovieweR"), winslash = "/", mustWork = FALSE),
  easyblupf90 = normalizePath(file.path(apps_base_dir, "easyblup"), winslash = "/", mustWork = FALSE)
)

child_source_specs <- list(
  dataviewer = list(app_file = file.path(child_paths$dataviewer, "app.R"), run_fn = "run_dataviewer_app"),
  pediviewer = list(app_file = file.path(child_paths$pediviewer, "app.R"), run_fn = "run_pediviewer_app"),
  genoviewer = list(app_file = file.path(child_paths$genoviewer, "app.R"), run_fn = "run_genoviewer_app"),
  easyblupf90 = list(app_file = file.path(child_paths$easyblupf90, "app.R"), run_fn = "run_easyblup_app")
)

child_runtime_config <- lapply(names(child_specs), function(name) {
  spec <- child_specs[[name]]
  remote_url <- .normalize_remote_url(Sys.getenv(spec$env_var, ""))
  mode <- if (nzchar(remote_url)) {
    "remote"
  } else if (identical(suite_deploy_mode, "hosted")) {
    "disabled"
  } else {
    "local"
  }
  list(
    name = name,
    label = spec$label,
    port = spec$port,
    env_var = spec$env_var,
    remote_url = remote_url,
    mode = mode,
    path = child_paths[[name]]
  )
})
names(child_runtime_config) <- names(child_specs)

# Child endpoints (local default or remote override)
child_apps <- lapply(child_runtime_config, function(cfg) {
  if (cfg$mode == "remote" && nzchar(cfg$remote_url)) {
    return(cfg$remote_url)
  }
  paste0("http://", .local_ip, ":", cfg$port)
})

.normalize_child_name <- function(name) {
  candidate <- tolower(trimws(as.character(name)))
  if (!nzchar(candidate)) return("")
  if (candidate %in% c("dataviewr", "dataviewer")) return("dataviewer")
  if (candidate %in% c("pediviewer")) return("pediviewer")
  if (candidate %in% c("genoviewer")) return("genoviewer")
  if (candidate %in% c("easyblup", "easyblupf90")) return("easyblupf90")
  ""
}

.requested_child_from_query <- function(query_string) {
  q <- as.character(query_string)
  if (!nzchar(q)) return(NULL)
  if (!startsWith(q, "?")) {
    q <- paste0("?", q)
  }
  parsed <- tryCatch(shiny::parseQueryString(q), error = function(e) list())
  view <- tolower(trimws(if (!is.null(parsed$view)) as.character(parsed$view) else ""))
  child_raw <- ""
  if (!is.null(parsed$child)) {
    child_raw <- as.character(parsed$child)
  } else if (!is.null(parsed$app)) {
    child_raw <- as.character(parsed$app)
  }
  if (!nzchar(child_raw) && !view %in% c("child", "app")) {
    return(NULL)
  }
  child <- .normalize_child_name(child_raw)
  if (!nzchar(child) || !(child %in% names(child_paths))) {
    return(NULL)
  }
  child
}

get_requested_child <- function(session) {
  q <- tryCatch(isolate(session$clientData$url_search), error = function(e) "")
  .requested_child_from_query(q)
}

build_self_child_url <- function(app_name, session, lang = "en", timestamp = NULL) {
  child <- .normalize_child_name(app_name)
  if (!nzchar(child)) return("")
  protocol <- tryCatch(as.character(session$clientData$url_protocol), error = function(e) "")
  host <- tryCatch(as.character(session$clientData$url_hostname), error = function(e) "")
  port <- tryCatch(as.character(session$clientData$url_port), error = function(e) "")
  pathname <- tryCatch(as.character(session$clientData$url_pathname), error = function(e) "")
  if (!nzchar(protocol) || !nzchar(host) || !nzchar(pathname)) return("")
  if (!startsWith(pathname, "/")) pathname <- paste0("/", pathname)
  base <- paste0(protocol, "//", host)
  if (nzchar(port) && !identical(port, "80") && !identical(port, "443")) {
    base <- paste0(base, ":", port)
  }
  qs <- paste0(
    "view=child&child=", utils::URLencode(child, reserved = TRUE),
    "&lang=", utils::URLencode(as.character(lang), reserved = TRUE)
  )
  if (!is.null(timestamp) && nzchar(as.character(timestamp))) {
    qs <- paste0(qs, "&_t=", utils::URLencode(as.character(timestamp), reserved = TRUE))
  }
  paste0(base, pathname, "?", qs)
}

load_child_runner <- function(app_name) {
  child <- .normalize_child_name(app_name)
  if (!nzchar(child)) {
    stop(sprintf("Unknown child app '%s'", app_name), call. = FALSE)
  }
  spec <- child_source_specs[[child]]
  if (is.null(spec) || !file.exists(spec$app_file)) {
    stop(sprintf("Child app file not found for '%s': %s", child, spec$app_file), call. = FALSE)
  }

  child_env <- new.env(parent = .GlobalEnv)
  # Ensure translation helpers are available in both global and child envs
  # regardless of how child app scripts source Language.R.
  .load_shared_language(target_env = .GlobalEnv, extra_dirs = c(apps_base_dir, suite_dir))
  .load_shared_language(target_env = child_env, extra_dirs = c(apps_base_dir, suite_dir))
  old_source_only <- Sys.getenv("EASYBREEDER_SOURCE_ONLY", unset = "")
  old_wd <- getwd()
  on.exit({
    Sys.setenv(EASYBREEDER_SOURCE_ONLY = old_source_only)
    setwd(old_wd)
  }, add = TRUE)

  Sys.setenv(EASYBREEDER_SOURCE_ONLY = "1")
  setwd(dirname(spec$app_file))
  source(spec$app_file, local = child_env)

  if (exists(spec$run_fn, envir = child_env, mode = "function", inherits = FALSE)) {
    runner <- child_env[[spec$run_fn]]()
  } else if (exists("ui", envir = child_env, inherits = FALSE) &&
             exists("server", envir = child_env, mode = "function", inherits = FALSE)) {
    runner <- list(ui = child_env$ui, server = child_env$server)
  } else {
    stop(sprintf("Failed to resolve ui/server for '%s'", child), call. = FALSE)
  }

  if (is.null(runner$ui) || !is.function(runner$server)) {
    stop(sprintf("Invalid runner returned for '%s'", child), call. = FALSE)
  }
  runner
}

# Track spawned child PIDs
.suite_child_pids <- new.env(parent = emptyenv())
.suite_child_pids$pids <- integer(0)

# Health check helper
is_alive <- function(url) {
  ok <- FALSE
  # Try curl first if available
  if (requireNamespace("curl", quietly = TRUE)) {
    try({
      h <- curl::new_handle()
      curl::handle_setopt(h, connecttimeout = 0.8, timeout = 1.5)
      curl::curl_fetch_memory(url, handle = h)
      ok <- TRUE
    }, silent = TRUE)
    return(ok)
  }
  # Fallback using base url() with short timeout
  old_timeout <- getOption("timeout")
  on.exit(options(timeout = old_timeout), add = TRUE)
  options(timeout = 2)
  con <- NULL
  try({
    con <- base::url(url, open = "rb")
    # Attempt to read a small chunk; if it works, server is up
    suppressWarnings(readBin(con, what = "raw", n = 1L))
    ok <- TRUE
  }, silent = TRUE)
  if (!is.null(con)) try(close(con), silent = TRUE)
  ok
}

# Spawn a child app if not alive
start_child <- function(path, port) {
  if (!dir.exists(path)) return(NA_integer_)
  rscript <- file.path(R.home("bin"), "Rscript")
  # On Windows, prefer Rscript.exe if plain name does not exist
  if (.Platform$OS.type == "windows" && !file.exists(rscript)) {
    candidate <- paste0(rscript, ".exe")
    if (file.exists(candidate)) rscript <- candidate
  }
  # Use host = "0.0.0.0" to allow LAN access
  cmd <- sprintf("shiny::runApp('%s', port=%d, host='0.0.0.0', launch.browser=FALSE)", path, as.integer(port))
  stdout <- tempfile(pattern = sprintf("child_%d_out_", as.integer(port)), fileext = ".log")
  stderr <- tempfile(pattern = sprintf("child_%d_err_", as.integer(port)), fileext = ".log")
  log_suite("spawning child", path, "port", port, "host=0.0.0.0", "stdout", stdout, "stderr", stderr)
  pid <- tryCatch({
    system2(rscript, c("-e", shQuote(cmd)), wait = FALSE, stdout = stdout, stderr = stderr)
  }, error = function(e) NA_integer_)
  log_suite("child pid:", pid)
  pid
}

# Small helper to create a responsive iframe panel
iframe_panel <- function(url) {
  tags$div(
    style = "position:relative; height:95vh; width:100%; overflow:hidden;",
    tags$iframe(
      src = url,
      width = "100%",
      height = "100%",
      frameborder = "0",
      style = "border:none; overflow:hidden;"
    )
  )
}


# ===== Diagnostics =====
log_suite <- function(..., verbose = FALSE) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", paste(..., collapse = " "))
  # Only write to file by default, not to console
  if (verbose) {
    try(cat(msg, "\n"), silent = TRUE)
  }
  try(write(x = msg, file = suite_log_path, append = TRUE), silent = TRUE)
}

diagnose_suite <- function(verbose = FALSE) {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (.Platform$OS.type == "windows" && !file.exists(rscript)) {
    candidate <- paste0(rscript, ".exe")
    if (file.exists(candidate)) rscript <- candidate
  }

  child_modes <- vapply(child_runtime_config, function(cfg) cfg$mode, character(1))
  child_dirs_exist <- vapply(names(child_paths), function(nm) {
    if (!identical(child_modes[[nm]], "local")) TRUE else dir.exists(child_paths[[nm]])
  }, logical(1))
  child_appR_exist <- vapply(names(child_paths), function(nm) {
    if (!identical(child_modes[[nm]], "local")) TRUE else file.exists(file.path(child_paths[[nm]], "app.R"))
  }, logical(1))

  ports <- vapply(child_runtime_config, function(cfg) cfg$port, integer(1))
  ports_in_use <- setNames(rep(FALSE, length(ports)), ports)
  local_names <- names(child_runtime_config)[child_modes == "local"]
  if (length(local_names) > 0) {
    check_ip <- tryCatch(.get_local_ip(), error = function(e) "127.0.0.1")
    for (nm in local_names) {
      port <- child_runtime_config[[nm]]$port
      url <- sprintf("http://%s:%d", check_ip, port)
      ports_in_use[as.character(port)] <- is_alive(url)
    }
  }

  result <- list(
    rscript_path = rscript,
    rscript_exists = file.exists(rscript),
    curl_available = requireNamespace("curl", quietly = TRUE),
    child_dirs_exist = child_dirs_exist,
    child_appR_exist = child_appR_exist,
    ports_in_use = ports_in_use,
    child_modes = child_modes,
    deploy_mode = suite_deploy_mode,
    remote_urls = vapply(child_runtime_config, function(cfg) cfg$remote_url, character(1))
  )

  # Only print to console if verbose mode or if there are issues
  has_issues <- !isTRUE(result$rscript_exists) || 
                !all(result$child_dirs_exist) || 
                !all(result$child_appR_exist)
  
  if (verbose || has_issues) {
    try({
      cat("\n=== easybreedeR Suite Diagnostics ===\n")
      cat("Rscript:", result$rscript_path, " exists=", result$rscript_exists, "\n", sep = "")
      cat("curl available:", result$curl_available, "\n")
      cat("Child directories exist:\n")
      for (nm in names(child_dirs_exist)) cat("  - ", nm, ": ", child_dirs_exist[[nm]], "  (", child_paths[[nm]], ")\n", sep = "")
      cat("Child app.R exist:\n")
      for (nm in names(child_appR_exist)) cat("  - ", nm, ": ", child_appR_exist[[nm]], "\n", sep = "")
      cat("Child modes:\n")
      for (nm in names(child_modes)) {
        url_hint <- if (nzchar(result$remote_urls[[nm]])) paste0(" (", result$remote_urls[[nm]], ")") else ""
        cat("  - ", nm, ": ", child_modes[[nm]], url_hint, "\n", sep = "")
      }
      cat("Ports in use (TRUE means something already listening):\n")
      for (pr in names(result$ports_in_use)) cat("  - ", pr, ": ", result$ports_in_use[[pr]], "\n", sep = "")
      cat("====================================\n\n")
    }, silent = TRUE)
  }

  # Log to file (without verbose flag, so no console output)
  log_suite("Rscript:", result$rscript_path, "exists=", result$rscript_exists)
  log_suite("curl available:", result$curl_available)
  for (nm in names(child_dirs_exist)) log_suite("dir exists", nm, "=", child_dirs_exist[[nm]], child_paths[[nm]])
  for (nm in names(child_appR_exist)) log_suite("app.R exists", nm, "=", child_appR_exist[[nm]])
  for (nm in names(child_modes)) log_suite("child mode", nm, "=", child_modes[[nm]], if (nzchar(result$remote_urls[[nm]])) result$remote_urls[[nm]] else "")
  for (pr in names(result$ports_in_use)) log_suite("port", pr, "in_use=", result$ports_in_use[[pr]])

  result
}
