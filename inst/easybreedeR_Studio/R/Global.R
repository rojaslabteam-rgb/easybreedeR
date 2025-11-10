suppressPackageStartupMessages({
  # Best-effort installs BEFORE library() to avoid missing function errors
  options(repos = getOption("repos", c(CRAN = "https://cloud.r-project.org")))
  if (!requireNamespace("bslib", quietly = TRUE)) {
    try(suppressWarnings(install.packages("bslib", repos = getOption("repos"))), silent = TRUE)
    # Reload namespace after install attempt
    try(loadNamespace("bslib"), silent = TRUE)
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    try(suppressWarnings(install.packages("shinyjs", repos = getOption("repos"))), silent = TRUE)
    try(loadNamespace("shinyjs"), silent = TRUE)
  }
  if (!requireNamespace("curl", quietly = TRUE)) {
    try(suppressWarnings(install.packages("curl", repos = getOption("repos"))), silent = TRUE)
    try(loadNamespace("curl"), silent = TRUE)
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
# Try to load shared language utilities
if (exists("APP_DIR")) {
  lang_file <- normalizePath(file.path(dirname(APP_DIR), "Language.R"), winslash = "/", mustWork = FALSE)
  if (file.exists(lang_file)) {
    try(source(lang_file, local = FALSE), silent = TRUE)
  }
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


# Child endpoints and paths
child_apps <- list(
  datapreviewer = "http://127.0.0.1:8001",
  pediviewer   = "http://127.0.0.1:8002",
  easyblupf90  = "http://127.0.0.1:8003",
  core_tools   = "http://127.0.0.1:8004"
)

# Resolve app directory from the calling context (works when sourced by app.R)
.resolve_suite_dir <- function() {
  # app.R sets APP_DIR in parent env; fall back to cwd if missing
  if (exists("APP_DIR", inherits = TRUE)) {
    return(get("APP_DIR", inherits = TRUE))
  }
  getwd()
}

# Child app folders relative to the Suite app directory
suite_dir <- .resolve_suite_dir()
apps_base_dir <- normalizePath(file.path(suite_dir, ".."), winslash = "/", mustWork = FALSE)
suite_log_path <- normalizePath(file.path(suite_dir, "suite_diagnostics.log"), winslash = "/", mustWork = FALSE)

child_paths <- list(
  datapreviewer = normalizePath(file.path(apps_base_dir, "dataprevieweR"), winslash = "/", mustWork = FALSE),
  pediviewer   = normalizePath(file.path(apps_base_dir, "pedivieweR"), winslash = "/", mustWork = FALSE),
  easyblupf90  = normalizePath(file.path(apps_base_dir, "easyblup"), winslash = "/", mustWork = FALSE),
  core_tools   = normalizePath(file.path(apps_base_dir, "RCW"), winslash = "/", mustWork = FALSE)
)

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
  cmd <- sprintf("shiny::runApp('%s', port=%d, launch.browser=FALSE)", path, as.integer(port))
  stdout <- tempfile(pattern = sprintf("child_%d_out_", as.integer(port)), fileext = ".log")
  stderr <- tempfile(pattern = sprintf("child_%d_err_", as.integer(port)), fileext = ".log")
  log_suite("spawning child", path, "port", port, "stdout", stdout, "stderr", stderr)
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
log_suite <- function(...) {
  msg <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " ", paste(..., collapse = " "))
  try(cat(msg, "\n"), silent = TRUE)
  try(write(x = msg, file = suite_log_path, append = TRUE), silent = TRUE)
}

diagnose_suite <- function() {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (.Platform$OS.type == "windows" && !file.exists(rscript)) {
    candidate <- paste0(rscript, ".exe")
    if (file.exists(candidate)) rscript <- candidate
  }

  child_dirs_exist <- vapply(child_paths, dir.exists, logical(1))
  child_appR_exist <- vapply(child_paths, function(p) file.exists(file.path(p, "app.R")), logical(1))

  ports <- c(8001L, 8002L, 8003L, 8004L)
  port_urls <- sprintf("http://127.0.0.1:%d", ports)
  ports_in_use <- vapply(port_urls, is_alive, logical(1))

  result <- list(
    rscript_path = rscript,
    rscript_exists = file.exists(rscript),
    curl_available = requireNamespace("curl", quietly = TRUE),
    child_dirs_exist = child_dirs_exist,
    child_appR_exist = child_appR_exist,
    ports_in_use = setNames(ports_in_use, ports)
  )

  # Pretty print to console for developers
  try({
    cat("\n=== easybreedeR Suite Diagnostics ===\n")
    cat("Rscript:", result$rscript_path, " exists=", result$rscript_exists, "\n", sep = "")
    cat("curl available:", result$curl_available, "\n")
    cat("Child directories exist:\n")
    for (nm in names(child_dirs_exist)) cat("  - ", nm, ": ", child_dirs_exist[[nm]], "  (", child_paths[[nm]], ")\n", sep = "")
    cat("Child app.R exist:\n")
    for (nm in names(child_appR_exist)) cat("  - ", nm, ": ", child_appR_exist[[nm]], "\n", sep = "")
    cat("Ports in use (TRUE means something already listening):\n")
    for (pr in names(result$ports_in_use)) cat("  - ", pr, ": ", result$ports_in_use[[pr]], "\n", sep = "")
    cat("====================================\n\n")
  }, silent = TRUE)

  # Also log to file
  log_suite("Rscript:", result$rscript_path, "exists=", result$rscript_exists)
  log_suite("curl available:", result$curl_available)
  for (nm in names(child_dirs_exist)) log_suite("dir exists", nm, "=", child_dirs_exist[[nm]], child_paths[[nm]])
  for (nm in names(child_appR_exist)) log_suite("app.R exists", nm, "=", child_appR_exist[[nm]])
  for (pr in names(result$ports_in_use)) log_suite("port", pr, "in_use=", result$ports_in_use[[pr]])

  result
}


