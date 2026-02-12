# Open BreedX (OBX) Clean Three-Panel Layout

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(dplyr)
  library(DT)
  library(data.table)
  library(digest)
  library(igraph)
  library(plotly)
  library(visNetwork)
  library(Rcpp)
})

# Force-load shared Language.R early to ensure get_label/TRANSLATIONS are available
try({
  suite_dir <- getwd()
  # app.R later resolves APP_DIR; here compute relative to this file if possible
  lang_candidate <- tryCatch({
    this_file <- normalizePath(sys.frames()[[1]]$ofile)
    normalizePath(file.path(dirname(this_file), "..", "Language.R"))
  }, error = function(e) NA)
  if (!is.na(lang_candidate) && file.exists(lang_candidate)) {
    source(lang_candidate, local = FALSE)
  } else {
    # Fallbacks for local run and shinyapps bundle root
    lang_candidate2 <- normalizePath(file.path(suite_dir, "..", "Language.R"), mustWork = FALSE)
    lang_candidate3 <- normalizePath(file.path(suite_dir, "inst", "Language.R"), mustWork = FALSE)
    if (file.exists(lang_candidate2)) source(lang_candidate2, local = FALSE)
    if (file.exists(lang_candidate3)) source(lang_candidate3, local = FALSE)
  }
}, silent = TRUE)

# Resolve the application directory reliably whether run via Rscript or runApp
.resolve_app_dir <- function() {
  # Try sys.frames (Rscript)
  p <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NA)
  if (!is.na(p) && file.exists(p)) return(dirname(p))
  # Try commandArgs --file
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("--file=", "", m[[1]])
    p <- tryCatch(normalizePath(p), error = function(e) NA)
    if (!is.na(p) && file.exists(p)) return(dirname(p))
  }
  # Fallback to working directory
  getwd()
}

APP_DIR <- .resolve_app_dir()
.resolve_suite_dir <- function(app_dir) {
  candidates <- unique(c(
    app_dir,
    file.path(app_dir, "inst", "easybreedeR_Studio"),
    file.path(getwd(), "inst", "easybreedeR_Studio")
  ))
  for (d in candidates) {
    if (dir.exists(d) && file.exists(file.path(d, "R", "Global.R"))) {
      return(normalizePath(d, winslash = "/", mustWork = FALSE))
    }
  }
  normalizePath(app_dir, winslash = "/", mustWork = FALSE)
}
SUITE_DIR <- .resolve_suite_dir(APP_DIR)

# Ensure LAN access is enabled by default
# This ensures the app listens on 0.0.0.0 even if .Rprofile is not loaded
if (is.null(getOption("shiny.host"))) {
  options(shiny.host = "0.0.0.0")
}

# Source all scripts in global environment (packages load globally anyway)
# But ensure suite_language reactiveVal is accessible everywhere
source(file.path(SUITE_DIR, "R/Global.R"), local = FALSE)
source(file.path(SUITE_DIR, "R/Page_Suite.R"), local = FALSE)
source(file.path(SUITE_DIR, "R/run_easybreedeR_Studio.R"), local = FALSE)

runner <- run_easybreedeR_Studio()
shinyApp(ui = runner$ui, server = runner$server)

