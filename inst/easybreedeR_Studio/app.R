# Open BreedX (OBX) Clean Three-Panel Layout
# Version: 0.4.0 (Fixed Language Settings)
# Created: 2025-10-22
# Last Modified: 2025-10-31

suppressPackageStartupMessages({
  library(shiny)
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
    # Fallback to cwd/.. pattern
    lang_candidate2 <- normalizePath(file.path(suite_dir, "..", "Language.R"), mustWork = FALSE)
    if (file.exists(lang_candidate2)) source(lang_candidate2, local = FALSE)
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

# Ensure LAN access is enabled by default
# This ensures the app listens on 0.0.0.0 even if .Rprofile is not loaded
if (is.null(getOption("shiny.host"))) {
  options(shiny.host = "0.0.0.0")
}

# Source all scripts in global environment (packages load globally anyway)
# But ensure suite_language reactiveVal is accessible everywhere
source(file.path(APP_DIR, "R/Global.R"), local = FALSE)
source(file.path(APP_DIR, "R/Page_Suite.R"), local = FALSE)
source(file.path(APP_DIR, "R/run_easybreedeR_Studio.R"), local = FALSE)

runner <- run_easybreedeR_Studio()
shinyApp(ui = runner$ui, server = runner$server)


