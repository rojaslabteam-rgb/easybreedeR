# Quiet shiny::devmode() notices before loading shiny/bslib.
options(
  shiny.autoreload = TRUE,
  shiny.autoreload.legacy_warning = FALSE,
  shiny.minified = TRUE,
  sass.cache = TRUE,
  bslib.precompiled = TRUE,
  bslib.color_contrast_warnings = FALSE
)

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
  lang_candidates <- unique(c(
    if (!is.na(lang_candidate)) lang_candidate else character(0),
    normalizePath(file.path(suite_dir, "inst", "Language.R"), mustWork = FALSE),
    normalizePath(file.path(suite_dir, "Language.R"), mustWork = FALSE),
    normalizePath(file.path(suite_dir, "..", "Language.R"), mustWork = FALSE)
  ))
  for (cand in lang_candidates) {
    if (file.exists(cand)) {
      source(cand, local = FALSE)
      break
    }
  }
}, silent = TRUE)

# Resolve the application directory reliably whether run via Rscript,
# shiny::runApp(dir), or Cursor/Posit "Run Shiny App" (runShinyApp.R <app.R> <port>).
.resolve_app_dir <- function() {
  # 1) Cursor / Posit runner: first trailing arg is often the app.R path
  ta <- commandArgs(trailingOnly = TRUE)
  if (length(ta) >= 1) {
    p <- ta[[1]]
    if (is.character(p) && grepl("\\.[Rr]$", p)) {
      p <- tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
      if (!is.na(p) && file.exists(p)) {
        d <- dirname(p)
        if (file.exists(file.path(d, "R", "Global.R"))) return(d)
      }
    }
  }

  # 2) Walk the call stack for a sourced app.R that owns R/Global.R
  n <- sys.nframe()
  if (n >= 1) {
    for (i in seq_len(n)) {
      ofile <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
      if (is.null(ofile) || !is.character(ofile) || length(ofile) != 1) next
      p <- tryCatch(normalizePath(ofile, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
      if (is.na(p) || !file.exists(p)) next
      d <- dirname(p)
      if (identical(basename(p), "app.R") && file.exists(file.path(d, "R", "Global.R"))) return(d)
    }
  }

  # 3) commandArgs --file=...
  ca <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", ca, value = TRUE)
  if (length(m) > 0) {
    p <- sub("^--file=", "", m[[1]])
    p <- tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE), error = function(e) NA_character_)
    if (!is.na(p) && file.exists(p)) {
      d <- dirname(p)
      if (file.exists(file.path(d, "R", "Global.R"))) return(d)
    }
  }

  # 4) Search upward from getwd() for inst/easybreedeR_Studio
  cur <- tryCatch(normalizePath(getwd(), winslash = "/", mustWork = FALSE), error = function(e) getwd())
  for (i in 0:6) {
    candidates <- c(
      file.path(cur, "inst", "easybreedeR_Studio"),
      file.path(cur, "easybreedeR_Studio"),
      cur
    )
    for (d in candidates) {
      if (dir.exists(d) && file.exists(file.path(d, "R", "Global.R"))) {
        return(normalizePath(d, winslash = "/", mustWork = FALSE))
      }
    }
    parent <- dirname(cur)
    if (identical(parent, cur)) break
    cur <- parent
  }

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

# Fail fast with a clear message if Studio sources are missing
.global_r <- file.path(SUITE_DIR, "R", "Global.R")
if (!file.exists(.global_r)) {
  stop(
    "Cannot locate easybreedeR Studio sources (R/Global.R).\n",
    "  APP_DIR   = ", APP_DIR, "\n",
    "  SUITE_DIR = ", SUITE_DIR, "\n",
    "  getwd()   = ", getwd(), "\n",
    "Run the app folder (inst/easybreedeR_Studio) rather than a random working directory.",
    call. = FALSE
  )
}

# Listen on this computer only unless the caller asks otherwise, e.g.
# run_easybreedeR(host = "0.0.0.0") or options(shiny.host = "0.0.0.0").

# Source all scripts in global environment (packages load globally anyway)
# But ensure suite_language reactiveVal is accessible everywhere
source(file.path(SUITE_DIR, "R/Global.R"), local = FALSE)
source(file.path(SUITE_DIR, "R/Page_Suite.R"), local = FALSE)
source(file.path(SUITE_DIR, "R/run_easybreedeR_Studio.R"), local = FALSE)

runner <- run_easybreedeR_Studio()
shinyApp(ui = runner$ui, server = runner$server)
