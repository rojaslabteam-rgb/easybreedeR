# Shared app runtime helpers. No mutable user state belongs in this file.

# Quiet shiny::devmode() notices (Posit/Cursor often launch with --devmode).
# Explicit options skip the once-per-8h "Turning on/off …" messages and
# bslib Sass contrast warnings during theme compile.
eb_quiet_devmode <- function() {
  options(
    shiny.autoreload = TRUE,
    shiny.autoreload.legacy_warning = FALSE,
    shiny.minified = TRUE,
    sass.cache = TRUE,
    bslib.precompiled = TRUE,
    bslib.color_contrast_warnings = FALSE
  )
  invisible(NULL)
}

eb_positive_limit <- function(name, default) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, as.character(default))))
  if (length(value) != 1L || !is.finite(value) || value <= 0) {
    stop(sprintf("%s must be a positive finite number.", name), call. = FALSE)
  }
  value
}

eb_configure_uploads <- function() {
  eb_quiet_devmode()
  options(shiny.maxRequestSize = eb_positive_limit("EASYBREEDER_UPLOAD_MB", 100) * 1024^2)
}

# Shared Xmosome chrome CSS (tokens, header, buttons, tabs).
eb_chrome_css_path <- function() {
  candidates <- c(
    file.path("..", "www", "easybreedeR-chrome.css"),
    file.path("www", "easybreedeR-chrome.css"),
    file.path("inst", "www", "easybreedeR-chrome.css"),
    system.file("www", "easybreedeR-chrome.css", package = "easybreedeR")
  )
  hit <- candidates[nzchar(candidates) & file.exists(candidates)][1L]
  if (length(hit) && !is.na(hit)) normalizePath(hit, winslash = "/", mustWork = FALSE) else NA_character_
}

# Register once and return a <link> (or inline <style> fallback).
eb_chrome_css_tag <- function() {
  path <- eb_chrome_css_path()
  if (is.na(path)) return(NULL)
  dir <- dirname(path)
  # Shiny static prefix shared across suite modules.
  try(shiny::addResourcePath("easybreeder-chrome", dir), silent = TRUE)
  href <- "easybreeder-chrome/easybreedeR-chrome.css"
  # If resource path failed (rare), inline the file.
  if (!dir.exists(dir)) {
    css <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    return(htmltools::tags$style(htmltools::HTML(css)))
  }
  htmltools::tags$link(rel = "stylesheet", type = "text/css", href = href)
}

# Brand accent used in R (plots / inline styles) — keep in sync with CSS --accent.
# Purdue Boilermaker Gold (#CFB991); Aged (#8E6F3E) for text on light backgrounds.
eb_accent <- function() "#CFB991"
eb_accent_deep <- function() "#8E6F3E"

# Hover / focus the label text to reveal help (Xmosome-style dotted underline).
# Usage: eb_with_tip("Title", "Help line 1", tags$div(tags$b("Note:"), " …"))
eb_with_tip <- function(label, ..., placement = "auto") {
  dots <- list(...)
  if (!length(dots)) return(label)
  body <- if (length(dots) == 1L) dots[[1L]] else do.call(htmltools::tagList, dots)
  bslib::tooltip(
    htmltools::tags$span(class = "tip-text", tabindex = "0", label),
    body,
    placement = placement
  )
}

eb_session_workspace <- function(session) {
  path <- tempfile("easybreeder-session-")
  if (!dir.create(path)) stop("Cannot create session workspace.", call. = FALSE)
  session$onSessionEnded(function() unlink(path, recursive = TRUE, force = TRUE))
  path
}

eb_clear_workspace <- function(path) {
  # Keep the session root; callers and cleanup callbacks retain the same path.
  files <- list.files(path, all.files = TRUE, full.names = TRUE, no.. = TRUE)
  unlink(files, recursive = TRUE, force = TRUE)
  invisible(NULL)
}

eb_cached_read <- function(path, key, read) {
  cache <- file.path(path, paste0(key, ".rds"))
  cached <- if (file.exists(cache)) tryCatch(readRDS(cache), error = function(e) NULL) else NULL
  if (!is.null(cached)) return(cached)
  value <- read()
  # Cache failure must not prevent analysis. Rename avoids partially written RDS.
  tmp <- tempfile("cache-", tmpdir = path)
  on.exit(unlink(tmp), add = TRUE)
  tryCatch({
    saveRDS(value, tmp)
    if (!file.rename(tmp, cache)) warning("Could not commit session cache.")
  }, error = function(e) warning("Session cache unavailable; using uploaded data."))
  value
}

eb_check_pca_budget <- function(n, m) {
  if (length(n) != 1L || length(m) != 1L || !is.finite(n) || !is.finite(m) || n < 0 || m < 0) {
    stop("Local PCA requires an in-memory genotype matrix. Use the PLINK backend for binary input.", call. = FALSE)
  }
  # Input, dosage/standardization copies, Gram/eigen workspace; conservative
  # estimate, not a guarantee of whole-process peak memory.
  estimated_mb <- (24 * as.double(n) * m + 48 * as.double(n)^2) / 1024^2
  budget <- eb_positive_limit("EASYBREEDER_PCA_MEMORY_MB", 512)
  max_samples <- eb_positive_limit("EASYBREEDER_PCA_MAX_SAMPLES", 5000)
  max_comparisons <- eb_positive_limit("EASYBREEDER_PCA_MAX_COMPARISONS", 1e8)
  if (as.double(n) * (n + 1) / 2 * m > max_comparisons) {
    stop("Local PCA exceeds the configured computation limit. Use a smaller dataset or run PLINK locally.", call. = FALSE)
  }
  if (!is.finite(estimated_mb) || n > max_samples || estimated_mb > budget) {
    stop(sprintf("Local PCA exceeds the configured limit (estimate %.0f MiB; budget %.0f MiB; sample limit %.0f). Use a smaller dataset or run PLINK locally.",
                 estimated_mb, budget, max_samples), call. = FALSE)
  }
  invisible(estimated_mb)
}
