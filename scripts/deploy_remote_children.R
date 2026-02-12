# easybreedeR shinyapps.io deployment helpers
#
# Recommended (single deployed app):
#   source("scripts/deploy_remote_children.R")
#   deploy_unified_suite(account = "myaccount", appName = "easybreeder-suite")
#
# Legacy (multi-app):
#   deploy_child_app("dataviewer", account = "myaccount")
#   deploy_child_app("pediviewer", account = "myaccount")
#   deploy_child_app("genoviewer", account = "myaccount")
#   deploy_child_app("easyblup", account = "myaccount")
#   deploy_suite(account = "myaccount")

# ---- prerequisites -----------------------------------------------------------
.require_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Package '%s' is required. Please install it before deploying.", pkg), call. = FALSE)
  }
}

.require_package("rsconnect")

# ---- project root ------------------------------------------------------------
.get_project_root <- function() {
  this_file <- tryCatch(normalizePath(sys.frames()[[1]]$ofile), error = function(e) NA)
  if (!is.na(this_file) && file.exists(this_file)) {
    return(normalizePath(file.path(dirname(this_file), "..")))
  }
  getwd()
}

PROJECT_ROOT <- .get_project_root()

# ---- deployment specs --------------------------------------------------------
# Legacy child apps (separate deployments)
child_specs <- list(
  dataviewer = list(
    dir = "inst/datavieweR",
    primary = "inst/datavieweR/app.R",
    title = "datavieweR",
    env = "EASYBREEDER_DATAVIEWER_URL",
    files = c("inst/datavieweR/app.R")
  ),
  pediviewer = list(
    dir = "inst/pedivieweR",
    primary = "inst/pedivieweR/app.R",
    title = "PedivieweR",
    env = "EASYBREEDER_PEDIVIEWER_URL",
    files = c(
      "inst/pedivieweR/app.R",
      "inst/pedivieweR/pedigree_qc.cpp",
      "inst/pedivieweR/pedigree_cache",
      "inst/pedivieweR/temp_inbreeding"
    )
  ),
  genoviewer = list(
    dir = "inst/genovieweR",
    primary = "inst/genovieweR/app.R",
    title = "genovieweR",
    env = "EASYBREEDER_GENOVIEWER_URL",
    files = c("inst/genovieweR/app.R")
  ),
  easyblup = list(
    dir = "inst/easyblup",
    primary = "inst/easyblup/app.R",
    title = "easyBLUP",
    env = "EASYBREEDER_EASYBLUP_URL",
    files = c(
      "inst/easyblup/app.R",
      "inst/easyblup/ai_assistant.R",
      "inst/easyblup/ai_rules.json",
      "inst/easyblup/global.R",
      "inst/easyblup/mcp_tools.R",
      "inst/easyblup/www"
    )
  )
)

# Unified suite app (recommended): includes child folders in one deployment
suite_spec <- list(
  dir = "inst/easybreedeR_Studio",
  primary = "inst/easybreedeR_Studio/app.R",
  title = "easybreedeR Studio",
  files = c(
    "inst/easybreedeR_Studio/app.R",
    "inst/easybreedeR_Studio/R",
    "inst/easybreedeR_Studio/www",
    "inst/datavieweR",
    "inst/pedivieweR",
    "inst/genovieweR",
    "inst/easyblup"
  )
)

# ---- helpers ----------------------------------------------------------------
.assert_non_empty <- function(value, name) {
  if (is.null(value) || !nzchar(trimws(as.character(value)))) {
    stop(sprintf("`%s` must be a non-empty string.", name), call. = FALSE)
  }
}

.resolve_app_files <- function(files) {
  candidates <- unique(c("inst/Language.R", files))
  exists_flag <- vapply(
    candidates,
    function(p) {
      abs <- file.path(PROJECT_ROOT, p)
      file.exists(abs) || dir.exists(abs)
    },
    logical(1)
  )
  existing <- candidates[exists_flag]
  missing <- candidates[!exists_flag]
  if (length(missing) > 0) {
    message("Skipping missing deployment paths: ", paste(missing, collapse = ", "))
  }
  existing
}

.deploy_from_spec <- function(spec, account, appName, logLevel = "normal", ...) {
  .assert_non_empty(account, "account")
  .assert_non_empty(appName, "appName")

  app_dir <- file.path(PROJECT_ROOT, spec$dir)
  if (!dir.exists(app_dir)) {
    stop(sprintf("App directory not found: %s", app_dir), call. = FALSE)
  }

  app_files <- .resolve_app_files(spec$files)
  message(
    "Deploying '", appName, "' from ", PROJECT_ROOT,
    " (", length(app_files), " paths)"
  )

  rsconnect::deployApp(
    appDir = PROJECT_ROOT,
    appPrimaryDoc = spec$primary,
    appFiles = app_files,
    appName = appName,
    account = account,
    appTitle = spec$title,
    logLevel = logLevel,
    ...
  )
}

# ---- public API --------------------------------------------------------------
#' Deploy a single child app (legacy multi-app mode)
#' @param child one of names(child_specs)
#' @param account shinyapps.io account name
#' @param appName optional target app name; default easybreeder-<child>
#' @param ... passed to rsconnect::deployApp
#' @return result from rsconnect::deployApp
deploy_child_app <- function(child, account, appName = NULL, ...) {
  if (!child %in% names(child_specs)) {
    stop(sprintf("Unknown child '%s'. Valid: %s", child, paste(names(child_specs), collapse = ", ")), call. = FALSE)
  }
  spec <- child_specs[[child]]
  if (is.null(appName) || !nzchar(trimws(appName))) {
    appName <- sprintf("easybreeder-%s", child)
  }
  .deploy_from_spec(spec = spec, account = account, appName = appName, logLevel = "normal", ...)
}

#' Deploy suite app
#'
#' In the current architecture this is the same as unified deployment and is
#' the recommended default.
#'
#' @param account shinyapps.io account name
#' @param appName target app identifier on shinyapps.io
#' @param ... passed to rsconnect::deployApp
#' @return result from rsconnect::deployApp
deploy_suite <- function(account, appName = "easybreeder-suite", ...) {
  .deploy_from_spec(spec = suite_spec, account = account, appName = appName, ...)
}

#' Deploy unified suite app (recommended)
#' @param account shinyapps.io account name
#' @param appName target app identifier on shinyapps.io
#' @param ... passed to rsconnect::deployApp
#' @return result from rsconnect::deployApp
deploy_unified_suite <- function(account, appName = "easybreeder-suite", ...) {
  deploy_suite(account = account, appName = appName, ...)
}

#' Print suite env vars for legacy multi-app mode
#' @param child_urls optional named list; names from names(child_specs)
print_suite_env_vars <- function(child_urls = NULL) {
  message("Configure these env vars only for legacy multi-app mode:")
  for (nm in names(child_specs)) {
    env_name <- child_specs[[nm]]$env
    url <- if (!is.null(child_urls) && nm %in% names(child_urls)) child_urls[[nm]] else "<https://...>"
    message("  ", env_name, " = ", url)
  }
}
