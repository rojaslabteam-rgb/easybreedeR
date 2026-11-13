app_file <- function(...) {
  system.file(..., package = "easybreedeR", mustWork = TRUE)
}

source_helpers <- function(file) {
  env <- new.env(parent = globalenv())
  sys.source(app_file(file), envir = env)
  env
}

load_test_app <- function(name, unavailable = character()) {
  env <- new.env(parent = globalenv())
  env$requireNamespace <- function(package, quietly = TRUE, ...) {
    if (package %in% unavailable) return(FALSE)
    base::requireNamespace(package, quietly = quietly, ...)
  }
  withr::local_dir(app_file(name))
  withr::local_envvar(EASYBREEDER_SOURCE_ONLY = "1")
  source("app.R", local = env)
  env
}
