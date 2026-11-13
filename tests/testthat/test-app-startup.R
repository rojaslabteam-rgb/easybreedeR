test_that("all core standalone apps expose a UI and server", {
  withr::local_options(options())
  on.exit(graphics.off(), add = TRUE)
  for (name in c("datavieweR", "pedivieweR", "genovieweR", "easyblup")) {
    devices_before <- grDevices::dev.list()
    app <- load_test_app(name, unavailable = c("plinkR", "linkbreedeR", "pedigreeTools", "gridExtra", "cowplot"))
    expect_true(is.function(app$server), info = name)
    expect_false(is.null(app$ui), info = name)
    if (name == "genovieweR") expect_true(app$use_rcpp_stats)
    if (name == "pedivieweR") expect_true(app$use_fast_inbreeding_cpp)
    expect_identical(grDevices::dev.list(), devices_before, info = name)
  }
})

test_that("Studio language changes belong to one session", {
  withr::local_options(options())
  withr::local_envvar(c(EASYBREEDER_DEPLOY_MODE = "", R_CONFIG_ACTIVE = "shinyapps"))
  # Load support files into a private environment, as used by the suite runner.
  env <- new.env(parent = globalenv())
  env$APP_DIR <- app_file("easybreedeR_Studio")
  withr::local_dir(env$APP_DIR)
  for (file in c("Global.R", "Page_Suite.R", "run_easybreedeR_Studio.R")) {
    sys.source(file.path("R", file), envir = env)
  }
  expect_identical(env$.detect_deploy_mode(), "hosted")
  expect_false(exists("suite_language", env, inherits = FALSE))
  expect_true(all(env$diagnose_suite()$child_appR_exist))
  if (requireNamespace("curl", quietly = TRUE)) {
    testthat::local_mocked_bindings(curl = function(...) textConnection("# Test README"), .package = "curl")
  }
  runner <- env$run_easybreedeR_Studio()
  start_session <- function() {
    session <- shiny::MockShinySession$new()
    scope <- list2env(list(input = session$input, output = session$output,
                          session = session), parent = environment(runner$server))
    shiny::withReactiveDomain(session, shiny::isolate(eval(body(runner$server), scope)))
    list(session = session, scope = scope)
  }
  a <- start_session()
  b <- start_session()
  on.exit({ a$session$close(); b$session$close() }, add = TRUE)
  a$session$setInputs(lang_select_top = "en")
  b$session$setInputs(lang_select_top = "en")
  a$session$setInputs(lang_select_top = "zh")
  stamp <- shiny::isolate(a$scope$suite_lang_timestamp())
  b$session$setInputs(lang_select_top = "pt")
  expect_identical(shiny::isolate(a$scope$suite_language()), "中文")
  expect_identical(shiny::isolate(b$scope$suite_language()), "Português")
  expect_identical(shiny::isolate(a$scope$suite_lang_timestamp()), stamp)
})
