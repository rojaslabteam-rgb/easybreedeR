test_that("single-trait animal model exports the expected data and effect columns", {
  withr::local_options(options())
  app <- load_test_app("easyblup", unavailable = "plinkR")
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("ID,weight,herd", "A,10,H1", "B,12,H2"), path)
  shiny::testServer(app$server, {
    session$setInputs(
      pheno_file = data.frame(name = "phenotype.csv", datapath = path,
                             size = file.info(path)$size, type = "text/csv"),
      opt_pe = FALSE, opt_mat = FALSE, opt_mpe = FALSE,
      opt_remove_all_missing = FALSE, opt_missing_in_weights = FALSE,
      opt_no_basic_statistics = FALSE, opt_sol_se = FALSE, opt_origID = FALSE,
      opt_store_accuracy = FALSE, opt_store_accuracy_orig = FALSE,
      opt_method = "BLUP", opt_missing_value = "-999"
    )
    values$traits <- "weight"
    values$fixed <- "herd"
    values$animal <- "ID"
    session$flushReact()
    card <- values$current_param
    expect_match(card, "DATAFILE\nphenotype.csv\n", fixed = TRUE)
    expect_match(card, "TRAITS # Specify trait columns\n2\n", fixed = TRUE)
    expect_match(card, "EFFECT\n3 cross alpha # herd fixed effect", fixed = TRUE)
    expect_match(card, "EFFECT\n1 cross alpha # Animal ID effect", fixed = TRUE)
    expect_match(card, "RESIDUAL_VARIANCE\n0.1\n", fixed = TRUE)
    expect_match(card, "OPTION method BLUP", fixed = TRUE)
    expect_match(card, "OPTION missing -999", fixed = TRUE)
  })
})
