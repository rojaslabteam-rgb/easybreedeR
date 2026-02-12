utils::globalVariables(c(
  "child_paths",
  "suite_ui",
  "diagnose_suite",
  "iframe_panel",
  "load_child_runner",
  "get_requested_child",
  "build_self_child_url",
  ".requested_child_from_query"
))

run_easybreedeR_Studio <- function() {
  ui <- function(request) {
    requested_child <- .requested_child_from_query(tryCatch(request$QUERY_STRING, error = function(e) ""))
    if (!is.null(requested_child)) {
      child_runner <- tryCatch(load_child_runner(requested_child), error = function(e) e)
      if (inherits(child_runner, "error")) {
        return(
          shiny::fluidPage(
            shiny::tags$div(
              style = "max-width:900px; margin:60px auto; padding:24px; border:1px solid #e0e0e0; border-radius:10px;",
              shiny::tags$h3("Child app failed to load"),
              shiny::tags$p(shiny::strong("App: "), requested_child),
              shiny::tags$pre(conditionMessage(child_runner))
            )
          )
        )
      }
      return(child_runner$ui)
    }
    suite_ui()
  }

  # server logic
  server <- function(input, output, session) {
    requested_child <- get_requested_child(session)
    if (!is.null(requested_child)) {
      child_runner <- tryCatch(load_child_runner(requested_child), error = function(e) e)
      if (inherits(child_runner, "error")) return(invisible(NULL))
      child_runner$server(input, output, session)
      return(invisible(NULL))
    }

    # diagnostics on startup
    diag <- diagnose_suite()
    issues <- character(0)
    if (!all(diag$child_dirs_exist)) issues <- c(issues, paste0("Missing child app directories: ", paste(names(diag$child_dirs_exist)[!diag$child_dirs_exist], collapse = ", ")))
    if (!all(diag$child_appR_exist)) issues <- c(issues, paste0("Missing app.R: ", paste(names(diag$child_appR_exist)[!diag$child_appR_exist], collapse = ", ")))
    # Always show a brief startup notification with log location
    log_hint <- tryCatch(get("suite_log_path", envir = as.environment("package:base")), error = function(e) NULL)
    # If above fails, construct from runtime env
    if (is.null(log_hint)) {
      log_hint <- tryCatch(normalizePath(file.path(dirname(get("APP_DIR", inherits = TRUE)), "suite_diagnostics.log"), winslash = "/", mustWork = FALSE), error = function(e) "suite_diagnostics.log")
    }
    if (length(issues) > 0) {
      shiny::showNotification(paste("Diagnostics:", paste(issues, collapse = " | "), "| Log:", log_hint), type = "error", duration = NULL)
      shiny::showModal(shiny::modalDialog(
        title = "Environment Diagnostics",
        shiny::tagList(lapply(issues, function(x) shiny::tags$p(x))),
        easyClose = TRUE, footer = shiny::modalButton("Close")
      ))
    } else {
      shiny::showNotification(paste("Self-check complete, no issues | Log:", log_hint), type = "message", duration = 5)
    }
    # Source language helpers if not already loaded
    # Language.R is at easybreedeR/Language.R, Studio is at easybreedeR/easybreedeR_Studio/
    lang_path <- try({
      app_dir <- try(get("APP_DIR", inherits = TRUE), silent = TRUE)
      if (inherits(app_dir, "try-error")) app_dir <- getwd()
      # From Suite dir, go up one level to easybreedeR/, then Language.R
      normalizePath(file.path(dirname(app_dir), "Language.R"), winslash = "/", mustWork = FALSE)
    }, silent = TRUE)
    if (!inherits(lang_path, "try-error") && file.exists(lang_path)) {
      try(source(lang_path, local = FALSE), silent = TRUE)
    }
    
    # Safe wrapper around shared get_label
    suite_safe_get_label <- function(key, lang) {
      # Try shared helper first
      if (exists("get_label", mode = "function")) {
        out <- tryCatch(get_label(key, lang), error = function(e) NULL)
        if (!is.null(out)) return(out)
      } else {
        # Attempt to source Language.R once more if missing
        if (!inherits(lang_path, "try-error") && file.exists(lang_path)) {
          try(source(lang_path, local = FALSE), silent = TRUE)
        }
        if (exists("get_label", mode = "function")) {
          out <- tryCatch(get_label(key, lang), error = function(e) NULL)
          if (!is.null(out)) return(out)
        }
      }
      # Fallback: perform a lightweight lookup if TRANSLATIONS is available
      if (exists("TRANSLATIONS", mode = "list")) {
        norm_lang <- tolower(trimws(as.character(lang)))
        if (!nzchar(norm_lang)) norm_lang <- "en"
        if (grepl("portugu", norm_lang)) norm_lang <- "pt"
        if (norm_lang %in% c("english", "eng")) norm_lang <- "en"
        if (key %in% names(TRANSLATIONS)) {
          tr <- TRANSLATIONS[[key]]
          if (norm_lang %in% names(tr)) return(tr[[norm_lang]])
          if ("en" %in% names(tr)) return(tr[["en"]])
        }
      }
      # Last resort: show key
      key
    }
    
    # Render hero content with translations
    output$hero_content <- shiny::renderUI({
      # Ensure get_label is available (reload if needed)
      if (!exists("get_label", envir = .GlobalEnv)) {
        lang_path <- try({
          app_dir <- try(get("APP_DIR", inherits = TRUE), silent = TRUE)
          if (inherits(app_dir, "try-error")) app_dir <- getwd()
          normalizePath(file.path(dirname(app_dir), "Language.R"), winslash = "/", mustWork = FALSE)
        }, silent = TRUE)
        if (!inherits(lang_path, "try-error") && file.exists(lang_path)) {
          try(source(lang_path, local = FALSE), silent = TRUE)
        }
      }
      lang <- suite_language()
      lang_code <- tryCatch(language_code(lang), error = function(e) "en")
      tagList(
        div(class = "hero",
            span(class = "kicker", suite_safe_get_label("hero_kicker", lang_code)),
            tags$h1(suite_safe_get_label("hero_title", lang_code)),
            div(class = "subhead", suite_safe_get_label("hero_subhead", lang_code))
        )
      )
    })

    # Dynamic tab labels (translate according to suite_language)
    output$tab_home <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("suite_title", code))
    })
    output$tab_dataviewer <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("dataviewer_app_name", code))
    })
    output$tab_pediviewer <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("pediviewer_app_name", code))
    })
    output$tab_genoviewer <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("genoviewer_app_name", code))
    })
    output$tab_easyblup <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("easyblup_app_name", code))
    })
    # Home page cards and sections (multi-language)
    output$home_page_content <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      l <- function(key) suite_safe_get_label(key, code)
      shiny::tagList(
        div(class = "cards-container",
          div(class = "section-title",
            shiny::h2(
              shiny::tags$span(class = "material-symbols-outlined", "apps"),
              l("suite_interactive_modules")
            )
          ),
          shiny::fluidRow(
            shiny::column(3,
              div(class = "app-card",
                div(class = "app-card-header",
                  div(class = "app-card-icon blue",
                    shiny::tags$span(class = "material-symbols-outlined blue", "bar_chart")
                  ),
                  div(class = "app-card-title-wrapper",
                    shiny::h3(l("dataviewer_app_name")),
                    shiny::p(l("suite_dataviewer_desc"))
                  )
                ),
                shiny::actionButton("open_dataviewR", l("suite_launch"), class = "btn-primary")
              )
            ),
            shiny::column(3,
              div(class = "app-card",
                div(class = "app-card-header",
                  div(class = "app-card-icon green",
                    shiny::tags$span(class = "material-symbols-outlined green", "account_tree")
                  ),
                  div(class = "app-card-title-wrapper",
                    shiny::h3(l("pediviewer_app_name")),
                    shiny::p(l("suite_pediviewer_desc"))
                  )
                ),
                shiny::actionButton("open_pediviewer", l("suite_launch"), class = "btn-primary")
              )
            ),
            shiny::column(3,
              div(class = "app-card",
                div(class = "app-card-header",
                  div(class = "app-card-icon orange",
                    shiny::tags$span(class = "material-symbols-outlined orange", "scatter_plot")
                  ),
                  div(class = "app-card-title-wrapper",
                    shiny::h3(l("genoviewer_app_name")),
                    shiny::p(l("suite_genoviewer_desc"))
                  )
                ),
                shiny::actionButton("open_genoviewer", l("suite_launch"), class = "btn-primary")
              )
            ),
            shiny::column(3,
              div(class = "app-card",
                div(class = "app-card-header",
                  div(class = "app-card-icon red",
                    shiny::tags$span(class = "material-symbols-outlined red", "show_chart")
                  ),
                  div(class = "app-card-title-wrapper",
                    shiny::h3(l("easyblup_app_name")),
                    shiny::p(l("suite_easyblup_desc"))
                  )
                ),
                shiny::actionButton("open_easyblup", l("suite_launch"), class = "btn-primary")
              )
            )
          ),
          div(class = "section-title", style = "margin-top: 48px;",
            shiny::h2(
              shiny::tags$span(class = "material-symbols-outlined", "extension"),
              l("suite_optional_dependencies")
            )
          ),
          shiny::fluidRow(
            shiny::column(6,
              div(class = "app-card dependency-card",
                div(class = "app-card-header",
                  div(class = "app-card-icon purple",
                    shiny::tags$span(class = "material-symbols-outlined purple", "code")
                  ),
                  div(class = "app-card-title-wrapper",
                    shiny::h3("plinkR"),
                    shiny::p(l("suite_plinkr_desc"))
                  )
                ),
                shiny::tags$a(
                  href = "https://github.com/Thymine2001/plinkR",
                  target = "_blank",
                  class = "btn btn-primary",
                  style = "width: 100%; padding: 14px 24px; font-size: 16px; font-weight: 600; border-radius: 6px; text-decoration: none; display: block; text-align: center;",
                  l("suite_view_on_github")
                )
              )
            ),
            shiny::column(6,
              div(class = "app-card dependency-card",
                div(class = "app-card-header",
                  div(class = "app-card-icon teal",
                    shiny::tags$span(class = "material-symbols-outlined teal", "settings")
                  ),
                  div(class = "app-card-title-wrapper",
                    shiny::h3("linkbreedeR"),
                    shiny::p(l("suite_linkbreedeR_desc"))
                  )
                ),
                shiny::tags$a(
                  href = "https://github.com/Thymine2001/linkbreedeR",
                  target = "_blank",
                  class = "btn btn-primary",
                  style = "width: 100%; padding: 14px 24px; font-size: 16px; font-weight: 600; border-radius: 6px; text-decoration: none; display: block; text-align: center;",
                  l("suite_view_on_github")
                )
              )
            )
          ),
          div(class = "section-title", style = "margin-top: 48px;",
            shiny::h2(
              shiny::tags$span(class = "material-symbols-outlined", "library_books"),
              l("suite_required_dependencies")
            )
          ),
          shiny::fluidRow(
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/rstudio/shiny", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "shiny")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/rstudio/bslib", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "bslib")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/rstudio/DT", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "DT")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/jeroen/jsonlite", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "jsonlite"))))
          ),
          shiny::fluidRow(
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/ropensci/plotly", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "plotly")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/thomasp85/shinyFiles", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "shinyFiles")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/r-lib/fs", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "fs")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/datastorm-open/visNetwork", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "visNetwork"))))
          ),
          shiny::fluidRow(
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/igraph/igraph", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "igraph")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/tidyverse/readxl", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "readxl")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/RcppCore/Rcpp", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "Rcpp")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/Rdatatable/data.table", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "data.table"))))
          ),
          shiny::fluidRow(
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/jeroen/curl", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "curl")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/r-lib/testthat", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "testthat")))),
            shiny::column(3, div(class = "dependency-item", shiny::tags$a(href = "https://github.com/Rpedigree/pedigreeTools", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "pedigreeTools"))))
          ),
          div(class = "section-title", style = "margin-top: 48px;",
            shiny::h2(
              shiny::tags$span(class = "material-symbols-outlined", "computer"),
              l("suite_related_software")
            )
          ),
          shiny::fluidRow(
            shiny::column(6, div(class = "dependency-item", shiny::tags$a(href = "https://www.cog-genomics.org/plink/", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "PLINK")))),
            shiny::column(6, div(class = "dependency-item", shiny::tags$a(href = "http://nce.ads.uga.edu/wiki/doku.php?id=readme.blupf90", target = "_blank", class = "dependency-link", shiny::tags$span(class = "dependency-name", "BLUPF90"))))
          )
        )
      )
    })
    
    # Render README content from GitHub
    output$readmeContent <- shiny::renderUI({
      # Fetch README from GitHub raw content
      readme_url <- "https://raw.githubusercontent.com/rojaslabteam-rgb/easybreedeR/main/README.md"
      
      # Try to fetch from GitHub
      readme_text <- try({
        if (requireNamespace("curl", quietly = TRUE)) {
          readme_con <- curl::curl(readme_url)
          on.exit(close(readme_con), add = TRUE)
          readLines(readme_con, warn = FALSE, encoding = "UTF-8")
        } else if (requireNamespace("httr", quietly = TRUE)) {
          response <- httr::GET(readme_url)
          if (httr::status_code(response) == 200) {
            content <- httr::content(response, as = "text", encoding = "UTF-8")
            strsplit(content, "\n", fixed = TRUE)[[1]]
          } else {
            stop("Failed to fetch README")
          }
        } else {
          # Fallback: try base R download
          temp_file <- tempfile(fileext = ".md")
          download.file(readme_url, temp_file, quiet = TRUE, mode = "wb")
          on.exit(unlink(temp_file), add = TRUE)
          readLines(temp_file, warn = FALSE, encoding = "UTF-8")
        }
      }, silent = TRUE)
      
      if (!inherits(readme_text, "try-error") && length(readme_text) > 0) {
        # Convert markdown to HTML
        html_content <- paste(readme_text, collapse = "\n")
        
        # Try to use markdown package if available
        if (requireNamespace("markdown", quietly = TRUE)) {
          tryCatch({
            html_content <- markdown::markdownToHTML(
              text = html_content,
              fragment.only = TRUE
            )
          }, error = function(e) {
            # Fallback to basic conversion
            html_content <- gsub("^# (.+)$", "<h1>\\1</h1>", html_content, perl = TRUE)
            html_content <- gsub("^## (.+)$", "<h2>\\1</h2>", html_content, perl = TRUE)
            html_content <- gsub("^### (.+)$", "<h3>\\1</h3>", html_content, perl = TRUE)
            html_content <- gsub("`([^`]+)`", "<code>\\1</code>", html_content)
            html_content <- gsub("\n", "<br>", html_content)
          })
        } else {
          # Basic markdown conversion
          html_content <- gsub("^# (.+)$", "<h1>\\1</h1>", html_content, perl = TRUE)
          html_content <- gsub("^## (.+)$", "<h2>\\1</h2>", html_content, perl = TRUE)
          html_content <- gsub("^### (.+)$", "<h3>\\1</h3>", html_content, perl = TRUE)
          html_content <- gsub("`([^`]+)`", "<code>\\1</code>", html_content)
          html_content <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", html_content)
          # Fix relative links to point to GitHub (use capture groups only to avoid
          # Windows gsub passing closure as first arg and triggering as.character error)
          html_content <- gsub("\\[([^\\]]+)\\]\\(([^)]+)\\)", function(ignore, link_text, link_url) {
            if (!grepl("^https?://", link_url)) {
              link_url <- paste0("https://github.com/rojaslabteam-rgb/easybreedeR/blob/main/", link_url)
            }
            paste0("<a href='", link_url, "' target='_blank'>", link_text, "</a>")
          }, html_content, perl = TRUE)
          html_content <- gsub("\n", "<br>", html_content)
        }
        
        return(shiny::HTML(html_content))
      }
      
      # Fallback: show message with link to GitHub
      shiny::tagList(
        shiny::tags$p("Unable to load README from GitHub."),
        shiny::tags$p(
          "Please visit: ",
          shiny::tags$a(
            href = "https://github.com/rojaslabteam-rgb/easybreedeR#readme",
            target = "_blank",
            "https://github.com/rojaslabteam-rgb/easybreedeR"
          )
        )
      )
    })

    # Bottom-right gear language selection handler
    shiny::observeEvent(input$lang_select_bottom, ignoreInit = TRUE, {
      code <- as.character(input$lang_select_bottom)
      name <- switch(code,
                     zh = "中文",
                     pt = "Português",
                     "English")
      suite_language(name)
      suite_lang_timestamp(as.numeric(Sys.time()))
      shiny::showNotification(paste("Language:", name), type = "message", duration = 2)
    })

    # Top-right gear language selection handler
    shiny::observeEvent(input$lang_select_top, ignoreInit = TRUE, {
      code <- as.character(input$lang_select_top)
      name <- switch(code,
                     zh = "中文",
                     pt = "Português",
                     "English")
      suite_language(name)
      suite_lang_timestamp(as.numeric(Sys.time()))
      shiny::showNotification(paste("Language:", name), type = "message", duration = 2)
    })

    # Home quick-link buttons -> switch tabs
    shiny::observeEvent(input$open_dataviewR, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "dataviewR")
    })
    shiny::observeEvent(input$open_pediviewer, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "pediviewer")
    })
    shiny::observeEvent(input$open_genoviewer, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "genoviewer")
    })
    shiny::observeEvent(input$open_easyblup, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "easyblup")
    })
    # Remove old modal-based language selection (replaced by gear button)

    # Render iframes with selected language as query param.
    lang_code <- shiny::reactive(language_code(suite_language()))

    render_child_iframe <- function(app_name) {
      shiny::renderUI({
        shiny::req(lang_code())
        lang_val <- lang_code()
        timestamp <- suite_lang_timestamp()
        url <- build_self_child_url(app_name = app_name, session = session, lang = lang_val, timestamp = timestamp)
        if (!nzchar(url)) {
          shiny::invalidateLater(1200)
          return(tags$div(
            style = "height:30vh; display:flex; align-items:center; justify-content:center; color:#666;",
            tags$div(tags$span("Preparing ", shiny::strong(app_name), " ..."))
          ))
        }
        iframe_panel(url)
      })
    }

    output$frame_dataviewer <- render_child_iframe("dataviewer")
    output$frame_pediviewer   <- render_child_iframe("pediviewer")
    output$frame_genoviewer   <- render_child_iframe("genoviewer")
    output$frame_easyblup     <- render_child_iframe("easyblupf90")

  }

  list(ui = ui, server = server)
}
