utils::globalVariables(c("child_apps", "child_paths", "suite_ui", "diagnose_suite", "is_alive", "start_child"))

run_easybreedeR_Studio <- function() {
  # build UI
  ui <- suite_ui()

  # server logic
  server <- function(input, output, session) {
    # diagnostics on startup
    diag <- diagnose_suite()
    issues <- character(0)
    if (!isTRUE(diag$rscript_exists)) issues <- c(issues, "Rscript not found")
    if (!all(diag$child_dirs_exist)) issues <- c(issues, paste0("Missing child app directories: ", paste(names(diag$child_dirs_exist)[!diag$child_dirs_exist], collapse = ", ")))
    if (!all(diag$child_appR_exist)) issues <- c(issues, paste0("Missing app.R: ", paste(names(diag$child_appR_exist)[!diag$child_appR_exist], collapse = ", ")))
    if (any(diag$ports_in_use)) {
      busy <- names(diag$ports_in_use)[diag$ports_in_use]
      issues <- c(issues, paste0("Ports in use: ", paste(busy, collapse = ", ")))
    }
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
    # ensure children alive or spawn them once
    shiny::observeEvent(TRUE, {
      # Use detected IP for health check (same logic as get_child_url)
      check_ip <- tryCatch({
        if (!is.null(session$clientData) && !is.null(session$clientData$url_hostname)) {
          hostname <- session$clientData$url_hostname
          if (hostname %in% c("127.0.0.1", "localhost", "::1")) {
            detected_ip <- tryCatch(.get_local_ip(), error = function(e) "127.0.0.1")
            if (detected_ip != "127.0.0.1") detected_ip else "127.0.0.1"
          } else {
            hostname
          }
        } else {
          tryCatch(.get_local_ip(), error = function(e) "127.0.0.1")
        }
      }, error = function(e) tryCatch(.get_local_ip(), error = function(e) "127.0.0.1"))
      
      if (!is_alive(paste0("http://", check_ip, ":8001"))) {
        pid <- start_child(child_paths$datapreviewer, 8001)
        if (!is.na(pid)) .suite_child_pids$pids <- c(.suite_child_pids$pids, pid)
      }
      if (!is_alive(paste0("http://", check_ip, ":8002"))) {
        pid <- start_child(child_paths$pediviewer, 8002)
        if (!is.na(pid)) .suite_child_pids$pids <- c(.suite_child_pids$pids, pid)
      }
      if (!is_alive(paste0("http://", check_ip, ":8003"))) {
        pid <- start_child(child_paths$easyblupf90, 8003)
        if (!is.na(pid)) .suite_child_pids$pids <- c(.suite_child_pids$pids, pid)
      }
      if (!is_alive(paste0("http://", check_ip, ":8004"))) {
        pid <- start_child(child_paths$core_tools, 8004)
        if (!is.na(pid)) .suite_child_pids$pids <- c(.suite_child_pids$pids, pid)
      }
    }, once = TRUE)

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
    output$tab_datapreviewer <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("datapreviewer_app_name", code))
    })
    output$tab_pediviewer <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("pediviewer_app_name", code))
    })
    output$tab_easyblup <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("easyblup_app_name", code))
    })
    output$tab_rnotebook <- shiny::renderUI({
      lang <- suite_language()
      code <- tryCatch(language_code(lang), error = function(e) "en")
      shiny::tags$span(suite_safe_get_label("rnotebook_app_name", code))
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
    shiny::observeEvent(input$open_datapreviewR, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "datapreviewR")
    })
    shiny::observeEvent(input$open_pediviewer, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "pediviewer")
    })
    shiny::observeEvent(input$open_easyblup, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "easyblup")
    })
    shiny::observeEvent(input$open_rnotebook, ignoreInit = TRUE, {
      shiny::updateNavbarPage(session, "main_nav", selected = "rnotebook")
    })

    # Remove old modal-based language selection (replaced by gear button)

    # Render iframes with selected language as query param, but wait until service is alive
    lang_code <- shiny::reactive(language_code(suite_language()))

    # Helper to get child app URL based on current request hostname
    get_child_url <- function(port, session) {
      # Get the hostname from the current request
      # This ensures iframe URLs work for both local and remote access
      hostname <- tryCatch({
        # Try to get from session clientData (available in Shiny)
        if (!is.null(session$clientData) && !is.null(session$clientData$url_hostname)) {
          hostname <- session$clientData$url_hostname
          # If hostname is localhost/127.0.0.1, try to use detected IP for LAN access
          if (hostname %in% c("127.0.0.1", "localhost", "::1")) {
            # Use detected IP if available, otherwise keep localhost
            detected_ip <- tryCatch(.get_local_ip(), error = function(e) "127.0.0.1")
            if (detected_ip != "127.0.0.1") {
              return(paste0("http://", detected_ip, ":", port))
            }
          }
          # Use the request hostname (works for both local and remote)
          return(paste0("http://", hostname, ":", port))
        }
        # Fallback: use detected IP or localhost
        detected_ip <- tryCatch(.get_local_ip(), error = function(e) "127.0.0.1")
        paste0("http://", detected_ip, ":", port)
      }, error = function(e) {
        # Ultimate fallback
        detected_ip <- tryCatch(.get_local_ip(), error = function(e) "127.0.0.1")
        paste0("http://", detected_ip, ":", port)
      })
    }

    render_waiting_iframe <- function(port, lang_code_reactive, label, session) {
      shiny::renderUI({
        shiny::req(lang_code_reactive())
        # Dynamically generate URL based on current request hostname
        url_base <- get_child_url(port, session)
        # Use timestamp reactive to force browser refresh when language changes
        lang_val <- lang_code_reactive()
        timestamp <- suite_lang_timestamp()
        url <- paste0(url_base, "?lang=", lang_val, "&_t=", timestamp)
        if (is_alive(url_base)) {
          tags$div(style = "position:relative; height:95vh; width:100%; overflow:hidden;",
                   tags$iframe(src = url, width = "100%", height = "100%", frameborder = "0",
                               style = "border:none; overflow:hidden;"))
        } else {
          shiny::invalidateLater(800)
          tags$div(style = "height:30vh; display:flex; align-items:center; justify-content:center; color:#666;",
                   tags$div(tags$span("Starting ", strong(label), " …")))
        }
      })
    }

    output$frame_datapreviewer <- render_waiting_iframe(8001, lang_code, "datapreviewR", session)
    output$frame_pediviewer   <- render_waiting_iframe(8002, lang_code, "PedivieweR", session)
    output$frame_easyblup     <- render_waiting_iframe(8003, lang_code, "easyblup", session)
    output$frame_rnotebook    <- render_waiting_iframe(8004, lang_code, "RNotebook", session)

    session$onSessionEnded(function() {
      if (length(.suite_child_pids$pids) > 0) {
        for (p in unique(.suite_child_pids$pids)) {
          if (!is.na(p)) {
            try(tools::pskill(p), silent = TRUE)
          }
        }
      }
    })
  }

  list(ui = ui, server = server)
}


