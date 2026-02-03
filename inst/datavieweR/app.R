# Open BreedX (OBX) Clean Three-Panel Layout
# Version: 0.4.0 (Fixed Language Settings)
# Created: 2025-10-22
# Last Modified: 2025-10-31

# ============================================================================
# LIBRARIES
# ============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(readxl)
  library(DT)
  library(ggplot2)
  library(magrittr)
  library(tools)
})

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Source shared language helpers
try(source(normalizePath(file.path("..", "Language.R"), winslash = "/", mustWork = FALSE), local = TRUE), silent = TRUE)

# Capture shared get_label before defining local alias
shared_get_label <- if (exists("get_label", mode = "function")) get_label else NULL

# Use shared get_label, but wrap it to add app prefix automatically
get_label_local <- function(key, lang = "en") {
  # Use shared get_label if available
  gl <- shared_get_label
  if (is.null(gl) || !is.function(gl)) return(key)
  # Only prefix if not already prefixed
  effective_key <- if (startsWith(key, "dataviewer_")) key else paste0("dataviewer_", key)
  result <- try(gl(effective_key, lang, app = "dataviewer"), silent = TRUE)
  if (inherits(result, "try-error")) {
    # Fallback to direct key lookup (for non-prefixed/global keys)
    result <- try(gl(key, lang, app = "dataviewer"), silent = TRUE)
    if (inherits(result, "try-error")) return(key)
  }
  result
}

# Alias for compatibility (safe: uses captured shared_get_label inside)
get_label <- get_label_local

# Convert missing values
convert_missing_values <- function(data, format = "na") {
  if (!is.data.frame(data)) return(data)
  if (format == "na") return(data)
  
  result <- data
  numeric_cols <- sapply(result, is.numeric)
  
  if (format == "zero") {
    for (i in which(numeric_cols)) {
      result[[i]][is.na(result[[i]])] <- 0
    }
  } else if (format == "minus999") {
    for (i in which(numeric_cols)) {
      result[[i]][is.na(result[[i]])] <- -999
    }
  }
  result
}

# Convert values to NA
convert_values_to_na <- function(data, missing_values) {
  if (!is.data.frame(data)) return(data)
  result <- data
  
  for (i in seq_along(result)) {
    col <- result[[i]]
    if (is.numeric(col)) {
      for (val in missing_values) {
        if (val == "NA") next
        else if (val == "0") result[[i]][col == 0] <- NA
        else if (val == "-999") result[[i]][col == -999] <- NA
        else {
          num_val <- tryCatch(as.numeric(val), error = function(e) NA)
          if (!is.na(num_val)) result[[i]][col == num_val] <- NA
        }
      }
    } else if (is.character(col)) {
      for (val in missing_values) {
        if (val == "NA") next
        else if (val == "") result[[i]][col == ""] <- NA
        else result[[i]][col == val] <- NA
      }
    }
  }
  result
}

# ============================================================================
# USER INTERFACE
# ============================================================================
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#CEB888",
    base_font = font_google("Crimson Text")
  ),
  shinyjs::useShinyjs(),
  
  # JavaScript for CSV download
  tags$script("
    Shiny.addCustomMessageHandler('downloadCSV', function(message) {
      var blob = new Blob([message.content], { type: 'text/csv;charset=utf-8;' });
      var link = document.createElement('a');
      var url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', message.filename);
      link.style.visibility = 'hidden';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    });
  "),
  
  # Custom CSS for layout
  tags$head(
    tags$style(HTML("
      /* Page setup */
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
      }
      
      .page-fillable {
        height: 100vh;
        overflow: hidden;
        display: flex;
        flex-direction: column;
      }
      
      /* Title bar */
      .title-bar {
        background: linear-gradient(135deg, #CEB888 0%, #B89D5D 100%);
        padding: 15px 20px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
        border-bottom: 3px solid #CFB991;
        flex-shrink: 0;
      }
      
      .title-bar h1 {
        margin: 0;
        font-size: clamp(1.5rem, 3vw, 2.5rem);
        font-weight: 700;
        color: #000000;
        font-family: 'Crimson Text', 'Noto Sans SC', 'PingFang SC', 'Microsoft YaHei', 'Heiti SC', 'SimSun', 'Noto Sans', Arial, sans-serif;
      }
      .title-bar p { margin: 5px 0 0 0; font-size: 1rem; color: #000000; opacity: .9; 
        font-family: 'Crimson Text', 'Noto Sans SC', 'PingFang SC', 'Microsoft YaHei', 'Heiti SC', 'SimSun', 'Noto Sans', Arial, sans-serif; }
      /* Button styles aligned with PedivieweR 2.0 */
      .btn-primary, .btn.btn-primary { background-color:#CEB888 !important; border-color:#CEB888 !important; color:#000000 !important; font-weight:600; }
      .btn-primary:hover, .btn.btn-primary:hover { background-color:#B89D5D !important; border-color:#B89D5D !important; }
      .btn-secondary { background-color:#95A5A6 !important; border-color:#95A5A6 !important; color:#fff !important; }
      .btn-secondary:hover { background-color:#7F8C8D !important; border-color:#7F8C8D !important; }
      /* Tabs style aligned with PedivieweR navset */
      .nav-tabs { border-bottom: none; }
      .nav-tabs .nav-link {
        font-weight:700; color:#000 !important;
        border:none; border-bottom:3px solid transparent;
        background:transparent !important; padding:10px 18px;
      }
      .nav-tabs .nav-link:hover { border-bottom-color:#B89D5D; background:transparent !important; }
      .nav-tabs .nav-link.active {
        color:#000 !important;
        background:#FFFFFF !important;
        border:2px solid #CEB888 !important;
        border-bottom:0 !important;
        border-top-left-radius:8px; border-top-right-radius:8px;
        box-shadow:0 2px 6px rgba(0,0,0,0.06);
      }
      /* top separator below tabs to match card seam */
      .tab-content { border-top:2px solid #CEB888; }
      /* bslib nav-underline variant fallback */
      .nav-underline .nav-link { font-weight:700; color:#000 !important; }
      .nav-underline .nav-link.active { color:#000 !important; border-bottom:3px solid #CEB888; }
      
      /* Three-panel container */
      .three-panel-container {
        display: flex;
        flex: 1;
        overflow: hidden;
        position: relative;
      }
      
      /* Left panel */
      .left-panel {
        width: 320px;
        overflow-y: auto;
        overflow-x: hidden;
        background-color: #FEFEFE;
        border-right: 2px solid #CFB991;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        padding: 20px;
        flex-shrink: 0;
      }
      
      .left-panel.hidden {
        width: 0;
        padding: 0 0;
        overflow: hidden;
      }
      
      /* Right panel */
      .right-panel {
        width: 320px;
        overflow-y: auto;
        overflow-x: hidden;
        background-color: #FEFEFE;
        border-left: 2px solid #CFB991;
        transition: all 0.4s cubic-bezier(0.4, 0, 0.2, 1);
        padding: 20px;
        flex-shrink: 0;
      }
      /* hide visible scrollbars but keep scroll behavior (match later apps) */
      .left-panel, .right-panel { -ms-overflow-style: none; scrollbar-width: none; }
      .left-panel::-webkit-scrollbar, .right-panel::-webkit-scrollbar { display: none; }
      
      .right-panel.hidden {
        width: 0;
        padding: 0 0;
        overflow: hidden;
      }
      
      /* Center panel */
      .center-panel {
        flex: 1;
        overflow-y: auto;
        overflow-x: hidden;
        padding: 20px;
        background-color: #FFFFFF;
      }
      
      /* Toggle handles - slim vertical bars centered on edges */
      .toggle-btn-left, .toggle-btn-right {
        position: fixed;
        top: 50%;
        transform: translateY(-50%);
        z-index: 1100;
      }

      .toggle-btn-left { left: 8px; }
      .toggle-btn-right { right: 8px; }

      /* Prevent any positional shift when panels open/close */
      .toggle-btn-left.panel-open { left: 8px; }
      .toggle-btn-right.panel-open { right: 8px; }

      .toggle-btn-left .btn, .toggle-btn-right .btn {
        width: 14px;
        height: 64px;
        padding: 0;
        border-radius: 8px;
        border: 2px solid #CFB991;
        background-color: #CEB888;
        color: #000000;
        box-shadow: 0 4px 12px rgba(0,0,0,0.2);
        display: flex;
        align-items: center;
        justify-content: center;
        line-height: 1;
        font-size: 1.1rem;
        font-weight: 700;
      }

      .toggle-btn-left .btn:hover, .toggle-btn-right .btn:hover {
        background-color: #B89D5D;
        box-shadow: 0 6px 16px rgba(0,0,0,0.3);
        transform: translateY(-1px);
      }
      
      /* Panel sections */
      .panel-section {
        background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%);
        border: 2px solid #CFB991;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(206, 184, 136, 0.15);
      }
      
      .section-title {
        font-size: 1.1rem;
        font-weight: 700;
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 2px solid #CEB888;
      }
      
      .subsection-title {
        font-size: 0.95rem;
        font-weight: 600;
        color: #34495e;
        margin-top: 12px;
        margin-bottom: 8px;
      }
      
      /* Button styling */
      .btn-primary, .btn.btn-primary {
        background-color: #CEB888 !important;
        border-color: #CEB888 !important;
        color: #000000 !important;
        font-weight: 600;
      }
      
      .btn-primary:hover, .btn.btn-primary:hover {
        background-color: #B89D5D !important;
        border-color: #B89D5D !important;
        color: #000000 !important;
      }
      
      .btn-success {
        background-color: #CEB888 !important;
        border-color: #CEB888 !important;
        color: #000000 !important;
      }
      
      .btn-success:hover {
        background-color: #B89D5D !important;
        border-color: #B89D5D !important;
      }
      
      .btn-outline-secondary {
        border-color: #CFB991;
        color: #34495e;
      }
      
      .btn-sm {
        font-size: 0.875rem;
        padding: 6px 12px;
      }
      
      /* Tab styling */
      .nav-tabs .nav-link.active {
        background-color: #CEB888 !important;
        border-color: #CEB888 !important;
        color: #000000 !important;
        font-weight: 600;
      }
      
      .nav-tabs .nav-link {
        color: #34495e;
      }
      
      .nav-tabs .nav-link:hover {
        background-color: #F5ECD7;
        border-color: #CFB991;
      }
      
      /* Form controls */
      .form-control, .form-select {
        border-color: #CFB991;
        font-size: 0.9rem;
      }
      
      .form-control:focus, .form-select:focus {
        border-color: #CEB888;
        box-shadow: 0 0 0 0.2rem rgba(206, 184, 136, 0.25);
      }
      
      .control-label {
        font-weight: 600;
        color: #34495e;
        font-size: 0.9rem;
        margin-bottom: 6px;
      }
      
      /* Color customization section */
      .color-section {
        margin-top: 15px;
      }
      
      .color-input-group {
        margin-bottom: 15px;
      }
      
      .color-palette-label {
        display: block;
        font-weight: 600;
        color: #34495e;
        font-size: 0.9rem;
        margin-bottom: 10px;
        margin-top: 15px;
      }
      
      .color-grid {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 8px;
        margin-top: 10px;
      }
      
      .color-grid .btn {
        width: 100%;
        aspect-ratio: 1;
        padding: 0;
        min-height: 40px;
        border: 2px solid #CFB991;
        transition: transform 0.2s;
      }
      
      .color-grid .btn:hover {
        transform: scale(1.1);
        border-color: #000000;
        box-shadow: 0 4px 8px rgba(0,0,0,0.2);
      }
      
      /* Scrollbar styling */
      .left-panel::-webkit-scrollbar, 
      .right-panel::-webkit-scrollbar,
      .center-panel::-webkit-scrollbar {
        width: 10px;
      }
      
      .left-panel::-webkit-scrollbar-track,
      .right-panel::-webkit-scrollbar-track,
      .center-panel::-webkit-scrollbar-track {
        background: #F8F9FA;
        border-radius: 5px;
      }
      
      .left-panel::-webkit-scrollbar-thumb,
      .right-panel::-webkit-scrollbar-thumb,
      .center-panel::-webkit-scrollbar-thumb {
        background: #CEB888;
        border-radius: 5px;
      }
      
      .left-panel::-webkit-scrollbar-thumb:hover,
      .right-panel::-webkit-scrollbar-thumb:hover,
      .center-panel::-webkit-scrollbar-thumb:hover {
        background: #B89D5D;
      }
      
      /* Help text */
      .help-block {
        font-size: 0.85rem;
        color: #6c757d;
        margin-top: 5px;
      }
      
      /* Action buttons row */
      .btn-row {
        display: flex;
        flex-direction: column;
        gap: 10px;
        width: 100%;
      }
      
      .btn-row .btn {
        width: 100%;
        padding: 10px;
        font-size: 0.95rem;
        font-weight: 600;
      }
      
      /* Responsive design */
      @media (max-width: 1200px) {
        .left-panel, .right-panel {
          width: 280px;
        }
        
        .toggle-btn-left.panel-open {
          left: 290px;
        }
        
        .toggle-btn-right.panel-open {
          right: 290px;
        }
      }
      
      @media (max-width: 992px) {
        .left-panel, .right-panel {
          width: 260px;
        }
        
        .color-grid {
          grid-template-columns: repeat(3, 1fr);
        }
      }
      
      @media (max-width: 768px) {
        .left-panel.hidden, .right-panel.hidden {
          display: none;
        }
        
        .color-grid {
          grid-template-columns: repeat(2, 1fr);
        }
      }
    "))
  ),
  
  # Title Bar
  div(class = "title-bar",
      h1(textOutput("appTitle")),
      p(textOutput("appSubtitle"))
  ),
  
  # Floating toggle handles (slim bars)
  div(id = "toggleLeftBtn", class = "toggle-btn-left",
    actionButton("toggleLeftPanel", HTML("&#10094;"),
          class = "btn btn-sm",
          title = "Show/Hide Controls")
  ),
  
  div(id = "toggleRightBtn", class = "toggle-btn-right",
    actionButton("toggleRightPanel", HTML("&#10095;"),
          class = "btn btn-sm",
          title = "Show/Hide Settings")
  ),
  
  # Three-panel layout container
  div(class = "three-panel-container",
      
      # LEFT PANEL - Functional Controls
      div(id = "leftPanel", class = "left-panel",
          
          # File Upload Section
      div(class = "panel-section",
        h4(textOutput("dataUploadTitle"), class = "section-title"),
              uiOutput("fileUploadUI")
          ),
          
          # Column Selection (moved here from visualization)
      div(class = "panel-section",
        h4(textOutput("columnSelectionTitle"), class = "section-title"),
              uiOutput("columnSelectionUI")
          ),
          
          # QC Filter Options
          div(class = "panel-section",
              h4(textOutput("qcFilterTitle"), class = "section-title"),
              uiOutput("qcModeUI"),
              conditionalPanel(
                condition = "input.qcMode == 'uniform'",
                uiOutput("uniformQCControls")
              ),
              conditionalPanel(
                condition = "input.qcMode == 'individual'",
                div(
                  h5(textOutput("individualQCTitle"), class = "subsection-title"),
                  uiOutput("individualQCControls")
                )
              ),
        div(style = "margin-top: 20px;",
          h5(textOutput("applyDownloadTitle"), class = "subsection-title"),
                  uiOutput("actionButtonsUI")
              )
          )
      ),
      
      # CENTER PANEL - Main Content
      div(class = "center-panel",
          navset_card_tab(
            id = "mainTabs",
            nav_panel(
              textOutput("dataPreviewTabTitle"),
              value = "data_preview",
              div(style = "margin-top: 20px;",
                  uiOutput("dataSummaryUI"),
                  uiOutput("plotDownloadUI"),
                  plotOutput("preDistPlot", height = "500px"),
                  br(),
                  DTOutput("previewTable")
              )
            ),
            nav_panel(
              textOutput("qcResultsTabTitle"),
              value = "qc_results",
              div(style = "margin-top: 20px;",
                  uiOutput("qcResultsContent")
              )
            )
          )
      ),
      
      # RIGHT PANEL - Visualization Settings
      div(id = "rightPanel", class = "right-panel",
          
          # Chart Type & Settings
          div(class = "panel-section",
              h4(textOutput("plotTypeTitle"), class = "section-title"),
              uiOutput("plotTypeUI"),
              uiOutput("binsUI")
          ),
          
          # Color Customization
          div(class = "panel-section",
              h4(textOutput("colorCustomizationTitle"), class = "section-title"),
              div(style = "text-align: center; margin-bottom: 15px;",
                  actionButton("toggleColorOptions",
                              textOutput("showColorOptionsText", inline = TRUE),
                              class = "btn btn-primary btn-sm")
              ),
              conditionalPanel(
                condition = "input.toggleColorOptions % 2 == 1",
                div(class = "color-section",
                    uiOutput("colorCustomizationUI")
                )
              )
          )
      )
  ),
  
  # JavaScript for smooth panel toggling
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      // Initial state
      var leftPanelOpen = true;
      var rightPanelOpen = true;
      
      // No-op (handles are pinned to edges)
      function updateButtonPositions() {}
      
      // Toggle left panel
      $(document).on('click', '#toggleLeftPanel', function() {
        leftPanelOpen = !leftPanelOpen;
        if (leftPanelOpen) {
          $('#leftPanel').removeClass('hidden');
        } else {
          $('#leftPanel').addClass('hidden');
        }
        updateButtonPositions();
      });
      
      // Toggle right panel
      $(document).on('click', '#toggleRightPanel', function() {
        rightPanelOpen = !rightPanelOpen;
        if (rightPanelOpen) {
          $('#rightPanel').removeClass('hidden');
        } else {
          $('#rightPanel').addClass('hidden');
        }
        updateButtonPositions();
      });
      
      // Initialize
      updateButtonPositions();
    });
  ")),
  
  uiOutput("hiddenDownloadLink")
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10 * 1024^3)
  
  # Color reactive values
  preFilterColor <- reactiveVal("#DB3124")
  postFilterColor <- reactiveVal("#4B74B2")
  plot_ready <- reactiveVal(FALSE)
  
  # Get current language using shared resolver
  current_lang <- reactive({
    resolved <- try(resolve_suite_lang(session, default = "en"), silent = TRUE)
    if (inherits(resolved, "try-error")) return("en")
    map_suite_lang_for_app(resolved, app = "dataviewer")
  })
  
  # App title (use unified translation keys)
  output$appTitle <- renderText({ get_label("dataviewer_app_name", current_lang()) })
  output$appSubtitle <- renderText({ get_label("dataviewer_app_subtitle", current_lang()) })
  output$dataUploadTitle <- renderText({ get_label("data_upload", current_lang()) })
  output$columnSelectionTitle <- renderText({ get_label("column_selection", current_lang()) })
  output$applyDownloadTitle <- renderText({ get_label("apply_download", current_lang()) })
  
  # Read data
  data <- reactive({
    req(input$file)
    file_ext <- tolower(tools::file_ext(input$file$name))
    
    if (!file_ext %in% c("csv", "txt", "dat", "tsv", "xlsx", "xls", "rds", "")) {
      showNotification(
        paste0("🚫 <strong>", get_label("unsupported_file", current_lang()), "</strong> <code>", file_ext, "</code>"),
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    tryCatch({
      res <- switch(file_ext,
        "csv" = tryCatch({
          readr::read_csv(input$file$datapath, col_names = TRUE, show_col_types = FALSE, guess_max = 1000,
                         locale = readr::locale(encoding = "UTF-8"))
        }, error = function(e) {
          read.csv(input$file$datapath, header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        }),
        "tsv" = tryCatch({
          readr::read_tsv(input$file$datapath, col_names = TRUE, show_col_types = FALSE, guess_max = 1000,
                         locale = readr::locale(encoding = "UTF-8"))
        }, error = function(e) {
          read.table(input$file$datapath, header = TRUE, sep = "\t", stringsAsFactors = FALSE, fileEncoding = "UTF-8")
        }),
        "txt" = read.table(input$file$datapath, header = TRUE, sep = "", stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
        "xlsx" = readxl::read_excel(input$file$datapath),
        "xls" = readxl::read_excel(input$file$datapath),
        "rds" = readRDS(input$file$datapath),
        NULL
      )
      
      if (is.null(res)) {
        showNotification(
          HTML(paste0("🚫 <strong>", get_label("unsupported_file", current_lang()), "</strong> <code>", file_ext, "</code>")),
          type = "error"
        )
        return(NULL)
      } else {
        missing_values <- input$missingValueOptions %||% "NA"
        custom_mv <- trimws(input$missingValueCustom %||% "")
        if (nzchar(custom_mv)) missing_values <- c(missing_values, custom_mv)
        if (!is.null(missing_values) && length(missing_values) > 0) {
          res <- convert_values_to_na(res, missing_values)
        }
        return(res)
      }
    }, error = function(e) {
      showNotification(
        HTML(paste0("⚠️ <strong>", get_label("file_error", current_lang()), "</strong>")),
        type = "error"
      )
      return(NULL)
    })
  })
  
  # Update column choices
  observe({
    req(data())
    updateSelectInput(session, "columns", choices = names(data()))
  })
  
  # File Upload UI
  output$fileUploadUI <- renderUI({
    lang <- current_lang()
    div(
      fileInput("file", get_label("file_upload", lang), 
               accept = c(".csv", ".txt", ".tsv", ".xlsx", ".xls", ".rds")),
      HTML(paste0("<span style='color: #444;'>", get_label("supported_types", lang), "</span>")),
      br(),
      div(
        style = "margin-top: 10px; padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px;",
        h6(textOutput("missingValueFormatLabel"), style = "margin-bottom:6px; font-size:0.95rem;"),
        div(style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap; font-size:0.95rem;",
            tagList(
              checkboxGroupInput("missingValueOptions", 
                                label = NULL,
                                choices = list("NA" = "NA", "0" = "0", "-999" = "-999", "Empty" = ""),
                                selected = "NA",
                                inline = TRUE),
              tags$style(HTML("#missingValueOptions .checkbox-inline{display:inline-block;width:calc(50% - 6px);margin-right:6px;}"))
            ),
            div(style="display:flex; align-items:center; gap:6px;",
                span("Custom:"),
                textInput("missingValueCustom", label = NULL, placeholder = "e.g. -1 or NULL",
                          width = "120px"))
        ),
        helpText(textOutput("missingValueFormatHelp"), style = "margin-top:6px; font-size:0.92rem;")
      )
    )
  })
  
  output$missingValueFormatLabel <- renderText({ get_label("missing_value_format_label", current_lang()) })
  output$missingValueFormatHelp <- renderText({ get_label("missing_value_format_help", current_lang()) })
  
  # Column selection UI
  output$columnSelectionUI <- renderUI({
    lang <- current_lang()
    selectInput("columns", HTML(paste0("🗂️ ", get_label("select_columns", lang))), 
               choices = NULL, multiple = TRUE)
  })
  
  # Plot type UI
  output$plotTypeTitle <- renderText({ get_label("plot_type_title", current_lang()) })
  output$plotTypeUI <- renderUI({
    lang <- current_lang()
    selectInput("plotType", HTML(paste0("📊 ", get_label("plot_type", lang))),
               choices = stats::setNames(
                 c("histogram", "boxplot", "qqplot"),
                 c(get_label("histogram", lang), get_label("boxplot", lang), get_label("qqplot", lang))
               ),
               selected = "histogram")
  })
  
  output$binsUI <- renderUI({
    lang <- current_lang()
    conditionalPanel(
      condition = "input.plotType == 'histogram'",
      numericInput("bins", get_label("hist_bin", lang), value = 30, min = 1, step = 1)
    )
  })
  
  # Color customization UI
  output$colorCustomizationTitle <- renderText({ get_label("color_customization", current_lang()) })
  output$showColorOptionsText <- renderText({
    lang <- current_lang()
    if (is.null(input$toggleColorOptions) || input$toggleColorOptions %% 2 == 0) {
      get_label("show_color_options", lang)
    } else {
      get_label("hide_color_options", lang)
    }
  })
  
  output$colorCustomizationUI <- renderUI({
    lang <- current_lang()
    div(
      # Pre-filter color section
      div(class = "color-input-group",
        h5(get_label("pre_filter_color", lang), class = "subsection-title"),
        div(
          textInput("preFilterColorInput", get_label("color_input_label", lang), 
                   value = preFilterColor(), width = "100%")
        ),
        div(
          span(get_label("color_palette_label", lang), class = "color-palette-label")
        ),
        div(class = "color-grid",
          actionButton("preFilterRed1", "", style = "background-color: #FFB3B3;"),
          actionButton("preFilterGreen1", "", style = "background-color: #B3FFB3;"),
          actionButton("preFilterBlue1", "", style = "background-color: #B3CCFF;"),
          actionButton("preFilterYellow1", "", style = "background-color: #FFFFB3;"),
          actionButton("preFilterRed2", "", style = "background-color: #FF6666;"),
          actionButton("preFilterGreen2", "", style = "background-color: #66FF66;"),
          actionButton("preFilterBlue2", "", style = "background-color: #6699FF;"),
          actionButton("preFilterYellow2", "", style = "background-color: #FFFF66;"),
          actionButton("preFilterRed3", "", style = "background-color: #FF0000;"),
          actionButton("preFilterGreen3", "", style = "background-color: #00FF00;"),
          actionButton("preFilterBlue3", "", style = "background-color: #0066FF;"),
          actionButton("preFilterYellow3", "", style = "background-color: #FFFF00;"),
          actionButton("preFilterRed4", "", style = "background-color: #CC0000;"),
          actionButton("preFilterGreen4", "", style = "background-color: #00CC00;"),
          actionButton("preFilterBlue4", "", style = "background-color: #0033CC;"),
          actionButton("preFilterYellow4", "", style = "background-color: #CCCC00;")
        )
      ),
      
      # Post-filter color section
      div(class = "color-input-group",
        h5(get_label("post_filter_color", lang), class = "subsection-title"),
        div(
          textInput("postFilterColorInput", get_label("color_input_label", lang), 
                   value = postFilterColor(), width = "100%")
        ),
        div(
          span(get_label("color_palette_label", lang), class = "color-palette-label")
        ),
        div(class = "color-grid",
          actionButton("postFilterRed1", "", style = "background-color: #FFB3B3;"),
          actionButton("postFilterGreen1", "", style = "background-color: #B3FFB3;"),
          actionButton("postFilterBlue1", "", style = "background-color: #B3CCFF;"),
          actionButton("postFilterYellow1", "", style = "background-color: #FFFFB3;"),
          actionButton("postFilterRed2", "", style = "background-color: #FF6666;"),
          actionButton("postFilterGreen2", "", style = "background-color: #66FF66;"),
          actionButton("postFilterBlue2", "", style = "background-color: #6699FF;"),
          actionButton("postFilterYellow2", "", style = "background-color: #FFFF66;"),
          actionButton("postFilterRed3", "", style = "background-color: #FF0000;"),
          actionButton("postFilterGreen3", "", style = "background-color: #00FF00;"),
          actionButton("postFilterBlue3", "", style = "background-color: #0066FF;"),
          actionButton("postFilterYellow3", "", style = "background-color: #FFFF00;"),
          actionButton("postFilterRed4", "", style = "background-color: #CC0000;"),
          actionButton("postFilterGreen4", "", style = "background-color: #00CC00;"),
          actionButton("postFilterBlue4", "", style = "background-color: #0033CC;"),
          actionButton("postFilterYellow4", "", style = "background-color: #CCCC00;")
        )
      ),
      
      # Reset button
      div(style = "text-align: center; margin-top: 20px;",
        actionButton("resetColors", get_label("reset_colors", lang), 
                    class = "btn btn-outline-secondary btn-sm")
      )
    )
  })
  
  # Color button observers (pre-filter)
  observeEvent(input$preFilterRed1, { preFilterColor("#FFB3B3"); updateTextInput(session, "preFilterColorInput", value = "#FFB3B3") })
  observeEvent(input$preFilterRed2, { preFilterColor("#FF6666"); updateTextInput(session, "preFilterColorInput", value = "#FF6666") })
  observeEvent(input$preFilterRed3, { preFilterColor("#FF0000"); updateTextInput(session, "preFilterColorInput", value = "#FF0000") })
  observeEvent(input$preFilterRed4, { preFilterColor("#CC0000"); updateTextInput(session, "preFilterColorInput", value = "#CC0000") })
  observeEvent(input$preFilterGreen1, { preFilterColor("#B3FFB3"); updateTextInput(session, "preFilterColorInput", value = "#B3FFB3") })
  observeEvent(input$preFilterGreen2, { preFilterColor("#66FF66"); updateTextInput(session, "preFilterColorInput", value = "#66FF66") })
  observeEvent(input$preFilterGreen3, { preFilterColor("#00FF00"); updateTextInput(session, "preFilterColorInput", value = "#00FF00") })
  observeEvent(input$preFilterGreen4, { preFilterColor("#00CC00"); updateTextInput(session, "preFilterColorInput", value = "#00CC00") })
  observeEvent(input$preFilterBlue1, { preFilterColor("#B3CCFF"); updateTextInput(session, "preFilterColorInput", value = "#B3CCFF") })
  observeEvent(input$preFilterBlue2, { preFilterColor("#6699FF"); updateTextInput(session, "preFilterColorInput", value = "#6699FF") })
  observeEvent(input$preFilterBlue3, { preFilterColor("#0066FF"); updateTextInput(session, "preFilterColorInput", value = "#0066FF") })
  observeEvent(input$preFilterBlue4, { preFilterColor("#0033CC"); updateTextInput(session, "preFilterColorInput", value = "#0033CC") })
  observeEvent(input$preFilterYellow1, { preFilterColor("#FFFFB3"); updateTextInput(session, "preFilterColorInput", value = "#FFFFB3") })
  observeEvent(input$preFilterYellow2, { preFilterColor("#FFFF66"); updateTextInput(session, "preFilterColorInput", value = "#FFFF66") })
  observeEvent(input$preFilterYellow3, { preFilterColor("#FFFF00"); updateTextInput(session, "preFilterColorInput", value = "#FFFF00") })
  observeEvent(input$preFilterYellow4, { preFilterColor("#CCCC00"); updateTextInput(session, "preFilterColorInput", value = "#CCCC00") })
  
  # Color palette button handlers for post-filter
  observeEvent(input$postFilterRed1, { postFilterColor("#FFB3B3"); updateTextInput(session, "postFilterColorInput", value = "#FFB3B3") })
  observeEvent(input$postFilterRed2, { postFilterColor("#FF6666"); updateTextInput(session, "postFilterColorInput", value = "#FF6666") })
  observeEvent(input$postFilterRed3, { postFilterColor("#FF0000"); updateTextInput(session, "postFilterColorInput", value = "#FF0000") })
  observeEvent(input$postFilterRed4, { postFilterColor("#CC0000"); updateTextInput(session, "postFilterColorInput", value = "#CC0000") })
  
  observeEvent(input$postFilterGreen1, { postFilterColor("#B3FFB3"); updateTextInput(session, "postFilterColorInput", value = "#B3FFB3") })
  observeEvent(input$postFilterGreen2, { postFilterColor("#66FF66"); updateTextInput(session, "postFilterColorInput", value = "#66FF66") })
  observeEvent(input$postFilterGreen3, { postFilterColor("#00FF00"); updateTextInput(session, "postFilterColorInput", value = "#00FF00") })
  observeEvent(input$postFilterGreen4, { postFilterColor("#00CC00"); updateTextInput(session, "postFilterColorInput", value = "#00CC00") })
  
  observeEvent(input$postFilterBlue1, { postFilterColor("#B3CCFF"); updateTextInput(session, "postFilterColorInput", value = "#B3CCFF") })
  observeEvent(input$postFilterBlue2, { postFilterColor("#6699FF"); updateTextInput(session, "postFilterColorInput", value = "#6699FF") })
  observeEvent(input$postFilterBlue3, { postFilterColor("#0066FF"); updateTextInput(session, "postFilterColorInput", value = "#0066FF") })
  observeEvent(input$postFilterBlue4, { postFilterColor("#0033CC"); updateTextInput(session, "postFilterColorInput", value = "#0033CC") })
  
  observeEvent(input$postFilterYellow1, { postFilterColor("#FFFFB3"); updateTextInput(session, "postFilterColorInput", value = "#FFFFB3") })
  observeEvent(input$postFilterYellow2, { postFilterColor("#FFFF66"); updateTextInput(session, "postFilterColorInput", value = "#FFFF66") })
  observeEvent(input$postFilterYellow3, { postFilterColor("#FFFF00"); updateTextInput(session, "postFilterColorInput", value = "#FFFF00") })
  observeEvent(input$postFilterYellow4, { postFilterColor("#CCCC00"); updateTextInput(session, "postFilterColorInput", value = "#CCCC00") })
  
  observeEvent(input$preFilterColorInput, {
    if (!is.null(input$preFilterColorInput) && nchar(input$preFilterColorInput) > 0) {
      if (grepl("^#[0-9A-Fa-f]{6}$", input$preFilterColorInput)) {
        preFilterColor(input$preFilterColorInput)
      }
    }
  })
  
  observeEvent(input$postFilterColorInput, {
    if (!is.null(input$postFilterColorInput) && nchar(input$postFilterColorInput) > 0) {
      if (grepl("^#[0-9A-Fa-f]{6}$", input$postFilterColorInput)) {
        postFilterColor(input$postFilterColorInput)
      }
    }
  })
  
  observeEvent(input$resetColors, {
    preFilterColor("#DB3124")
    postFilterColor("#4B74B2")
    updateTextInput(session, "preFilterColorInput", value = "#DB3124")
    updateTextInput(session, "postFilterColorInput", value = "#4B74B2")
  })
  
  # QC UI
  output$qcFilterTitle <- renderText({ get_label("qc_filter_options", current_lang()) })
  output$qcModeUI <- renderUI({
    lang <- current_lang()
    radioButtons("qcMode", get_label("qc_mode", lang),
                choices = stats::setNames(c("uniform", "individual"),
                                        c(get_label("uniform_qc", lang), get_label("individual_qc", lang))),
                selected = "uniform")
  })
  
  output$uniformQCControls <- renderUI({
    lang <- current_lang()
    div(
      radioButtons("filterType", get_label("filter_type", lang),
                  choices = stats::setNames(c("threshold", "sd", "iqr"),
                                          c(get_label("threshold_range", lang),
                                            get_label("sd_multiplier", lang),
                                            get_label("iqr_multiplier", lang)))),
      conditionalPanel(
        condition = "input.filterType == 'threshold'",
        numericInput("minVal", get_label("min_threshold", lang), value = NA),
        numericInput("maxVal", get_label("max_threshold", lang), value = NA)
      ),
      conditionalPanel(
        condition = "input.filterType == 'sd'",
        numericInput("sdMultiplier", get_label("sd_multiplier", lang), value = 2, min = 0.1, step = 0.1)
      ),
      conditionalPanel(
        condition = "input.filterType == 'iqr'",
        numericInput("iqrMultiplier", get_label("iqr_multiplier", lang), value = 1.5, min = 0.1, step = 0.1)
      )
    )
  })
  
  output$individualQCTitle <- renderText({ get_label("individual_qc_title", current_lang()) })
  
  # Individual QC controls (dynamic per column)
  output$individualQCControls <- renderUI({
    req(input$columns, input$qcMode == "individual")
    lang <- current_lang()
    
    if (length(input$columns) == 0) {
      return(div(get_label("select_columns_first", lang)))
    }
    
    controls <- list()
    
    for (i in seq_along(input$columns)) {
      col_name <- input$columns[i]
      
      controls[[i]] <- div(
        class = "well",
        style = "margin-bottom: 10px; padding: 10px; background-color: #f9f9f9; border: 1px solid #ddd;",
        h6(strong(paste(get_label("trait_label", lang), ":", col_name))),
        
        radioButtons(
          inputId = paste0("filterType_", i),
          label = get_label("filter_type", lang),
          choices = stats::setNames(
            c("threshold", "sd", "iqr"),
            c(
              get_label("threshold_range", lang),
              get_label("sd_multiplier", lang),
              get_label("iqr_multiplier", lang)
            )
          ),
          selected = "sd"
        ),
        
        conditionalPanel(
          condition = paste0("input.filterType_", i, " == 'threshold'"),
          div(
            style = "display: inline-block; width: 48%; margin-right: 2%;",
            numericInput(
              inputId = paste0("minVal_", i),
              label = get_label("min_threshold", lang),
              value = NA
            )
          ),
          div(
            style = "display: inline-block; width: 48%;",
            numericInput(
              inputId = paste0("maxVal_", i),
              label = get_label("max_threshold", lang),
              value = NA
            )
          )
        ),
        
        conditionalPanel(
          condition = paste0("input.filterType_", i, " == 'sd'"),
          numericInput(
            inputId = paste0("sdMultiplier_", i),
            label = get_label("sd_multiplier", lang),
            value = 2,
            min = 0.1,
            step = 0.1
          )
        ),
        
        conditionalPanel(
          condition = paste0("input.filterType_", i, " == 'iqr'"),
          numericInput(
            inputId = paste0("iqrMultiplier_", i),
            label = get_label("iqr_multiplier", lang),
            value = 1.5,
            min = 0.1,
            step = 0.1
          )
        )
      )
    }
    
    return(controls)
  })
  
  output$actionButtonsUI <- renderUI({
    lang <- current_lang()
    div(
      actionButton("applyFilter", get_label("apply_filter", lang), 
                  class = "btn btn-success",
                  style = "width: 100%; margin-bottom: 10px;"),
      actionButton("showDownloadModal", get_label("download_filtered", lang), 
                  class = "btn btn-success",
                  style = "width: 100%;")
    )
  })
  
  # Tab titles
  output$dataPreviewTabTitle <- renderText({ get_label("data_preview", current_lang()) })
  output$qcResultsTabTitle <- renderText({ get_label("qc_results", current_lang()) })
  
  # Selected data (after categorical filtering)
  selectedData <- reactive({
    req(input$columns, data())
    df <- data()
    df %>% dplyr::select(dplyr::all_of(input$columns))
  })
  
  # Data summary
  output$dataSummaryUI <- renderUI({
    req(selectedData(), data())
    lang <- current_lang()
    original_rows <- nrow(data())
    filtered_rows <- nrow(selectedData())
    
    summary_text <- paste0(
      "<div style='background-color: #E8F4FD; padding: 10px; border-radius: 5px; margin-bottom: 15px; font-family: Times New Roman, SimSun, serif;'>",
      "<strong>", get_label("data_summary", lang), "</strong><br>",
      get_label("original_dataset", lang), " ", original_rows, " ", get_label("rows", lang), "<br>",
      get_label("after_filtering", lang), " ", filtered_rows, " ", get_label("rows", lang), "<br>",
      get_label("filtered_out", lang), " ", original_rows - filtered_rows, " ", get_label("rows", lang), " (",
      round((original_rows - filtered_rows) / original_rows * 100, 1), "%)",
      "</div>"
    )
    HTML(summary_text)
  })
  
  # Preview table
  output$previewTable <- renderDT({
    req(selectedData())
    selectedData()
  }, options = list(pageLength = 10, dom = 't'))
  
  # Preview plot
  output$preDistPlot <- renderPlot({
    req(selectedData(), input$plotType)
    numeric_cols <- selectedData() %>% dplyr::select_if(is.numeric)
    if (ncol(numeric_cols) == 0) {
      showNotification(get_label("no_data_plot", current_lang()), type = "warning")
      return(NULL)
    }
    
    df_long <- numeric_cols %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value))
    plot_ready(TRUE)
    
    if (nrow(df_long) == 0) {
      showNotification(get_label("no_data_plot", current_lang()), type = "warning")
      return(NULL)
    }
    
    if (input$plotType == "histogram") {
      ggplot(df_long, aes(x = Value)) +
        geom_histogram(bins = input$bins, fill = preFilterColor(), alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        labs(title = "Pre-Filter Data Distribution", x = "Value", y = "Frequency") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (input$plotType == "boxplot") {
      ggplot(df_long, aes(y = Value)) +
        geom_boxplot(fill = preFilterColor(), alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    } else {
      ggplot(df_long, aes(sample = Value)) +
        stat_qq(color = preFilterColor(), alpha = 0.7) +
        stat_qq_line(color = "#333333", linewidth = 0.6) +
        facet_wrap(~ Column, scales = "free") +
        labs(
          title = "Pre-Filter Q-Q Plot",
          subtitle = "X: Theoretical Quantiles (Normal) | Y: Sample Quantiles",
          x = "Theoretical Quantiles",
          y = "Sample Quantiles"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  output$plotDownloadUI <- renderUI({
    if (plot_ready()) {
      lang <- current_lang()
      downloadButton("downloadPlot", get_label("download_plot", lang), class = "btn btn-success")
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0("pre_filter_plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(selectedData(), input$plotType)
      numeric_cols <- selectedData() %>% dplyr::select_if(is.numeric)
      if (ncol(numeric_cols) == 0) return(NULL)
      
      df_long <- numeric_cols %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value))
      
      if (nrow(df_long) == 0) return(NULL)
      
      grDevices::png(file, width = 1200, height = 800, res = 120)
      if (input$plotType == "histogram") {
        p <- ggplot(df_long, aes(x = Value)) +
          geom_histogram(bins = input$bins, fill = preFilterColor(), alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          labs(title = "Pre-Filter Data Distribution", x = "Value", y = "Frequency") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      } else if (input$plotType == "boxplot") {
        p <- ggplot(df_long, aes(y = Value)) +
          geom_boxplot(fill = preFilterColor(), alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          labs(title = "Pre-Filter Data Distribution (Boxplot)", x = "Column", y = "Value") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks.x = element_blank())
      } else {
        p <- ggplot(df_long, aes(sample = Value)) +
          stat_qq(color = preFilterColor(), alpha = 0.7) +
          stat_qq_line(color = "#333333", linewidth = 0.6) +
          facet_wrap(~ Column, scales = "free") +
          labs(
            title = "Pre-Filter Q-Q Plot",
            subtitle = "X: Theoretical Quantiles (Normal) | Y: Sample Quantiles",
            x = "Theoretical Quantiles",
            y = "Sample Quantiles"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      print(p)
      grDevices::dev.off()
    }
  )
  
  # Apply filter and calculate statistics
  filteredData <- eventReactive(input$applyFilter, {
    req(input$columns, data())
    
    df <- data()
    
    # Convert selected columns to numeric
    for (col in input$columns) {
      df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
    }
    
    filter_stats <- list()
    
    if (input$qcMode == "uniform") {
      # Uniform filtering logic
      if (input$filterType == "threshold") {
        min_val <- input$minVal
        max_val <- input$maxVal
        criteria <- paste0("Threshold: [",
                          ifelse(is.na(min_val), "-Inf", min_val), ", ",
                          ifelse(is.na(max_val), "Inf", max_val), "]")
        for (col in input$columns) {
          original_count <- sum(!is.na(df[[col]]))
          keep <- rep(TRUE, nrow(df))
          if (!is.na(min_val)) keep <- keep & (df[[col]] >= min_val | is.na(df[[col]]))
          if (!is.na(max_val)) keep <- keep & (df[[col]] <= max_val | is.na(df[[col]]))
          
          df[[col]][!keep] <- NA
          filtered_count <- sum(!is.na(df[[col]]))
          filter_stats[[col]] <- list(
            removed = original_count - filtered_count,
            remaining = filtered_count,
            criteria = criteria
          )
        }
        
      } else if (input$filterType == "sd") {
        multiplier <- input$sdMultiplier
        criteria <- paste0("Mean +/- ", multiplier, " * SD")
        for (col in input$columns) {
          original_count <- sum(!is.na(df[[col]]))
          col_mean <- mean(df[[col]], na.rm = TRUE)
          col_sd <- sd(df[[col]], na.rm = TRUE)
          lower <- col_mean - multiplier * col_sd
          upper <- col_mean + multiplier * col_sd
          df[[col]][!(df[[col]] >= lower & df[[col]] <= upper | is.na(df[[col]]))] <- NA
          filtered_count <- sum(!is.na(df[[col]]))
          filter_stats[[col]] <- list(
            removed = original_count - filtered_count,
            remaining = filtered_count,
            criteria = criteria
          )
        }
        
      } else if (input$filterType == "iqr") {
        multiplier <- input$iqrMultiplier
        criteria <- paste0("IQR * ", multiplier)
        for (col in input$columns) {
          original_count <- sum(!is.na(df[[col]]))
          qs <- stats::quantile(df[[col]], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
          iqr <- qs[2] - qs[1]
          lower_bound <- qs[1] - multiplier * iqr
          upper_bound <- qs[2] + multiplier * iqr
          df[[col]][!(df[[col]] >= lower_bound & df[[col]] <= upper_bound | is.na(df[[col]]))] <- NA
          filtered_count <- sum(!is.na(df[[col]]))
          filter_stats[[col]] <- list(
            removed = original_count - filtered_count,
            remaining = filtered_count,
            criteria = criteria
          )
        }
      }
    } else {
      # Individual filtering logic
      for (i in seq_along(input$columns)) {
        col <- input$columns[i]
        filter_type <- input[[paste0("filterType_", i)]]
        
        original_count <- sum(!is.na(df[[col]]))
        criteria <- ""
        
        if (filter_type == "threshold") {
          min_val <- input[[paste0("minVal_", i)]]
          max_val <- input[[paste0("maxVal_", i)]]
          criteria <- paste0("Threshold: [",
                            ifelse(is.na(min_val), "-Inf", min_val), ", ",
                            ifelse(is.na(max_val), "Inf", max_val), "]")
          keep <- rep(TRUE, nrow(df))
          if (!is.na(min_val)) keep <- keep & (df[[col]] >= min_val | is.na(df[[col]]))
          if (!is.na(max_val)) keep <- keep & (df[[col]] <= max_val | is.na(df[[col]]))
          df[[col]][!keep] <- NA
          
        } else if (filter_type == "sd") {
          multiplier <- input[[paste0("sdMultiplier_", i)]]
          criteria <- paste0("Mean +/- ", multiplier, " * SD")
          col_mean <- mean(df[[col]], na.rm = TRUE)
          col_sd <- sd(df[[col]], na.rm = TRUE)
          lower <- col_mean - multiplier * col_sd
          upper <- col_mean + multiplier * col_sd
          df[[col]][!(df[[col]] >= lower & df[[col]] <= upper | is.na(df[[col]]))] <- NA
          
        } else if (filter_type == "iqr") {
          multiplier <- input[[paste0("iqrMultiplier_", i)]]
          criteria <- paste0("IQR * ", multiplier)
          qs <- stats::quantile(df[[col]], probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
          iqr <- qs[2] - qs[1]
          lower_bound <- qs[1] - multiplier * iqr
          upper_bound <- qs[2] + multiplier * iqr
          df[[col]][!(df[[col]] >= lower_bound & df[[col]] <= upper_bound | is.na(df[[col]]))] <- NA
        }
        
        filtered_count <- sum(!is.na(df[[col]]))
        filter_stats[[col]] <- list(
          removed = original_count - filtered_count,
          remaining = filtered_count,
          criteria = criteria
        )
      }
    }
    
    list(data = df, stats = filter_stats)
  })
  
  # Filter statistics table
  output$filterStats <- renderDT({
    req(filteredData())
    stats <- filteredData()$stats
    
    if (length(stats) == 0) {
      return(datatable(
        data.frame(Column = character(0), Original = integer(0),
                   Removed = integer(0), Remaining = integer(0), RemovalRate = numeric(0), QC_Criteria = character(0)),
        options = list(dom = 't')
      ))
    }
    
    df <- data.frame(
      Column = names(stats),
      Original = sapply(stats, function(x) (x$removed %||% 0) + (x$remaining %||% 0)),
      Removed = sapply(stats, function(x) x$removed),
      Remaining = sapply(stats, function(x) x$remaining),
      QC_Criteria = sapply(stats, function(x) x$criteria %||% ""),
      stringsAsFactors = FALSE
    )
    df$RemovalRate <- ifelse(df$Original > 0, df$Removed / df$Original, NA_real_)
    
    total_row <- data.frame(
      Column = "Total",
      Original = sum(df$Original, na.rm = TRUE),
      Removed = sum(df$Removed, na.rm = TRUE),
      Remaining = sum(df$Remaining, na.rm = TRUE),
      QC_Criteria = "",
      RemovalRate = ifelse(sum(df$Original, na.rm = TRUE) > 0,
                           sum(df$Removed, na.rm = TRUE) / sum(df$Original, na.rm = TRUE),
                           NA_real_)
    )
    df <- rbind(df, total_row)
    
    complete_cases <- filteredData()$data %>%
      dplyr::select(dplyr::all_of(input$columns)) %>%
      stats::complete.cases() %>%
      sum()
    
    complete_row <- data.frame(
      Column = "Complete_Cases",
      Original = NA_integer_,
      Removed = NA_integer_,
      Remaining = complete_cases,
      QC_Criteria = "",
      RemovalRate = NA_real_)
    df <- rbind(df, complete_row)
    
    datatable(df, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE)) %>%
      formatRound(c("Original", "Removed", "Remaining"), 0) %>%
      formatPercentage("RemovalRate", 2)
  })
  
  # QC Summary (Pre/Post comparison)
  qcSummary <- reactive({
    req(selectedData(), filteredData(), input$columns)
    
    pre_df <- selectedData()
    post_df <- filteredData()$data[, input$columns, drop = FALSE]
    
    for (col in input$columns) {
      pre_df[[col]]  <- suppressWarnings(as.numeric(pre_df[[col]]))
      post_df[[col]] <- suppressWarnings(as.numeric(post_df[[col]]))
    }
    
    res <- lapply(input$columns, function(col) {
      pre_vals  <- pre_df[[col]]
      post_vals <- post_df[[col]]
      
      pre_mean  <- mean(pre_vals,  na.rm = TRUE)
      pre_sd    <- sd(pre_vals, na.rm = TRUE)
      post_mean <- mean(post_vals, na.rm = TRUE)
      post_sd   <- sd(post_vals, na.rm = TRUE)
      
      data.frame(
        Column      = col,
        Pre_Mean    = pre_mean,
        Pre_SD      = pre_sd,
        Post_Mean   = post_mean,
        Post_SD     = post_sd,
        Delta_Mean  = post_mean - pre_mean,
        Delta_SD    = post_sd - pre_sd,
        stringsAsFactors = FALSE
      )
    })
    
    do.call(rbind, res)
  })
  
  output$qcSummaryTable <- renderDT({
    req(qcSummary())
    datatable(qcSummary(), rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE)) %>%
      formatRound(c("Pre_Mean","Pre_SD","Post_Mean","Post_SD","Delta_Mean","Delta_SD"), 3)
  })

  # Normality test (Pre/Post)
  normalityResults <- reactive({
    req(selectedData(), filteredData(), input$columns)
    
    run_test <- function(values) {
      values <- values[!is.na(values)]
      n <- length(values)
      if (n < 3) {
        return(list(
          n = n, method = "Insufficient data", statistic = NA_real_,
          p_value = NA_real_, normal = NA
        ))
      }
      if (n < 5000) {
        res <- stats::shapiro.test(values)
        return(list(
          n = n, method = "Shapiro-Wilk",
          statistic = unname(res$statistic), p_value = res$p.value,
          normal = res$p.value > 0.05
        ))
      }
      sd_val <- stats::sd(values)
      if (!is.finite(sd_val) || sd_val == 0) {
        return(list(
          n = n, method = "Kolmogorov-Smirnov",
          statistic = NA_real_, p_value = NA_real_, normal = NA
        ))
      }
      res <- stats::ks.test(values, "pnorm", mean = mean(values), sd = sd_val)
      list(
        n = n, method = "Kolmogorov-Smirnov",
        statistic = unname(res$statistic), p_value = res$p.value,
        normal = res$p.value > 0.05
      )
    }
    
    pre_df <- selectedData()
    post_df <- filteredData()$data[, input$columns, drop = FALSE]
    for (col in input$columns) {
      pre_df[[col]]  <- suppressWarnings(as.numeric(pre_df[[col]]))
      post_df[[col]] <- suppressWarnings(as.numeric(post_df[[col]]))
    }
    
    rows <- lapply(input$columns, function(col) {
      pre_res <- run_test(pre_df[[col]])
      post_res <- run_test(post_df[[col]])
      rbind(
        data.frame(
          Column = col, Sample = "Pre-Filter", N = pre_res$n,
          Method = pre_res$method, Statistic = pre_res$statistic,
          P_value = pre_res$p_value, Normal = pre_res$normal,
          stringsAsFactors = FALSE
        ),
        data.frame(
          Column = col, Sample = "Post-Filter", N = post_res$n,
          Method = post_res$method, Statistic = post_res$statistic,
          P_value = post_res$p_value, Normal = post_res$normal,
          stringsAsFactors = FALSE
        )
      )
    })
    do.call(rbind, rows)
  })
  
  output$normalityTable <- renderDT({
    req(normalityResults())
    lang <- current_lang()
    labeled <- normalityResults()
    stat_col <- get_label("normality_col_statistic", lang)
    p_col <- get_label("normality_col_p_value", lang)
    colnames(labeled) <- c(
      get_label("normality_col_column", lang),
      get_label("normality_col_sample", lang),
      get_label("normality_col_n", lang),
      get_label("normality_col_method", lang),
      stat_col,
      p_col,
      get_label("normality_col_normal", lang)
    )
    datatable(labeled, rownames = FALSE, options = list(pageLength = 10, autoWidth = TRUE)) %>%
      formatRound(stat_col, 4) %>%
      formatSignif(p_col, 3)
  })
  
  # Comparison plots
  output$comparisonPlots <- renderPlot({
    req(filteredData(), input$plotType, input$columns)
    
    selected_cols <- input$columns
    if (is.null(selected_cols) || length(selected_cols) == 0) return(NULL)
    
    pre_data <- selectedData()
    pre_numeric <- pre_data[, selected_cols, drop = FALSE]
    pre_numeric <- pre_numeric[, sapply(pre_numeric, is.numeric), drop = FALSE]
    
    post_data <- filteredData()$data
    post_numeric <- post_data[, selected_cols, drop = FALSE]
    post_numeric <- post_numeric[, sapply(post_numeric, is.numeric), drop = FALSE]
    
    if (ncol(pre_numeric) == 0 || ncol(post_numeric) == 0) {
      showNotification(get_label("no_data_comparison", current_lang()), type = "warning")
      return(NULL)
    }
    
    pre_long <- pre_numeric %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::mutate(Type = "Pre-Filter")
    
    post_long <- post_numeric %>%
      tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
      dplyr::filter(!is.na(Value)) %>%
      dplyr::mutate(Type = "Post-Filter")
    
    combined <- dplyr::bind_rows(pre_long, post_long)
    if (nrow(combined) == 0) {
      showNotification(get_label("no_data_comparison", current_lang()), type = "warning")
      return(NULL)
    }
    
    if (input$plotType == "histogram") {
      ggplot(combined, aes(x = Value, fill = Type)) +
        geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge") +
        facet_wrap(~ Column, scales = "free") +
        scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
        labs(title = "Pre-Filter vs Post-Filter Data Distribution", x = "Value", y = "Frequency") +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    } else if (input$plotType == "boxplot") {
      ggplot(combined, aes(y = Value, x = Type, fill = Type)) +
        geom_boxplot(alpha = 0.7) +
        facet_wrap(~ Column, scales = "free") +
        scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
        labs(title = "Pre-Filter vs Post-Filter Data Distribution (Boxplot)", x = "Type", y = "Value") +
        theme_minimal(base_size = 14) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
    } else {
      ggplot(combined, aes(sample = Value, color = Type)) +
        stat_qq(alpha = 0.6) +
        stat_qq_line(linewidth = 0.6) +
        facet_wrap(~ Column, scales = "free") +
        scale_color_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
        labs(
          title = "Pre-Filter vs Post-Filter Q-Q Plot",
          subtitle = "X: Theoretical Quantiles (Normal) | Y: Sample Quantiles",
          x = "Theoretical Quantiles",
          y = "Sample Quantiles"
        ) +
        theme_minimal(base_size = 14) +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })
  
  # Switch to QC Results tab after applying filter
  observeEvent(input$applyFilter, {
    updateTabsetPanel(session, inputId = "mainTabs", selected = "qc_results")
  })
  
  # Download handlers
  output$downloadComparisonPlot <- downloadHandler(
    filename = function() { paste0("comparison_plot_", Sys.Date(), ".png") },
    content = function(file) {
      req(filteredData(), input$plotType, input$columns)
      
      selected_cols <- input$columns
      if (is.null(selected_cols) || length(selected_cols) == 0) return(NULL)
      
      pre_data <- selectedData()
      pre_numeric <- pre_data[, selected_cols, drop = FALSE]
      pre_numeric <- pre_numeric[, sapply(pre_numeric, is.numeric), drop = FALSE]
      
      post_data <- filteredData()$data
      post_numeric <- post_data[, selected_cols, drop = FALSE]
      post_numeric <- post_numeric[, sapply(post_numeric, is.numeric), drop = FALSE]
      
      if (ncol(pre_numeric) == 0 || ncol(post_numeric) == 0) return(NULL)
      
      pre_long <- pre_numeric %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::mutate(Type = "Pre-Filter")
      
      post_long <- post_numeric %>%
        tidyr::pivot_longer(everything(), names_to = "Column", values_to = "Value") %>%
        dplyr::filter(!is.na(Value)) %>%
        dplyr::mutate(Type = "Post-Filter")
      
      combined <- dplyr::bind_rows(pre_long, post_long)
      if (nrow(combined) == 0) return(NULL)
      
      grDevices::png(file, width = 1200, height = 800, res = 120)
      
      if (input$plotType == "histogram") {
        p <- ggplot(combined, aes(x = Value, fill = Type)) +
          geom_histogram(bins = input$bins, alpha = 0.7, position = "dodge") +
          facet_wrap(~ Column, scales = "free") +
          scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
          labs(title = "Pre vs Post Filter Data Distribution", x = "Value", y = "Frequency") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      } else if (input$plotType == "boxplot") {
        p <- ggplot(combined, aes(y = Value, x = Type, fill = Type)) +
          geom_boxplot(alpha = 0.7) +
          facet_wrap(~ Column, scales = "free") +
          scale_fill_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
          labs(title = "Pre vs Post Filter Data Distribution (Boxplot)", x = "Type", y = "Value") +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
      } else {
        p <- ggplot(combined, aes(sample = Value, color = Type)) +
          stat_qq(alpha = 0.6) +
          stat_qq_line(linewidth = 0.6) +
          facet_wrap(~ Column, scales = "free") +
          scale_color_manual(values = c("Pre-Filter" = preFilterColor(), "Post-Filter" = postFilterColor())) +
          labs(
            title = "Pre vs Post Filter Q-Q Plot",
            subtitle = "X: Theoretical Quantiles (Normal) | Y: Sample Quantiles",
            x = "Theoretical Quantiles",
            y = "Sample Quantiles"
          ) +
          theme_minimal(base_size = 14) +
          theme(plot.title = element_text(hjust = 0.5))
      }
      
      print(p)
      grDevices::dev.off()
    }
  )
  
  output$downloadFilterStats <- downloadHandler(
    filename = function() { paste0("filter_stats_", Sys.Date(), ".csv") },
    content = function(file) {
      req(filteredData())
      stats <- filteredData()$stats
      
      if (length(stats) == 0) {
        df <- data.frame(Column = character(0), Original = integer(0), Removed = integer(0),
                        Remaining = integer(0), RemovalRate = numeric(0), QC_Criteria = character(0))
      } else {
        df <- data.frame(
          Column = names(stats),
          Original = sapply(stats, function(x) (x$removed %||% 0) + (x$remaining %||% 0)),
          Removed = sapply(stats, function(x) x$removed),
          Remaining = sapply(stats, function(x) x$remaining),
          QC_Criteria = sapply(stats, function(x) x$criteria %||% ""),
          stringsAsFactors = FALSE
        )
        df$RemovalRate <- ifelse(df$Original > 0, df$Removed / df$Original, NA_real_)
        
        total_row <- data.frame(
          Column = "Total", Original = sum(df$Original, na.rm = TRUE),
          Removed = sum(df$Removed, na.rm = TRUE), Remaining = sum(df$Remaining, na.rm = TRUE),
          QC_Criteria = "", RemovalRate = ifelse(sum(df$Original, na.rm = TRUE) > 0,
                                                 sum(df$Removed, na.rm = TRUE) / sum(df$Original, na.rm = TRUE), NA_real_))
        df <- rbind(df, total_row)
        
        complete_cases <- filteredData()$data %>%
          dplyr::select(dplyr::all_of(input$columns)) %>%
          stats::complete.cases() %>% sum()
        
        complete_row <- data.frame(Column = "Complete_Cases", Original = NA_integer_,
                                  Removed = NA_integer_, Remaining = complete_cases,
                                  QC_Criteria = "", RemovalRate = NA_real_)
        df <- rbind(df, complete_row)
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$downloadQcSummary <- downloadHandler(
    filename = function() { paste0("qc_summary_", Sys.Date(), ".csv") },
    content = function(file) {
      req(qcSummary())
      write.csv(qcSummary(), file, row.names = FALSE)
    }
  )
  
  # QC Results content
  output$qcResultsContent <- renderUI({
    lang <- current_lang()
    
    if (is.null(filteredData()) || is.null(selectedData())) {
      return(div(style = "text-align: center; padding: 50px; color: #666;",
                h4(get_label("no_data_comparison", lang))))
    }
    
    div(
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          div(get_label("removed_records", lang),
              style = "font-weight:bold; font-family:'Times New Roman', 'SimSun', serif; font-size:18px;"),
          downloadButton("downloadFilterStats", "Download CSV", class = "btn btn-sm btn-outline-primary")),
      DTOutput("filterStats"),
      br(),
      
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          div(get_label("comparison_means", lang),
              style = "font-weight:bold; font-family:'Times New Roman', 'SimSun', serif; font-size:18px;"),
          downloadButton("downloadQcSummary", "Download CSV", class = "btn btn-sm btn-outline-primary")),
      DTOutput("qcSummaryTable"),
      br(),

      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          div(get_label("normality_test_title", lang),
              style = "font-weight:bold; font-family:'Times New Roman', 'SimSun', serif; font-size:18px;")),
      DTOutput("normalityTable"),
      br(),
      
      div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
          div("Distribution Comparison Plots",
              style = "font-weight:bold; font-family:'Times New Roman', 'SimSun', serif; font-size:18px;"),
          downloadButton("downloadComparisonPlot", get_label("download_comparison", lang), 
                        class = "btn btn-sm btn-outline-primary")),
      plotOutput("comparisonPlots", height = "600px")
    )
  })
  
  # Show download modal
  observeEvent(input$showDownloadModal, {
    req(filteredData())
    showModal(modalDialog(
      title = get_label("missing_value_modal_title", current_lang()),
      div(style = "text-align: center;",
          br(),
          div(style = "text-align: center;",
              h5(get_label("missing_value_format_label", current_lang()), 
                 style = "margin-bottom: 15px;")),
          div(style = "text-align: center; display: flex; justify-content: center; align-items: center;",
              radioButtons("missingValueFormat",
                          label = NULL,
                          choices = list("NA" = "na", "0" = "zero", "-999" = "minus999"),
                          selected = "na", inline = TRUE)),
          br(),
          div(style = "text-align: center;",
              actionButton("confirmDownload", get_label("confirm_download_text", current_lang()),
                          class = "btn btn-primary", style = "margin-right: 10px;"),
              actionButton("cancelDownload", get_label("cancel_download_text", current_lang()),
                          class = "btn btn-secondary"))),
      size = "m", easyClose = FALSE, footer = NULL
    ))
  })
  
  observeEvent(input$cancelDownload, { removeModal() })
  
  downloadFormat <- reactiveVal("na")
  
  observeEvent(input$confirmDownload, {
    removeModal()
    downloadFormat(input$missingValueFormat)
    showNotification("Starting download...", type = "message")
    
    req(filteredData())
    data_to_download <- filteredData()$data
    data_to_download <- convert_missing_values(data_to_download, input$missingValueFormat)
    
    txt_content <- readr::format_delim(data_to_download, delim = " ")
    filename <- paste("phenotype_", Sys.Date(), ".txt", sep = "")
    
    session$sendCustomMessage("downloadCSV", list(content = txt_content, filename = filename))
  })
  
  # Hidden download link
  output$hiddenDownloadLink <- renderUI({
    downloadLink("downloadData", "", style = "display: none;")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste("phenotype_", Sys.Date(), ".txt", sep = "") },
    content = function(file) {
      req(filteredData())
      data_to_download <- filteredData()$data
      data_to_download <- convert_missing_values(data_to_download, downloadFormat())
      readr::write_delim(data_to_download, file, delim = " ")
    }
  )
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)
