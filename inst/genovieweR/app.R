# genovieweR - Genotype Viewer and Quality Control
# Version: 0.1.0
# Created: 2025-01-XX
# A Shiny application for genotype data visualization and quality control

# ============================================================================
# LIBRARIES
# ============================================================================
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(dplyr)
  library(DT)
  library(ggplot2)
})

# Optional: gridExtra and cowplot for arranging plots
if (requireNamespace("gridExtra", quietly = TRUE)) {
  library(gridExtra)
}
if (requireNamespace("cowplot", quietly = TRUE)) {
  library(cowplot)
}

# Source shared language helpers (if available)
try(source(normalizePath(file.path("..", "Language.R"), winslash = "/", mustWork = FALSE), local = TRUE), silent = TRUE)

# Local wrapper to prefix translation keys for this app (matches Language.R genoviewer_* keys)
get_label_local <- function(key, lang = NULL) {
  prefixed <- if (startsWith(key, "genoviewer_")) key else paste0("genoviewer_", key)
  if (exists("get_label", mode = "function")) {
    get_label(prefixed, lang)
  } else {
    prefixed
  }
}

# Check if linkbreedeR is available for extended analysis
use_linkbreedeR <- FALSE
tryCatch({
  if (requireNamespace("linkbreedeR", quietly = TRUE)) {
    use_linkbreedeR <- TRUE
    cat("✓ linkbreedeR available - will use for extended genotype analysis\n")
  }
}, error = function(e) {
  cat("Note: linkbreedeR not available, extended analysis features will be limited\n")
  use_linkbreedeR <- FALSE
})

# Check if plinkR is available (prefer PLINK when installed)
use_plinkR <- FALSE
tryCatch({
  if (requireNamespace("plinkR", quietly = TRUE)) {
    use_plinkR <- TRUE
    cat("✓ plinkR available - will prefer PLINK for statistics/QC\n")
  }
}, error = function(e) {
  cat("Note: plinkR not available, using local fallback calculations\n")
  use_plinkR <- FALSE
})

# Check if data.table is available for faster file reading
use_data_table <- FALSE
tryCatch({
  if (requireNamespace("data.table", quietly = TRUE)) {
    library(data.table)
    use_data_table <- TRUE
    cat("✓ data.table available - will use for faster file reading\n")
  }
}, error = function(e) {
  cat("Note: data.table not available, using base R read.table\n")
  use_data_table <- FALSE
})

# Check if plotly is available for interactive plots
use_plotly <- FALSE
tryCatch({
  if (requireNamespace("plotly", quietly = TRUE)) {
    library(plotly)
    use_plotly <- TRUE
    cat("✓ plotly available - will use for interactive plots\n")
  }
}, error = function(e) {
  cat("Note: plotly not available, using static ggplot2 plots\n")
  use_plotly <- FALSE
})

# Rcpp backend for PLINK-compatible fallback when PLINK is unavailable.
# C++ implementation is kept in genotype_qc.cpp beside this app.
use_rcpp_stats <- FALSE
tryCatch({
  if (requireNamespace("Rcpp", quietly = TRUE)) {
    cpp_candidates <- c(
      "genotype_qc.cpp",
      file.path("inst", "genovieweR", "genotype_qc.cpp")
    )
    cpp_file <- cpp_candidates[file.exists(cpp_candidates)][1]
    if (is.na(cpp_file) || !nzchar(cpp_file)) {
      stop("genotype_qc.cpp not found")
    }
    Rcpp::sourceCpp(file = cpp_file)
    use_rcpp_stats <- TRUE
    cat("✓ Rcpp backend available - PLINK-compatible fallback stats enabled\n")
  }
}, error = function(e) {
  use_rcpp_stats <- FALSE
  cat("Note: Rcpp backend unavailable; local non-PLINK statistics are disabled.\n")
})

# Suppress SASS color contrast warnings from bslib
options(bslib.color_contrast_warnings = FALSE)

# Disable PDF device to prevent Rplots.pdf generation
# In Shiny apps, we use renderPlot which handles graphics devices automatically
options(device = "png")
pdf(NULL)  # Close any default PDF device

purdue_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#333333",
  primary = "#CEB888",
  base_font = font_google("Crimson Text")
)

# ============================================================================
# USER INTERFACE
# ============================================================================
ui <- page_fillable(
  theme = purdue_theme,
  shinyjs::useShinyjs(),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
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
      
      .title-bar p {
        margin: 5px 0 0 0;
        font-size: 1rem;
        color: #000000;
        opacity: .9;
        font-family: 'Crimson Text', 'Noto Sans SC', 'PingFang SC', 'Microsoft YaHei', 'Heiti SC', 'SimSun', 'Noto Sans', Arial, sans-serif;
      }
      
      /* Button styles */
      .btn-primary, .btn.btn-primary {
        background-color: #CEB888 !important;
        border-color: #CEB888 !important;
        color: #000000 !important;
        font-weight: 600;
      }
      
      .btn-primary:hover, .btn.btn-primary:hover {
        background-color: #B89D5D !important;
        border-color: #B89D5D !important;
      }
      
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
        padding: 0;
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
      
      .right-panel.hidden {
        width: 0;
        padding: 0;
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
      
      /* Toggle handles */
      .toggle-btn-left, .toggle-btn-right {
        position: fixed;
        top: 50%;
        transform: translateY(-50%);
        z-index: 1100;
      }
      
      .toggle-btn-left { left: 8px; }
      .toggle-btn-right { right: 8px; }
      
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
    "))
  ),
  
  # Title Bar (localized)
  uiOutput("title_bar"),
  # Floating toggle handles (localized)
  uiOutput("toggle_left_btn"),
  uiOutput("toggle_right_btn"),
  
  # Three-panel layout container
  div(class = "three-panel-container",
      
      # LEFT PANEL - Controls
      div(id = "leftPanel", class = "left-panel",
          
          # File Upload Section (localized)
          div(class = "panel-section",
              uiOutput("data_upload_title"),
              uiOutput("geno_format_label"),
              selectInput("geno_format", NULL,
                         choices = list(
                           "PLINK (.ped/.map)" = "plink_ped",
                           "PLINK (.bed/.bim/.fam)" = "plink_bed",
                           "VCF (.vcf/.vcf.gz)" = "vcf",
                           "BLUPF90 (.txt + .map)" = "blupf90_txt"
                         ),
                         selected = "plink"),
              numericInput("max_chromosome", "Chromosome", value = 18, min = 1, max = 100, step = 1),
              p(style = "font-size: 0.8rem; color: #888; margin-top: -10px; margin-bottom: 10px;",
                "Maximum chromosome number to include (1 to selected number). Only chromosomes 1 to selected number will be included in analysis"),
              br(),
              
              # PLINK Format File Uploads (2 files)
              conditionalPanel(
                condition = "input.geno_format == 'plink_ped'",
                div(
                  div(style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #CEB888;",
                      tags$strong("📋 PLINK Format: Upload 2 files"),
                      tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px; font-size: 0.9rem;",
                              tags$li(tags$code(".ped"), " file - genotype data"),
                              tags$li(tags$code(".map"), " file - marker map information")
                      )
                  ),
                  fileInput("plink_ped_file", 
                           label = tags$span("📄 Upload .ped File", 
                                            style = "font-weight: 600;"),
                           accept = c(".ped"),
                           buttonLabel = "Browse...",
                           placeholder = "No .ped file selected"),
                  fileInput("plink_map_file", 
                           label = tags$span("📄 Upload .map File", 
                                            style = "font-weight: 600;"),
                           accept = c(".map"),
                           buttonLabel = "Browse...",
                           placeholder = "No .map file selected"),
                  div(style = "margin-top: 8px; font-size: 0.85rem; color: #666;",
                      "💡 Both files should have the same prefix (e.g., data.ped and data.map)")
                )
              ),
              
              # PLINK BED Format File Uploads (3 files)
              conditionalPanel(
                condition = "input.geno_format == 'plink_bed'",
                div(
                  div(style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #CEB888;",
                      tags$strong("📋 PLINK BED Format: Upload 3 files"),
                      tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px; font-size: 0.9rem;",
                              tags$li(tags$code(".bed"), " file - binary genotype data"),
                              tags$li(tags$code(".bim"), " file - marker information"),
                              tags$li(tags$code(".fam"), " file - sample information")
                      )
                  ),
                  fileInput("plink_bed_file", 
                           label = tags$span("📄 Upload .bed File", 
                                            style = "font-weight: 600;"),
                           accept = c(".bed"),
                           buttonLabel = "Browse...",
                           placeholder = "No .bed file selected"),
                  fileInput("plink_bim_file", 
                           label = tags$span("📄 Upload .bim File", 
                                            style = "font-weight: 600;"),
                           accept = c(".bim"),
                           buttonLabel = "Browse...",
                           placeholder = "No .bim file selected"),
                  fileInput("plink_fam_file", 
                           label = tags$span("📄 Upload .fam File", 
                                            style = "font-weight: 600;"),
                           accept = c(".fam"),
                           buttonLabel = "Browse...",
                           placeholder = "No .fam file selected")
                )
              ),
              
              # VCF Format File Uploads (1 file)
              conditionalPanel(
                condition = "input.geno_format == 'vcf'",
                div(
                  div(style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #CEB888;",
                      tags$strong("📋 VCF Format: Upload 1 file"),
                      tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px; font-size: 0.9rem;",
                              tags$li(tags$code(".vcf"), " or ", tags$code(".vcf.gz"), " file")
                      )
                  ),
                  fileInput("vcf_file", 
                           label = tags$span("📄 Upload .vcf File", 
                                            style = "font-weight: 600;"),
                           accept = c(".vcf", ".vcf.gz"),
                           buttonLabel = "Browse...",
                           placeholder = "No .vcf file selected")
                )
              ),
              
              # BLUPF90 TXT Format File Uploads (2 files)
              conditionalPanel(
                condition = "input.geno_format == 'blupf90_txt'",
                div(
                  div(style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #CEB888;",
                      tags$strong("📋 BLUPF90 TXT Format: Upload 2 files"),
                      tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px; font-size: 0.9rem;",
                              tags$li(tags$code(".txt"), " file - genotype data"),
                              tags$li(tags$code(".map"), " file - marker map information")
                      )
                  ),
                  fileInput("blupf90_txt_file", 
                           label = tags$span("📄 Upload .txt File", 
                                            style = "font-weight: 600;"),
                           accept = c(".txt"),
                           buttonLabel = "Browse...",
                           placeholder = "No .txt file selected"),
                  fileInput("blupf90_map_file", 
                           label = tags$span("📄 Upload .map File", 
                                            style = "font-weight: 600;"),
                           accept = c(".map"),
                           buttonLabel = "Browse...",
                           placeholder = "No .map file selected")
                )
              ),
              
              br(),
              div(style = "margin-top: 15px;",
                  uiOutput("show_summary_btn"),
                  uiOutput("show_summary_help")
              ),
              br()
          ),
          
          # QC Options Section (for quality control only, localized)
          div(class = "panel-section",
              uiOutput("qc_section_title"),
              uiOutput("qc_intro_text"),
              numericInput("geno_threshold", "--geno (SNP missing rate)", value = 0.1, min = 0, max = 1, step = 0.01),
              p(style = "font-size: 0.8rem; color: #888; margin-top: -10px; margin-bottom: 10px;",
                "Exclude SNPs with missing rate > threshold"),
              numericInput("mind_threshold", "--mind (Sample missing rate)", value = 0.1, min = 0, max = 1, step = 0.01),
              p(style = "font-size: 0.8rem; color: #888; margin-top: -10px; margin-bottom: 10px;",
                "Exclude samples with missing rate > threshold"),
              numericInput("maf_threshold", "--maf (Minor allele frequency)", value = 0.01, min = 0, max = 0.5, step = 0.01),
              p(style = "font-size: 0.8rem; color: #888; margin-top: -10px; margin-bottom: 10px;",
                "Exclude SNPs with MAF < threshold"),
              numericInput("hwe_threshold", "--hwe (HWE p-value)", value = 1e-5, min = 0, max = 1, step = 1e-6),
              p(style = "font-size: 0.8rem; color: #888; margin-top: -10px; margin-bottom: 10px;",
                "Exclude SNPs with HWE p-value < threshold"),
              br(),
              uiOutput("run_qc_btn")
          )
      ),
      
      # CENTER PANEL - Main Content
      div(class = "center-panel",
          navset_card_tab(
            id = "mainTabs",
            nav_panel(
              textOutput("tab_data_preview_title"),
              value = "preview",
              div(style = "margin-top: 20px;",
                  h4("Genotype Data Summary"),
                  verbatimTextOutput("data_summary"),
                  br(),
                  h4("Data Table"),
                  DTOutput("geno_table")
              )
            ),
            nav_panel(
              "Per-individual Plots",
              value = "per_individual",
                  div(style = "margin-top: 20px;",
                  h4("Per-Individual Plots"),
                  p("Visualization of individual-level metrics: Sample missing rate and sample relatedness (IBD/PI_HAT)."),
                  br(),
                  if (use_plotly) {
                    tagList(
                      div(style = "margin-bottom: 30px;",
                          h5("Sample Missing Rate Distribution"),
                          plotlyOutput("per_individual_plot_missing", height = "400px")
                      ),
                      div(style = "margin-bottom: 30px;",
                          h5("Sample Relatedness Distribution"),
                          plotlyOutput("per_individual_plot_relatedness", height = "400px")
                      )
                    )
                  } else {
                    tagList(
                      div(style = "margin-bottom: 30px;",
                          h5("Sample Missing Rate Distribution"),
                          plotOutput("per_individual_plot_missing", height = "400px")
                      ),
                      div(style = "margin-bottom: 30px;",
                          h5("Sample Relatedness Distribution"),
                          plotOutput("per_individual_plot_relatedness", height = "400px")
                      )
                    )
                  },
                  br(),
                  downloadButton("download_per_individual_plots", "Download Plots", 
                               class = "btn btn-primary")
              )
            ),
            nav_panel(
              "Per-marker Plots",
              value = "per_marker",
              div(style = "margin-top: 20px;",
                  h4("Per-Marker Plots"),
                  p("Visualization of marker-level metrics: SNP missing rate, Minor allele frequency (MAF), and Hardy-Weinberg equilibrium (HWE)."),
                  br(),
                  if (use_plotly) {
                    tagList(
                      div(style = "margin-bottom: 30px;",
                          h5("SNP Missing Rate Distribution"),
                          plotlyOutput("per_marker_plot_missing", height = "400px")
                      ),
                      div(style = "margin-bottom: 30px;",
                          h5("Minor Allele Frequency (MAF) Distribution"),
                          plotlyOutput("per_marker_plot_maf", height = "400px")
                      ),
                      div(style = "margin-bottom: 30px;",
                          h5("Hardy-Weinberg Equilibrium (HWE) Distribution"),
                          plotlyOutput("per_marker_plot_hwe", height = "400px")
                      )
                    )
                  } else {
                    tagList(
                      div(style = "margin-bottom: 30px;",
                          h5("SNP Missing Rate Distribution"),
                          plotOutput("per_marker_plot_missing", height = "400px")
                      ),
                      div(style = "margin-bottom: 30px;",
                          h5("Minor Allele Frequency (MAF) Distribution"),
                          plotOutput("per_marker_plot_maf", height = "400px")
                      ),
                      div(style = "margin-bottom: 30px;",
                          h5("Hardy-Weinberg Equilibrium (HWE) Distribution"),
                          plotOutput("per_marker_plot_hwe", height = "400px")
                      )
                    )
                  },
                  br(),
                  downloadButton("download_per_marker_plots", "Download Plots", 
                               class = "btn btn-primary")
              )
            ),
            nav_panel(
              "PCA Plots",
              value = "pca_plots",
              div(style = "margin-top: 20px;",
                  h4("Principal Component Analysis (PCA)"),
                  p("Visualization of population structure using PCA. Supports both 2D and 3D visualization."),
                  br(),
                  div(style = "margin-bottom: 15px;",
                      radioButtons("pca_dimension", "Visualization Dimension:",
                                 choices = list("2D" = "2d", "3D" = "3d"),
                                 selected = "2d",
                                 inline = TRUE)
                  ),
                  if (use_plotly) {
                    plotlyOutput("pca_plots", height = "800px")
                  } else {
                    div(class = "alert alert-warning",
                        p("plotly package is required for PCA visualization. Please install it:"),
                        tags$code("install.packages('plotly')")
                    )
                  },
                  br(),
                  div(style = "margin-bottom: 10px;",
                      downloadButton("download_pca_plots", "Download Plots (HTML)", 
                                     class = "btn btn-primary")
                  ),
                  div(style = "margin-bottom: 10px;",
                      tags$span(style = "font-weight: 600;", "PCA results (PLINK format):"),
                      downloadButton("download_pca_eigenvec", ".eigenvec", class = "btn btn-default btn-sm"),
                      downloadButton("download_pca_eigenval", ".eigenval", class = "btn btn-default btn-sm")
                  )
              )
            ),
            nav_panel(
              "QC Results",
              value = "qc_results",
              div(style = "margin-top: 20px;",
                  h4("Quality Control Report"),
                  DTOutput("qc_report_table"),
                  br(),
                  h4("QC Summary"),
                  verbatimTextOutput("qc_summary")
              )
            ),
          )
      ),
      
      # RIGHT PANEL - Settings
      div(id = "rightPanel", class = "right-panel",
          
          # Export Options
          div(class = "panel-section",
              h4("Export Options", class = "section-title"),
              downloadButton("download_qc_report", "Download QC Report", 
                           class = "btn btn-primary", style = "width: 100%; margin-bottom: 10px;"),
              downloadButton("download_filtered_data", "Download Filtered Data",
                           class = "btn btn-primary", style = "width: 100%;")
          )
      )
  ),
  
  # JavaScript for smooth panel toggling
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var leftPanelOpen = true;
      var rightPanelOpen = true;
      
      $(document).on('click', '#toggleLeftPanel', function() {
        leftPanelOpen = !leftPanelOpen;
        if (leftPanelOpen) {
          $('#leftPanel').removeClass('hidden');
        } else {
          $('#leftPanel').addClass('hidden');
        }
      });
      
      $(document).on('click', '#toggleRightPanel', function() {
        rightPanelOpen = !rightPanelOpen;
        if (rightPanelOpen) {
          $('#rightPanel').removeClass('hidden');
        } else {
          $('#rightPanel').addClass('hidden');
        }
      });
    });
  "))
)

# ============================================================================
# SERVER
# ============================================================================
server <- function(input, output, session) {
  options(shiny.maxRequestSize = 10 * 1024^3)
  
  # Reactive values
  geno_data <- reactiveVal(NULL)
  qc_results <- reactiveVal(NULL)
  
  # Current language for localized UI
  current_lang <- reactive({
    tryCatch({
      if (exists("resolve_suite_lang", mode = "function")) {
        resolved <- resolve_suite_lang(session, default = "en")
        if (exists("map_suite_lang_for_app", mode = "function")) {
          map_suite_lang_for_app(resolved, app = "genoviewer")
        } else {
          tolower(trimws(as.character(resolved)))
        }
      } else {
        "en"
      }
    }, error = function(e) "en")
  })

  # Localized title bar and toggles
  output$title_bar <- renderUI({
    lang <- current_lang()
    div(class = "title-bar",
        h1(get_label_local("app_name", lang)),
        p(get_label_local("app_subtitle", lang))
    )
  })
  output$toggle_left_btn <- renderUI({
    lang <- current_lang()
    div(id = "toggleLeftBtn", class = "toggle-btn-left",
        actionButton("toggleLeftPanel", HTML("&#10094;"),
                    class = "btn btn-sm",
                    title = get_label_local("show_hide_controls", lang))
    )
  })
  output$toggle_right_btn <- renderUI({
    lang <- current_lang()
    div(id = "toggleRightBtn", class = "toggle-btn-right",
        actionButton("toggleRightPanel", HTML("&#10095;"),
                    class = "btn btn-sm",
                    title = get_label_local("show_hide_settings", lang))
    )
  })
  output$data_upload_title <- renderUI({
    lang <- current_lang()
    h4(get_label_local("data_upload", lang), class = "section-title")
  })
  output$geno_format_label <- renderUI({
    lang <- current_lang()
    tags$label(get_label_local("genotype_format", lang), class = "control-label", `for` = "geno_format")
  })
  output$tab_data_preview_title <- renderText({
    lang <- current_lang()
    get_label_local("data_preview", lang)
  })
  output$show_summary_btn <- renderUI({
    lang <- current_lang()
    actionButton("show_summary", get_label_local("show_summary_plots", lang),
                 class = "btn btn-primary", style = "width: 100%; font-weight: 600;")
  })
  output$show_summary_help <- renderUI({
    lang <- current_lang()
    helpText(style = "margin-top: 8px; font-size: 0.85rem; color: #666; text-align: center;",
             get_label_local("show_summary_help", lang))
  })
  output$qc_section_title <- renderUI({
    lang <- current_lang()
    h4(get_label_local("quality_control", lang), class = "section-title")
  })
  output$qc_intro_text <- renderUI({
    lang <- current_lang()
    p(style = "font-size: 0.9rem; color: #666; margin-bottom: 15px;",
      get_label_local("qc_intro", lang))
  })
  output$run_qc_btn <- renderUI({
    lang <- current_lang()
    actionButton("run_qc", get_label_local("run_qc", lang), class = "btn btn-primary", style = "width: 100%; font-weight: 600;")
  })
  
  # Clear data and reset file inputs when format changes
  observeEvent(input$geno_format, {
    geno_data(NULL)
    qc_results(NULL)
    
    # Reset all file inputs
    shinyjs::reset("plink_ped_file")
    shinyjs::reset("plink_map_file")
    shinyjs::reset("plink_bed_file")
    shinyjs::reset("plink_bim_file")
    shinyjs::reset("plink_fam_file")
    shinyjs::reset("vcf_file")
    shinyjs::reset("blupf90_txt_file")
    shinyjs::reset("blupf90_map_file")
  })
  
  # Data loading is now done in show_summary event, not automatically on file upload
  
  # Helper function to read PLINK manually (fallback) - optimized with data.table
  read_plink_manual <- function(ped_path, map_path) {
    # Use data.table for faster reading if available
    if (use_data_table && requireNamespace("data.table", quietly = TRUE)) {
      # Read map file
      map_data <- data.table::fread(map_path, header = FALSE, data.table = FALSE, showProgress = FALSE)
      colnames(map_data) <- c("Chromosome", "SNP_ID", "Genetic_Distance", "Physical_Position")
      
      # Read ped file (large file, use fread for speed)
      ped_data <- data.table::fread(ped_path, header = FALSE, data.table = FALSE, showProgress = FALSE)
      
      # Extract sample IDs and genotypes
      sample_ids <- ped_data[, 1:2]
      colnames(sample_ids) <- c("Family_ID", "Sample_ID")
      
      # Extract genotype data (columns 7 onwards) and combine allele pairs
      allele_matrix <- as.matrix(ped_data[, 7:ncol(ped_data)])
      if (ncol(allele_matrix) %% 2 != 0) {
        stop("Invalid PED format: genotype columns must be in allele pairs.")
      }
      snp_count <- ncol(allele_matrix) / 2
      geno_matrix <- matrix(NA_character_, nrow = nrow(allele_matrix), ncol = snp_count)
      for (i in seq_len(snp_count)) {
        a1 <- allele_matrix[, (i - 1) * 2 + 1]
        a2 <- allele_matrix[, (i - 1) * 2 + 2]
        geno_matrix[, i] <- paste(a1, a2)
      }
      if (!is.null(map_data) && nrow(map_data) == ncol(geno_matrix)) {
        colnames(geno_matrix) <- map_data$SNP_ID
      }
    } else {
      # Fallback to base R
      map_data <- read.table(map_path, header = FALSE, stringsAsFactors = FALSE)
      colnames(map_data) <- c("Chromosome", "SNP_ID", "Genetic_Distance", "Physical_Position")
      
      ped_data <- read.table(ped_path, header = FALSE, stringsAsFactors = FALSE)
      sample_ids <- ped_data[, 1:2]
      colnames(sample_ids) <- c("Family_ID", "Sample_ID")
      allele_matrix <- as.matrix(ped_data[, 7:ncol(ped_data)])
      if (ncol(allele_matrix) %% 2 != 0) {
        stop("Invalid PED format: genotype columns must be in allele pairs.")
      }
      snp_count <- ncol(allele_matrix) / 2
      geno_matrix <- matrix(NA_character_, nrow = nrow(allele_matrix), ncol = snp_count)
      for (i in seq_len(snp_count)) {
        a1 <- allele_matrix[, (i - 1) * 2 + 1]
        a2 <- allele_matrix[, (i - 1) * 2 + 2]
        geno_matrix[, i] <- paste(a1, a2)
      }
      if (!is.null(map_data) && nrow(map_data) == ncol(geno_matrix)) {
        colnames(geno_matrix) <- map_data$SNP_ID
      }
    }
    
    list(
      samples = sample_ids,
      genotypes = geno_matrix,
      map = map_data
    )
  }
  
  # Data summary
  output$data_summary <- renderText({
    req(geno_data())
    data <- geno_data()
    
    # All data is now in a unified format
    if (is.list(data) && "samples" %in% names(data) && "map" %in% names(data)) {
      format_label <- switch(input$geno_format,
                             "plink_ped" = "PLINK (.ped/.map)",
                             "plink_bed" = "PLINK (.bed/.bim/.fam)",
                             "vcf" = "VCF",
                             "blupf90_txt" = "BLUPF90 (.txt/.map)",
                             "PLINK")
      paste0("Samples: ", nrow(data$samples), "\n",
             "SNPs: ", nrow(data$map), "\n",
             "Format: ", format_label, "\n",
             "Analysis Format: ", format_label)
    } else if (is.data.frame(data)) {
      paste0("Rows: ", nrow(data), "\n",
             "Columns: ", ncol(data), "\n",
             "Format: ", input$geno_format)
    } else {
      "Data format not recognized"
    }
  })
  
  # Reactive value to track if summary has been generated
  summary_generated <- reactiveVal(FALSE)
  
  # Generate summary and basic plots when Show Summary button is clicked
  observeEvent(input$show_summary, {
    summary_plink_path <- if (input$geno_format %in% c("plink_ped", "plink_bed", "blupf90_txt")) find_plink_path() else NULL
    summary_backend <- if (!is.null(summary_plink_path)) "PLINK" else "Rcpp"
    summary_backend_suffix <- paste0(" (backend: ", summary_backend, ")")
    withProgress(message = paste0("Loading and processing data...", summary_backend_suffix), value = 0, {
      tryCatch({
        # Step 1: Load data based on format
        setProgress(value = 0.1, message = paste0("Loading data files...", summary_backend_suffix))
        
        # Step 2: Read data by format
        setProgress(value = 0.4, message = paste0("Reading data...", summary_backend_suffix))
        
        data <- tryCatch({
          if (input$geno_format == "plink_ped") {
            if (is.null(input$plink_ped_file) || is.null(input$plink_map_file)) {
              showNotification("Please upload both .ped and .map files", type = "error")
              return(NULL)
            }
            
            ped_file <- input$plink_ped_file
            map_file <- input$plink_map_file
            ped_valid <- grepl("\\.ped$", ped_file$name, ignore.case = TRUE)
            map_valid <- grepl("\\.map$", map_file$name, ignore.case = TRUE)
            if (length(ped_valid) == 0 || length(map_valid) == 0 || !isTRUE(ped_valid) || !isTRUE(map_valid)) {
              showNotification("Please upload valid .ped and .map files", type = "error")
              return(NULL)
            }
            
            read_plink_manual(ped_file$datapath, map_file$datapath)
          } else if (input$geno_format == "plink_bed") {
            if (is.null(input$plink_bed_file) || is.null(input$plink_bim_file) || is.null(input$plink_fam_file)) {
              showNotification("Please upload .bed, .bim, and .fam files", type = "error")
              return(NULL)
            }
            read_plink_bed(input$plink_bed_file$datapath, input$plink_bim_file$datapath, input$plink_fam_file$datapath)
          } else if (input$geno_format == "vcf") {
            if (is.null(input$vcf_file)) {
              showNotification("Please upload a .vcf file", type = "error")
              return(NULL)
            }
            read_vcf_file(input$vcf_file$datapath)
          } else if (input$geno_format == "blupf90_txt") {
            if (is.null(input$blupf90_txt_file) || is.null(input$blupf90_map_file)) {
              showNotification("Please upload BLUPF90 .txt and .map files", type = "error")
              return(NULL)
            }
            read_blupf90_txt(input$blupf90_txt_file$datapath, input$blupf90_map_file$datapath)
          } else {
            showNotification("Unknown genotype format", type = "error")
            return(NULL)
          }
        }, error = function(e) {
          showNotification(paste("Error reading data:", e$message), type = "error")
          NULL
        })
        
        if (is.null(data)) {
          return(NULL)
        }
        
        # Step 3: Filter by chromosome using map (R-based)
        setProgress(value = 0.6, message = paste0("Filtering chromosomes...", summary_backend_suffix))
        max_chr <- if (!is.null(input$max_chromosome) && length(input$max_chromosome) == 1 && input$max_chromosome >= 1) {
          as.integer(input$max_chromosome[1])
        } else {
          18
        }
        
        if (is.list(data) && !is.null(data$map) && nrow(data$map) > 0) {
          chr_numeric <- suppressWarnings(as.integer(data$map$Chromosome))
          keep_idx <- !is.na(chr_numeric) & chr_numeric >= 1 & chr_numeric <= max_chr
          if (sum(keep_idx) == 0) {
            showNotification("No markers remain after chromosome filtering.", type = "error")
            return(NULL)
          }
          data$map <- data$map[keep_idx, , drop = FALSE]
          if (is.matrix(data$genotypes) && ncol(data$genotypes) == length(keep_idx)) {
            data$genotypes <- data$genotypes[, keep_idx, drop = FALSE]
          }
        }
        
        # Store input format for downstream decisions
        data$input_format <- input$geno_format
        
        # Store the loaded data
        geno_data(data)
        
        # Verify data is in PLINK format
    if (!is.list(data) || length(data) == 0 || !isTRUE(all(c("samples", "genotypes", "map") %in% names(data)))) {
          showNotification(
            HTML(paste0(
              "<strong>❌ Data Format Error</strong><br>",
              "Data must include samples, genotypes, and map."
            )),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        
        setProgress(value = 0.8, message = paste0("Calculating basic statistics...", summary_backend_suffix))
        
        # Calculate basic statistics for visualization (without QC thresholds).
        # For PLINK-compatible formats, prefer PLINK backend if available; otherwise fall back to R.
        if (!is.null(summary_plink_path)) {
          basic_stats <- calculate_all_stats_plink(data)
        } else {
          basic_stats <- calculate_all_stats_r(data)
        }
        if (is.null(basic_stats)) {
          showNotification(
            HTML(paste0(
              "<strong>❌ Statistics Calculation Failed</strong><br>",
              "Unable to calculate statistics from the uploaded data."
            )),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        
        # Store basic stats for plotting (not QC results)
        summary_stats(basic_stats)
        summary_generated(TRUE)
        
        setProgress(value = 1, message = paste0("Summary generated!", summary_backend_suffix))
        showNotification("Summary and plots generated successfully!", type = "message")
        
        # Switch to per-individual plots tab
        updateTabsetPanel(session, "mainTabs", selected = "per_individual")
        
      }, error = function(e) {
        showNotification(paste("Error generating summary:", e$message), type = "error")
      })
    })
  })
  
  # Reactive value to store summary statistics (separate from QC results)
  summary_stats <- reactiveVal(NULL)
  
  # Data table
  output$geno_table <- renderDT({
    req(geno_data())
    data <- geno_data()
    
    tryCatch({
      if (is.data.frame(data)) {
        if (nrow(data) > 0) {
          datatable(head(data, 100), options = list(pageLength = 10, scrollX = TRUE))
        } else {
          datatable(data.frame(Message = "No data rows available"), 
                   options = list(pageLength = 10, scrollX = TRUE))
        }
      } else if (is.list(data) && "samples" %in% names(data)) {
        # Show sample information
        if (!is.null(data$samples) && nrow(data$samples) > 0) {
          datatable(data$samples, options = list(pageLength = 10, scrollX = TRUE))
        } else {
          datatable(data.frame(Message = "No sample data available"), 
                   options = list(pageLength = 10))
        }
      } else {
        datatable(data.frame(Message = "No data to display"), 
                 options = list(pageLength = 10))
      }
    }, error = function(e) {
      datatable(data.frame(Error = paste("Error displaying data:", e$message)), 
               options = list(pageLength = 10))
    })
  }, server = FALSE)
  
  # Run QC
  # Note: Data is expected in PLINK format
  observeEvent(input$run_qc, {
    req(geno_data())
    qc_plink_path <- if (input$geno_format %in% c("plink_ped", "plink_bed", "blupf90_txt")) find_plink_path() else NULL
    qc_backend <- if (!is.null(qc_plink_path)) "PLINK" else "Rcpp"
    qc_backend_suffix <- paste0(" (backend: ", qc_backend, ")")
    withProgress(message = paste0("Running quality control filtering...", qc_backend_suffix), value = 0, {
      tryCatch({
        data <- geno_data()
        
        # Verify data is in PLINK format (list with samples, genotypes, map)
        if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
          showNotification(
            HTML(paste0(
              "<strong>❌ Data Format Error</strong><br>",
              "Data must include samples, genotypes, and map."
            )),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        
        setProgress(value = 0.2, message = paste0("Preparing data...", qc_backend_suffix))
        
        # For PLINK-compatible formats, prefer PLINK QC if available; otherwise use R.
        plink_path <- qc_plink_path
        if (!is.null(plink_path)) {
          tmp_dir <- tempfile(pattern = "genovieweR_qc_")
          dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
          prefix <- file.path(tmp_dir, "temp_data")
          
          if (!write_plink_text_files(prefix, data)) {
            showNotification("Failed to prepare PLINK input files.", type = "error")
            return(NULL)
          }
          
          count_plink_items <- function(prefix) {
            samples <- 0
            snps <- 0
            if (file.exists(paste0(prefix, ".ped"))) {
              samples <- length(readLines(paste0(prefix, ".ped"), warn = FALSE))
            }
            if (file.exists(paste0(prefix, ".map"))) {
              snps <- length(readLines(paste0(prefix, ".map"), warn = FALSE))
            }
            list(samples = samples, snps = snps)
          }
          
          initial_counts <- count_plink_items(prefix)
          
          filter_stats <- list(
            geno_removed_snps = 0,
            mind_removed_samples = 0,
            maf_removed_snps = 0,
            hwe_removed_snps = 0
          )
          
          current_prefix <- prefix
          
          if (!is.null(input$geno_threshold) && input$geno_threshold < 1) {
            geno_output <- paste0(prefix, "_geno_step")
            geno_result <- system2(
              plink_path,
              args = c("--file", current_prefix, "--allow-extra-chr", "--nonfounders", "--geno",
                      as.character(input$geno_threshold), "--make-bed", "--out", geno_output),
              stdout = FALSE, stderr = FALSE
            )
            if (geno_result == 0 && file.exists(paste0(geno_output, ".bed"))) {
              geno_counts <- list(
                samples = length(readLines(paste0(geno_output, ".fam"), warn = FALSE)),
                snps = length(readLines(paste0(geno_output, ".bim"), warn = FALSE))
              )
              filter_stats$geno_removed_snps <- initial_counts$snps - geno_counts$snps
              current_prefix <- geno_output
            }
          }
          
          if (!is.null(input$mind_threshold) && input$mind_threshold < 1) {
            mind_output <- paste0(prefix, "_mind_step")
            mind_result <- system2(
              plink_path,
              args = c("--bfile", current_prefix, "--allow-extra-chr", "--nonfounders", "--mind",
                      as.character(input$mind_threshold), "--make-bed", "--out", mind_output),
              stdout = FALSE, stderr = FALSE
            )
            if (mind_result == 0 && file.exists(paste0(mind_output, ".bed"))) {
              mind_counts <- list(
                samples = length(readLines(paste0(mind_output, ".fam"), warn = FALSE)),
                snps = length(readLines(paste0(mind_output, ".bim"), warn = FALSE))
              )
              filter_stats$mind_removed_samples <- initial_counts$samples - mind_counts$samples
              current_prefix <- mind_output
            }
          }
          
          if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
            maf_output <- paste0(prefix, "_maf_step")
            maf_result <- system2(
              plink_path,
              args = c("--bfile", current_prefix, "--allow-extra-chr", "--nonfounders", "--maf",
                      as.character(input$maf_threshold), "--make-bed", "--out", maf_output),
              stdout = FALSE, stderr = FALSE
            )
            if (maf_result == 0 && file.exists(paste0(maf_output, ".bed"))) {
              maf_counts <- list(
                samples = length(readLines(paste0(maf_output, ".fam"), warn = FALSE)),
                snps = length(readLines(paste0(maf_output, ".bim"), warn = FALSE))
              )
              current_counts <- list(
                samples = length(readLines(paste0(current_prefix, ".fam"), warn = FALSE)),
                snps = length(readLines(paste0(current_prefix, ".bim"), warn = FALSE))
              )
              filter_stats$maf_removed_snps <- current_counts$snps - maf_counts$snps
              current_prefix <- maf_output
            }
          }
          
          if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
            hwe_output <- paste0(prefix, "_hwe_step")
            hwe_result <- system2(
              plink_path,
              args = c("--bfile", current_prefix, "--allow-extra-chr", "--nonfounders", "--hwe",
                      as.character(input$hwe_threshold), "--make-bed", "--out", hwe_output),
              stdout = FALSE, stderr = FALSE
            )
            if (hwe_result == 0 && file.exists(paste0(hwe_output, ".bed"))) {
              hwe_counts <- list(
                samples = length(readLines(paste0(hwe_output, ".fam"), warn = FALSE)),
                snps = length(readLines(paste0(hwe_output, ".bim"), warn = FALSE))
              )
              current_counts <- list(
                samples = length(readLines(paste0(current_prefix, ".fam"), warn = FALSE)),
                snps = length(readLines(paste0(current_prefix, ".bim"), warn = FALSE))
              )
              filter_stats$hwe_removed_snps <- current_counts$snps - hwe_counts$snps
              current_prefix <- hwe_output
            }
          }
          
          qc_output <- paste0(prefix, "_qc")
          plink_args <- c("--file", prefix, "--allow-extra-chr", "--nonfounders")
          if (!is.null(input$geno_threshold) && input$geno_threshold < 1) {
            plink_args <- c(plink_args, "--geno", as.character(input$geno_threshold))
          }
          if (!is.null(input$mind_threshold) && input$mind_threshold < 1) {
            plink_args <- c(plink_args, "--mind", as.character(input$mind_threshold))
          }
          if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
            plink_args <- c(plink_args, "--maf", as.character(input$maf_threshold))
          }
          if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
            plink_args <- c(plink_args, "--hwe", as.character(input$hwe_threshold))
          }
          plink_args <- c(plink_args, "--make-bed", "--out", qc_output)
          
          result <- system2(plink_path, args = plink_args, stdout = FALSE, stderr = FALSE)
          if (result != 0 || !file.exists(paste0(qc_output, ".bed"))) {
            showNotification("PLINK quality control failed", type = "error")
            return(NULL)
          }
          
          final_counts <- list(
            samples = length(readLines(paste0(qc_output, ".fam"), warn = FALSE)),
            snps = length(readLines(paste0(qc_output, ".bim"), warn = FALSE))
          )
          
          geno_after_snps <- if (file.exists(paste0(prefix, "_geno_step.bed"))) {
            length(readLines(paste0(prefix, "_geno_step.bim"), warn = FALSE))
          } else {
            initial_counts$snps
          }
          
          maf_before_snps <- if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
            if (file.exists(paste0(prefix, "_geno_step.bed"))) {
              length(readLines(paste0(prefix, "_geno_step.bim"), warn = FALSE))
            } else {
              initial_counts$snps
            }
          } else {
            initial_counts$snps
          }
          
          maf_after_snps <- if (file.exists(paste0(prefix, "_maf_step.bed"))) {
            length(readLines(paste0(prefix, "_maf_step.bim"), warn = FALSE))
          } else {
            maf_before_snps
          }
          
          hwe_before_snps <- if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
            maf_after_snps
          } else {
            initial_counts$snps
          }
          
          qc_report <- list(
            filtered = TRUE,
            message = "Quality control filtering completed",
            samples_before = initial_counts$samples,
            samples_after = final_counts$samples,
            snps_before = initial_counts$snps,
            snps_after = final_counts$snps,
            thresholds = list(
              geno = input$geno_threshold,
              mind = input$mind_threshold,
              maf = input$maf_threshold,
              hwe = input$hwe_threshold
            ),
            filter_stats = filter_stats,
            intermediate_counts = list(
              geno_after_snps = geno_after_snps,
              maf_before_snps = maf_before_snps,
              maf_after_snps = maf_after_snps,
              hwe_before_snps = hwe_before_snps
            ),
            output_files = list(
              bed = paste0(qc_output, ".bed"),
              bim = paste0(qc_output, ".bim"),
              fam = paste0(qc_output, ".fam")
            ),
            filtered_data = NULL
          )
          
          qc_results(qc_report)
          setProgress(value = 1, message = paste0("QC completed!", qc_backend_suffix))
          
          samples_info <- paste0("Samples: ", qc_report$samples_before, " → ", qc_report$samples_after)
          snps_info <- paste0("SNPs: ", qc_report$snps_before, " → ", qc_report$snps_after)
          
          showNotification(
            HTML(paste0(
              "<strong>✓ Quality Control Completed</strong><br>",
              samples_info, "<br>",
              snps_info, "<br>",
              "Filtered data saved. Check QC Results tab for details."
            )),
            type = "message",
            duration = 8
          )
          
          return(NULL)
        }
        
        # Count initial numbers (R fallback)
        initial_counts <- list(
          samples = nrow(data$samples),
          snps = nrow(data$map)
        )
        
        # Initialize filter statistics
        filter_stats <- list(
          geno_removed_snps = 0,
          mind_removed_samples = 0,
          maf_removed_snps = 0,
          hwe_removed_snps = 0
        )
        
        setProgress(value = 0.3, message = paste0("Applying QC filters...", qc_backend_suffix))
        
        current_data <- data
        
        # --geno: exclude SNPs with missing rate > threshold
        geno_after_snps <- initial_counts$snps
        if (!is.null(input$geno_threshold) && input$geno_threshold < 1) {
          marker_call_rate <- calculate_call_rate(current_data)
          marker_missing_rate <- 1 - marker_call_rate
          keep_snps <- is.finite(marker_missing_rate) & marker_missing_rate <= input$geno_threshold
          before <- ncol(current_data$genotypes)
          current_data$genotypes <- current_data$genotypes[, keep_snps, drop = FALSE]
          current_data$map <- current_data$map[keep_snps, , drop = FALSE]
          after <- ncol(current_data$genotypes)
          filter_stats$geno_removed_snps <- before - after
          geno_after_snps <- after
        }
        
        # --mind: exclude samples with missing rate > threshold
        if (!is.null(input$mind_threshold) && input$mind_threshold < 1) {
          individual_call_rate <- calculate_individual_call_rate(current_data)
          individual_missing_rate <- 1 - individual_call_rate
          keep_samples <- is.finite(individual_missing_rate) & individual_missing_rate <= input$mind_threshold
          before <- nrow(current_data$samples)
          current_data$genotypes <- current_data$genotypes[keep_samples, , drop = FALSE]
          current_data$samples <- current_data$samples[keep_samples, , drop = FALSE]
          after <- nrow(current_data$samples)
          filter_stats$mind_removed_samples <- before - after
        }
        
        # --maf: exclude SNPs with MAF < threshold
        maf_before_snps <- if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
          ncol(current_data$genotypes)
        } else {
          geno_after_snps
        }
        maf_after_snps <- maf_before_snps
        if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
          maf_values <- calculate_maf(current_data)
          keep_snps <- is.finite(maf_values) & maf_values >= input$maf_threshold
          before <- ncol(current_data$genotypes)
          current_data$genotypes <- current_data$genotypes[, keep_snps, drop = FALSE]
          current_data$map <- current_data$map[keep_snps, , drop = FALSE]
          after <- ncol(current_data$genotypes)
          filter_stats$maf_removed_snps <- before - after
          maf_after_snps <- after
        }
        
        # --hwe: exclude SNPs with HWE p-value < threshold
        hwe_before_snps <- if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
          ncol(current_data$genotypes)
        } else {
          maf_after_snps
        }
        if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
          hwe_values <- calculate_hwe(current_data)
          keep_snps <- is.finite(hwe_values) & hwe_values >= input$hwe_threshold
          before <- ncol(current_data$genotypes)
          current_data$genotypes <- current_data$genotypes[, keep_snps, drop = FALSE]
          current_data$map <- current_data$map[keep_snps, , drop = FALSE]
          after <- ncol(current_data$genotypes)
          filter_stats$hwe_removed_snps <- before - after
        }
        
        setProgress(value = 0.8, message = paste0("Generating QC report...", qc_backend_suffix))
        
        final_counts <- list(
          samples = nrow(current_data$samples),
          snps = nrow(current_data$map)
        )
        
        # Build comprehensive QC report
        qc_report <- list(
          filtered = TRUE,
          message = "Quality control filtering completed",
          samples_before = initial_counts$samples,
          samples_after = final_counts$samples,
          snps_before = initial_counts$snps,
          snps_after = final_counts$snps,
          thresholds = list(
            geno = input$geno_threshold,
            mind = input$mind_threshold,
            maf = input$maf_threshold,
            hwe = input$hwe_threshold
          ),
          filter_stats = filter_stats,
          intermediate_counts = list(
            geno_after_snps = geno_after_snps,
            maf_before_snps = maf_before_snps,
            maf_after_snps = maf_after_snps,
            hwe_before_snps = hwe_before_snps
          ),
          filtered_data = current_data,
          output_files = NULL
        )
        
        # Set QC results for display
        qc_results(qc_report)
        
        setProgress(value = 1, message = paste0("QC completed!", qc_backend_suffix))
        
        samples_info <- if (!is.null(qc_report$samples_after)) {
          paste0("Samples: ", qc_report$samples_before, " → ", qc_report$samples_after)
        } else {
          paste0("Samples: ", qc_report$samples_before, " (check log for filtered count)")
        }
        
        snps_info <- if (!is.null(qc_report$snps_after)) {
          paste0("SNPs: ", qc_report$snps_before, " → ", qc_report$snps_after)
        } else {
          paste0("SNPs: ", qc_report$snps_before, " (check log for filtered count)")
        }
        
        showNotification(
          HTML(paste0(
            "<strong>✓ Quality Control Completed</strong><br>",
            samples_info, "<br>",
            snps_info, "<br>",
            "Filtered data saved. Check QC Results tab for details."
          )),
          type = "message",
          duration = 5
        )
        
        # Switch to QC Results tab to show report
        updateTabsetPanel(session, "mainTabs", selected = "qc_results")
        
      }, error = function(e) {
        showNotification(paste("Error running QC:", e$message), type = "error")
      })
    })
  })
  
  # QC report table
  output$qc_report_table <- renderDT({
    req(qc_results())
    report <- qc_results()
    
    if (!is.null(report) && is.list(report)) {
      # Create report table
      result_df <- data.frame(
        Parameter = character(0),
        Value = character(0),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(report$samples_before)) {
        samples_after <- if (!is.null(report$samples_after)) {
          if (report$samples_before > 0) {
            pct_removed <- round((1 - report$samples_after/report$samples_before) * 100, 2)
            paste0(report$samples_before, " → ", report$samples_after, 
                   " (", pct_removed, "% removed)")
          } else {
            paste0(report$samples_before, " → ", report$samples_after, " (N/A% removed)")
          }
        } else {
          paste0(report$samples_before, " (filtered count not available)")
        }
        result_df <- rbind(result_df,
                          data.frame(Parameter = "Samples", Value = samples_after,
                                   stringsAsFactors = FALSE))
      }
      
      if (!is.null(report$snps_before)) {
        snps_after <- if (!is.null(report$snps_after)) {
          if (report$snps_before > 0) {
            pct_removed <- round((1 - report$snps_after/report$snps_before) * 100, 2)
            paste0(report$snps_before, " → ", report$snps_after,
                   " (", pct_removed, "% removed)")
          } else {
            paste0(report$snps_before, " → ", report$snps_after, " (N/A% removed)")
          }
        } else {
          paste0(report$snps_before, " (filtered count not available)")
        }
        result_df <- rbind(result_df,
                          data.frame(Parameter = "SNPs", Value = snps_after,
                                   stringsAsFactors = FALSE))
      }
      
      # Add detailed filter statistics
      if (!is.null(report$filter_stats)) {
        if (!is.null(report$thresholds$geno) && !is.null(report$filter_stats$geno_removed_snps)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--geno (SNP missing rate)", 
                                     Value = paste0("Threshold: ", report$thresholds$geno, 
                                                   " | Removed SNPs: ", report$filter_stats$geno_removed_snps),
                                     stringsAsFactors = FALSE))
        }
        if (!is.null(report$thresholds$mind) && !is.null(report$filter_stats$mind_removed_samples)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--mind (Sample missing rate)", 
                                     Value = paste0("Threshold: ", report$thresholds$mind, 
                                                   " | Removed Samples: ", report$filter_stats$mind_removed_samples),
                                     stringsAsFactors = FALSE))
        }
        if (!is.null(report$thresholds$maf) && !is.null(report$filter_stats$maf_removed_snps)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--maf (Minor allele frequency)", 
                                     Value = paste0("Threshold: ", report$thresholds$maf, 
                                                   " | Removed SNPs: ", report$filter_stats$maf_removed_snps),
                                     stringsAsFactors = FALSE))
        }
        if (!is.null(report$thresholds$hwe) && !is.null(report$filter_stats$hwe_removed_snps)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--hwe (HWE p-value)", 
                                     Value = paste0("Threshold: ", report$thresholds$hwe, 
                                                   " | Removed SNPs: ", report$filter_stats$hwe_removed_snps),
                                     stringsAsFactors = FALSE))
        }
      } else if (!is.null(report$thresholds)) {
        # Fallback: show thresholds only if filter_stats not available
        if (!is.null(report$thresholds$geno)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--geno threshold", 
                                     Value = as.character(report$thresholds$geno),
                                     stringsAsFactors = FALSE))
        }
        if (!is.null(report$thresholds$mind)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--mind threshold", 
                                     Value = as.character(report$thresholds$mind),
                                     stringsAsFactors = FALSE))
        }
        if (!is.null(report$thresholds$maf)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--maf threshold", 
                                     Value = as.character(report$thresholds$maf),
                                     stringsAsFactors = FALSE))
        }
        if (!is.null(report$thresholds$hwe)) {
          result_df <- rbind(result_df,
                            data.frame(Parameter = "--hwe threshold", 
                                     Value = as.character(report$thresholds$hwe),
                                     stringsAsFactors = FALSE))
        }
      }
      
      # Ensure we always return a valid data frame
      if (nrow(result_df) == 0) {
        result_df <- data.frame(
          Message = "No QC report available. Please run QC first.",
          stringsAsFactors = FALSE
        )
      }
      
      datatable(result_df, 
               options = list(
                 pageLength = 10,
                 scrollX = TRUE,
                 dom = 't',
                 ordering = FALSE
               ),
               rownames = FALSE)
    } else {
      datatable(data.frame(Message = "No QC report available. Please run QC first."), 
               options = list(pageLength = 10))
    }
  }, server = FALSE)
  
  # QC summary
  output$qc_summary <- renderText({
    req(qc_results())
    report <- qc_results()
    
    if (!is.null(report) && is.list(report)) {
      summary_text <- paste0(
        "Quality Control Summary:\n",
        "========================\n\n",
        "Status: ", if (!is.null(report$filtered) && report$filtered) "Completed" else "Not completed", "\n",
        "Message: ", if (!is.null(report$message)) report$message else "N/A", "\n\n"
      )
      
      if (!is.null(report$output_files)) {
        summary_text <- paste0(summary_text, "Output Files:\n")
        if (!is.null(report$output_files$bed)) {
          summary_text <- paste0(summary_text, "  - BED: ", report$output_files$bed, "\n")
        }
        if (!is.null(report$output_files$bim)) {
          summary_text <- paste0(summary_text, "  - BIM: ", report$output_files$bim, "\n")
        }
        if (!is.null(report$output_files$fam)) {
          summary_text <- paste0(summary_text, "  - FAM: ", report$output_files$fam, "\n")
        }
      }
      
      return(summary_text)
    } else {
      return("No QC report available. Please run QC first.")
    }
  })
  
  # Per-individual plots: Sample missing rate (Interactive with plotly)
  if (use_plotly) {
    # Plot 1: Sample missing rate distribution
    output$per_individual_plot_missing <- renderPlotly({
      req(geno_data())
      data <- geno_data()
      
      # Use summary_stats if available, otherwise use qc_results
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(plotly_empty() %>% 
               add_annotations(text = "Please click 'Show Summary & Plots' or run QC first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.list(data) || !all(c("samples", "genotypes") %in% names(data))) {
        return(plotly_empty() %>% 
               add_annotations(text = "Please load PLINK format data first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      # Plot 1: Sample missing rate distribution
      if (!is.null(stats$individual_missing_rate)) {
        miss_df <- data.frame(MissingRate = stats$individual_missing_rate)
        p_miss <- plot_ly() %>%
          add_histogram(data = miss_df, x = ~MissingRate, 
                       nbinsx = 30,
                       marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                       name = "Missing Rate") %>%
          layout(title = list(text = "Sample Missing Rate Distribution", font = list(size = 16)),
                xaxis = list(title = list(text = "Missing Rate", font = list(size = 14))),
                yaxis = list(title = list(text = "Number of Samples", font = list(size = 14))),
                showlegend = FALSE,
                margin = list(t = 60, b = 60, l = 80, r = 40))
        p_miss
      } else {
        plotly_empty() %>% 
          add_annotations(text = "Sample missing rate data not available",
                        x = 0.5, y = 0.5, showarrow = FALSE)
      }
    })
    output$per_individual_plot_relatedness <- renderPlotly({
      req(geno_data())
      data <- geno_data()
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(plotly_empty() %>%
               add_annotations(text = "Please click 'Show Summary & Plots' or run QC first",
                               x = 0.5, y = 0.5, showarrow = FALSE))
      }

      if (!is.list(data) || !all(c("samples", "genotypes") %in% names(data))) {
        return(plotly_empty() %>%
               add_annotations(text = "Please load genotype data first",
                               x = 0.5, y = 0.5, showarrow = FALSE))
      }

      if (!is.null(stats$individual_relatedness) &&
          is.data.frame(stats$individual_relatedness) &&
          nrow(stats$individual_relatedness) > 0 &&
          "PI_HAT" %in% names(stats$individual_relatedness)) {
        relatedness_df <- stats$individual_relatedness
        plot_ly() %>%
          add_histogram(data = relatedness_df, x = ~PI_HAT,
                        nbinsx = 50,
                        marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                        name = "Relatedness") %>%
          layout(title = list(text = "Sample Relatedness Distribution", font = list(size = 16)),
                 xaxis = list(title = list(text = "PI_HAT (Proportion IBD)", font = list(size = 14))),
                 yaxis = list(title = list(text = "Number of Sample Pairs", font = list(size = 14))),
                 showlegend = FALSE,
                 margin = list(t = 60, b = 60, l = 80, r = 40))
      } else {
        plotly_empty() %>%
          add_annotations(text = "Sample relatedness data not available",
                          x = 0.5, y = 0.5, showarrow = FALSE)
      }
    })
  } else {
    # Fallback to static plots if plotly not available
    # Plot 1: Sample missing rate distribution
    output$per_individual_plot_missing <- renderPlot({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please click 'Show Summary & Plots' or run QC first", size = 5) +
               theme_void())
      }
      
      if (!is.list(data) || !all(c("samples", "genotypes") %in% names(data))) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please load PLINK format data first", size = 5) +
               theme_void())
      }
      
      if (!is.null(stats$individual_missing_rate)) {
        miss_df <- data.frame(MissingRate = stats$individual_missing_rate)
        ggplot(miss_df, aes_string(x = "MissingRate")) +
          geom_histogram(bins = 30, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "Sample Missing Rate Distribution",
               x = "Missing Rate",
               y = "Number of Samples") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      } else {
        ggplot() + annotate("text", x = 0.5, y = 0.5, 
                           label = "Sample missing rate data not available", size = 5) +
          theme_void()
      }
    })
    output$per_individual_plot_relatedness <- renderPlot({
      req(geno_data())
      data <- geno_data()
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                   label = "Please click 'Show Summary & Plots' or run QC first", size = 5) +
               theme_void())
      }

      if (!is.list(data) || !all(c("samples", "genotypes") %in% names(data))) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5,
                                   label = "Please load genotype data first", size = 5) +
               theme_void())
      }

      if (!is.null(stats$individual_relatedness) &&
          is.data.frame(stats$individual_relatedness) &&
          nrow(stats$individual_relatedness) > 0 &&
          "PI_HAT" %in% names(stats$individual_relatedness)) {
        ggplot(stats$individual_relatedness, aes_string(x = "PI_HAT")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "Sample Relatedness Distribution",
               x = "PI_HAT (Proportion IBD)",
               y = "Number of Sample Pairs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      } else {
        ggplot() + annotate("text", x = 0.5, y = 0.5,
                            label = "Sample relatedness data not available", size = 5) +
          theme_void()
      }
    })
  }
  
  # Per-marker QC plots (Interactive with plotly)
  if (use_plotly) {
    # Plot 1: SNP missing rate distribution
    output$per_marker_plot_missing <- renderPlotly({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(plotly_empty() %>% 
               add_annotations(text = "Please click 'Show Summary & Plots' or run QC first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.list(data) || !all(c("genotypes", "map") %in% names(data))) {
        return(plotly_empty() %>% 
               add_annotations(text = "Please load PLINK format data first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      # Plot 1: SNP missing rate distribution
      if (!is.null(stats$marker_missing_rate)) {
        miss_df <- data.frame(MissingRate = stats$marker_missing_rate)
        p_miss <- plot_ly() %>%
          add_histogram(data = miss_df, x = ~MissingRate,
                       nbinsx = 50,
                       marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                       name = "SNP Missing Rate") %>%
          layout(title = list(text = "SNP Missing Rate Distribution", font = list(size = 16)),
                xaxis = list(title = list(text = "Missing Rate", font = list(size = 14))),
                yaxis = list(title = list(text = "Number of SNPs", font = list(size = 14))),
                showlegend = FALSE,
                margin = list(t = 60, b = 60, l = 80, r = 40))
        p_miss
      } else if (!is.null(stats$call_rate)) {
        marker_miss <- 1 - stats$call_rate
        miss_df <- data.frame(MissingRate = marker_miss)
        p_miss <- plot_ly() %>%
          add_histogram(data = miss_df, x = ~MissingRate,
                       nbinsx = 50,
                       marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                       name = "SNP Missing Rate") %>%
          layout(title = list(text = "SNP Missing Rate Distribution", font = list(size = 16)),
                xaxis = list(title = list(text = "Missing Rate", font = list(size = 14))),
                yaxis = list(title = list(text = "Number of SNPs", font = list(size = 14))),
                showlegend = FALSE,
                margin = list(t = 60, b = 60, l = 80, r = 40))
        p_miss
      } else {
        plotly_empty() %>% 
          add_annotations(text = "SNP missing rate data not available",
                        x = 0.5, y = 0.5, showarrow = FALSE)
      }
    })
    
    # Plot 2: Minor allele frequency (MAF) distribution
    output$per_marker_plot_maf <- renderPlotly({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(plotly_empty() %>% 
               add_annotations(text = "Please click 'Show Summary & Plots' or run QC first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.list(data) || !all(c("genotypes", "map") %in% names(data))) {
        return(plotly_empty() %>% 
               add_annotations(text = "Please load PLINK format data first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.null(stats$maf)) {
        maf_df <- data.frame(MAF = stats$maf)
        p_maf <- plot_ly() %>%
          add_histogram(data = maf_df, x = ~MAF,
                      nbinsx = 50,
                      marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                      name = "MAF") %>%
          layout(title = list(text = "Minor Allele Frequency (MAF) Distribution", font = list(size = 16)),
                xaxis = list(title = list(text = "Minor Allele Frequency", font = list(size = 14))),
                yaxis = list(title = list(text = "Number of SNPs", font = list(size = 14))),
                showlegend = FALSE,
                margin = list(t = 60, b = 60, l = 80, r = 40))
        
        # Add threshold line if QC has been run
        if (!is.null(qc_results()) && exists("input") && !is.null(input$maf_threshold)) {
          threshold <- input$maf_threshold
          maf_hist <- hist(maf_df$MAF, breaks = 50, plot = FALSE)
          y_max <- max(maf_hist$counts)
          p_maf <- p_maf %>% 
            add_lines(x = rep(threshold, 2), y = c(0, y_max), 
                     line = list(dash = "dash", color = "red", width = 2), 
                     showlegend = FALSE, hoverinfo = "skip")
        }
        
        p_maf
      } else {
        plotly_empty() %>% 
          add_annotations(text = "MAF data not available",
                        x = 0.5, y = 0.5, showarrow = FALSE)
      }
    })
    
    # Plot 3: Hardy-Weinberg equilibrium (HWE) p-value distribution
    output$per_marker_plot_hwe <- renderPlotly({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(plotly_empty() %>% 
               add_annotations(text = "Please click 'Show Summary & Plots' or run QC first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.list(data) || !all(c("genotypes", "map") %in% names(data))) {
        return(plotly_empty() %>% 
               add_annotations(text = "Please load PLINK format data first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.null(stats$hwe_pvalues)) {
        hwe_df <- data.frame(HWE_pvalue = stats$hwe_pvalues)
        hwe_df <- hwe_df[!is.na(hwe_df$HWE_pvalue), , drop = FALSE]
        
        if (nrow(hwe_df) > 0) {
          hwe_df <- hwe_df[hwe_df$HWE_pvalue > 0 & hwe_df$HWE_pvalue <= 1 & 
                          !is.na(hwe_df$HWE_pvalue) & is.finite(hwe_df$HWE_pvalue), , drop = FALSE]
          
          if (nrow(hwe_df) > 0) {
            hwe_df$minus_log10_p <- -log10(hwe_df$HWE_pvalue)
            hwe_df <- hwe_df[is.finite(hwe_df$minus_log10_p) & !is.na(hwe_df$minus_log10_p) & 
                            hwe_df$minus_log10_p >= 0, , drop = FALSE]
            
            if (nrow(hwe_df) > 0) {
              hwe_threshold <- 1e-5
              hwe_threshold_log10 <- -log10(hwe_threshold)
              
              # Get histogram data for y-axis range
              hwe_hist <- hist(hwe_df$minus_log10_p, breaks = 50, plot = FALSE)
              y_max <- max(hwe_hist$counts)
              
              p_hwe <- plot_ly() %>%
                add_histogram(data = hwe_df, x = ~minus_log10_p,
                            nbinsx = 50,
                            marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                            name = "HWE") %>%
                add_lines(x = rep(hwe_threshold_log10, 2), y = c(0, y_max),
                         line = list(dash = "dash", color = "red", width = 2),
                         showlegend = FALSE, hoverinfo = "skip") %>%
                layout(title = list(text = "Hardy-Weinberg Equilibrium (HWE) Distribution", font = list(size = 16)),
                      xaxis = list(title = list(text = "-log10(HWE exact test p-value)", font = list(size = 14))),
                      yaxis = list(title = list(text = "Number of SNPs", font = list(size = 14))),
                      showlegend = FALSE,
                      margin = list(t = 60, b = 60, l = 80, r = 40))
              
              p_hwe
            } else {
              plotly_empty() %>% 
                add_annotations(text = "No valid HWE p-values to plot\n(all values are invalid)",
                              x = 0.5, y = 0.5, showarrow = FALSE)
            }
          } else {
            plotly_empty() %>% 
              add_annotations(text = "No valid HWE p-values\n(all NA, <= 0, or > 1)",
                            x = 0.5, y = 0.5, showarrow = FALSE)
          }
        } else {
          plotly_empty() %>% 
            add_annotations(text = "No HWE data available\n(all values are NA)",
                          x = 0.5, y = 0.5, showarrow = FALSE)
        }
      } else {
        plotly_empty() %>% 
          add_annotations(text = "HWE p-values not calculated\nPlease run Summary or QC first",
                        x = 0.5, y = 0.5, showarrow = FALSE)
      }
    })
  } else {
    # Fallback to static plots if plotly not available
    # Plot 1: SNP missing rate distribution
    output$per_marker_plot_missing <- renderPlot({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please click 'Show Summary & Plots' or run QC first", size = 5) +
               theme_void())
      }
      
      if (!is.list(data) || !all(c("genotypes", "map") %in% names(data))) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please load PLINK format data first", size = 5) +
               theme_void())
      }
      
      if (!is.null(stats$marker_missing_rate)) {
        miss_df <- data.frame(MissingRate = stats$marker_missing_rate)
        ggplot(miss_df, aes_string(x = "MissingRate")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "SNP Missing Rate Distribution",
               x = "Missing Rate",
               y = "Number of SNPs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      } else if (!is.null(stats$call_rate)) {
        marker_miss <- 1 - stats$call_rate
        miss_df <- data.frame(MissingRate = marker_miss)
        ggplot(miss_df, aes_string(x = "MissingRate")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "SNP Missing Rate Distribution",
               x = "Missing Rate",
               y = "Number of SNPs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
      } else {
        ggplot() + annotate("text", x = 0.5, y = 0.5, 
                           label = "SNP missing rate data not available", size = 5) +
          theme_void()
      }
    })
    
    # Plot 2: Minor allele frequency (MAF) distribution
    output$per_marker_plot_maf <- renderPlot({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please click 'Show Summary & Plots' or run QC first", size = 5) +
               theme_void())
      }
      
      if (!is.list(data) || !all(c("genotypes", "map") %in% names(data))) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please load PLINK format data first", size = 5) +
               theme_void())
      }
      
      if (!is.null(stats$maf)) {
        maf_df <- data.frame(MAF = stats$maf)
        p_maf <- ggplot(maf_df, aes_string(x = "MAF")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "Minor Allele Frequency (MAF) Distribution",
               x = "Minor Allele Frequency",
               y = "Number of SNPs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
        if (!is.null(qc_results())) {
          p_maf <- p_maf + geom_vline(xintercept = input$maf_threshold, linetype = "dashed", color = "red", linewidth = 1)
        }
        p_maf
      } else {
        ggplot() + annotate("text", x = 0.5, y = 0.5, 
                           label = "MAF data not available", size = 5) +
          theme_void()
      }
    })
    
    # Plot 3: Hardy-Weinberg equilibrium (HWE) p-value distribution
    output$per_marker_plot_hwe <- renderPlot({
      req(geno_data())
      data <- geno_data()
      
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please click 'Show Summary & Plots' or run QC first", size = 5) +
               theme_void())
      }
      
      if (!is.list(data) || !all(c("genotypes", "map") %in% names(data))) {
        return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
                                 label = "Please load PLINK format data first", size = 5) +
               theme_void())
      }
      
      if (!is.null(stats$hwe_pvalues)) {
        hwe_df <- data.frame(HWE_pvalue = stats$hwe_pvalues)
        hwe_df <- hwe_df[!is.na(hwe_df$HWE_pvalue), , drop = FALSE]
        
        if (nrow(hwe_df) > 0) {
          hwe_df <- hwe_df[hwe_df$HWE_pvalue > 0 & hwe_df$HWE_pvalue <= 1 & 
                          !is.na(hwe_df$HWE_pvalue) & is.finite(hwe_df$HWE_pvalue), , drop = FALSE]
          
          if (nrow(hwe_df) > 0) {
            hwe_df$minus_log10_p <- -log10(hwe_df$HWE_pvalue)
            hwe_df <- hwe_df[is.finite(hwe_df$minus_log10_p) & !is.na(hwe_df$minus_log10_p) & 
                            hwe_df$minus_log10_p >= 0, , drop = FALSE]
            
            if (nrow(hwe_df) > 0) {
              hwe_threshold <- 1e-5
              hwe_threshold_log10 <- -log10(hwe_threshold)
              
              ggplot(hwe_df, aes_string(x = "minus_log10_p")) +
                geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
                labs(title = "Hardy-Weinberg Equilibrium (HWE) Distribution",
                     x = expression(-log[10](HWE~exact~test~p-value)),
                     y = "Number of SNPs") +
                scale_x_continuous(limits = c(0, NA)) +
                geom_vline(xintercept = hwe_threshold_log10, linetype = "dashed", color = "red", linewidth = 1) +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5, size = 14))
            } else {
              ggplot() + 
                annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid HWE p-values to plot\n(all values are invalid)", size = 4) +
                theme_void()
            }
          } else {
            ggplot() + 
              annotate("text", x = 0.5, y = 0.5, 
                      label = "No valid HWE p-values\n(all NA, <= 0, or > 1)", size = 4) +
              theme_void()
          }
        } else {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, 
                    label = "No HWE data available\n(all values are NA)", size = 4) +
            theme_void()
        }
      } else {
        ggplot() + 
          annotate("text", x = 0.5, y = 0.5, 
                  label = "HWE p-values not calculated\nPlease run Summary or QC first", size = 4) +
          theme_void()
      }
    })
  }
  
  # Store plots for download
  per_individual_plot_obj <- reactiveVal(NULL)
  per_marker_plot_obj <- reactiveVal(NULL)
  pca_result <- reactiveVal(NULL)
  pca_plot_obj <- reactiveVal(NULL)
  # Default number of PCs to match PLINK 2 (--pca typically outputs 20)
  pca_default_n_components <- 20L

  # Compute PCA from summary_stats (from PLINK --pca) or genotype data
  compute_pca <- function(geno_data, summary_stats = NULL) {
    # First, try to get PCA from summary_stats (PLINK output)
    if (!is.null(summary_stats) && 
        !is.null(summary_stats$pca_scores) && 
        !is.null(summary_stats$pca_variance)) {
      tryCatch({
        pca_scores <- summary_stats$pca_scores
        pca_variance <- summary_stats$pca_variance
        n_keep <- min(pca_default_n_components, ncol(pca_scores))
        pca_scores <- pca_scores[, seq_len(n_keep), drop = FALSE]
        pca_variance <- pca_variance[seq_len(n_keep)]
        pca_eigenvalues <- if (!is.null(summary_stats$pca_eigenvalues)) {
          summary_stats$pca_eigenvalues[seq_len(n_keep)]
        } else {
          NULL
        }
        sample_ids <- if (!is.null(summary_stats$pca_sample_ids)) {
          summary_stats$pca_sample_ids
        } else if (!is.null(geno_data$samples)) {
          geno_data$samples$Sample_ID
        } else {
          paste0("Sample_", 1:nrow(pca_scores))
        }
        
        # Remove rows with all NA
        valid_rows <- rowSums(!is.na(pca_scores)) > 0
        if (sum(valid_rows) > 0) {
          return(list(
            scores = pca_scores[valid_rows, , drop = FALSE],
            variance = pca_variance,
            eigenvalues = pca_eigenvalues,
            samples = sample_ids[valid_rows],
            n_components = n_keep
          ))
        }
      }, error = function(e) {
        cat("Error reading PCA from summary_stats:", e$message, "\n")
      })
    }
    
    # Rcpp mode fallback: compute PCA locally from genotype matrix when PLINK PCA
    # outputs are not available.
    if (!is.list(geno_data) || !all(c("genotypes", "samples") %in% names(geno_data))) {
      return(NULL)
    }
    if (!isTRUE(use_rcpp_stats)) {
      return(NULL)
    }
    geno_matrix <- get_numeric_genotype_matrix(geno_data)
    if (is.null(geno_matrix)) {
      return(NULL)
    }

    tryCatch({
      # Remove markers with all missing
      missing_by_marker <- colSums(is.na(geno_matrix))
      valid_markers <- missing_by_marker < nrow(geno_matrix)
      if (sum(valid_markers) < 2) return(NULL)
      geno_matrix <- geno_matrix[, valid_markers, drop = FALSE]
      
      # Remove samples with all missing
      missing_by_sample <- rowSums(is.na(geno_matrix))
      valid_samples <- missing_by_sample < ncol(geno_matrix)
      if (sum(valid_samples) < 2) return(NULL)
      geno_matrix <- geno_matrix[valid_samples, , drop = FALSE]
      
      # PLINK-like standardization
      p <- colMeans(geno_matrix, na.rm = TRUE) / 2
      p[!is.finite(p)] <- NA
      non_mono <- which(p > 1e-10 & p < (1 - 1e-10))
      if (length(non_mono) < 2) return(NULL)
      geno_matrix <- geno_matrix[, non_mono, drop = FALSE]
      p <- p[non_mono]
      
      sd_marker <- sqrt(2 * p * (1 - p))
      sd_marker[sd_marker < 1e-10] <- NA
      keep_sd <- !is.na(sd_marker) & is.finite(sd_marker)
      if (sum(keep_sd) < 2) return(NULL)
      geno_matrix <- geno_matrix[, keep_sd, drop = FALSE]
      p <- p[keep_sd]
      sd_marker <- sd_marker[keep_sd]
      
      geno_std <- sweep(geno_matrix, 2L, 2 * p, `-`)
      geno_std <- sweep(geno_std, 2L, sd_marker, `/`)
      valid_mask <- !is.na(geno_std)
      geno_std[!valid_mask] <- 0

      # PLINK-style PCA fallback:
      # decompose variance-standardized relationship matrix K = XX' / M.
      # PLINK .eigenvec stores eigenvectors (direction vectors), with arbitrary sign.
      # Keep eigenvectors unscaled, and enforce deterministic sign orientation for stability.
      m_markers <- ncol(geno_std)
      if (m_markers < 2) return(NULL)
      K_num <- tcrossprod(geno_std)
      K_den <- tcrossprod(matrix(as.numeric(valid_mask), nrow = nrow(valid_mask), ncol = ncol(valid_mask)))
      K <- K_num / pmax(K_den, 1)
      K[K_den <= 0] <- 0
      K <- (K + t(K)) / 2
      eig <- eigen(K, symmetric = TRUE)
      if (is.null(eig$vectors) || is.null(eig$values)) return(NULL)
      keep <- which(is.finite(eig$values) & eig$values > 0)
      if (length(keep) < 2) return(NULL)
      n_keep <- min(pca_default_n_components, length(keep))
      keep <- keep[seq_len(n_keep)]
      eigenvalues <- eig$values[keep]
      pc_scores <- eig$vectors[, keep, drop = FALSE]
      # Deterministic sign convention (eigenvector sign is otherwise arbitrary).
      for (pc_idx in seq_len(ncol(pc_scores))) {
        v <- pc_scores[, pc_idx]
        pivot <- which.max(abs(v))
        if (length(pivot) == 1 && is.finite(v[pivot]) && v[pivot] < 0) {
          pc_scores[, pc_idx] <- -v
        }
      }
      variance_explained <- (eigenvalues / sum(eigenvalues)) * 100
      
      sample_ids <- if (!is.null(geno_data$samples) && is.data.frame(geno_data$samples) &&
                       "Sample_ID" %in% names(geno_data$samples)) {
        geno_data$samples$Sample_ID[valid_samples]
      } else {
        paste0("Sample_", seq_len(nrow(pc_scores)))
      }
      
      list(
        scores = pc_scores,
        variance = variance_explained,
        eigenvalues = eigenvalues,
        samples = sample_ids,
        n_components = n_keep
      )
    }, error = function(e) {
      cat("Error computing PCA fallback:", e$message, "\n")
      NULL
    })
  }

  # Align PCA coordinates for backend-consistent raw visualization:
  # deterministic sign orientation per PC (no re-scaling).
  align_pca_scores_for_plot <- function(scores) {
    if (is.null(scores)) return(NULL)
    m <- as.matrix(scores)
    if (nrow(m) == 0 || ncol(m) == 0) return(m)

    for (j in seq_len(ncol(m))) {
      v <- as.numeric(m[, j])
      finite_idx <- is.finite(v)
      if (!any(finite_idx)) {
        m[, j] <- v
        next
      }

      # Deterministic sign: largest absolute loading should be positive.
      pivot <- which.max(abs(v[finite_idx]))
      pivot_val <- v[which(finite_idx)[pivot]]
      if (is.finite(pivot_val) && pivot_val < 0) v <- -v
      m[, j] <- v
    }
    m
  }
  
  # Compute PCA when data or stats change (from PLINK --pca outputs)
  observe({
    req(geno_data())
    data <- geno_data()
    
    if (!is.list(data) || !all(c("genotypes", "samples") %in% names(data))) {
      pca_result(NULL)
      return()
    }
    
    # Prefer PLINK PCA from summary_stats if available.
    stats <- if (summary_generated() && !is.null(summary_stats())) {
      summary_stats()
    } else if (!is.null(qc_results())) {
      qc_results()
    } else {
      NULL
    }
    
    # When QC results are active, compute local PCA on filtered data to match PLINK/QC views.
    pca_data <- data
    if (!is.null(stats) && is.list(stats) && "filtered_data" %in% names(stats) &&
        is.list(stats$filtered_data) &&
        all(c("genotypes", "samples") %in% names(stats$filtered_data))) {
      pca_data <- stats$filtered_data
    }

    pca <- compute_pca(pca_data, summary_stats = stats)
    if (!is.null(pca) && !is.null(pca$scores) && nrow(pca$scores) > 0) {
      pca_result(pca)
      cat("PCA computed successfully:", nrow(pca$scores), "samples,", ncol(pca$scores), "components\n")
    } else {
      pca_result(NULL)
      cat("PCA unavailable: no usable PCA result from PLINK output or local fallback\n")
    }
  })
  
  # PCA Plots (2D and 3D) - requires plotly
  if (use_plotly) {
    output$pca_plots <- renderPlotly({
      req(geno_data(), input$pca_dimension)
      data <- geno_data()
      empty_pca_plot <- function(msg) {
        plot_ly(type = "scatter", mode = "markers", x = numeric(0), y = numeric(0)) %>%
          layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)) %>%
          add_annotations(text = msg, x = 0.5, y = 0.5, showarrow = FALSE)
      }
      
      if (!is.list(data) || !all(c("genotypes", "samples") %in% names(data))) {
        return(empty_pca_plot("Please load genotype data (PLINK or BLUPF90) first"))
      }
      
      # Prefer PLINK PCA from summary_stats if available.
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        NULL
      }
      
      pca_data <- data
      if (!is.null(stats) && is.list(stats) && "filtered_data" %in% names(stats) &&
          is.list(stats$filtered_data) &&
          all(c("genotypes", "samples") %in% names(stats$filtered_data))) {
        pca_data <- stats$filtered_data
      }

      pca <- pca_result()
      if (is.null(pca) || is.null(pca$scores) || nrow(pca$scores) == 0) {
        pca <- compute_pca(pca_data, summary_stats = stats)
        if (!is.null(pca) && !is.null(pca$scores) && nrow(pca$scores) > 0) {
          pca_result(pca)
        } else {
          return(empty_pca_plot("PCA computation failed. Please check your data."))
        }
      }
      
      # Re-validate after computation
      pca <- pca_result()
      if (is.null(pca) || is.null(pca$scores) || nrow(pca$scores) == 0) {
        return(empty_pca_plot("PCA computation returned empty result. Please check your data."))
      }
      pca_plot <- pca
      pca_plot$scores <- align_pca_scores_for_plot(pca_plot$scores)
      
      # Validate PCA data
      if (ncol(pca_plot$scores) < 2) {
        return(empty_pca_plot("Insufficient PCA components. Need at least 2 components for 2D plot."))
      }
      
      # Get dimension selection
      dimension <- input$pca_dimension
      
      if (dimension == "2d") {
        # 2D PCA plot (PC1 vs PC2)
        # Ensure we have valid data
        if (length(pca_plot$samples) != nrow(pca_plot$scores)) {
          pca_plot$samples <- paste0("Sample_", 1:nrow(pca_plot$scores))
        }
        
        pca_df <- data.frame(
          PC1 = as.numeric(pca_plot$scores[, 1]),
          PC2 = as.numeric(pca_plot$scores[, 2]),
          Sample = as.character(pca_plot$samples)
        )
        
        # Remove any rows with invalid data
        pca_df <- pca_df[is.finite(pca_df$PC1) & is.finite(pca_df$PC2), ]
        
        if (nrow(pca_df) == 0) {
          return(empty_pca_plot("No valid PCA data to display."))
        }
        
        p <- plot_ly(pca_df, x = ~PC1, y = ~PC2, 
                    text = ~Sample, hoverinfo = "text",
                    type = "scatter", mode = "markers",
                    marker = list(size = 8, color = "#CEB888", 
                                line = list(color = "#333333", width = 1))) %>%
          layout(
            title = list(text = paste0("PCA Plot (2D)<br>",
                          "PC1: ", round(pca$variance[1], 2), "% variance, ",
                          "PC2: ", round(pca$variance[2], 2), "% variance"), 
                        font = list(size = 16)),
            xaxis = list(
              title = list(text = paste0("PC1 (", round(pca$variance[1], 2), "%)"), font = list(size = 14)),
              zeroline = TRUE, scaleanchor = "y", scaleratio = 1
            ),
            yaxis = list(
              title = list(text = paste0("PC2 (", round(pca$variance[2], 2), "%)"), font = list(size = 14)),
              zeroline = TRUE
            ),
            hovermode = "closest",
            margin = list(t = 80, b = 60, l = 80, r = 40)
          )
        
        pca_plot_obj(p)
        p
      } else {
        # 3D PCA plot (PC1 vs PC2 vs PC3)
        if (ncol(pca_plot$scores) >= 3) {
          # Ensure we have valid data
          if (length(pca_plot$samples) != nrow(pca_plot$scores)) {
            pca_plot$samples <- paste0("Sample_", 1:nrow(pca_plot$scores))
          }
          
          pca_df <- data.frame(
            PC1 = as.numeric(pca_plot$scores[, 1]),
            PC2 = as.numeric(pca_plot$scores[, 2]),
            PC3 = as.numeric(pca_plot$scores[, 3]),
            Sample = as.character(pca_plot$samples)
          )
          
          # Remove any rows with invalid data
          pca_df <- pca_df[is.finite(pca_df$PC1) & is.finite(pca_df$PC2) & is.finite(pca_df$PC3), ]
          
          if (nrow(pca_df) == 0) {
            return(empty_pca_plot("No valid PCA data to display."))
          }
          
          p <- plot_ly(pca_df, x = ~PC1, y = ~PC2, z = ~PC3,
                      text = ~Sample, hoverinfo = "text",
                      type = "scatter3d", mode = "markers",
                      marker = list(size = 5, color = "#CEB888",
                                  line = list(color = "#333333", width = 1))) %>%
            layout(
              title = list(text = paste0("PCA Plot (3D)<br>",
                            "PC1: ", round(pca$variance[1], 2), "%, ",
                            "PC2: ", round(pca$variance[2], 2), "%, ",
                            "PC3: ", round(pca$variance[3], 2), "% variance"),
                        font = list(size = 16)),
              scene = list(
                xaxis = list(title = list(text = paste0("PC1 (", round(pca$variance[1], 2), "%)"), 
                                         font = list(size = 14))),
                yaxis = list(title = list(text = paste0("PC2 (", round(pca$variance[2], 2), "%)"), 
                                         font = list(size = 14))),
                zaxis = list(title = list(text = paste0("PC3 (", round(pca$variance[3], 2), "%)"), 
                                         font = list(size = 14)))
              ),
              margin = list(t = 80, b = 40, l = 40, r = 40)
            )
          
          pca_plot_obj(p)
          p
        } else {
          empty_pca_plot("Insufficient dimensions for 3D plot. Need at least 3 principal components.")
        }
      }
    })
  } else {
    # Fallback if plotly not available
    output$pca_plots <- renderUI({
      div(class = "alert alert-warning", style = "padding: 20px; margin: 20px;",
          h4("plotly Package Required"),
          p("The PCA visualization feature requires the plotly package."),
          p("Please install it:"),
          tags$code("install.packages('plotly')"),
          br(), br(),
          p("After installation, please refresh the application.")
      )
    })
  }
  
  # Reset PCA when data changes
  observeEvent(geno_data(), {
    pca_result(NULL)
    pca_plot_obj(NULL)
  })
  
  # Update plot objects when plots are rendered
  # Update plot objects for download (supports both summary_stats and qc_results)
  observe({
    req(geno_data())
    data <- geno_data()
    
    # Use summary_stats if available, otherwise use qc_results
    stats <- if (summary_generated() && !is.null(summary_stats())) {
      summary_stats()
    } else if (!is.null(qc_results())) {
      qc_results()
    } else {
      return(NULL)
    }
    
    if (is.list(data) && all(c("samples", "genotypes") %in% names(data))) {
      plots <- list()
      
      # Plot 1: Sample missing rate
      if (!is.null(stats$individual_missing_rate)) {
        miss_df <- data.frame(MissingRate = stats$individual_missing_rate)
        p_miss <- ggplot(miss_df, aes_string(x = "MissingRate")) +
          geom_histogram(bins = 30, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "Sample Missing Rate Distribution",
               x = "Missing Rate",
               y = "Number of Samples") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
        
        if (!is.null(qc_results())) {
        }
        
        plots$missing_rate <- p_miss
      }
      if (!is.null(stats$individual_relatedness) &&
          is.data.frame(stats$individual_relatedness) &&
          nrow(stats$individual_relatedness) > 0 &&
          "PI_HAT" %in% names(stats$individual_relatedness)) {
        rel_df <- stats$individual_relatedness
        p_rel <- ggplot(rel_df, aes_string(x = "PI_HAT")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "Sample Relatedness Distribution",
               x = "PI_HAT (Proportion IBD)",
               y = "Number of Sample Pairs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
        plots$relatedness <- p_rel
      }
      
      if (length(plots) > 0) {
        if (requireNamespace("gridExtra", quietly = TRUE)) {
          combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
          per_individual_plot_obj(combined_plot)
        } else if (requireNamespace("cowplot", quietly = TRUE)) {
          combined_plot <- do.call(cowplot::plot_grid, c(plots, ncol = 1))
          per_individual_plot_obj(combined_plot)
        }
      }
    }
  })
  
  observe({
    req(geno_data())
    data <- geno_data()
    
    # Use summary_stats if available, otherwise use qc_results
    stats <- if (summary_generated() && !is.null(summary_stats())) {
      summary_stats()
    } else if (!is.null(qc_results())) {
      qc_results()
    } else {
      return(NULL)
    }
    
    if (is.list(data) && all(c("genotypes", "map") %in% names(data))) {
      plots <- list()
      
      # Plot 1: SNP missing rate
      if (!is.null(stats$marker_missing_rate)) {
        miss_df <- data.frame(MissingRate = stats$marker_missing_rate)
        p_miss <- ggplot(miss_df, aes_string(x = "MissingRate")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "SNP Missing Rate Distribution",
               x = "Missing Rate",
               y = "Number of SNPs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
        
        if (!is.null(qc_results())) {
        }
        
        plots$snp_missing <- p_miss
      } else if (!is.null(stats$call_rate)) {
        # Fallback
        marker_miss <- 1 - stats$call_rate
        miss_df <- data.frame(MissingRate = marker_miss)
        p_miss <- ggplot(miss_df, aes_string(x = "MissingRate")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "SNP Missing Rate Distribution",
               x = "Missing Rate",
               y = "Number of SNPs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
        
        if (!is.null(qc_results())) {
        }
        
        plots$snp_missing <- p_miss
      }
      
      # Plot 2: MAF distribution
      if (!is.null(stats$maf)) {
        maf_df <- data.frame(MAF = stats$maf)
        p_maf <- ggplot(maf_df, aes_string(x = "MAF")) +
          geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
          labs(title = "Minor Allele Frequency (MAF) Distribution",
               x = "Minor Allele Frequency",
               y = "Number of SNPs") +
          theme_bw() +
          theme(plot.title = element_text(hjust = 0.5, size = 14))
        
        if (!is.null(qc_results())) {
            geom_vline(xintercept = input$maf_threshold, linetype = "dashed", color = "red", linewidth = 1)
        }
        
        plots$maf <- p_maf
      }
      
      # Plot 3: HWE p-value distribution
      # x-axis: -log10(HWE exact test p-value), y-axis: Number of SNPs
      if (!is.null(stats$hwe_pvalues)) {
        hwe_df <- data.frame(HWE_pvalue = stats$hwe_pvalues)
        hwe_df <- hwe_df[!is.na(hwe_df$HWE_pvalue), , drop = FALSE]
        
        if (nrow(hwe_df) > 0) {
          # Filter out invalid p-values (NA, <= 0, > 1, or non-finite)
          hwe_df <- hwe_df[hwe_df$HWE_pvalue > 0 & hwe_df$HWE_pvalue <= 1 & 
                           !is.na(hwe_df$HWE_pvalue) & is.finite(hwe_df$HWE_pvalue), , drop = FALSE]
          
          if (nrow(hwe_df) > 0) {
            # Calculate -log10(p-value) for x-axis
            hwe_df$minus_log10_p <- -log10(hwe_df$HWE_pvalue)
            
            # Remove any infinite or invalid values after log transformation
            hwe_df <- hwe_df[is.finite(hwe_df$minus_log10_p) & !is.na(hwe_df$minus_log10_p) & 
                             hwe_df$minus_log10_p >= 0, , drop = FALSE]
            
            if (nrow(hwe_df) > 0) {
              hwe_threshold <- 1e-5
              hwe_threshold_log10 <- -log10(hwe_threshold)
              
              # Plot HWE histogram with -log10(p-value) on x-axis
              p_hwe <- ggplot(hwe_df, aes_string(x = "minus_log10_p")) +
                geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
                labs(title = "Hardy-Weinberg Equilibrium (HWE) Distribution",
                     x = expression(-log[10](HWE~exact~test~p-value)),
                     y = "Number of SNPs") +
                scale_x_continuous(limits = c(0, NA)) +  # x-axis starts from 0
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5, size = 14))
              
              p_hwe <- p_hwe +
                geom_vline(xintercept = hwe_threshold_log10, linetype = "dashed", color = "red", linewidth = 1)
              
              plots$hwe <- p_hwe
            } else {
              # No valid data after filtering
              plots$hwe <- ggplot() + 
                annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid HWE p-values to plot\n(all values are invalid)", size = 4) +
                theme_void()
            }
          } else {
            # No valid p-values
            plots$hwe <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, 
                      label = "No valid HWE p-values\n(all NA, <= 0, or > 1)", size = 4) +
              theme_void()
          }
        }
      }
      
      if (length(plots) > 0) {
        if (requireNamespace("gridExtra", quietly = TRUE)) {
          combined_plot <- do.call(gridExtra::grid.arrange, c(plots, ncol = 1))
          per_marker_plot_obj(combined_plot)
        } else if (requireNamespace("cowplot", quietly = TRUE)) {
          combined_plot <- do.call(cowplot::plot_grid, c(plots, ncol = 1))
          per_marker_plot_obj(combined_plot)
        }
      }
    }
  })
  
  # Download handlers for new plots
  output$download_per_individual_plots <- downloadHandler(
    filename = function() {
      paste0("per_individual_plots_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(per_individual_plot_obj())
      png(file, width = 12, height = 10, units = "in", res = 300)
      if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::grid.arrange(per_individual_plot_obj())
      } else {
        print(per_individual_plot_obj())
      }
      dev.off()
    }
  )
  
  output$download_per_marker_plots <- downloadHandler(
    filename = function() {
      paste0("per_marker_plots_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(per_marker_plot_obj())
      png(file, width = 12, height = 10, units = "in", res = 300)
      if (requireNamespace("gridExtra", quietly = TRUE)) {
        gridExtra::grid.arrange(per_marker_plot_obj())
      } else {
        print(per_marker_plot_obj())
      }
      dev.off()
    }
  )
  
  # Download PCA plots
  output$download_pca_plots <- downloadHandler(
    filename = function() {
      dimension <- if (!is.null(input$pca_dimension)) input$pca_dimension else "2d"
      paste0("pca_plots_", dimension, "_", Sys.Date(), ".html")
    },
    content = function(file) {
      req(pca_plot_obj())
      if (use_plotly) {
        # Export plotly plot as HTML
        htmlwidgets::saveWidget(pca_plot_obj(), file, selfcontained = TRUE)
      } else {
        # Fallback: create a simple message file
        writeLines("plotly package is required to download PCA plots", file)
      }
    }
  )

  # Download PCA results in PLINK format (.eigenvec: FID IID PC1 PC2 ...; .eigenval: one eigenvalue per line)
  output$download_pca_eigenvec <- downloadHandler(
    filename = function() {
      paste0("pca_", Sys.Date(), ".eigenvec")
    },
    content = function(file) {
      pca <- pca_result()
      if (is.null(pca) || is.null(pca$scores) || nrow(pca$scores) == 0) {
        writeLines("No PCA result available. Load data and open PCA Plots tab first.", file)
        return()
      }
      sid <- as.character(pca$samples)
      out <- data.frame(FID = sid, IID = sid, as.data.frame(pca$scores))
      write.table(out, file, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
    }
  )
  output$download_pca_eigenval <- downloadHandler(
    filename = function() {
      paste0("pca_", Sys.Date(), ".eigenval")
    },
    content = function(file) {
      pca <- pca_result()
      if (is.null(pca) || is.null(pca$eigenvalues)) {
        if (!is.null(pca) && !is.null(pca$variance)) {
          # Reconstruct eigenvalues from variance (total = sum of eigenvalues unknown, use 1)
          ev <- pca$variance / 100
          writeLines(as.character(ev), file)
        } else {
          writeLines("No PCA eigenvalues available. Load data and open PCA Plots tab first.", file)
        }
        return()
      }
      writeLines(as.character(pca$eigenvalues), file)
    }
  )

  # Download handlers
  output$download_qc_report <- downloadHandler(
    filename = function() {
      paste0("genovieweR_QC_report_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(qc_results())
      report <- qc_results()
      
      # Create a comprehensive report
      report_df <- data.frame(
        Parameter = character(0),
        Threshold = character(0),
        Before = character(0),
        After = character(0),
        Removed = character(0),
        Percentage_Removed = character(0),
        stringsAsFactors = FALSE
      )
      
      if (!is.null(report) && is.list(report)) {
        # Overall summary
        if (!is.null(report$samples_before)) {
          samples_removed <- if (!is.null(report$samples_after)) {
            report$samples_before - report$samples_after
          } else {
            NA
          }
          samples_pct <- if (!is.na(samples_removed) && report$samples_before > 0) {
            round((samples_removed / report$samples_before) * 100, 2)
          } else {
            NA
          }
          report_df <- rbind(report_df,
                            data.frame(Parameter = "Samples (Overall)",
                                     Threshold = "N/A",
                                     Before = as.character(report$samples_before),
                                     After = if (!is.null(report$samples_after)) as.character(report$samples_after) else "N/A",
                                     Removed = if (!is.na(samples_removed)) as.character(samples_removed) else "N/A",
                                     Percentage_Removed = if (!is.na(samples_pct)) paste0(samples_pct, "%") else "N/A",
                                     stringsAsFactors = FALSE))
        }
        
        if (!is.null(report$snps_before)) {
          snps_removed <- if (!is.null(report$snps_after)) {
            report$snps_before - report$snps_after
          } else {
            NA
          }
          snps_pct <- if (!is.na(snps_removed) && report$snps_before > 0) {
            round((snps_removed / report$snps_before) * 100, 2)
          } else {
            NA
          }
          report_df <- rbind(report_df,
                            data.frame(Parameter = "SNPs (Overall)",
                                     Threshold = "N/A",
                                     Before = as.character(report$snps_before),
                                     After = if (!is.null(report$snps_after)) as.character(report$snps_after) else "N/A",
                                     Removed = if (!is.na(snps_removed)) as.character(snps_removed) else "N/A",
                                     Percentage_Removed = if (!is.na(snps_pct)) paste0(snps_pct, "%") else "N/A",
                                     stringsAsFactors = FALSE))
        }
        
        # Detailed filter statistics
        if (!is.null(report$filter_stats)) {
          # --geno filter
          if (!is.null(report$thresholds$geno) && !is.null(report$filter_stats$geno_removed_snps)) {
            geno_before <- report$snps_before
            geno_after <- if (!is.null(report$intermediate_counts$geno_after_snps)) {
              report$intermediate_counts$geno_after_snps
            } else {
              geno_before - report$filter_stats$geno_removed_snps
            }
            geno_pct <- if (geno_before > 0) {
              round((report$filter_stats$geno_removed_snps / geno_before) * 100, 2)
            } else {
              0
            }
            report_df <- rbind(report_df,
                              data.frame(Parameter = "--geno (SNP missing rate)",
                                       Threshold = as.character(report$thresholds$geno),
                                       Before = as.character(geno_before),
                                       After = as.character(geno_after),
                                       Removed = as.character(report$filter_stats$geno_removed_snps),
                                       Percentage_Removed = paste0(geno_pct, "%"),
                                       stringsAsFactors = FALSE))
          }
          
          # --mind filter
          if (!is.null(report$thresholds$mind) && !is.null(report$filter_stats$mind_removed_samples)) {
            mind_before <- report$samples_before
            mind_after <- mind_before - report$filter_stats$mind_removed_samples
            mind_pct <- if (mind_before > 0) {
              round((report$filter_stats$mind_removed_samples / mind_before) * 100, 2)
            } else {
              0
            }
            report_df <- rbind(report_df,
                              data.frame(Parameter = "--mind (Sample missing rate)",
                                       Threshold = as.character(report$thresholds$mind),
                                       Before = as.character(mind_before),
                                       After = as.character(mind_after),
                                       Removed = as.character(report$filter_stats$mind_removed_samples),
                                       Percentage_Removed = paste0(mind_pct, "%"),
                                       stringsAsFactors = FALSE))
          }
          
          # --maf filter
          if (!is.null(report$thresholds$maf) && !is.null(report$filter_stats$maf_removed_snps)) {
            maf_before <- if (!is.null(report$intermediate_counts$maf_before_snps)) {
              report$intermediate_counts$maf_before_snps
            } else {
              # Fallback: if geno was applied, use geno_after, otherwise use initial
              if (!is.null(report$thresholds$geno) && !is.null(report$intermediate_counts$geno_after_snps)) {
                report$intermediate_counts$geno_after_snps
              } else {
                report$snps_before
              }
            }
            maf_after <- if (!is.null(report$intermediate_counts$maf_after_snps)) {
              report$intermediate_counts$maf_after_snps
            } else {
              maf_before - report$filter_stats$maf_removed_snps
            }
            maf_pct <- if (maf_before > 0) {
              round((report$filter_stats$maf_removed_snps / maf_before) * 100, 2)
            } else {
              0
            }
            report_df <- rbind(report_df,
                              data.frame(Parameter = "--maf (Minor allele frequency)",
                                       Threshold = as.character(report$thresholds$maf),
                                       Before = as.character(maf_before),
                                       After = as.character(maf_after),
                                       Removed = as.character(report$filter_stats$maf_removed_snps),
                                       Percentage_Removed = paste0(maf_pct, "%"),
                                       stringsAsFactors = FALSE))
          }
          
          # --hwe filter
          if (!is.null(report$thresholds$hwe) && !is.null(report$filter_stats$hwe_removed_snps)) {
            hwe_before <- if (!is.null(report$intermediate_counts$hwe_before_snps)) {
              report$intermediate_counts$hwe_before_snps
            } else {
              # Fallback: if maf was applied, use maf_after, else if geno was applied, use geno_after, else use initial
              if (!is.null(report$thresholds$maf) && !is.null(report$intermediate_counts$maf_after_snps)) {
                report$intermediate_counts$maf_after_snps
              } else if (!is.null(report$thresholds$geno) && !is.null(report$intermediate_counts$geno_after_snps)) {
                report$intermediate_counts$geno_after_snps
              } else {
                report$snps_before
              }
            }
            hwe_after <- hwe_before - report$filter_stats$hwe_removed_snps
            hwe_pct <- if (hwe_before > 0) {
              round((report$filter_stats$hwe_removed_snps / hwe_before) * 100, 2)
            } else {
              0
            }
            report_df <- rbind(report_df,
                              data.frame(Parameter = "--hwe (HWE p-value)",
                                       Threshold = as.character(report$thresholds$hwe),
                                       Before = as.character(hwe_before),
                                       After = as.character(hwe_after),
                                       Removed = as.character(report$filter_stats$hwe_removed_snps),
                                       Percentage_Removed = paste0(hwe_pct, "%"),
                                       stringsAsFactors = FALSE))
          }
        }
        
        # Add status message
        if (!is.null(report$message)) {
          report_df <- rbind(report_df,
                            data.frame(Parameter = "Status",
                                     Threshold = "N/A",
                                     Before = "N/A",
                                     After = "N/A",
                                     Removed = "N/A",
                                     Percentage_Removed = report$message,
                                     stringsAsFactors = FALSE))
        }
      }
      
      if (nrow(report_df) == 0) {
        report_df <- data.frame(
          Parameter = "Message",
          Threshold = "N/A",
          Before = "N/A",
          After = "N/A",
          Removed = "N/A",
          Percentage_Removed = "No QC report available. Please run QC first.",
          stringsAsFactors = FALSE
        )
      }
      
      write.csv(report_df, file, row.names = FALSE)
    }
  )
  
  output$download_filtered_data <- downloadHandler(
    filename = function() {
      req(qc_results())
      report <- qc_results()
      paste0("genovieweR_filtered_data_", Sys.Date(), ".zip")
    },
    content = function(file) {
      req(qc_results())
      report <- qc_results()
      
      if (is.null(report)) {
        writeLines("No QC results available. Please run QC first.", file)
        return(NULL)
      }
      
      # Prefer PLINK output files if available
      if (!is.null(report$output_files) && !is.null(report$output_files$bed)) {
        plink_path <- find_plink_path()
        if (is.null(plink_path)) {
          writeLines("PLINK executable not found. Cannot convert filtered data.", file)
          return(NULL)
        }
        
        bed_file <- report$output_files$bed
        bim_file <- report$output_files$bim
        fam_file <- report$output_files$fam
        if (is.null(bed_file) || !file.exists(bed_file) ||
            is.null(bim_file) || !file.exists(bim_file) ||
            is.null(fam_file) || !file.exists(fam_file)) {
          writeLines("Filtered data files not found. Please run QC again.", file)
          return(NULL)
        }
        
        temp_dir <- tempfile(pattern = "genovieweR_download_")
        dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
        
        bed_base <- sub("\\.bed$", "", basename(bed_file))
        bed_dir <- dirname(bed_file)
        ped_output <- file.path(temp_dir, paste0(bed_base, "_filtered"))
        
        convert_result <- system2(
          plink_path,
          args = c(
            "--bfile", file.path(bed_dir, bed_base),
            "--allow-extra-chr",
            "--nonfounders",
            "--recode",
            "--out", ped_output
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        
        files_to_zip <- c(bed_file, bim_file, fam_file)
        ped_file <- paste0(ped_output, ".ped")
        map_file <- paste0(ped_output, ".map")
        if (convert_result == 0 && file.exists(ped_file) && file.exists(map_file)) {
          files_to_zip <- c(files_to_zip, ped_file, map_file)
        }
        
        temp_zip <- tempfile(fileext = ".zip")
        zip(temp_zip, files_to_zip)
        file.copy(temp_zip, file)
        unlink(temp_dir, recursive = TRUE, force = TRUE)
        return(NULL)
      }
      
      # R-based filtered data fallback
      if (is.null(report$filtered_data)) {
        writeLines("Filtered data is not available in expected format.", file)
        return(NULL)
      }
      
      filtered_data <- report$filtered_data
      if (!is.list(filtered_data) || !all(c("samples", "genotypes", "map") %in% names(filtered_data))) {
        writeLines("Filtered data is not available in expected format.", file)
        return(NULL)
      }
      
      temp_dir <- tempfile(pattern = "genovieweR_download_")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      
      ped_file <- file.path(temp_dir, "filtered.ped")
      map_file <- file.path(temp_dir, "filtered.map")
      
      # Write MAP file
      map_data <- filtered_data$map
      write.table(map_data, map_file, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
      
      # Write PED file (expand genotype pairs)
      geno_matrix <- filtered_data$genotypes
      allele_a <- substr(geno_matrix, 1, 1)
      allele_b <- substr(geno_matrix, 3, 3)
      allele_cols <- matrix(NA_character_, nrow = nrow(geno_matrix), ncol = ncol(geno_matrix) * 2)
      for (i in seq_len(ncol(geno_matrix))) {
        allele_cols[, (i - 1) * 2 + 1] <- allele_a[, i]
        allele_cols[, (i - 1) * 2 + 2] <- allele_b[, i]
      }
      
      ped_data <- cbind(
        filtered_data$samples$Family_ID,
        filtered_data$samples$Sample_ID,
        0, 0, 0, -9,
        allele_cols
      )
      write.table(ped_data, ped_file, row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
      
      files_to_zip <- c(ped_file, map_file)
      temp_zip <- tempfile(fileext = ".zip")
      zip(temp_zip, files_to_zip)
      file.copy(temp_zip, file)
      unlink(temp_dir, recursive = TRUE, force = TRUE)
    }
  )
}

# Read VCF file into internal format
read_vcf_file <- function(vcf_path) {
  if (!requireNamespace("vcfR", quietly = TRUE)) {
    stop("vcfR package is required to read VCF files.")
  }
  
  vcf <- vcfR::read.vcfR(vcf_path, verbose = FALSE)
  gt <- vcfR::extract.gt(vcf, element = "GT", as.numeric = FALSE)
  if (is.null(gt) || nrow(gt) == 0) {
    stop("No genotype data found in VCF.")
  }
  
  # Convert GT to numeric minor allele counts (0/1/2)
  geno_matrix <- matrix(NA_real_, nrow = nrow(gt), ncol = ncol(gt))
  for (i in seq_len(nrow(gt))) {
    for (j in seq_len(ncol(gt))) {
      g <- gt[i, j]
      if (is.na(g) || g == "." || g == "./." || g == ".|.") {
        geno_matrix[i, j] <- NA_real_
      } else {
        alleles <- unlist(strsplit(g, "[/|]"))
        if (length(alleles) == 2 && all(alleles %in% c("0", "1"))) {
          geno_matrix[i, j] <- as.numeric(alleles[1]) + as.numeric(alleles[2])
        } else {
          geno_matrix[i, j] <- NA_real_
        }
      }
    }
  }
  
  geno_matrix <- t(geno_matrix)  # samples x markers
  colnames(geno_matrix) <- vcf@fix[, "ID"]
  
  map <- data.frame(
    Chromosome = vcf@fix[, "CHROM"],
    SNP_ID = ifelse(vcf@fix[, "ID"] == ".", paste0(vcf@fix[, "CHROM"], ":", vcf@fix[, "POS"]), vcf@fix[, "ID"]),
    Genetic_Distance = 0,
    Physical_Position = as.integer(vcf@fix[, "POS"]),
    stringsAsFactors = FALSE
  )
  
  samples <- data.frame(
    Family_ID = colnames(gt),
    Sample_ID = colnames(gt),
    stringsAsFactors = FALSE
  )
  
  list(samples = samples, genotypes = geno_matrix, map = map)
}

# Read PLINK BED/BIM/FAM into internal format
read_plink_bed <- function(bed_path, bim_path, fam_path) {
  if (!requireNamespace("SNPRelate", quietly = TRUE)) {
    stop("SNPRelate package is required to read PLINK .bed files.")
  }
  
  temp_gds <- tempfile(fileext = ".gds")
  SNPRelate::snpgdsBED2GDS(bed.fn = bed_path, bim.fn = bim_path, fam.fn = fam_path,
                           out.gdsfn = temp_gds, verbose = FALSE)
  gds <- SNPRelate::snpgdsOpen(temp_gds, readonly = TRUE)
  on.exit({
    SNPRelate::snpgdsClose(gds)
    unlink(temp_gds)
  }, add = TRUE)
  
  geno <- SNPRelate::snpgdsGetGeno(gds)
  geno[geno == 3] <- NA  # missing
  geno_matrix <- t(geno)  # samples x markers
  
  snp_id <- SNPRelate::snpgdsGetSNPID(gds)
  chr <- SNPRelate::snpgdsGetSNPChromosome(gds)
  pos <- SNPRelate::snpgdsGetSNPPosition(gds)
  sample_id <- SNPRelate::snpgdsGetSampleID(gds)
  
  map <- data.frame(
    Chromosome = chr,
    SNP_ID = snp_id,
    Genetic_Distance = 0,
    Physical_Position = pos,
    stringsAsFactors = FALSE
  )
  
  samples <- data.frame(
    Family_ID = sample_id,
    Sample_ID = sample_id,
    stringsAsFactors = FALSE
  )
  
  list(samples = samples, genotypes = geno_matrix, map = map)
}

# Read BLUPF90 TXT + MAP into internal format (numeric genotypes)
read_blupf90_txt <- function(txt_path, map_path) {
  map_data <- if (use_data_table && requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(map_path, header = FALSE, data.table = FALSE, showProgress = FALSE)
  } else {
    read.table(map_path, header = FALSE, stringsAsFactors = FALSE)
  }
  if (ncol(map_data) < 4) {
    stop("BLUPF90 map file must contain at least 4 columns.", call. = FALSE)
  }
  map_data <- map_data[, 1:4, drop = FALSE]
  # Remove fully empty rows
  non_empty_map <- rowSums(is.na(map_data) | trimws(as.character(as.matrix(map_data))) == "") < ncol(map_data)
  map_data <- map_data[non_empty_map, , drop = FALSE]
  # Drop optional header row
  if (nrow(map_data) > 0) {
    first_map_row <- tolower(paste(as.character(map_data[1, ]), collapse = " "))
    if (grepl("chrom|snp|marker|position|id", first_map_row)) {
      map_data <- map_data[-1, , drop = FALSE]
    }
  }
  if (nrow(map_data) == 0) {
    stop("BLUPF90 map file has no marker rows after cleaning.", call. = FALSE)
  }
  colnames(map_data) <- c("Chromosome", "SNP_ID", "Genetic_Distance", "Physical_Position")
  
  geno_data <- if (use_data_table && requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(
      txt_path,
      header = FALSE,
      data.table = FALSE,
      showProgress = FALSE,
      colClasses = "character"
    )
  } else {
    read.table(txt_path, header = FALSE, stringsAsFactors = FALSE, colClasses = "character")
  }

  if (ncol(geno_data) < 2) {
    stop("BLUPF90 txt file must contain at least one ID column and genotype columns.", call. = FALSE)
  }
  # Remove fully empty rows/columns
  non_empty_rows <- rowSums(is.na(geno_data) | trimws(as.character(as.matrix(geno_data))) == "") < ncol(geno_data)
  geno_data <- geno_data[non_empty_rows, , drop = FALSE]
  non_empty_cols <- colSums(is.na(geno_data) | trimws(as.character(as.matrix(geno_data))) == "") < nrow(geno_data)
  geno_data <- geno_data[, non_empty_cols, drop = FALSE]
  if (nrow(geno_data) == 0 || ncol(geno_data) < 2) {
    stop("BLUPF90 txt file has no usable data rows/columns after cleaning.", call. = FALSE)
  }

  map_n <- nrow(map_data)
  total_cols <- ncol(geno_data)

  # Support "ID + concatenated genotype string" format: 2 columns, col2 has one digit per SNP
  if (total_cols == 2L) {
    geno_str <- as.character(geno_data[, 2L])
    nch <- nchar(geno_str)
    if (all(!is.na(nch)) && all(nch == map_n)) {
      split_mat <- do.call(rbind, lapply(strsplit(geno_str, "", fixed = TRUE), function(x) {
        if (length(x) != map_n) return(rep(NA_character_, map_n))
        x
      }))
      geno_data <- data.frame(geno_data[, 1L, drop = FALSE], split_mat, stringsAsFactors = FALSE)
      total_cols <- ncol(geno_data)
    }
  }

  id_col_candidates <- 1:min(6, total_cols - 1)
  matched_id_cols <- id_col_candidates[(total_cols - id_col_candidates) == map_n]

  # Some files include a header row in txt; if not matched yet, try dropping first row.
  dropped_header <- FALSE
  if (length(matched_id_cols) == 0 && nrow(geno_data) > 1) {
    first_txt_row <- tolower(paste(as.character(geno_data[1, ]), collapse = " "))
    if (grepl("id|sample|animal|snp|marker|chrom", first_txt_row)) {
      geno_data <- geno_data[-1, , drop = FALSE]
      dropped_header <- TRUE
      total_cols <- ncol(geno_data)
      matched_id_cols <- id_col_candidates[(total_cols - id_col_candidates) == map_n]
    }
  }

  id_cols <- if (length(matched_id_cols) > 0) {
    matched_id_cols[1]
  } else {
    # Fallback: assume extra leading metadata columns and align to map length.
    inferred <- total_cols - map_n
    if (inferred >= 1 && inferred <= 6) inferred else NA_integer_
  }

  if (is.na(id_cols) || (total_cols - id_cols) < map_n) {
    stop(
      sprintf(
        paste0(
          "BLUPF90 txt/map column mismatch after cleaning: txt_cols=%d, map_rows=%d, ",
          "header_removed=%s. Please ensure txt has ID columns + one genotype column per map row."
        ),
        total_cols, map_n, if (dropped_header) "yes" else "no"
      ),
      call. = FALSE
    )
  }

  geno_end_col <- id_cols + map_n
  actual_cols <- ncol(geno_data)
  if (actual_cols < geno_end_col) {
    stop(
      sprintf(
        paste0(
          "BLUPF90 txt has fewer columns than expected: txt has %d columns, need %d (ID cols=%d + map markers=%d). ",
          "Ensure txt has ID column(s) plus one genotype column per map row, in the same order as the map file."
        ),
        actual_cols, geno_end_col, id_cols, map_n
      ),
      call. = FALSE
    )
  }

  sample_id_col <- 1L
  sample_ids <- data.frame(
    Family_ID = as.character(geno_data[, sample_id_col]),
    Sample_ID = as.character(geno_data[, sample_id_col]),
    stringsAsFactors = FALSE
  )

  geno_matrix <- as.matrix(geno_data[, (id_cols + 1):geno_end_col, drop = FALSE])
  suppressWarnings(mode(geno_matrix) <- "numeric")
  
  # Treat common missing codes as NA
  geno_matrix[geno_matrix %in% c(-9, 9, 5)] <- NA

  if (ncol(geno_matrix) != nrow(map_data)) {
    stop(
      sprintf(
        paste0(
          "BLUPF90 txt genotype columns do not match map rows (txt_geno_cols=%d, map_rows=%d). ",
          "Txt must have ID column(s) plus exactly one column per marker in the map file, same order."
        ),
        ncol(geno_matrix), nrow(map_data)
      ),
      call. = FALSE
    )
  }
  
  list(samples = sample_ids, genotypes = geno_matrix, map = map_data)
}

# Find PLINK executable if plinkR is available
find_plink_path <- function() {
  plink_path <- tryCatch({
    if (use_plinkR && requireNamespace("plinkR", quietly = TRUE) &&
        exists("find_plink", where = "package:plinkR", mode = "function")) {
      plinkR::find_plink()
    } else {
      Sys.which("plink")
    }
  }, error = function(e) {
    Sys.which("plink")
  })
  
  if (is.null(plink_path) || length(plink_path) == 0 || plink_path == "" ||
      length(plink_path) > 1 || !file.exists(plink_path)) {
    return(NULL)
  }
  plink_path
}

# Write PLINK .ped/.map from in-memory data.
# Supports both allele-pair strings ("A T") and dosage-coded numeric genotypes (0/1/2).
write_plink_text_files <- function(prefix, data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(FALSE)
  }
  
  # Write .map file
  map_data <- data$map
  write.table(map_data, paste0(prefix, ".map"),
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  
  # Expand genotypes into allele pairs for .ped
  geno_matrix <- data$genotypes
  allele_cols <- matrix("0", nrow = nrow(geno_matrix), ncol = ncol(geno_matrix) * 2)
  col_has_dosage <- function(v) {
    vals <- suppressWarnings(as.numeric(v))
    valid <- !is.na(vals)
    if (!any(valid)) return(FALSE)
    all(vals[valid] %in% c(0, 1, 2))
  }

  for (i in seq_len(ncol(geno_matrix))) {
    col_i <- geno_matrix[, i]
    a_col <- rep("0", nrow(geno_matrix))
    b_col <- rep("0", nrow(geno_matrix))

    if (is.numeric(col_i) || col_has_dosage(col_i)) {
      # Numeric dosage coding (0/1/2): map to synthetic bi-allelic genotypes A/C.
      # 0 -> A A, 1 -> A C, 2 -> C C, NA/other -> 0 0.
      d <- suppressWarnings(as.numeric(col_i))
      idx0 <- !is.na(d) & d == 0
      idx1 <- !is.na(d) & d == 1
      idx2 <- !is.na(d) & d == 2
      a_col[idx0] <- "A"; b_col[idx0] <- "A"
      a_col[idx1] <- "A"; b_col[idx1] <- "C"
      a_col[idx2] <- "C"; b_col[idx2] <- "C"
    } else {
      # Character allele-pair coding, e.g. "A T".
      x <- as.character(col_i)
      valid <- !is.na(x) & x != "0 0" & x != "N N" & x != ""
      if (any(valid)) {
        aa <- substr(x[valid], 1, 1)
        bb <- substr(x[valid], 3, 3)
        good <- grepl("^[A-Za-z]$", aa) & grepl("^[A-Za-z]$", bb)
        if (any(good)) {
          a_col[which(valid)[good]] <- toupper(aa[good])
          b_col[which(valid)[good]] <- toupper(bb[good])
        }
      }
    }

    allele_cols[, (i - 1) * 2 + 1] <- a_col
    allele_cols[, (i - 1) * 2 + 2] <- b_col
  }
  
  ped_data <- cbind(
    data$samples$Family_ID,
    data$samples$Sample_ID,
    0, 0, 0, -9,
    allele_cols
  )
  write.table(ped_data, paste0(prefix, ".ped"),
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
  
  TRUE
}

# Helper functions for QC calculations
assert_rcpp_backend <- function() {
  if (!isTRUE(use_rcpp_stats)) {
    stop("Rcpp backend is required for local statistics when PLINK is unavailable.", call. = FALSE)
  }
}

get_numeric_genotype_matrix <- function(data) {
  if (!is.list(data) || !"genotypes" %in% names(data)) return(NULL)
  geno <- data$genotypes
  if (is.matrix(geno) && is.numeric(geno)) return(geno)
  encoded <- encode_genotypes_to_numeric(as.matrix(geno))
  if (is.null(encoded)) return(NULL)
  suppressWarnings(storage.mode(encoded) <- "double")
  encoded
}

calculate_maf <- function(data) {
  assert_rcpp_backend()
  geno_matrix <- get_numeric_genotype_matrix(data)
  if (is.null(geno_matrix)) return(NULL)
  gvr_maf(geno_matrix)
}

calculate_call_rate <- function(data) {
  assert_rcpp_backend()
  geno_matrix <- get_numeric_genotype_matrix(data)
  if (is.null(geno_matrix)) return(NULL)
  gvr_marker_call_rate(geno_matrix)
}

calculate_heterozygosity <- function(data) {
  assert_rcpp_backend()
  geno_matrix <- get_numeric_genotype_matrix(data)
  if (is.null(geno_matrix)) return(NULL)
  gvr_marker_het(geno_matrix)
}

# Convert genotype strings ("A T") to numeric minor-allele counts (0/1/2)
encode_genotypes_to_numeric <- function(geno_matrix) {
  if (is.null(geno_matrix) || !is.matrix(geno_matrix) || ncol(geno_matrix) == 0) {
    return(NULL)
  }
  
  num_matrix <- matrix(NA_real_, nrow = nrow(geno_matrix), ncol = ncol(geno_matrix))
  colnames(num_matrix) <- colnames(geno_matrix)
  
  for (i in seq_len(ncol(geno_matrix))) {
    x <- as.character(geno_matrix[, i])
    valid <- !is.na(x) & x != "0 0" & x != "N N" & x != ""
    if (sum(valid) == 0) next
    
    a1 <- toupper(substr(x[valid], 1, 1))
    a2 <- toupper(substr(x[valid], 3, 3))
    good <- grepl("^[A-Z]$", a1) & grepl("^[A-Z]$", a2)
    if (!any(good)) next
    idx <- which(valid)[good]
    a1 <- a1[good]
    a2 <- a2[good]
    alleles <- c(a1, a2)
    allele_counts <- table(alleles)
    if (length(allele_counts) == 1) {
      # Monomorphic marker: fully observed, MAF=0.
      num_matrix[idx, i] <- 0
      next
    }
    minor_allele <- names(allele_counts)[which.min(allele_counts)]
    num_matrix[idx, i] <- (a1 == minor_allele) + (a2 == minor_allele)
  }
  
  num_matrix
}

# Calculate heterozygosity per individual (row-wise)
calculate_individual_heterozygosity <- function(data) {
  # If data already has heterozygosity computed, return it
  if (is.list(data) && "individual_heterozygosity" %in% names(data)) {
    return(data$individual_heterozygosity)
  }
  
  assert_rcpp_backend()
  geno_matrix <- get_numeric_genotype_matrix(data)
  if (is.null(geno_matrix)) return(NULL)
  gvr_individual_het(geno_matrix)
}

# Calculate call rate per individual (row-wise)
calculate_individual_call_rate <- function(data) {
  # If data already has call rate computed, return it
  if (is.list(data) && "individual_call_rate" %in% names(data)) {
    return(data$individual_call_rate)
  }
  
  assert_rcpp_backend()
  geno_matrix <- get_numeric_genotype_matrix(data)
  if (is.null(geno_matrix)) return(NULL)
  gvr_individual_call_rate(geno_matrix)
}

# Calculate all statistics using plink commands (via plinkR)
# This function runs plink --missing, --freq, --hardy, --het, and --pca.
calculate_all_stats_plink <- function(data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(NULL)
  }
  if (!is.null(data$input_format) && !data$input_format %in% c("plink_ped", "plink_bed", "blupf90_txt")) {
    return(calculate_all_stats_r(data))
  }
  
  plink_path <- find_plink_path()
  if (is.null(plink_path)) {
    return(calculate_all_stats_r(data))
  }
  
  tryCatch({
    # Create temporary directory for PLINK files
    tmp_dir <- tempfile(pattern = "genovieweR_stats_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- file.path(tmp_dir, "temp_data")
    
    if (!write_plink_text_files(prefix, data)) {
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
      return(calculate_all_stats_r(data))
    }
    
    stats_output <- paste0(prefix, "_stats")
    
    # Run plink --missing --freq --hardy --het --pca together (combined for efficiency)
    # This combines five commands into one to reduce file I/O and startup overhead
    # Add timeout to prevent hanging
    plink_start_time <- Sys.time()
    plink_success <- FALSE
    if (requireNamespace("R.utils", quietly = TRUE)) {
      tryCatch({
        R.utils::withTimeout({
          result <- system2(
            plink_path,
            args = c(
              "--file", prefix,
              "--allow-extra-chr",
              "--nonfounders",
              "--missing",
              "--freq",
              "--hardy",
              "--het",
              "--pca",
              "--out", stats_output
            ),
            stdout = FALSE,
            stderr = FALSE
          )
          plink_success <- (result == 0)
        }, timeout = 300)  # 5 minute timeout for PLINK commands
      }, TimeoutException = function(e) {
        cat("PLINK command timed out after 5 minutes\n")
        plink_success <- FALSE
      })
    } else {
      # Fallback without timeout
      result <- system2(
        plink_path,
        args = c(
          "--file", prefix,
          "--allow-extra-chr",
          "--nonfounders",
          "--missing",
          "--freq",
          "--hardy",
          "--het",
          "--pca",
          "--out", stats_output
        ),
        stdout = FALSE,
        stderr = FALSE
      )
      plink_success <- (result == 0)
      elapsed <- as.numeric(difftime(Sys.time(), plink_start_time, units = "secs"))
      if (elapsed > 300) {
        cat("PLINK command took longer than 5 minutes\n")
        plink_success <- FALSE
      }
    }
    
    # Check if PLINK output files were created (PCA files are optional)
    if (!plink_success || 
        !file.exists(paste0(stats_output, ".imiss")) ||
        !file.exists(paste0(stats_output, ".lmiss")) ||
        !file.exists(paste0(stats_output, ".frq")) ||
        !file.exists(paste0(stats_output, ".hwe")) ||
        !file.exists(paste0(stats_output, ".het"))) {
      cat("PLINK command failed or output files missing\n")
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
      return(NULL)
    }
    
    # Read PCA results if available (--pca generates .eigenvec and .eigenval files)
    pca_scores <- NULL
    pca_variance <- NULL
    pca_eigenvalues <- NULL
    sample_ids_eigenvec <- NULL
    eigenvec_file <- paste0(stats_output, ".eigenvec")
    eigenval_file <- paste0(stats_output, ".eigenval")
    
    if (file.exists(eigenvec_file) && file.exists(eigenval_file)) {
      tryCatch({
        # Read .eigenvec file: FID IID PC1 PC2 PC3 ...
        eigenvec_data <- read.table(eigenvec_file, header = FALSE, stringsAsFactors = FALSE, sep = "")
        if (nrow(eigenvec_data) > 0 && ncol(eigenvec_data) >= 4) {
          # Extract sample IDs and PC scores
          sample_ids_eigenvec <- eigenvec_data[, 2]  # IID column
          pca_scores <- as.matrix(eigenvec_data[, 3:ncol(eigenvec_data), drop = FALSE])
          colnames(pca_scores) <- paste0("PC", 1:ncol(pca_scores))
          
          # Read .eigenval file: one eigenvalue per line
          eigenval_data <- read.table(eigenval_file, header = FALSE, stringsAsFactors = FALSE, sep = "")
          if (nrow(eigenval_data) > 0) {
            pca_eigenvalues <- eigenval_data[, 1]
            # Calculate variance explained (percentage)
            total_var <- sum(pca_eigenvalues)
            pca_variance <- (pca_eigenvalues / total_var) * 100
            names(pca_variance) <- paste0("PC", 1:length(pca_variance))
          }
        }
      }, error = function(e) {
        cat("Error reading PCA files:", e$message, "\n")
        pca_scores <- NULL
        pca_variance <- NULL
        pca_eigenvalues <- NULL
        sample_ids_eigenvec <- NULL
      })
    }
    
    # Note: --hardy output will be in stats_output.hwe (not stats_output_hwe.hwe)
    # We'll adjust the file reading accordingly

    # Run PLINK IBD/relatedness (--genome)
    genome_output <- paste0(stats_output, "_genome")
    if (requireNamespace("R.utils", quietly = TRUE)) {
      tryCatch({
        R.utils::withTimeout({
          system2(
            plink_path,
            args = c(
              "--file", prefix,
              "--allow-extra-chr",
              "--nonfounders",
              "--genome",
              "--out", genome_output
            ),
            stdout = FALSE,
            stderr = FALSE
          )
        }, timeout = 180)
      }, error = function(e) {
        cat("Warning: PLINK --genome did not complete:", e$message, "\n")
      })
    } else {
      tryCatch({
        system2(
          plink_path,
          args = c(
            "--file", prefix,
            "--allow-extra-chr",
            "--nonfounders",
            "--genome",
            "--out", genome_output
          ),
          stdout = FALSE,
          stderr = FALSE
        )
      }, error = function(e) {
        cat("Warning: PLINK --genome failed:", e$message, "\n")
      })
    }
    
    # Read results and extract statistics
    results <- list()
    
    # Read .imiss (individual missing rates) - using base R match()
    imiss_file <- paste0(stats_output, ".imiss")
    if (file.exists(imiss_file)) {
      imiss_data <- read.table(imiss_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
      if (nrow(imiss_data) > 0 && "F_MISS" %in% names(imiss_data)) {
        individual_missing_rate <- rep(NA, nrow(data$samples))
        # Use base R match()
        idx_map <- match(imiss_data$IID, data$samples$Sample_ID)
        valid_idx <- !is.na(idx_map)
        individual_missing_rate[idx_map[valid_idx]] <- imiss_data$F_MISS[valid_idx]
        results$individual_missing_rate <- individual_missing_rate
        results$individual_call_rate <- 1 - individual_missing_rate
      }
    }
    
    # Read .het (individual heterozygosity) - from PLINK --het
    het_file <- paste0(stats_output, ".het")
    if (file.exists(het_file)) {
      # PLINK .het file format: FID IID O(HOM) E(HOM) N(NM) F
      # Column names may have spaces, use check.names = FALSE or handle manually
      het_data <- read.table(het_file, header = TRUE, stringsAsFactors = FALSE, sep = "", check.names = FALSE)
      if (nrow(het_data) > 0) {
        # Check for column names (may be "O(HOM)" or "O(HOM)" depending on read.table settings)
        hom_col <- NULL
        nm_col <- NULL
        if ("O(HOM)" %in% names(het_data)) {
          hom_col <- "O(HOM)"
        } else if ("O\\(HOM\\)" %in% names(het_data)) {
          hom_col <- "O\\(HOM\\)"
        } else if (ncol(het_data) >= 3) {
          hom_col <- names(het_data)[3]  # Usually column 3
        }
        
        if ("N(NM)" %in% names(het_data)) {
          nm_col <- "N(NM)"
        } else if ("N\\(NM\\)" %in% names(het_data)) {
          nm_col <- "N\\(NM\\)"
        } else if (ncol(het_data) >= 5) {
          nm_col <- names(het_data)[5]  # Usually column 5
        }
        
        if (!is.null(hom_col) && !is.null(nm_col)) {
          # Calculate heterozygosity rate: het = 1 - (O(HOM) / N(NM))
          o_hom <- as.numeric(het_data[[hom_col]])
          n_nm <- as.numeric(het_data[[nm_col]])
          valid_idx <- !is.na(o_hom) & !is.na(n_nm) & n_nm > 0
          het_rate <- rep(NA, length(valid_idx))
          het_rate[valid_idx] <- 1 - (o_hom[valid_idx] / n_nm[valid_idx])
          
          individual_het <- rep(NA, nrow(data$samples))
          # Use base R match()
          idx_map <- match(het_data$IID, data$samples$Sample_ID)
          valid_match <- !is.na(idx_map) & valid_idx
          individual_het[idx_map[valid_match]] <- het_rate[valid_match]
          results$individual_heterozygosity <- individual_het
        }
      }
    }
    
    # Read .lmiss (marker missing rates) - using base R match()
    lmiss_file <- paste0(stats_output, ".lmiss")
    if (file.exists(lmiss_file)) {
      lmiss_data <- read.table(lmiss_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
      if (nrow(lmiss_data) > 0 && "F_MISS" %in% names(lmiss_data)) {
        marker_missing_rate <- rep(NA, nrow(data$map))
        # Use base R match()
        idx_map <- match(lmiss_data$SNP, data$map[, 2])
        valid_idx <- !is.na(idx_map)
        marker_missing_rate[idx_map[valid_idx]] <- lmiss_data$F_MISS[valid_idx]
        results$marker_missing_rate <- marker_missing_rate
        results$marker_call_rate <- 1 - marker_missing_rate
      }
    }
    
    # Read .frq (MAF) - using base R match()
    frq_file <- paste0(stats_output, ".frq")
    if (file.exists(frq_file)) {
      frq_data <- read.table(frq_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
      if (nrow(frq_data) > 0 && "MAF" %in% names(frq_data)) {
        maf <- rep(NA, nrow(data$map))
        # Use base R match()
        idx_map <- match(frq_data$SNP, data$map[, 2])
        valid_idx <- !is.na(idx_map)
        maf[idx_map[valid_idx]] <- frq_data$MAF[valid_idx]
        results$maf <- maf
      }
    }
    
    # Read .hwe (HWE p-values) - using base R match()
    # Reference: hwe = read.table(file="plink.hwe", header=TRUE)
    # Column 9 is the P value: CHR SNP TEST A1 A2 GENO O(HET) E(HET) P
    hwe_file <- paste0(stats_output, ".hwe")
    if (file.exists(hwe_file)) {
      # Try reading with different separators
      hwe_data <- NULL
      # Try space-separated first (most common for PLINK)
      tryCatch({
        hwe_data <- read.table(hwe_file, header = TRUE, stringsAsFactors = FALSE, sep = "", fill = TRUE, comment.char = "")
        if (nrow(hwe_data) == 0) {
          # Try tab-separated
          hwe_data <- read.table(hwe_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE, comment.char = "")
        }
      }, error = function(e) {
        # Try tab-separated
        tryCatch({
          hwe_data <<- read.table(hwe_file, header = TRUE, stringsAsFactors = FALSE, sep = "\t", fill = TRUE, comment.char = "")
        }, error = function(e2) {
          # Try default (whitespace)
          tryCatch({
            hwe_data <<- read.table(hwe_file, header = TRUE, stringsAsFactors = FALSE, fill = TRUE, comment.char = "")
          }, error = function(e3) {
            cat("Error reading HWE file:", e3$message, "\n")
          })
        })
      })
      
      if (!is.null(hwe_data) && nrow(hwe_data) > 0) {
        # Filter for ALL test results (PLINK outputs multiple test types)
        # Check if TEST column exists
        if ("TEST" %in% names(hwe_data)) {
          hwe_all <- hwe_data[hwe_data$TEST == "ALL", , drop = FALSE]
          # If no ALL rows, try using all rows
          if (nrow(hwe_all) == 0) {
            hwe_all <- hwe_data
          }
        } else {
          # If no TEST column, use all rows
          hwe_all <- hwe_data
        }
        
        if (nrow(hwe_all) > 0 && ncol(hwe_all) >= 9) {
          # Directly read column 9 (P value) - ensure it's numeric
          p_col <- suppressWarnings(as.numeric(hwe_all[, 9]))
          # Read column 2 (SNP ID)
          snp_col <- as.character(hwe_all[, 2])
          
          # Filter valid p-values before matching
          valid_p_idx <- !is.na(p_col) & is.finite(p_col) & p_col > 0 & p_col <= 1
          if (sum(valid_p_idx) > 0) {
            hwe_pvalues <- rep(NA, nrow(data$map))
            # Use base R match()
            idx_map <- match(snp_col[valid_p_idx], data$map[, 2])
            valid_match <- !is.na(idx_map)
            hwe_pvalues[idx_map[valid_match]] <- p_col[valid_p_idx][valid_match]
            results$hwe_pvalues <- hwe_pvalues
          } else {
            cat("Warning: No valid HWE p-values found in file\n")
            results$hwe_pvalues <- rep(NA, nrow(data$map))
          }
        } else {
          cat("Warning: HWE file has insufficient columns or rows\n")
          cat("  Rows:", nrow(hwe_all), "Columns:", ncol(hwe_all), "\n")
          if (ncol(hwe_all) > 0) {
            cat("  Column names:", paste(names(hwe_all)[1:min(10, ncol(hwe_all))], collapse = ", "), "\n")
          }
          results$hwe_pvalues <- rep(NA, nrow(data$map))
        }
      } else {
        cat("Warning: Could not read HWE file or file is empty\n")
        results$hwe_pvalues <- rep(NA, nrow(data$map))
      }
    }

    # Read .genome (sample relatedness / IBD estimates)
    genome_file <- paste0(genome_output, ".genome")
    if (file.exists(genome_file)) {
      tryCatch({
        genome_data <- read.table(genome_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
        if (is.data.frame(genome_data) && nrow(genome_data) > 0 && "PI_HAT" %in% names(genome_data)) {
          results$individual_relatedness <- genome_data
        }
      }, error = function(e) {
        cat("Warning: failed to read .genome file:", e$message, "\n")
      })
    }
    
    # Clean up temporary files
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
    
    # Ensure all required statistics are present (even if NA)
    # This prevents NULL returns that would trigger R fallback calculations
    if (is.null(results$individual_call_rate)) {
      results$individual_call_rate <- rep(NA, nrow(data$samples))
    }
    if (is.null(results$individual_missing_rate)) {
      results$individual_missing_rate <- rep(NA, nrow(data$samples))
    }
    if (is.null(results$individual_heterozygosity)) {
      results$individual_heterozygosity <- rep(NA, nrow(data$samples))
    }
    if (is.null(results$maf)) {
      results$maf <- rep(NA, nrow(data$map))
    }
    if (is.null(results$marker_call_rate)) {
      results$marker_call_rate <- rep(NA, nrow(data$map))
    }
    if (is.null(results$marker_missing_rate)) {
      results$marker_missing_rate <- rep(NA, nrow(data$map))
    }
    if (is.null(results$hwe_pvalues)) {
      results$hwe_pvalues <- rep(NA, nrow(data$map))
    }
    # Add PCA results if available
    if (!is.null(pca_scores) && !is.null(pca_variance)) {
      # Match sample IDs from eigenvec to data$samples
      sample_idx <- match(sample_ids_eigenvec, data$samples$Sample_ID)
      valid_idx <- !is.na(sample_idx)
      
      if (sum(valid_idx) > 0) {
        # Create full PCA scores matrix aligned with original data
        full_pca_scores <- matrix(NA, nrow = nrow(data$samples), ncol = ncol(pca_scores))
        full_pca_scores[sample_idx[valid_idx], ] <- pca_scores[valid_idx, , drop = FALSE]
        
        results$pca_scores <- full_pca_scores
        results$pca_variance <- pca_variance
        results$pca_eigenvalues <- pca_eigenvalues
        results$pca_sample_ids <- data$samples$Sample_ID
      }
    }
    
    return(results)
    
  }, error = function(e) {
    # On error, log and return NULL (will trigger error message in UI)
    cat("Error in calculate_all_stats_plink:", e$message, "\n")
    return(calculate_all_stats_r(data))
  })
}

# Calculate all statistics using local fallback (Rcpp-backed for numeric genotypes)
# Used when PLINK is unavailable.
calculate_all_stats_r <- function(data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(NULL)
  }
  tryCatch({
    results <- list()
    results$individual_call_rate <- calculate_individual_call_rate(data)
    if (!is.null(results$individual_call_rate)) {
      results$individual_missing_rate <- 1 - results$individual_call_rate
    }
    results$individual_heterozygosity <- calculate_individual_heterozygosity(data)
    
    results$marker_call_rate <- calculate_call_rate(data)
    if (!is.null(results$marker_call_rate)) {
      results$marker_missing_rate <- 1 - results$marker_call_rate
    }
    
    results$maf <- calculate_maf(data)
    results$hwe_pvalues <- calculate_hwe(data)
    results$individual_relatedness <- calculate_sample_relatedness(data)
    
    results
  }, error = function(e) {
    cat("Error in calculate_all_stats_r:", e$message, "\n")
    NULL
  })
}

# Calculate sample relatedness.
# Prefer PLINK --genome when available; otherwise use Rcpp PLINK-style moment estimator.
calculate_sample_relatedness <- function(data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(data.frame(IID1 = character(0), IID2 = character(0), PI_HAT = numeric(0), stringsAsFactors = FALSE))
  }
  if (nrow(data$samples) < 2) {
    return(data.frame(IID1 = character(0), IID2 = character(0), PI_HAT = numeric(0), stringsAsFactors = FALSE))
  }

  plink_path <- find_plink_path()
  if (!is.null(plink_path)) {
    out <- tryCatch({
      tmp_dir <- tempfile(pattern = "genovieweR_rel_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
      prefix <- file.path(tmp_dir, "temp_data")
      if (!write_plink_text_files(prefix, data)) return(NULL)
      out_prefix <- paste0(prefix, "_genome")
      system2(
        plink_path,
        args = c("--file", prefix, "--allow-extra-chr", "--nonfounders", "--genome", "--out", out_prefix),
        stdout = FALSE, stderr = FALSE
      )
      genome_file <- paste0(out_prefix, ".genome")
      if (!file.exists(genome_file)) return(NULL)
      rel <- read.table(genome_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
      if (!is.data.frame(rel) || nrow(rel) == 0 || !"PI_HAT" %in% names(rel)) return(NULL)
      rel
    }, error = function(e) NULL)
    if (!is.null(out)) return(out)
  }

  assert_rcpp_backend()
  geno_num <- get_numeric_genotype_matrix(data)
  if (is.null(geno_num) || nrow(geno_num) < 2 || ncol(geno_num) < 2) {
    return(data.frame(IID1 = character(0), IID2 = character(0), PI_HAT = numeric(0), stringsAsFactors = FALSE))
  }
  ids <- as.character(data$samples$Sample_ID)
  if (length(ids) != nrow(geno_num)) ids <- as.character(seq_len(nrow(geno_num)))
  # Match PLINK behavior more closely: keep essentially all comparable pairs.
  # Do not enforce aggressive minimum-overlap filtering.
  min_valid <- 1L
  n_pairs <- nrow(geno_num) * (nrow(geno_num) - 1L) / 2L
  if (!is.finite(n_pairs) || n_pairs > .Machine$integer.max) {
    n_pairs <- .Machine$integer.max
  }
  rel <- gvr_relatedness_pairs(
    geno_num, ids,
    max_pairs = as.integer(n_pairs),
    max_markers = as.integer(ncol(geno_num)),
    min_valid = as.integer(min_valid)
  )
  if (!is.data.frame(rel)) {
    return(data.frame(IID1 = character(0), IID2 = character(0), PI_HAT = numeric(0), stringsAsFactors = FALSE))
  }
  # Keep all rows for PLINK-like pair coverage; downstream plots can ignore NA PI_HAT.
  rel
}

# Calculate Hardy-Weinberg equilibrium p-values using plink --hardy (via plinkR)
calculate_hwe_plink <- function(data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(NULL)
  }
  if (!is.null(data$input_format) && !data$input_format %in% c("plink_ped", "plink_bed", "blupf90_txt")) {
    return(calculate_hwe(data))
  }
  
  plink_path <- find_plink_path()
  if (is.null(plink_path)) {
    return(calculate_hwe(data))
  }
  
  tryCatch({
    # Create temporary directory for PLINK files
    tmp_dir <- tempfile(pattern = "genovieweR_hwe_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- file.path(tmp_dir, "temp_data")
    
    if (!write_plink_text_files(prefix, data)) {
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
      return(calculate_hwe(data))
    }
    
    # Run plink --hardy
    hwe_output <- paste0(prefix, "_hwe")
    system2(
      plink_path,
      args = c(
        "--file", prefix,
        "--allow-extra-chr",
        "--nonfounders",
        "--hardy",
        "--out", hwe_output
      ),
      stdout = FALSE,
      stderr = FALSE
    )
    
    # Read .hwe file
    # Reference: hwe = read.table(file="plink.hwe", header=TRUE)
    # Column 9 is the P value: CHR SNP TEST A1 A2 GENO O(HET) E(HET) P
    hwe_file <- paste0(hwe_output, ".hwe")
    if (file.exists(hwe_file)) {
      hwe_data <- read.table(hwe_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
      # Filter for "ALL" test results
      hwe_all <- hwe_data[hwe_data$TEST == "ALL", , drop = FALSE]
      
      if (nrow(hwe_all) > 0 && ncol(hwe_all) >= 9) {
        # Directly read column 9 (P value) - format: CHR SNP TEST A1 A2 GENO O(HET) E(HET) P
        p_col <- hwe_all[, 9]  # Column 9 is always the P value
        
        # Extract p-values in the same order as map
        hwe_pvalues <- rep(NA, nrow(data$map))
        names(hwe_pvalues) <- data$map[, 2]  # Use SNP IDs from map
        
        # Get SNP column (column 2)
        snp_col <- if (ncol(hwe_all) >= 2) {
          hwe_all[, 2]  # Column 2 is SNP ID
        } else {
          NULL
        }
        
        if (!is.null(snp_col) && length(snp_col) == length(p_col)) {
          # Match by SNP ID using base R match()
          p_col_num <- suppressWarnings(as.numeric(p_col))
          hwe_pvalues <- rep(NA, nrow(data$map))
          idx_map <- match(snp_col, data$map[, 2])
          valid_idx <- !is.na(idx_map) & !is.na(p_col_num) & is.finite(p_col_num) & p_col_num > 0 & p_col_num <= 1
          hwe_pvalues[idx_map[valid_idx]] <- p_col_num[valid_idx]
          
          # Clean up temporary files
          unlink(tmp_dir, recursive = TRUE, force = TRUE)
          
          return(hwe_pvalues)
        }
      }
    }
    
    # If plink failed, fallback to R calculation
    unlink(tmp_dir, recursive = TRUE, force = TRUE)
    return(calculate_hwe(data))
    
  }, error = function(e) {
    # On error, fallback to R calculation
    return(calculate_hwe(data))
  })
}

# Local fallback: Calculate Hardy-Weinberg equilibrium p-values using Rcpp exact test.
calculate_hwe <- function(data) {
  assert_rcpp_backend()
  geno_matrix <- get_numeric_genotype_matrix(data)
  if (is.null(geno_matrix)) return(NULL)
  gvr_hwe_exact(geno_matrix)
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)
