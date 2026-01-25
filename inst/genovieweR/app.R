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

# Local wrapper to prefix translation keys for this app
get_label_local <- function(key, lang = NULL) {
  prefixed <- if (startsWith(key, "genovieweR_")) key else paste0("genovieweR_", key)
  if (exists("get_label", mode = "function")) {
    get_label(prefixed, lang)
  } else {
    prefixed
  }
}

# Check if plinkR is available for genotype format conversion
use_plinkR <- FALSE
tryCatch({
  if (requireNamespace("plinkR", quietly = TRUE)) {
    use_plinkR <- TRUE
    cat("✓ plinkR available - will use for genotype format conversion\n")
  }
}, error = function(e) {
  cat("Note: plinkR not available, format conversion features will be limited\n")
  use_plinkR <- FALSE
})

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

# Rcpp removed - using base R functions only

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
  
  # Title Bar
  div(class = "title-bar",
      h1("genovieweR"),
      p("Genotype Viewer and Quality Control")
  ),
  
  # Floating toggle handles
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
      
      # LEFT PANEL - Controls
      div(id = "leftPanel", class = "left-panel",
          
          # File Upload Section
          div(class = "panel-section",
              h4("Data Upload", class = "section-title"),
              selectInput("geno_format", "Genotype Format",
                         choices = list("PLINK" = "plink", "BLUPF90" = "blupf90"),
                         selected = "plink"),
              numericInput("max_chromosome", "Chromosome", value = 18, min = 1, max = 100, step = 1),
              p(style = "font-size: 0.8rem; color: #888; margin-top: -10px; margin-bottom: 10px;",
                "Maximum chromosome number to include (1 to selected number). Only chromosomes 1 to selected number will be included in analysis"),
              br(),
              
              # PLINK Format File Uploads (2 files)
              conditionalPanel(
                condition = "input.geno_format == 'plink'",
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
              
              # BLUPF90 Format File Uploads (3 files)
              conditionalPanel(
                condition = "input.geno_format == 'blupf90'",
                div(
                  div(style = "margin-bottom: 10px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; border-left: 3px solid #CEB888;",
                      tags$strong("📋 BLUPF90 Format: Upload 3 files"),
                      tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px; font-size: 0.9rem;",
                              tags$li(tags$code(".txt"), " file - genotype data"),
                              tags$li(tags$code(".map"), " file - marker map information"),
                              tags$li(tags$code(".bim"), " file - marker information")
                      ),
                      div(style = "margin-top: 10px; padding: 8px; background-color: #e7f3ff; border-radius: 4px; border-left: 3px solid #2196F3;",
                          tags$strong("🔄 Auto-Conversion:"), 
                          " BLUPF90 files will be automatically converted to PLINK format for analysis.",
                          br(),
                          tags$em("Note: Requires plinkR package to be installed.")
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
                           placeholder = "No .map file selected"),
                  fileInput("blupf90_bim_file", 
                           label = tags$span("📄 Upload .bim File", 
                                            style = "font-weight: 600;"),
                           accept = c(".bim"),
                           buttonLabel = "Browse...",
                           placeholder = "No .bim file selected")
                )
              ),
              
              br(),
              div(style = "margin-top: 15px;",
                  actionButton("show_summary", "📊 Show Summary & Plots", 
                             class = "btn btn-primary", 
                             style = "width: 100%; font-weight: 600;"),
                  helpText(style = "margin-top: 8px; font-size: 0.85rem; color: #666; text-align: center;",
                          "Generate basic visualizations from loaded data")
              ),
              br(),
              conditionalPanel(
                condition = if (use_plinkR) "true" else "false",
                div(style = "margin-top: 10px;",
                    actionLink("format_convert_help", "🔄 Format Conversion Help ?",
                             style = "font-size: 0.9rem; color: #CEB888; text-decoration: none;")
                )
              )
          ),
          
          # QC Options Section (for quality control only)
          div(class = "panel-section",
              h4("Quality Control", class = "section-title"),
              p(style = "font-size: 0.9rem; color: #666; margin-bottom: 15px;",
                "Set PLINK quality control thresholds and filter the data."),
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
              actionButton("run_qc", "🔍 Run Quality Control", class = "btn btn-primary", style = "width: 100%; font-weight: 600;")
          )
      ),
      
      # CENTER PANEL - Main Content
      div(class = "center-panel",
          navset_card_tab(
            id = "mainTabs",
            nav_panel(
              "Data Preview",
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
                  p("Visualization of individual-level metrics: Sample missing rate and Sample relatedness."),
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
                  downloadButton("download_pca_plots", "Download Plots", 
                               class = "btn btn-primary")
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
  
  # Get current language (for future multilingual support)
  # current_lang <- reactive({
  #   tryCatch({
  #     if (exists("resolve_suite_lang", mode = "function")) {
  #       resolved <- resolve_suite_lang(session, default = "en")
  #       map_suite_lang_for_app(resolved, app = "genovieweR")
  #     } else {
  #       "en"
  #     }
  #   }, error = function(e) "en")
  # })
  
  # Clear data and reset file inputs when format changes
  observeEvent(input$geno_format, {
    geno_data(NULL)
    qc_results(NULL)
    
    # Reset file inputs
    if (input$geno_format == "plink") {
      # Clear BLUPF90 inputs
      shinyjs::reset("blupf90_txt_file")
      shinyjs::reset("blupf90_map_file")
      shinyjs::reset("blupf90_bim_file")
    } else if (input$geno_format == "blupf90") {
      # Clear PLINK inputs
      shinyjs::reset("plink_ped_file")
      shinyjs::reset("plink_map_file")
    }
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
      
      # Extract genotype data (columns 7 onwards)
      geno_matrix <- as.matrix(ped_data[, 7:ncol(ped_data)])
    } else {
      # Fallback to base R
      map_data <- read.table(map_path, header = FALSE, stringsAsFactors = FALSE)
      colnames(map_data) <- c("Chromosome", "SNP_ID", "Genetic_Distance", "Physical_Position")
      
      ped_data <- read.table(ped_path, header = FALSE, stringsAsFactors = FALSE)
      sample_ids <- ped_data[, 1:2]
      colnames(sample_ids) <- c("Family_ID", "Sample_ID")
      geno_matrix <- as.matrix(ped_data[, 7:ncol(ped_data)])
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
    
    # All data is now in PLINK format (BLUPF90 is converted)
    if (is.list(data) && "samples" %in% names(data) && "map" %in% names(data)) {
      original_format <- if (input$geno_format == "blupf90") "BLUPF90 (converted to PLINK)" else "PLINK"
      paste0("Samples: ", nrow(data$samples), "\n",
             "SNPs: ", nrow(data$map), "\n",
             "Format: ", original_format, "\n",
             "Analysis Format: PLINK")
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
    if (!use_plinkR || !requireNamespace("plinkR", quietly = TRUE)) {
      showNotification(
        HTML(paste0(
          "<strong>❌ PLINK Not Available</strong><br>",
          "plinkR package is required for data loading and analysis.<br>",
          "Please install plinkR: install.packages('plinkR')"
        )),
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    withProgress(message = "Loading and processing data...", value = 0, {
      tryCatch({
        # Step 1: Load data based on format
        setProgress(value = 0.1, message = "Loading data files...")
        
        if (input$geno_format == "plink") {
          # Check if files are uploaded
          if (is.null(input$plink_ped_file) || is.null(input$plink_map_file)) {
            showNotification("Please upload both .ped and .map files", type = "error")
            return(NULL)
          }
          
          ped_file <- input$plink_ped_file
          map_file <- input$plink_map_file
          
          # Validate file extensions
          ped_valid <- grepl("\\.ped$", ped_file$name, ignore.case = TRUE)
          map_valid <- grepl("\\.map$", map_file$name, ignore.case = TRUE)
          if (length(ped_valid) == 0 || length(map_valid) == 0 || !isTRUE(ped_valid) || !isTRUE(map_valid)) {
            showNotification("Please upload valid .ped and .map files", type = "error")
            return(NULL)
          }
          
          # Create temporary directory for initial PLINK files
          tmp_dir <- tempfile(pattern = "genovieweR_load_")
          dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
          initial_prefix <- file.path(tmp_dir, "initial_data")
          
          file.copy(ped_file$datapath, paste0(initial_prefix, ".ped"), overwrite = TRUE)
          file.copy(map_file$datapath, paste0(initial_prefix, ".map"), overwrite = TRUE)
          
        } else if (input$geno_format == "blupf90") {
          # Check if files are uploaded
          if (is.null(input$blupf90_txt_file) || 
              is.null(input$blupf90_map_file) || 
              is.null(input$blupf90_bim_file)) {
            showNotification("Please upload all BLUPF90 files (.txt, .map, .bim)", type = "error")
            return(NULL)
          }
          
          setProgress(value = 0.2, message = "Converting BLUPF90 to PLINK...")
          
          txt_file <- input$blupf90_txt_file
          map_file <- input$blupf90_map_file
          bim_file <- input$blupf90_bim_file
          
          # Create temporary directory for BLUPF90 files
          tmp_dir <- tempfile(pattern = "genovieweR_blupf90_")
          dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
          
          base_prefix <- tools::file_path_sans_ext(basename(txt_file$name))
          blupf90_prefix <- file.path(tmp_dir, base_prefix)
          
          file.copy(txt_file$datapath, paste0(blupf90_prefix, ".txt"), overwrite = TRUE)
          file.copy(map_file$datapath, paste0(blupf90_prefix, ".map"), overwrite = TRUE)
          file.copy(bim_file$datapath, paste0(blupf90_prefix, ".bim"), overwrite = TRUE)
          
          # Convert BLUPF90 to PLINK
          plink_output_dir <- tempfile(pattern = "genovieweR_plink_converted_")
          dir.create(plink_output_dir, recursive = TRUE, showWarnings = FALSE)
          initial_prefix <- file.path(plink_output_dir, base_prefix)
          
          conversion_result <- tryCatch({
            plinkR::blupf90_to_plink(
              blupf90_prefix = blupf90_prefix,
              geno_file = paste0(blupf90_prefix, ".txt"),
              map_file = paste0(blupf90_prefix, ".map"),
              bim_file = paste0(blupf90_prefix, ".bim"),
              out_prefix = initial_prefix,
              verbose = TRUE
            )
          }, error = function(e) {
            showNotification(paste("BLUPF90 conversion failed:", e$message), type = "error")
            return(NULL)
          })
          
          if (is.null(conversion_result) || 
              !file.exists(paste0(initial_prefix, ".ped")) ||
              !file.exists(paste0(initial_prefix, ".map"))) {
            showNotification("BLUPF90 conversion failed", type = "error")
            return(NULL)
          }
        } else {
          showNotification("Unknown genotype format", type = "error")
          return(NULL)
        }
        
        # Step 2: Filter by chromosome using PLINK --chr
        setProgress(value = 0.4, message = "Filtering chromosomes...")
        
        # Get max chromosome number
        max_chr <- if (!is.null(input$max_chromosome) && length(input$max_chromosome) == 1 && input$max_chromosome >= 1) {
          as.integer(input$max_chromosome[1])
        } else {
          18  # Default
        }
        
        # Find plink executable
        plink_path <- tryCatch({
          if (exists("find_plink", where = "package:plinkR", mode = "function")) {
            plinkR::find_plink()
          } else {
            Sys.which("plink")
          }
        }, error = function(e) {
          Sys.which("plink")
        })
        
        if (is.null(plink_path) || length(plink_path) == 0 || plink_path == "" || length(plink_path) > 1 || !file.exists(plink_path)) {
          showNotification("PLINK executable not found", type = "error")
          return(NULL)
        }
        
        # Create chromosome-filtered output
        chr_filtered_prefix <- file.path(tmp_dir, "chr_filtered")
        
        # Run PLINK to filter chromosomes: --chr 1-max_chr with --allow-extra-chr
        chr_result <- system2(
          plink_path,
          args = c(
            "--file", initial_prefix,
            "--allow-extra-chr",
            "--chr", paste0("1-", max_chr),
            "--make-bed",
            "--out", chr_filtered_prefix
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        
        if (chr_result != 0 || !file.exists(paste0(chr_filtered_prefix, ".bed"))) {
          showNotification("Chromosome filtering failed", type = "error")
          return(NULL)
        }
        
        # Convert .bed back to .ped for reading
        setProgress(value = 0.6, message = "Converting to PED format...")
        
        ped_output <- paste0(chr_filtered_prefix, "_ped")
        convert_result <- system2(
          plink_path,
          args = c(
            "--bfile", chr_filtered_prefix,
            "--allow-extra-chr",
            "--recode",
            "--out", ped_output
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        
        if (convert_result != 0 || !file.exists(paste0(ped_output, ".ped"))) {
          showNotification("Failed to convert filtered data to PED format", type = "error")
          return(NULL)
        }
        
        # Step 3: Read filtered data
        setProgress(value = 0.7, message = "Reading filtered data...")
        
        data <- tryCatch({
          read_plink_manual(
            paste0(ped_output, ".ped"),
            paste0(ped_output, ".map")
          )
        }, error = function(e) {
          showNotification(paste("Error reading filtered data:", e$message), type = "error")
          NULL
        })
        
        if (is.null(data)) {
          return(NULL)
        }
        
        # Store the loaded data
        geno_data(data)
        
        # Verify data is in PLINK format
        if (!is.list(data) || length(data) == 0 || !isTRUE(all(c("samples", "genotypes", "map") %in% names(data)))) {
          showNotification(
            HTML(paste0(
              "<strong>❌ Data Format Error</strong><br>",
              "Data must be in PLINK format for visualization."
            )),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        
        setProgress(value = 0.8, message = "Calculating basic statistics...")
        
        # Calculate basic statistics for visualization (without QC thresholds)
        basic_stats <- list()
        
        # Calculate all statistics using plink (if plinkR is available)
        if (use_plinkR && requireNamespace("plinkR", quietly = TRUE)) {
          # Use plink for all calculations (include relatedness with timeout)
          plink_stats <- calculate_all_stats_plink(data, skip_relatedness = FALSE)
          
          # Use ONLY plink results - no R fallback to avoid slow calculations
          if (!is.null(plink_stats)) {
            basic_stats$individual_call_rate <- plink_stats$individual_call_rate
            basic_stats$individual_missing_rate <- plink_stats$individual_missing_rate
            basic_stats$individual_heterozygosity <- plink_stats$individual_heterozygosity
            basic_stats$individual_relatedness <- plink_stats$individual_relatedness
            basic_stats$maf <- plink_stats$maf
            basic_stats$marker_call_rate <- plink_stats$marker_call_rate
            basic_stats$marker_missing_rate <- plink_stats$marker_missing_rate
            basic_stats$hwe_pvalues <- plink_stats$hwe_pvalues
            # Add PCA results if available
            if (!is.null(plink_stats$pca_scores)) {
              basic_stats$pca_scores <- plink_stats$pca_scores
              basic_stats$pca_variance <- plink_stats$pca_variance
              basic_stats$pca_sample_ids <- plink_stats$pca_sample_ids
            }
          } else {
            # PLINK calculation failed - show error and return
            showNotification(
              HTML(paste0(
                "<strong>❌ PLINK Calculation Failed</strong><br>",
                "Unable to calculate statistics using PLINK.<br>",
                "Please check that PLINK is installed and accessible."
              )),
              type = "error",
              duration = 10
            )
            return(NULL)
          }
        } else {
          # plinkR not available - show error and return
          showNotification(
            HTML(paste0(
              "<strong>❌ PLINK Not Available</strong><br>",
              "plinkR package is required for statistics calculation.<br>",
              "Please install plinkR: install.packages('plinkR')"
            )),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        
        # Store basic stats for plotting (not QC results)
        summary_stats(basic_stats)
        summary_generated(TRUE)
        
        setProgress(value = 1, message = "Summary generated!")
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
  # Note: All data is now in PLINK format (BLUPF90 is automatically converted)
  observeEvent(input$run_qc, {
    req(geno_data())
    
    if (!use_plinkR || !requireNamespace("plinkR", quietly = TRUE)) {
      showNotification(
        HTML(paste0(
          "<strong>❌ PLINK Not Available</strong><br>",
          "plinkR package is required for quality control filtering.<br>",
          "Please install plinkR: install.packages('plinkR')"
        )),
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    withProgress(message = "Running quality control filtering...", value = 0, {
      tryCatch({
        data <- geno_data()
        
        # Verify data is in PLINK format (list with samples, genotypes, map)
        if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
          showNotification(
            HTML(paste0(
              "<strong>❌ Data Format Error</strong><br>",
              "Data must be in PLINK format for analysis.<br>",
              "If you uploaded BLUPF90 format, please ensure plinkR is installed for conversion."
            )),
            type = "error",
            duration = 10
          )
          return(NULL)
        }
        
        setProgress(value = 0.2, message = "Preparing PLINK files...")
        
        # Create temporary directory for PLINK files
        tmp_dir <- tempfile(pattern = "genovieweR_qc_")
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        prefix <- file.path(tmp_dir, "temp_data")
        
        # Write PLINK files
        fam_data <- cbind(
          data$samples$Family_ID,
          data$samples$Sample_ID,
          0, 0, 0, -9  # Father, Mother, Sex, Phenotype
        )
        write.table(fam_data, paste0(prefix, ".fam"), 
                    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
        
        if (ncol(data$map) >= 4) {
          map_data <- cbind(
            data$map[, 1],  # Chromosome
            data$map[, 2],  # SNP_ID
            data$map[, 3],  # Genetic_Distance
            data$map[, 4]   # Physical_Position
          )
          write.table(map_data, paste0(prefix, ".map"), 
                      row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
        }
        
        ped_data <- cbind(
          data$samples$Family_ID,
          data$samples$Sample_ID,
          0, 0, 0, -9,  # Father, Mother, Sex, Phenotype
          data$genotypes
        )
        write.table(ped_data, paste0(prefix, ".ped"), 
                    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
        
        # Find plink executable
        plink_path <- tryCatch({
          if (exists("find_plink", where = "package:plinkR", mode = "function")) {
            plinkR::find_plink()
          } else {
            Sys.which("plink")
          }
        }, error = function(e) {
          Sys.which("plink")
        })
        
        if (is.null(plink_path) || length(plink_path) == 0 || plink_path == "" || length(plink_path) > 1 || !file.exists(plink_path)) {
          unlink(tmp_dir, recursive = TRUE, force = TRUE)
          showNotification("PLINK executable not found", type = "error")
          return(NULL)
        }
        
        # Helper function to count SNPs/samples in PLINK files
        # Supports both text format (.ped/.map) and binary format (.bed/.bim/.fam)
        count_plink_items <- function(prefix) {
          samples <- 0
          snps <- 0
          
          # Check for binary format (.fam and .bim)
          has_fam <- file.exists(paste0(prefix, ".fam"))
          has_bim <- file.exists(paste0(prefix, ".bim"))
          
          if (has_fam) {
            fam_lines <- readLines(paste0(prefix, ".fam"), warn = FALSE)
            samples <- length(fam_lines)
          }
          if (has_bim) {
            bim_lines <- readLines(paste0(prefix, ".bim"), warn = FALSE)
            snps <- length(bim_lines)
          }
          
          # If binary format incomplete or not found, try text format (.ped and .map)
          # Always check text format if binary format is missing
          if (!has_fam && file.exists(paste0(prefix, ".ped"))) {
            ped_lines <- readLines(paste0(prefix, ".ped"), warn = FALSE)
            samples <- length(ped_lines)
          }
          if (!has_bim && file.exists(paste0(prefix, ".map"))) {
            map_lines <- readLines(paste0(prefix, ".map"), warn = FALSE)
            snps <- length(map_lines)
          }
          
          list(samples = samples, snps = snps)
        }
        
        # Count initial numbers
        initial_counts <- count_plink_items(prefix)
        
        # Initialize filter statistics
        filter_stats <- list(
          geno_removed_snps = 0,
          mind_removed_samples = 0,
          maf_removed_snps = 0,
          hwe_removed_snps = 0
        )
        
        setProgress(value = 0.3, message = "Calculating individual filter statistics...")
        
        # Run each filter separately to get detailed removal counts
        # This is done in parallel conceptually, but sequentially for accuracy
        current_prefix <- prefix
        
        # --geno: exclude SNPs with missing rate > threshold
        if (!is.null(input$geno_threshold) && input$geno_threshold < 1) {
          geno_output <- paste0(prefix, "_geno_step")
          geno_result <- system2(
            plink_path,
            args = c("--file", current_prefix, "--allow-extra-chr", "--geno", as.character(input$geno_threshold),
                    "--make-bed", "--out", geno_output),
            stdout = FALSE,
            stderr = FALSE
          )
          if (geno_result == 0 && file.exists(paste0(geno_output, ".bed"))) {
            geno_counts <- count_plink_items(geno_output)
            filter_stats$geno_removed_snps <- initial_counts$snps - geno_counts$snps
            current_prefix <- geno_output  # Use filtered data for next step
          }
        }
        
        # --mind: exclude samples with missing rate > threshold
        if (!is.null(input$mind_threshold) && input$mind_threshold < 1) {
          mind_output <- paste0(prefix, "_mind_step")
          mind_result <- system2(
            plink_path,
            args = c("--bfile", current_prefix, "--allow-extra-chr", "--mind", as.character(input$mind_threshold),
                    "--make-bed", "--out", mind_output),
            stdout = FALSE,
            stderr = FALSE
          )
          if (mind_result == 0 && file.exists(paste0(mind_output, ".bed"))) {
            mind_counts <- count_plink_items(mind_output)
            filter_stats$mind_removed_samples <- initial_counts$samples - mind_counts$samples
            current_prefix <- mind_output  # Use filtered data for next step
          }
        }
        
        # --maf: exclude SNPs with MAF < threshold
        if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
          maf_output <- paste0(prefix, "_maf_step")
          maf_result <- system2(
            plink_path,
            args = c("--bfile", current_prefix, "--allow-extra-chr", "--maf", as.character(input$maf_threshold),
                    "--make-bed", "--out", maf_output),
            stdout = FALSE,
            stderr = FALSE
          )
          if (maf_result == 0 && file.exists(paste0(maf_output, ".bed"))) {
            maf_counts <- count_plink_items(maf_output)
            # Count SNPs removed by MAF (from current state)
            current_counts <- count_plink_items(current_prefix)
            filter_stats$maf_removed_snps <- current_counts$snps - maf_counts$snps
            current_prefix <- maf_output  # Use filtered data for next step
          }
        }
        
        # --hwe: exclude SNPs with HWE p-value < threshold
        if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
          hwe_output <- paste0(prefix, "_hwe_step")
          hwe_result <- system2(
            plink_path,
            args = c("--bfile", current_prefix, "--allow-extra-chr", "--hwe", as.character(input$hwe_threshold),
                    "--make-bed", "--out", hwe_output),
            stdout = FALSE,
            stderr = FALSE
          )
          if (hwe_result == 0 && file.exists(paste0(hwe_output, ".bed"))) {
            hwe_counts <- count_plink_items(hwe_output)
            # Count SNPs removed by HWE (from current state)
            current_counts <- count_plink_items(current_prefix)
            filter_stats$hwe_removed_snps <- current_counts$snps - hwe_counts$snps
            current_prefix <- hwe_output
          }
        }
        
        setProgress(value = 0.6, message = "Running final combined PLINK quality control...")
        
        # Now run all filters together in one command for final output (faster)
        qc_output <- paste0(prefix, "_qc")
        plink_args <- c("--file", prefix, "--allow-extra-chr")
        
        # Add all filters to single command
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
        
        # Run PLINK QC with all filters combined (single command for speed)
        result <- system2(
          plink_path,
          args = plink_args,
          stdout = FALSE,
          stderr = FALSE
        )
        
        if (result != 0 || !file.exists(paste0(qc_output, ".bed"))) {
          # Clean up intermediate files
          unlink(paste0(prefix, c("_geno_step", "_mind_step", "_maf_step", "_hwe_step")), recursive = TRUE, force = TRUE)
          unlink(tmp_dir, recursive = TRUE, force = TRUE)
          showNotification("PLINK quality control failed", type = "error")
          return(NULL)
        }
        
        setProgress(value = 0.8, message = "Generating QC report...")
        
        # Get final counts
        final_counts <- count_plink_items(qc_output)
        
        # Calculate intermediate counts for accurate reporting (before cleaning up files)
        # Get counts after each filter step for proper "Before" values
        geno_after_snps <- if (!is.null(input$geno_threshold) && input$geno_threshold < 1 && 
                               file.exists(paste0(prefix, "_geno_step.bed"))) {
          count_plink_items(paste0(prefix, "_geno_step"))$snps
        } else {
          initial_counts$snps
        }
        
        maf_before_snps <- if (!is.null(input$maf_threshold) && input$maf_threshold > 0) {
          # MAF is applied after geno, so before = after geno (or initial if no geno)
          if (!is.null(input$geno_threshold) && input$geno_threshold < 1 && 
              file.exists(paste0(prefix, "_geno_step.bed"))) {
            count_plink_items(paste0(prefix, "_geno_step"))$snps
          } else {
            initial_counts$snps
          }
        } else {
          initial_counts$snps
        }
        
        maf_after_snps <- if (!is.null(input$maf_threshold) && input$maf_threshold > 0 && 
                              file.exists(paste0(prefix, "_maf_step.bed"))) {
          count_plink_items(paste0(prefix, "_maf_step"))$snps
        } else {
          maf_before_snps
        }
        
        hwe_before_snps <- if (!is.null(input$hwe_threshold) && input$hwe_threshold > 0) {
          # HWE is applied after maf (and geno), so before = after maf
          maf_after_snps
        } else {
          initial_counts$snps
        }
        
        # Clean up intermediate step files
        unlink(paste0(prefix, c("_geno_step", "_mind_step", "_maf_step", "_hwe_step")), recursive = TRUE, force = TRUE)
        
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
          output_files = list(
            bed = paste0(qc_output, ".bed"),
            bim = paste0(qc_output, ".bim"),
            fam = paste0(qc_output, ".fam")
          )
        )
        
        # Set QC results for display
        qc_results(qc_report)
        
        # Clean up temporary files (keep output files for user to download if needed)
        # unlink(tmp_dir, recursive = TRUE, force = TRUE)
        
        setProgress(value = 1, message = "QC completed!")
        
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
  
  # Per-individual plots: Sample missing rate and Sample relatedness (Interactive with plotly)
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
    
    # Plot 2: Sample relatedness
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
               add_annotations(text = "Please load PLINK format data first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      if (!is.null(stats$individual_relatedness)) {
        relatedness_df <- stats$individual_relatedness
        
        # Check for required columns: Z0, Z1, and EXPECTED (or RT for relationship type)
        if (is.data.frame(relatedness_df) && nrow(relatedness_df) > 0) {
          # PLINK --genome output typically has RT (relationship type) column
          # Map RT to EXPECTED if needed, or use RT directly
          if ("RT" %in% names(relatedness_df) && !"EXPECTED" %in% names(relatedness_df)) {
            relatedness_df$EXPECTED <- as.factor(relatedness_df$RT)
          }
          
          # Ensure Z0 and Z1 columns exist
          if ("Z0" %in% names(relatedness_df) && "Z1" %in% names(relatedness_df) && "EXPECTED" %in% names(relatedness_df)) {
            # Filter valid data points
            valid_idx <- is.finite(relatedness_df$Z0) & is.finite(relatedness_df$Z1) & 
                        !is.na(relatedness_df$Z0) & !is.na(relatedness_df$Z1) &
                        relatedness_df$Z0 >= 0 & relatedness_df$Z0 <= 1 &
                        relatedness_df$Z1 >= 0 & relatedness_df$Z1 <= 1
            
            # Filter by PI_HAT > 0.1 if PI_HAT column exists
            if ("PI_HAT" %in% names(relatedness_df)) {
              valid_idx <- valid_idx & !is.na(relatedness_df$PI_HAT) & 
                          is.finite(relatedness_df$PI_HAT) & relatedness_df$PI_HAT > 0.1
            }
            
            if (sum(valid_idx) > 0) {
              plot_df <- relatedness_df[valid_idx, ]
              plot_df$EXPECTED <- as.factor(plot_df$EXPECTED)
              
              # Create Z0-Z1 IBD relationship plot with golden color
              # Build hover text with PI_HAT if available
              if ("PI_HAT" %in% names(plot_df)) {
                p_relatedness <- plot_ly(plot_df, x = ~Z0, y = ~Z1, 
                                        type = "scatter", mode = "markers",
                                        marker = list(size = 4, opacity = 0.6, color = "#CEB888"),
                                        text = ~paste("Pair:", if("IID1" %in% names(plot_df)) paste(IID1, IID2, sep="-") else "N/A",
                                                     "<br>Z0:", round(Z0, 3),
                                                     "<br>Z1:", round(Z1, 3),
                                                     "<br>PI_HAT:", round(PI_HAT, 4),
                                                     "<br>Expected:", EXPECTED),
                                        hoverinfo = "text")
              } else {
                p_relatedness <- plot_ly(plot_df, x = ~Z0, y = ~Z1, 
                                        type = "scatter", mode = "markers",
                                        marker = list(size = 4, opacity = 0.6, color = "#CEB888"),
                                        text = ~paste("Pair:", if("IID1" %in% names(plot_df)) paste(IID1, IID2, sep="-") else "N/A",
                                                     "<br>Z0:", round(Z0, 3),
                                                     "<br>Z1:", round(Z1, 3),
                                                     "<br>Expected:", EXPECTED),
                                        hoverinfo = "text")
              }
              
              p_relatedness <- p_relatedness %>%
                layout(title = list(text = "Sample Relatedness Distribution", font = list(size = 16)),
                      xaxis = list(title = list(text = "Z0", font = list(size = 14)), 
                                  range = c(0, 1)),
                      yaxis = list(title = list(text = "Z1", font = list(size = 14)), 
                                  range = c(0, 1)),
                      showlegend = FALSE,
                      margin = list(t = 60, b = 60, l = 80, r = 40))
              p_relatedness
            } else {
              plotly_empty() %>% 
                add_annotations(text = "No valid Z0/Z1 data points found",
                              x = 0.5, y = 0.5, showarrow = FALSE)
            }
          } else {
            # Fallback to PI_HAT histogram if Z0/Z1 not available
            if ("PI_HAT" %in% names(relatedness_df)) {
              p_relatedness <- plot_ly() %>%
                add_histogram(data = relatedness_df, x = ~PI_HAT,
                             nbinsx = 50,
                             marker = list(color = "#CEB888", line = list(color = "white", width = 1)),
                             name = "Relatedness") %>%
                layout(title = list(text = "Sample Relatedness Distribution", font = list(size = 16)),
                      xaxis = list(title = list(text = "PI_HAT (Proportion IBD)", font = list(size = 14))),
                      yaxis = list(title = list(text = "Number of Sample Pairs", font = list(size = 14))),
                      showlegend = FALSE,
                      margin = list(t = 60, b = 60, l = 80, r = 40))
              p_relatedness
            } else {
              plotly_empty() %>% 
                add_annotations(text = "Relatedness data missing Z0/Z1 or PI_HAT columns",
                              x = 0.5, y = 0.5, showarrow = FALSE)
            }
          }
        } else {
          plotly_empty() %>% 
            add_annotations(text = "Relatedness calculation in progress or no related pairs found",
                          x = 0.5, y = 0.5, showarrow = FALSE)
        }
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
    
    # Plot 2: Sample relatedness
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
                                 label = "Please load PLINK format data first", size = 5) +
               theme_void())
      }
      
      if (!is.null(stats$individual_relatedness)) {
        relatedness_df <- stats$individual_relatedness
        
        # Check for required columns: Z0, Z1, and EXPECTED (or RT for relationship type)
        if (is.data.frame(relatedness_df) && nrow(relatedness_df) > 0) {
          # PLINK --genome output typically has RT (relationship type) column
          # Map RT to EXPECTED if needed, or use RT directly
          if ("RT" %in% names(relatedness_df) && !"EXPECTED" %in% names(relatedness_df)) {
            relatedness_df$EXPECTED <- as.factor(relatedness_df$RT)
          }
          
          # Ensure Z0 and Z1 columns exist
          if ("Z0" %in% names(relatedness_df) && "Z1" %in% names(relatedness_df) && "EXPECTED" %in% names(relatedness_df)) {
            # Filter valid data points
            valid_idx <- is.finite(relatedness_df$Z0) & is.finite(relatedness_df$Z1) & 
                        !is.na(relatedness_df$Z0) & !is.na(relatedness_df$Z1) &
                        relatedness_df$Z0 >= 0 & relatedness_df$Z0 <= 1 &
                        relatedness_df$Z1 >= 0 & relatedness_df$Z1 <= 1
            
            # Filter by PI_HAT > 0.1 if PI_HAT column exists
            if ("PI_HAT" %in% names(relatedness_df)) {
              valid_idx <- valid_idx & !is.na(relatedness_df$PI_HAT) & 
                          is.finite(relatedness_df$PI_HAT) & relatedness_df$PI_HAT > 0.1
            }
            
            if (sum(valid_idx) > 0) {
              plot_df <- relatedness_df[valid_idx, ]
              plot_df$EXPECTED <- as.factor(plot_df$EXPECTED)
              
              # Create Z0-Z1 IBD relationship plot using ggplot2 with golden color
              ggplot(plot_df, aes_string(x = "Z0", y = "Z1")) +
                geom_point(alpha = 0.6, size = 2, color = "#CEB888") +
                labs(title = "Sample Relatedness Distribution",
                     x = "Z0",
                     y = "Z1") +
                xlim(0, 1) +
                ylim(0, 1) +
                theme_gray() +
                theme(plot.title = element_text(hjust = 0.5, size = 14))
            } else {
              ggplot() + 
                annotate("text", x = 0.5, y = 0.5, 
                        label = "No valid Z0/Z1 data points found", size = 4) +
                theme_void()
            }
          } else {
            # Fallback to PI_HAT histogram if Z0/Z1 not available
            if ("PI_HAT" %in% names(relatedness_df)) {
              ggplot(relatedness_df, aes_string(x = "PI_HAT")) +
                geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
                labs(title = "Sample Relatedness Distribution",
                     x = "PI_HAT (Proportion IBD)",
                     y = "Number of Sample Pairs") +
                theme_bw() +
                theme(plot.title = element_text(hjust = 0.5, size = 14))
            } else {
              ggplot() + 
                annotate("text", x = 0.5, y = 0.5, 
                        label = "Relatedness data missing Z0/Z1 or PI_HAT columns", size = 4) +
                theme_void()
            }
          }
        } else {
          ggplot() + 
            annotate("text", x = 0.5, y = 0.5, 
                    label = "Relatedness calculation in progress or no related pairs found", size = 4) +
            theme_void()
        }
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
          add_annotations(text = "HWE p-values not calculated\nPlease run PLINK statistics first",
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
                  label = "HWE p-values not calculated\nPlease run PLINK statistics first", size = 4) +
          theme_void()
      }
    })
  }
  
  # Store plots for download
  per_individual_plot_obj <- reactiveVal(NULL)
  per_marker_plot_obj <- reactiveVal(NULL)
  pca_result <- reactiveVal(NULL)
  pca_plot_obj <- reactiveVal(NULL)
  
  # Compute PCA from summary_stats (from PLINK --pca) or genotype data
  compute_pca <- function(geno_data, summary_stats = NULL) {
    # First, try to get PCA from summary_stats (PLINK output)
    if (!is.null(summary_stats) && 
        !is.null(summary_stats$pca_scores) && 
        !is.null(summary_stats$pca_variance)) {
      tryCatch({
        pca_scores <- summary_stats$pca_scores
        pca_variance <- summary_stats$pca_variance
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
            samples = sample_ids[valid_rows],
            n_components = min(10, ncol(pca_scores))
          ))
        }
      }, error = function(e) {
        cat("Error reading PCA from summary_stats:", e$message, "\n")
      })
    }
    
    # Fallback: compute PCA from genotype data (if PLINK PCA not available)
    if (!is.list(geno_data) || !all(c("genotypes", "samples") %in% names(geno_data))) {
      return(NULL)
    }
    
    tryCatch({
      # Get genotype matrix (samples x markers)
      geno_matrix <- geno_data$genotypes
      
      # Convert to numeric if needed
      if (!is.numeric(geno_matrix)) {
        geno_matrix <- apply(geno_matrix, 2, function(x) {
          as.numeric(as.character(x))
        })
      }
      
      # Remove markers with all missing values
      missing_by_marker <- colSums(is.na(geno_matrix))
      valid_markers <- missing_by_marker < nrow(geno_matrix)
      
      if (sum(valid_markers) < 2) {
        return(NULL)
      }
      
      geno_matrix <- geno_matrix[, valid_markers, drop = FALSE]
      
      # Remove samples with all missing values
      missing_by_sample <- rowSums(is.na(geno_matrix))
      valid_samples <- missing_by_sample < ncol(geno_matrix)
      
      if (sum(valid_samples) < 2) {
        return(NULL)
      }
      
      geno_matrix <- geno_matrix[valid_samples, , drop = FALSE]
      
      # Impute missing values with mean (simple imputation)
      for (i in 1:ncol(geno_matrix)) {
        col_mean <- mean(geno_matrix[, i], na.rm = TRUE)
        if (!is.na(col_mean) && is.finite(col_mean)) {
          geno_matrix[is.na(geno_matrix[, i]), i] <- col_mean
        }
      }
      
      # Center and scale the data
      geno_scaled <- scale(geno_matrix, center = TRUE, scale = TRUE)
      
      # Perform PCA
      pca <- prcomp(geno_scaled, center = FALSE, scale. = FALSE)
      
      # Extract PC scores and variance explained
      pc_scores <- pca$x
      variance_explained <- summary(pca)$importance[2, ] * 100  # Percentage
      
      # Get sample IDs
      sample_ids <- if (!is.null(geno_data$samples) && is.data.frame(geno_data$samples) && 
                       "Sample_ID" %in% names(geno_data$samples)) {
        geno_data$samples$Sample_ID[valid_samples]
      } else {
        paste0("Sample_", 1:nrow(pc_scores))
      }
      
      list(
        scores = pc_scores,
        variance = variance_explained,
        samples = sample_ids,
        n_components = min(10, ncol(pc_scores))
      )
    }, error = function(e) {
      cat("Error computing PCA:", e$message, "\n")
      NULL
    })
  }
  
  # Compute PCA when data or stats change (separate from rendering)
  observe({
    req(geno_data())
    data <- geno_data()
    
    if (!is.list(data) || !all(c("genotypes", "samples") %in% names(data))) {
      pca_result(NULL)
      return()
    }
    
    # Get summary_stats for PCA (from PLINK --pca)
    stats <- if (summary_generated() && !is.null(summary_stats())) {
      summary_stats()
    } else if (!is.null(qc_results())) {
      qc_results()
    } else {
      pca_result(NULL)
      return()
    }
    
    # Compute PCA
    pca <- compute_pca(data, summary_stats = stats)
    if (!is.null(pca) && !is.null(pca$scores) && nrow(pca$scores) > 0) {
      pca_result(pca)
      cat("PCA computed successfully:", nrow(pca$scores), "samples,", ncol(pca$scores), "components\n")
    } else {
      pca_result(NULL)
      cat("PCA computation returned NULL or empty result\n")
    }
  })
  
  # PCA Plots (2D and 3D) - requires plotly
  if (use_plotly) {
    output$pca_plots <- renderPlotly({
      req(geno_data(), input$pca_dimension)
      data <- geno_data()
      
      if (!is.list(data) || !all(c("genotypes", "samples") %in% names(data))) {
        return(plotly_empty() %>% 
               add_annotations(text = "Please load PLINK format data first",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      # Get summary_stats for PCA (from PLINK --pca)
      stats <- if (summary_generated() && !is.null(summary_stats())) {
        summary_stats()
      } else if (!is.null(qc_results())) {
        qc_results()
      } else {
        return(plotly_empty() %>% 
               add_annotations(text = "Please click 'Show Summary & Plots' or run QC first to compute PCA",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      # Get PCA result (compute if not available)
      pca <- pca_result()
      if (is.null(pca) || is.null(pca$scores) || nrow(pca$scores) == 0) {
        # Try to compute PCA now if not already computed
        pca <- compute_pca(data, summary_stats = stats)
        if (!is.null(pca) && !is.null(pca$scores) && nrow(pca$scores) > 0) {
          pca_result(pca)
        } else {
          return(plotly_empty() %>% 
                 add_annotations(text = "PCA computation failed. Please check your data and ensure PLINK is available.",
                               x = 0.5, y = 0.5, showarrow = FALSE))
        }
      }
      
      # Re-validate after computation
      pca <- pca_result()
      if (is.null(pca) || is.null(pca$scores) || nrow(pca$scores) == 0) {
        return(plotly_empty() %>% 
               add_annotations(text = "PCA computation returned empty result. Please check your data.",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      # Validate PCA data
      if (ncol(pca$scores) < 2) {
        return(plotly_empty() %>% 
               add_annotations(text = "Insufficient PCA components. Need at least 2 components for 2D plot.",
                             x = 0.5, y = 0.5, showarrow = FALSE))
      }
      
      # Get dimension selection
      dimension <- input$pca_dimension
      
      if (dimension == "2d") {
        # 2D PCA plot (PC1 vs PC2)
        # Ensure we have valid data
        if (length(pca$samples) != nrow(pca$scores)) {
          pca$samples <- paste0("Sample_", 1:nrow(pca$scores))
        }
        
        pca_df <- data.frame(
          PC1 = as.numeric(pca$scores[, 1]),
          PC2 = as.numeric(pca$scores[, 2]),
          Sample = as.character(pca$samples)
        )
        
        # Remove any rows with invalid data
        pca_df <- pca_df[is.finite(pca_df$PC1) & is.finite(pca_df$PC2), ]
        
        if (nrow(pca_df) == 0) {
          return(plotly_empty() %>% 
                 add_annotations(text = "No valid PCA data to display.",
                               x = 0.5, y = 0.5, showarrow = FALSE))
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
            xaxis = list(title = list(text = paste0("PC1 (", round(pca$variance[1], 2), "%)"), 
                                     font = list(size = 14))),
            yaxis = list(title = list(text = paste0("PC2 (", round(pca$variance[2], 2), "%)"), 
                                     font = list(size = 14))),
            hovermode = "closest",
            margin = list(t = 80, b = 60, l = 80, r = 40)
          )
        
        pca_plot_obj(p)
        p
      } else {
        # 3D PCA plot (PC1 vs PC2 vs PC3)
        if (ncol(pca$scores) >= 3) {
          # Ensure we have valid data
          if (length(pca$samples) != nrow(pca$scores)) {
            pca$samples <- paste0("Sample_", 1:nrow(pca$scores))
          }
          
          pca_df <- data.frame(
            PC1 = as.numeric(pca$scores[, 1]),
            PC2 = as.numeric(pca$scores[, 2]),
            PC3 = as.numeric(pca$scores[, 3]),
            Sample = as.character(pca$samples)
          )
          
          # Remove any rows with invalid data
          pca_df <- pca_df[is.finite(pca_df$PC1) & is.finite(pca_df$PC2) & is.finite(pca_df$PC3), ]
          
          if (nrow(pca_df) == 0) {
            return(plotly_empty() %>% 
                   add_annotations(text = "No valid PCA data to display.",
                                 x = 0.5, y = 0.5, showarrow = FALSE))
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
          plotly_empty() %>% 
            add_annotations(text = "Insufficient dimensions for 3D plot. Need at least 3 principal components.",
                          x = 0.5, y = 0.5, showarrow = FALSE)
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
      
      # Plot 2: Sample relatedness (Z0-Z1 IBD relationship plot)
      if (!is.null(stats$individual_relatedness)) {
        relatedness_df <- stats$individual_relatedness
        
        if (is.data.frame(relatedness_df) && nrow(relatedness_df) > 0) {
          # PLINK --genome output typically has RT (relationship type) column
          # Map RT to EXPECTED if needed, or use RT directly
          if ("RT" %in% names(relatedness_df) && !"EXPECTED" %in% names(relatedness_df)) {
            relatedness_df$EXPECTED <- as.factor(relatedness_df$RT)
          }
          
          # Check for Z0 and Z1 columns for IBD relationship plot
          if ("Z0" %in% names(relatedness_df) && "Z1" %in% names(relatedness_df) && "EXPECTED" %in% names(relatedness_df)) {
            # Filter valid data points
            valid_idx <- is.finite(relatedness_df$Z0) & is.finite(relatedness_df$Z1) & 
                        !is.na(relatedness_df$Z0) & !is.na(relatedness_df$Z1) &
                        relatedness_df$Z0 >= 0 & relatedness_df$Z0 <= 1 &
                        relatedness_df$Z1 >= 0 & relatedness_df$Z1 <= 1
            
            # Filter by PI_HAT > 0.1 if PI_HAT column exists
            if ("PI_HAT" %in% names(relatedness_df)) {
              valid_idx <- valid_idx & !is.na(relatedness_df$PI_HAT) & 
                          is.finite(relatedness_df$PI_HAT) & relatedness_df$PI_HAT > 0.1
            }
            
            if (sum(valid_idx) > 0) {
              plot_df <- relatedness_df[valid_idx, ]
              plot_df$EXPECTED <- as.factor(plot_df$EXPECTED)
              
              # Create Z0-Z1 IBD relationship plot using ggplot2 with golden color
              p_relatedness <- ggplot(plot_df, aes_string(x = "Z0", y = "Z1")) +
                geom_point(alpha = 0.6, size = 2, color = "#CEB888") +
                labs(title = "Sample Relatedness Distribution",
                     x = "Z0",
                     y = "Z1") +
                xlim(0, 1) +
                ylim(0, 1) +
                theme_gray() +
                theme(plot.title = element_text(hjust = 0.5, size = 14))
              
              plots$relatedness <- p_relatedness
            }
          } else if ("PI_HAT" %in% names(relatedness_df)) {
            # Fallback to PI_HAT histogram if Z0/Z1 not available
            p_relatedness <- ggplot(relatedness_df, aes_string(x = "PI_HAT")) +
              geom_histogram(bins = 50, fill = "#CEB888", alpha = 0.7, color = "white", boundary = 0) +
              labs(title = "Sample Relatedness Distribution",
                   x = "PI_HAT (Proportion IBD)",
                   y = "Number of Sample Pairs") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5, size = 14))
            
            plots$relatedness <- p_relatedness
          }
        }
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
  
  # Format conversion help modal (if plinkR is available)
  observeEvent(input$format_convert_help, {
    if (!use_plinkR) {
      showNotification("plinkR is not installed. Please install it for format conversion features.",
                      type = "warning")
      return(NULL)
    }
    
    showModal(modalDialog(
      title = div(style = "font-weight: bold;", "🔄 Genotype Format Conversion Help"),
      size = "l",
      easyClose = TRUE,
      tagList(
        div(style = "margin-bottom: 20px;",
            p(style = "font-size: 1.05rem; margin-bottom: 15px;",
              "Use plinkR to convert between PLINK and BLUPF90 formats.")
        ),
        
        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #CEB888;",
            h5(style = "font-weight: bold; color: #2c3e50; margin-top: 0;", "📤 PLINK Format"),
            p(style = "margin-bottom: 10px;", 
              tags$strong("Required files:"),
              tags$ul(
                tags$li(tags$code(".ped"), " file - Contains genotype data (individuals × markers)"),
                tags$li(tags$code(".map"), " file - Contains marker map information (chromosome, SNP ID, genetic distance, physical position)")
              )
            ),
            p(style = "margin-bottom: 0; color: #666; font-size: 0.95rem;",
              "💡 Note: Both files must have the same prefix (e.g., ", tags$code("data.ped"), " and ", tags$code("data.map"), ")")
        ),
        
        div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px; border-left: 4px solid #CEB888;",
            h5(style = "font-weight: bold; color: #2c3e50; margin-top: 0;", "📥 BLUPF90 Format"),
            p(style = "margin-bottom: 10px;",
              tags$strong("Required files:"),
              tags$ul(
                tags$li(tags$code(".txt"), " file - Contains genotype data"),
                tags$li(tags$code(".map"), " file - Contains marker map information"),
                tags$li(tags$code(".bim"), " file - Contains marker information")
              )
            )
        ),
        
        div(style = "margin-top: 20px; padding: 10px; background-color: #fff3cd; border-radius: 5px; border: 1px solid #ffc107;",
            tags$strong("⚠️ Important:"), 
            " This feature requires the ", tags$code("plinkR"), " package to be installed.",
            br(),
            "Install with: ", tags$code("remotes::install_github('Thymine2001/plinkR')")
        )
      ),
      footer = modalButton("Close")
    ))
  })
  
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
      if (!is.null(report) && !is.null(report$output_files) && !is.null(report$output_files$bed)) {
        # Extract base name from bed file path
        bed_path <- report$output_files$bed
        base_name <- basename(sub("\\.bed$", "", bed_path))
        paste0(base_name, "_filtered_", Sys.Date(), ".zip")
      } else {
        paste0("genovieweR_filtered_data_", Sys.Date(), ".zip")
      }
    },
    content = function(file) {
      req(qc_results())
      report <- qc_results()
      
      if (is.null(report) || is.null(report$output_files)) {
        writeLines("No QC results available. Please run QC first.", file)
        return(NULL)
      }
      
      # Check if PLINK executable is available
      plink_path <- tryCatch({
        if (exists("find_plink", where = "package:plinkR", mode = "function")) {
          plinkR::find_plink()
        } else {
          Sys.which("plink")
        }
      }, error = function(e) {
        Sys.which("plink")
      })
      
      if (is.null(plink_path) || length(plink_path) == 0 || plink_path == "" || 
          length(plink_path) > 1 || !file.exists(plink_path)) {
        writeLines("PLINK executable not found. Cannot convert filtered data.", file)
        return(NULL)
      }
      
      # Check if filtered binary files exist
      bed_file <- report$output_files$bed
      bim_file <- report$output_files$bim
      fam_file <- report$output_files$fam
      
      if (is.null(bed_file) || !file.exists(bed_file) ||
          is.null(bim_file) || !file.exists(bim_file) ||
          is.null(fam_file) || !file.exists(fam_file)) {
        writeLines("Filtered data files not found. Please run QC again.", file)
        return(NULL)
      }
      
      # Create temporary directory for conversion
      temp_dir <- tempfile(pattern = "genovieweR_download_")
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
      
      # Extract base name from bed file
      bed_base <- sub("\\.bed$", "", basename(bed_file))
      bed_dir <- dirname(bed_file)
      ped_output <- file.path(temp_dir, paste0(bed_base, "_filtered"))
      
      # Convert binary format to text format (.ped/.map) using PLINK
      convert_result <- system2(
        plink_path,
        args = c(
          "--bfile", file.path(bed_dir, bed_base),
          "--allow-extra-chr",
          "--recode",
          "--out", ped_output
        ),
        stdout = FALSE,
        stderr = FALSE
      )
      
      # Prepare files to zip
      files_to_zip <- c()
      
      # Add binary format files (.bed, .bim, .fam)
      files_to_zip <- c(files_to_zip, bed_file, bim_file, fam_file)
      
      # Add text format files (.ped, .map) if conversion succeeded
      ped_file <- paste0(ped_output, ".ped")
      map_file <- paste0(ped_output, ".map")
      
      if (convert_result == 0 && file.exists(ped_file) && file.exists(map_file)) {
        files_to_zip <- c(files_to_zip, ped_file, map_file)
      }
      
      # Create zip file
      if (length(files_to_zip) > 0) {
        temp_zip <- tempfile(fileext = ".zip")
        zip(temp_zip, files_to_zip)
        file.copy(temp_zip, file)
        
        # Clean up temporary directory
        unlink(temp_dir, recursive = TRUE, force = TRUE)
      } else {
        writeLines("Failed to prepare filtered data files for download.", file)
        unlink(temp_dir, recursive = TRUE, force = TRUE)
      }
    }
  )
}

# Helper functions for QC calculations
calculate_maf <- function(data) {
  # Placeholder - implement actual MAF calculation
  # This would depend on the data format
  if (is.list(data) && "genotypes" %in% names(data)) {
    # Calculate MAF from genotype matrix
    geno_matrix <- data$genotypes
    # Simplified MAF calculation
    maf_values <- apply(geno_matrix, 2, function(x) {
      alleles <- unlist(strsplit(as.character(x), ""))
      if (length(alleles) == 0) return(NA)
      allele_counts <- table(alleles)
      if (length(allele_counts) < 2) return(0)
      min_count <- min(allele_counts)
      total_count <- sum(allele_counts)
      min_count / total_count
    })
    return(maf_values)
  }
  return(NULL)
}

calculate_call_rate <- function(data) {
  # Placeholder - implement actual call rate calculation
  if (is.list(data) && "genotypes" %in% names(data)) {
    geno_matrix <- data$genotypes
    call_rates <- apply(geno_matrix, 2, function(x) {
      non_missing <- sum(!is.na(x) & x != "0 0" & x != "N N")
      total <- length(x)
      non_missing / total
    })
    return(call_rates)
  }
  return(NULL)
}

calculate_heterozygosity <- function(data) {
  # Calculate heterozygosity per marker (column-wise)
  if (is.list(data) && "genotypes" %in% names(data)) {
    geno_matrix <- data$genotypes
    het_values <- apply(geno_matrix, 2, function(x) {
      het_count <- sum(grepl("^[ATCG] [ATCG]$", x) & 
                      substr(x, 1, 1) != substr(x, 3, 3), na.rm = TRUE)
      total <- sum(!is.na(x) & x != "0 0" & x != "N N" & x != "")
      if (total == 0) return(NA)
      het_count / total
    })
    return(het_values)
  }
  return(NULL)
}

# Calculate heterozygosity per individual (row-wise) - using PLINK --het
# This function should be called after calculate_all_stats_plink which runs --het
calculate_individual_heterozygosity <- function(data) {
  # This function is now primarily a fallback
  # The main calculation is done via PLINK --het in calculate_all_stats_plink
  # and stored in results$individual_heterozygosity
  
  # If data already has heterozygosity from PLINK, return it
  if (is.list(data) && "individual_heterozygosity" %in% names(data)) {
    return(data$individual_heterozygosity)
  }
  
  # Calculate using base R
  if (is.list(data) && "genotypes" %in% names(data)) {
    geno_matrix <- data$genotypes
    
    # Calculate using base R
    het_values <- apply(geno_matrix, 1, function(x) {
      het_count <- sum(grepl("^[ATCG] [ATCG]$", as.character(x)) & 
                      substr(as.character(x), 1, 1) != substr(as.character(x), 3, 3), na.rm = TRUE)
      total <- sum(!is.na(x) & x != "0 0" & x != "N N" & x != "", na.rm = TRUE)
      if (total == 0) return(NA)
      het_count / total
    })
    return(het_values)
  }
  return(NULL)
}

# Calculate call rate per individual (row-wise) - using PLINK --missing
# This function should use results from calculate_all_stats_plink which runs --missing
calculate_individual_call_rate <- function(data) {
  # If data already has call rate from PLINK, return it
  if (is.list(data) && "individual_call_rate" %in% names(data)) {
    return(data$individual_call_rate)
  }
  
  # Calculate using base R
  if (is.list(data) && "genotypes" %in% names(data)) {
    geno_matrix <- data$genotypes
    
    # Calculate using base R
    call_rates <- apply(geno_matrix, 1, function(x) {
      non_missing <- sum(!is.na(x) & x != "0 0" & x != "N N" & x != "", na.rm = TRUE)
      total <- length(x)
      if (total == 0) return(NA)
      non_missing / total
    })
    return(call_rates)
  }
  return(NULL)
}

# Calculate all statistics using plink commands (via plinkR)
# This function runs plink --missing, --freq, --hardy, --het, and optionally --genome
calculate_all_stats_plink <- function(data, skip_relatedness = FALSE) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(NULL)
  }
  
  if (!requireNamespace("plinkR", quietly = TRUE)) {
    return(NULL)
  }
  
  tryCatch({
    # Create temporary directory for PLINK files
    tmp_dir <- tempfile(pattern = "genovieweR_stats_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- file.path(tmp_dir, "temp_data")
    
    # Write PLINK files
    # Write .fam file - optimized batch writing
    fam_data <- cbind(
      data$samples$Family_ID,
      data$samples$Sample_ID,
      0, 0, 0, -9  # Father, Mother, Sex, Phenotype
    )
    write.table(fam_data, paste0(prefix, ".fam"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    
    # Write .map file (from map data) - optimized
    if (ncol(data$map) >= 4) {
      map_data <- cbind(
        data$map[, 1],  # Chromosome
        data$map[, 2],  # SNP_ID
        data$map[, 3],  # Genetic_Distance
        data$map[, 4]   # Physical_Position
      )
      write.table(map_data, paste0(prefix, ".map"), 
                  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
    }
    
    # Write .ped file (genotypes) - optimized batch writing
    # Use cbind directly instead of creating data.frame
    ped_data <- cbind(
      data$samples$Family_ID,
      data$samples$Sample_ID,
      0, 0, 0, -9,  # Father, Mother, Sex, Phenotype
      data$genotypes
    )
    # Use write.table with optimized settings
    write.table(ped_data, paste0(prefix, ".ped"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    
    # Find plink executable
    plink_path <- tryCatch({
      if (exists("find_plink", where = "package:plinkR", mode = "function")) {
        plinkR::find_plink()
      } else {
        Sys.which("plink")
      }
    }, error = function(e) {
      Sys.which("plink")
    })
    
    if (is.null(plink_path) || length(plink_path) == 0 || plink_path == "" || length(plink_path) > 1 || !file.exists(plink_path)) {
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
      return(NULL)
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
            eigenvalues <- eigenval_data[, 1]
            # Calculate variance explained (percentage)
            total_var <- sum(eigenvalues)
            pca_variance <- (eigenvalues / total_var) * 100
            names(pca_variance) <- paste0("PC", 1:length(pca_variance))
          }
        }
      }, error = function(e) {
        cat("Error reading PCA files:", e$message, "\n")
        pca_scores <- NULL
        pca_variance <- NULL
        sample_ids_eigenvec <- NULL
      })
    }
    
    # Note: --hardy output will be in stats_output.hwe (not stats_output_hwe.hwe)
    # We'll adjust the file reading accordingly
    
    # Run plink --genome (for relatedness) - directly without LD pruning
    # Note: Pruning step is commented out as it may not be necessary for all datasets
    # and can be time-consuming. Direct --genome calculation uses all markers.
    if (!skip_relatedness) {
      relatedness_start_time <- Sys.time()
      
      # LD pruning step - COMMENTED OUT
      # prune_output <- paste0(prefix, "_pruned")
      # system2(
      #   plink_path,
      #   args = c(
      #     "--file", prefix,
      #     "--allow-extra-chr",
      #     "--indep-pairwise", "50", "5", "0.2",
      #     "--out", prune_output
      #   ),
      #   stdout = FALSE,
      #   stderr = FALSE
      # )
      
      # Run --genome directly on all markers (no pruning)
      remaining_time <- 60  # Increased timeout since we're using all markers
      if (requireNamespace("R.utils", quietly = TRUE)) {
        tryCatch({
          R.utils::withTimeout({
            system2(
              plink_path,
              args = c(
                "--file", prefix,
                "--allow-extra-chr",
                "--genome",
                "--out", paste0(stats_output, "_genome")
              ),
              stdout = FALSE,
              stderr = FALSE
            )
          }, timeout = remaining_time)
        }, TimeoutException = function(e) {
          cat("Relatedness calculation timed out after", remaining_time, "seconds\n")
        })
      } else {
        genome_start <- Sys.time()
        system2(
          plink_path,
          args = c(
            "--file", prefix,
            "--allow-extra-chr",
            "--genome",
            "--out", paste0(stats_output, "_genome")
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        elapsed_genome <- as.numeric(difftime(Sys.time(), genome_start, units = "secs"))
        if (elapsed_genome > remaining_time) {
          cat("Relatedness calculation exceeded timeout\n")
        }
      }
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
            cat("Warning: No rows with TEST=='ALL', using all rows\n")
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
            cat("Successfully read", sum(valid_match), "HWE p-values\n")
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
    
    # Read .genome (relatedness)
    genome_file <- paste0(stats_output, "_genome.genome")
    if (file.exists(genome_file)) {
      genome_data <- read.table(genome_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
      if (nrow(genome_data) > 0) {
        results$individual_relatedness <- genome_data
      }
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
    # relatedness can be NULL if skipped or failed
    
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
        results$pca_sample_ids <- data$samples$Sample_ID
      }
    }
    
    return(results)
    
  }, error = function(e) {
    # On error, log and return NULL (will trigger error message in UI)
    cat("Error in calculate_all_stats_plink:", e$message, "\n")
    return(NULL)
  })
}

# Calculate Hardy-Weinberg equilibrium p-values using plink --hardy (via plinkR)
calculate_hwe_plink <- function(data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(NULL)
  }
  
  if (!requireNamespace("plinkR", quietly = TRUE)) {
    return(calculate_hwe(data))  # Fallback to R calculation
  }
  
  tryCatch({
    # Create temporary directory for PLINK files
    tmp_dir <- tempfile(pattern = "genovieweR_hwe_")
    dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
    prefix <- file.path(tmp_dir, "temp_data")
    
    # Write .fam file
    fam_data <- data.frame(
      FID = data$samples$Family_ID,
      IID = data$samples$Sample_ID,
      Father = 0,
      Mother = 0,
      Sex = 0,
      Phenotype = -9,
      stringsAsFactors = FALSE
    )
    write.table(fam_data, paste0(prefix, ".fam"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    
    # Write .map file (from map data)
    if (ncol(data$map) >= 4) {
      map_data <- data.frame(
        Chromosome = data$map[, 1],
        SNP_ID = data$map[, 2],
        Genetic_Distance = data$map[, 3],
        Physical_Position = data$map[, 4],
        stringsAsFactors = FALSE
      )
      write.table(map_data, paste0(prefix, ".map"), 
                  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
    }
    
    # Write .ped file (genotypes)
    # PLINK .ped format: FID IID Father Mother Sex Phenotype Genotype1 Genotype2 ...
    ped_data <- cbind(
      data$samples$Family_ID,
      data$samples$Sample_ID,
      0,  # Father
      0,  # Mother
      0,  # Sex
      -9, # Phenotype
      data$genotypes
    )
    write.table(ped_data, paste0(prefix, ".ped"), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
    
    # Find plink executable
    plink_path <- tryCatch({
      # Try plinkR function if available
      if (exists("find_plink", where = "package:plinkR", mode = "function")) {
        plinkR::find_plink()
      } else {
        Sys.which("plink")
      }
    }, error = function(e) {
      Sys.which("plink")
    })
    
    if (is.null(plink_path) || length(plink_path) == 0 || plink_path == "" || length(plink_path) > 1 || !file.exists(plink_path)) {
      # Fallback to R calculation
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

# Fallback: Calculate Hardy-Weinberg equilibrium p-values in R (when plink is not available)
calculate_hwe <- function(data) {
  if (is.list(data) && "genotypes" %in% names(data)) {
    geno_matrix <- data$genotypes
    
    # Calculate HWE p-value for each marker (column)
    hwe_pvalues <- apply(geno_matrix, 2, function(x) {
      # Remove missing genotypes
      valid_genos <- x[!is.na(x) & x != "0 0" & x != "N N" & x != ""]
      if (length(valid_genos) < 10) return(NA)  # Need sufficient sample size
      
      # Count all alleles to get frequencies
      all_alleles <- unlist(strsplit(as.character(valid_genos), " "))
      if (length(all_alleles) < 2) return(NA)
      
      allele_counts <- table(all_alleles)
      if (length(allele_counts) < 2) return(NA)
      
      # Get allele frequencies
      total_alleles <- sum(allele_counts)
      p <- min(allele_counts) / total_alleles  # Minor allele frequency
      q <- 1 - p  # Major allele frequency
      
      # Count genotypes properly
      n_hom1 <- 0  # Minor allele homozygote
      n_het <- 0   # Heterozygote
      n_hom2 <- 0  # Major allele homozygote
      
      minor_allele <- names(allele_counts)[which.min(allele_counts)]
      major_allele <- names(allele_counts)[which.max(allele_counts)]
      
      for (geno in valid_genos) {
        alleles <- unlist(strsplit(as.character(geno), " "))
        if (length(alleles) == 2) {
          if (alleles[1] == alleles[2]) {
            # Homozygote
            if (alleles[1] == minor_allele) {
              n_hom1 <- n_hom1 + 1
            } else if (alleles[1] == major_allele) {
              n_hom2 <- n_hom2 + 1
            }
          } else {
            # Heterozygote
            n_het <- n_het + 1
          }
        }
      }
      
      n_total <- length(valid_genos)
      if (n_total == 0) return(NA)
      
      # Expected genotype frequencies under HWE
      exp_hom1 <- n_total * p^2
      exp_het <- n_total * 2 * p * q
      exp_hom2 <- n_total * q^2
      
      # Chi-square test
      chi_sq <- 0
      if (exp_hom1 > 0) chi_sq <- chi_sq + ((n_hom1 - exp_hom1)^2 / exp_hom1)
      if (exp_het > 0) chi_sq <- chi_sq + ((n_het - exp_het)^2 / exp_het)
      if (exp_hom2 > 0) chi_sq <- chi_sq + ((n_hom2 - exp_hom2)^2 / exp_hom2)
      
      # p-value from chi-square distribution with 1 df
      p_value <- 1 - pchisq(chi_sq, df = 1)
      
      return(p_value)
    })
    
    return(hwe_pvalues)
  }
  return(NULL)
}

# Calculate sample relatedness (IBD/PI_HAT) using plink --genome (via plinkR)
calculate_sample_relatedness <- function(data) {
  if (!is.list(data) || !all(c("samples", "genotypes", "map") %in% names(data))) {
    return(NULL)
  }
  
  n_samples <- nrow(data$samples)
  if (n_samples < 2) {
    return(data.frame(PI_HAT = numeric(0)))
  }
  
  # Try to use plink --genome if plinkR is available
  if (use_plinkR && requireNamespace("plinkR", quietly = TRUE)) {
    tryCatch({
      # Create temporary directory for PLINK files
      tmp_dir <- tempfile(pattern = "genovieweR_relatedness_")
      dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
      prefix <- file.path(tmp_dir, "temp_data")
      
      # Write PLINK files (.fam, .map, .ped)
      fam_data <- data.frame(
        FID = data$samples$Family_ID,
        IID = data$samples$Sample_ID,
        Father = 0,
        Mother = 0,
        Sex = 0,
        Phenotype = -9,
        stringsAsFactors = FALSE
      )
      write.table(fam_data, paste0(prefix, ".fam"), 
                  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
      
      if (ncol(data$map) >= 4) {
        map_data <- data.frame(
          Chromosome = data$map[, 1],
          SNP_ID = data$map[, 2],
          Genetic_Distance = data$map[, 3],
          Physical_Position = data$map[, 4],
          stringsAsFactors = FALSE
        )
        write.table(map_data, paste0(prefix, ".map"), 
                    row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
      }
      
      ped_data <- cbind(
        data$samples$Family_ID,
        data$samples$Sample_ID,
        0, 0, 0, -9,  # Father, Mother, Sex, Phenotype
        data$genotypes
      )
      write.table(ped_data, paste0(prefix, ".ped"), 
                  row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
      
      # Find plink executable
      plink_path <- tryCatch({
        if (exists("find_plink", where = "package:plinkR", mode = "function")) {
          plinkR::find_plink()
        } else {
          Sys.which("plink")
        }
      }, error = function(e) {
        Sys.which("plink")
      })
      
      if (!is.null(plink_path) && plink_path != "" && file.exists(plink_path)) {
        # Run plink --genome (for relatedness/IBD)
        genome_output <- paste0(prefix, "_genome")
        
        # First, prune for LD (recommended for relatedness calculation)
        prune_output <- paste0(prefix, "_pruned")
        system2(
          plink_path,
          args = c(
            "--file", prefix,
            "--allow-extra-chr",
            "--indep-pairwise", "50", "5", "0.2",
            "--out", prune_output
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        
        # Run --genome on pruned SNPs
        system2(
          plink_path,
          args = c(
            "--file", prefix,
            "--allow-extra-chr",
            "--extract", paste0(prune_output, ".prune.in"),
            "--genome",
            "--out", genome_output
          ),
          stdout = FALSE,
          stderr = FALSE
        )
        
        # Read .genome file
        genome_file <- paste0(genome_output, ".genome")
        if (file.exists(genome_file)) {
          genome_data <- read.table(genome_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
          
          if (nrow(genome_data) > 0 && "PI_HAT" %in% names(genome_data)) {
            # Clean up temporary files
            unlink(tmp_dir, recursive = TRUE, force = TRUE)
            
            return(genome_data)
          }
        }
      }
      
      # Clean up on failure
      unlink(tmp_dir, recursive = TRUE, force = TRUE)
      
    }, error = function(e) {
      # Fallback to simplified calculation
    })
  }
  
  # Fallback: Simplified IBS-based calculation (if plink not available or failed)
  geno_matrix <- data$genotypes
  relatedness_pairs <- list()
  
  # For large datasets, limit pairs to avoid computation explosion
  max_pairs <- min(1000, n_samples * (n_samples - 1) / 2)
  
  pair_count <- 0
  for (i in 1:(n_samples - 1)) {
    for (j in (i + 1):n_samples) {
      pair_count <- pair_count + 1
      if (pair_count > max_pairs) break
      
      geno_i <- geno_matrix[i, ]
      geno_j <- geno_matrix[j, ]
      
      valid_pos <- !is.na(geno_i) & !is.na(geno_j) & 
                  geno_i != "0 0" & geno_j != "0 0" &
                  geno_i != "N N" & geno_j != "N N" &
                  geno_i != "" & geno_j != ""
      
      if (sum(valid_pos) < 100) next
      
      shared_alleles <- 0
      total_alleles <- 0
      
      for (pos in which(valid_pos)[1:min(1000, sum(valid_pos))]) {  # Limit to 1000 markers for speed
        alleles_i <- unlist(strsplit(as.character(geno_i[pos]), " "))
        alleles_j <- unlist(strsplit(as.character(geno_j[pos]), " "))
        
        if (length(alleles_i) == 2 && length(alleles_j) == 2) {
          # Count shared alleles (simplified)
          if (alleles_i[1] == alleles_j[1] || alleles_i[1] == alleles_j[2]) shared_alleles <- shared_alleles + 1
          if (alleles_i[2] == alleles_j[1] || alleles_i[2] == alleles_j[2]) shared_alleles <- shared_alleles + 1
          total_alleles <- total_alleles + 2
        }
      }
      
      if (total_alleles > 0) {
        # Simplified PI_HAT approximation
        pi_hat <- shared_alleles / total_alleles
        
        relatedness_pairs[[length(relatedness_pairs) + 1]] <- data.frame(
          IID1 = data$samples$Sample_ID[i],
          IID2 = data$samples$Sample_ID[j],
          PI_HAT = pi_hat,
          stringsAsFactors = FALSE
        )
      }
    }
    if (pair_count > max_pairs) break
  }
  
  if (length(relatedness_pairs) > 0) {
    return(do.call(rbind, relatedness_pairs))
  } else {
    return(data.frame(PI_HAT = numeric(0)))
  }
}

# ============================================================================
# RUN APP
# ============================================================================
shinyApp(ui = ui, server = server)
