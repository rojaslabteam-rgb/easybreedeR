# Open BreedX (OBX) Clean Three-Panel Layout
# Version: 0.4.0 (Fixed Language Settings)
# Created: 2025-10-22
# Last Modified: 2025-10-31

library(shiny)
library(bslib)
library(shinyjs)
## Load shared translations and helpers (if available)
try({
  # Path assumed relative to app directory: easybreedeR/Language.R
  source(file.path("..", "Language.R"), local = FALSE)
}, silent = TRUE)

# Local wrapper to prefix translation keys for this app
get_label_local <- function(key, lang = NULL) {
  # prefix keys with pediviewer_ when not already prefixed
  prefixed <- if (startsWith(key, "pediviewer_")) key else paste0("pediviewer_", key)
  # if Language.R wasn't available, fallback to the key itself
  if (exists("get_label", mode = "function")) {
    get_label(prefixed, lang)
  } else {
    prefixed
  }
}
library(dplyr)
library(DT)
pedigreeTools_available <- requireNamespace("pedigreeTools", quietly = TRUE)
if (pedigreeTools_available) {
  library(pedigreeTools)
}
library(visNetwork)
library(igraph)
library(digest)
## Optional: used for interactive trend plot (hover tooltips)
## Kept optional to avoid hard dependency at install time.
if (requireNamespace("plotly", quietly = TRUE)) {
  library(plotly)
}

# Check if linkbreedeR is available for faster inbreeding calculation
use_linkbreedeR <- FALSE
tryCatch({
  if (requireNamespace("linkbreedeR", quietly = TRUE)) {
    use_linkbreedeR <- TRUE
    cat("✓ linkbreedeR available - will use fast inbupgf90 for inbreeding calculation\n")
  }
}, error = function(e) {
  if (pedigreeTools_available) {
    cat("Note: linkbreedeR not available, will use pedigreeTools for inbreeding calculation\n")
  } else {
    cat("Note: linkbreedeR and pedigreeTools not available; inbreeding fallback is limited\n")
  }
  use_linkbreedeR <- FALSE
})

# Try to load Rcpp QC function
use_rcpp <- FALSE
use_fast_inbreeding_cpp <- FALSE
get_pediviewer_dir <- function() {
  app_dir <- system.file("pedivieweR", package = "easybreedeR")
  if (nzchar(app_dir) && dir.exists(app_dir)) {
    return(app_dir)
  }
  dev_dir <- file.path(getwd(), "inst", "pedivieweR")
  if (dir.exists(dev_dir)) {
    return(dev_dir)
  }
  return(getwd())
}
tryCatch({
  library(Rcpp)
  app_dir <- get_pediviewer_dir()
  cpp_path <- file.path(app_dir, "pedigree_qc.cpp")
  if (file.exists(cpp_path)) {
    src_env <- environment()
    sourceCpp(cpp_path, env = src_env)
    use_rcpp <- TRUE
    cat("✓ Rcpp QC functions loaded successfully\n")
    # Check if Rcpp functions are available
    if (!exists("fast_find_deepest_ancestor")) {
      cat("Note: fast_find_deepest_ancestor not found, will use R version\n")
    }
    if (!exists("fast_lap_distribution")) {
      cat("Note: fast_lap_distribution not found, will use R version\n")
    }
    if (!exists("fast_lap_depths")) {
      cat("Note: fast_lap_depths not found, will use R version\n")
    }
    has_fast_inbreeding <- exists(
      "fast_inbreeding_cpp",
      mode = "function",
      envir = src_env,
      inherits = FALSE
    )
    if (!has_fast_inbreeding &&
        exists("fast_inbreeding_cpp", mode = "function", envir = .GlobalEnv)) {
      assign(
        "fast_inbreeding_cpp",
        get("fast_inbreeding_cpp", envir = .GlobalEnv),
        envir = src_env
      )
      has_fast_inbreeding <- TRUE
    }
    if (has_fast_inbreeding) {
      use_fast_inbreeding_cpp <- TRUE
      cat("✓ fast inbreeding C++ available - will use Rcpp method\n")
    } else {
      if (pedigreeTools_available) {
        cat("Note: fast_inbreeding_cpp not found, will use inbupgf90/pedigreeTools\n")
      } else {
        cat("Note: fast_inbreeding_cpp not found, will use inbupgf90 if available\n")
      }
    }
    if (exists("fast_top_contrib_cpp", mode = "function", envir = src_env, inherits = FALSE)) {
      cat("✓ fast_top_contrib_cpp available - will show Top 5 contributors\n")
    } else {
      cat("Note: fast_top_contrib_cpp not found - Top 5 contributors disabled\n")
    }
  } else {
    cat("Note: pedigree_qc.cpp not found at:", cpp_path, "\n")
  }
}, error = function(e) {
  cat("Note: Rcpp not available or compilation failed:", e$message, "\n")
  use_rcpp <- FALSE
  use_fast_inbreeding_cpp <- FALSE
})

# Suppress SASS color contrast warnings from bslib
# Set option to disable color contrast warnings (bslib >= 0.2.4)
options(bslib.color_contrast_warnings = FALSE)

purdue_theme <- bs_theme(
  version = 5,
  bg = "#ffffff",
  fg = "#333333",
  primary = "#CEB888",
  base_font = font_google("Crimson Text")
)

# ====== UI Definition ======
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
        opacity: 0.9;
        font-family: 'Crimson Text', 'Noto Sans SC', 'PingFang SC', 'Microsoft YaHei', 'Heiti SC', 'SimSun', 'Noto Sans', Arial, sans-serif;
      }
      .three-panel-container {
        display: flex;
        flex: 1;
        overflow: hidden;
        position: relative;
      }
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
      /* hide visible scrollbars but keep scroll behavior */
      .left-panel, .right-panel { -ms-overflow-style: none; scrollbar-width: none; }
      .left-panel::-webkit-scrollbar, .right-panel::-webkit-scrollbar { display: none; }
      .right-panel.hidden {
        width: 0;
        padding: 0;
        overflow: hidden;
      }
      .center-panel {
        flex: 1;
        overflow-y: auto;
        overflow-x: hidden;
        padding: 20px;
        background-color: #FFFFFF;
      }
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
      .panel-section {
        background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%);
        border: 2px solid #CFB991;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 20px;
        box-shadow: 0 2px 8px rgba(206, 184, 136, 0.15);
      }
      /* Right panel tables: contain overflow, center and truncate long IDs with ellipsis */
      .right-panel .panel-section .dataTables_wrapper {
        overflow-x: auto;
        max-width: 100%;
      }
      .right-panel .panel-section table.dataTable {
        table-layout: fixed;
        width: 100% !important;
      }
      .right-panel .panel-section table.dataTable th.dt-id-cell,
      .right-panel .panel-section table.dataTable td.dt-id-cell {
        max-width: 140px;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        text-align: center;
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
      /* 统一按钮样式 - 字号和高度（排除侧边栏按钮） */
      .btn {
        font-size: 1rem !important;
        padding: 10px 16px !important;
        height: auto !important;
        min-height: 40px !important;
        line-height: 1.4 !important;
      }
      .btn-sm {
        font-size: 0.875rem !important;
        padding: 6px 12px !important;
        min-height: 32px !important;
      }
      /* 侧边栏按钮保持原有样式 */
      .toggle-btn-left .btn,
      .toggle-btn-right .btn {
        font-size: 1.1rem !important;
        padding: 0 !important;
        min-height: 64px !important;
        height: 64px !important;
      }
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
      .btn-secondary {
        background-color: #95A5A6 !important;
        border-color: #95A5A6 !important;
      }
      .btn-secondary:hover {
        background-color: #7F8C8D !important;
      }
      .vis-network {
        border: 2px solid #CEB888;
        border-radius: 8px;
      }
      .left-panel::-webkit-scrollbar, 
      .right-panel::-webkit-scrollbar,
      .center-panel::-webkit-scrollbar {
        width: 10px;
      }
      .left-panel::-webkit-scrollbar-thumb,
      .right-panel::-webkit-scrollbar-thumb,
      .center-panel::-webkit-scrollbar-thumb {
        background: #CEB888;
        border-radius: 5px;
      }
    "))
  ),
  
  # Title Bar
  div(class = "title-bar", 
      uiOutput("app_title_ui"),
      uiOutput("app_subtitle_ui")
  ),
  
  # Toggle buttons
  div(id = "toggleLeftBtn", class = "toggle-btn-left",
      actionButton("toggleLeftPanel", HTML("&#10094;"),
                   class = "btn btn-sm",
                   title = "Show/Hide Data Controls")
  ),
  
  div(id = "toggleRightBtn", class = "toggle-btn-right",
      actionButton("toggleRightPanel", HTML("&#10095;"),
                   class = "btn btn-sm",
                   title = "Show/Hide Analysis")
  ),
  
  # Three-panel layout
  div(class = "three-panel-container",
      
      # LEFT PANEL - Data Upload & Controls
      div(id = "leftPanel", class = "left-panel",
          
          div(class = "panel-section",
              uiOutput("left_upload_title"),
              uiOutput("file_upload_input"),
              div(
                style = "display: flex; gap: 10px; margin-top: 10px;",
                div(style = "flex: 1;", 
              uiOutput("sep_selector"))
              ),
              hr(),
              uiOutput("auto_process_checkbox"),
              uiOutput("auto_process_help")
          ),
          
          div(class = "panel-section",
              h4(textOutput("column_mapping_title"), class = "section-title"),
              uiOutput("col_mapping_ui"),
              conditionalPanel(
                condition = "false",
                uiOutput("data_validation_status_ui")
              ),
              hr(),
              uiOutput("start_analysis_button"),
              conditionalPanel(
                condition = "!input.auto_process",
                uiOutput("process_data_button")
              ),
              conditionalPanel(
                condition = "input.auto_process",
                div(class = "alert alert-info mt-2", style = "padding: 8px; font-size: 0.9rem;",
                    "🔄 Auto-processing enabled - Data will be processed automatically when validation passes")
              )
          ),
          
          div(class = "panel-section",
              uiOutput("quick_stats_title"),
              verbatimTextOutput("quick_stats", placeholder = TRUE)
          )
      ),
      
      # CENTER PANEL - Visualization
      div(class = "center-panel",
          navset_card_tab(
            id = "mainTabs",
            nav_panel(
              textOutput("tab_network_title"),
              value = "viz",
              div(style = "margin-top: 10px;",
                  uiOutput("network_controls_ui"),
                  uiOutput("viz_info"),
                  
                  # Legend for node colors and sizes
                  uiOutput("network_legend_ui"),
                  
                  visNetworkOutput("pedigree_network", height = "600px"),
                  
                  # Custom right-click context menu (draggable)
                  tags$div(
                    id = "contextMenu",
                    style = "display:none; position:absolute; background:white; border:2px solid #B89D5D; 
                             box-shadow: 3px 3px 12px rgba(0,0,0,0.3); z-index:9999; border-radius:6px;
                             min-width:220px;",
                    tags$div(
                      style = "padding:10px 14px; cursor:move; border-bottom:2px solid #CEB888;
                               background:linear-gradient(135deg, #CEB888, #B89D5D); 
                               font-weight:700; color:#fff; border-radius:4px 4px 0 0;
                               text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
                               display:flex; justify-content:space-between; align-items:center;",
                      id = "menuTitle",
                      tags$span("🐾 Animal: --"),
                      tags$button(
                        id = "menuClose",
                        style = "background:transparent; border:none; color:white; font-size:20px;
                                 cursor:pointer; padding:0; width:24px; height:24px; line-height:20px;
                                 border-radius:3px; transition:background 0.2s;",
                        onmouseover = "this.style.background='rgba(255,255,255,0.2)';",
                        onmouseout = "this.style.background='transparent';",
                        "×"
                      )
                    ),
                    tags$div(
                      id = "menuDownload",
                      style = "padding:12px 14px; cursor:pointer; transition: all 0.2s;
                               display:flex; align-items:center; gap:8px; font-size:15px;
                               border-bottom:1px solid #eee;",
                      onmouseover = "this.style.background='#f0f0f0'; this.style.paddingLeft='18px';",
                      onmouseout = "this.style.background='white'; this.style.paddingLeft='14px';",
                      tags$span(style = "font-size:18px;", "📥"),
                      tags$span("Download Relatives")
                    ),
                    tags$div(
                      style = "padding:10px 14px; font-size:13px; color:#555;",
                      tags$div(
                        style = "margin-bottom:5px; font-weight:600;",
                        "🔍 Highlight Generations:"
                      ),
                      tags$div(
                        style = "display:flex; gap:5px; flex-wrap:wrap;",
                        tags$button(
                          class = "gen-btn",
                          `data-gen` = "1",
                          style = "padding:4px 10px; border:1px solid #CEB888; background:white; 
                                   border-radius:3px; cursor:pointer; font-size:12px; transition:all 0.2s;",
                          onmouseover = "this.style.background='#CEB888'; this.style.color='white';",
                          onmouseout = "this.style.background='white'; this.style.color='black';",
                          "1"
                        ),
                        tags$button(
                          class = "gen-btn",
                          `data-gen` = "2",
                          style = "padding:4px 10px; border:1px solid #CEB888; background:white; 
                                   border-radius:3px; cursor:pointer; font-size:12px; transition:all 0.2s;",
                          onmouseover = "this.style.background='#CEB888'; this.style.color='white';",
                          onmouseout = "this.style.background='white'; this.style.color='black';",
                          "2"
                        ),
                        tags$button(
                          class = "gen-btn",
                          `data-gen` = "3",
                          style = "padding:4px 10px; border:1px solid #CEB888; background:white; 
                                   border-radius:3px; cursor:pointer; font-size:12px; transition:all 0.2s;",
                          onmouseover = "this.style.background='#CEB888'; this.style.color='white';",
                          onmouseout = "this.style.background='white'; this.style.color='black';",
                          "3"
                        ),
                        tags$button(
                          class = "gen-btn",
                          `data-gen` = "5",
                          style = "padding:4px 10px; border:1px solid #CEB888; background:white; 
                                   border-radius:3px; cursor:pointer; font-size:12px; transition:all 0.2s;",
                          onmouseover = "this.style.background='#CEB888'; this.style.color='white';",
                          onmouseout = "this.style.background='white'; this.style.color='black';",
                          "5"
                        ),
                        tags$button(
                          class = "gen-btn",
                          `data-gen` = "10",
                          style = "padding:4px 10px; border:1px solid #B89D5D; background:#CEB888; color:white;
                                   border-radius:3px; cursor:pointer; font-size:12px; font-weight:600;
                                   transition:all 0.2s;",
                          onmouseover = "this.style.background='#B89D5D';",
                          onmouseout = "this.style.background='#CEB888';",
                          "ALL"
                        )
                      )
                    )
                  )
              )
            ),
            nav_panel(
              textOutput("tab_data_title"),
              value = "data",
              div(style = "margin-top: 10px;",
                  DTOutput("data_preview")
              )
            ),
            nav_panel(
              textOutput("tab_qc_title"),
              value = "qc",
              div(style = "margin-top: 10px;",
                verbatimTextOutput("qc_report"),
                div(style = "margin-top:10px; display: flex; gap: 10px;",
                    downloadButton("download_qc_full_report", "📥 Download QC Report",
                                   class = "btn btn-info btn-sm"),
                    downloadButton("download_fixed_pedigree", "📥 Download Fixed Pedigree",
                                   class = "btn btn-primary btn-sm")
                )
              )
            ),
            nav_panel(
              "Pedigree Structure",
              value = "structure",
              div(style = "margin-top: 10px;",
                verbatimTextOutput("pedigree_structure_report"),
                div(style = "margin-top:10px; display: flex; gap: 10px;",
                    downloadButton("download_structure_report", "📥 Download Structure Report",
                                   class = "btn btn-info btn-sm")
                )
              )
            )
          )
      ),
      
      # RIGHT PANEL - Analysis & Results
      div(id = "rightPanel", class = "right-panel",
          
          div(class = "panel-section",
              uiOutput("inbreeding_analysis_title"),
              conditionalPanel(
                condition = "!input.auto_process",
                actionButton("calc_f", "Calculate F Coefficients", class = "btn-primary w-100")
              ),
              conditionalPanel(
                condition = "input.auto_process",
                div(class = "alert alert-success", style = "padding: 8px; font-size: 0.9rem; margin-bottom: 10px;",
                    "✓ Auto-calculation enabled")
              ),
              # Progress bar for inbreeding calculation
              uiOutput("f_calculation_progress"),
              hr(),
              verbatimTextOutput("f_summary"),
              hr(),
              h5(textOutput("top10_inbred_title"), style = "font-size: 0.95rem; font-weight: 600;"),
              DTOutput("f_table_top"),
              uiOutput("download_all_f_button"),
              hr(),
              h5(textOutput("top10_sire_title"), style = "font-size: 0.95rem; font-weight: 600;"),
              DTOutput("sire_top_table"),
              downloadButton("download_sire_descendants", "Download All Sires", class = "btn-sm btn-secondary mt-2 w-100"),
              hr(),
              h5(textOutput("top10_dam_title"), style = "font-size: 0.95rem; font-weight: 600;"),
              DTOutput("dam_top_table"),
              downloadButton("download_dam_descendants", "Download All Dams", class = "btn-sm btn-secondary mt-2 w-100")
          ),
          
          div(class = "panel-section",
              h4(textOutput("selected_animal_export_title"), class = "section-title"),
              uiOutput("selected_node_info"),
              uiOutput("download_selected_range_button"),
              uiOutput("export_scope_help")
          ),
          
          conditionalPanel(
            condition = "false",
            div(class = "panel-section",
                h4(textOutput("smart_visualization_title"), class = "section-title"),
                hr(),
                uiOutput("node_size_slider"),
                uiOutput("show_labels_checkbox")
            )
          )
      )
  ),
  
  # JavaScript for panel toggling and context menu
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      var leftPanelOpen = true;
      var rightPanelOpen = true;
      var currentNode = null;
      var isDragging = false;
      var dragOffsetX = 0;
      var dragOffsetY = 0;
      var menuElement = null;
      
      $(document).on('click', '#toggleLeftPanel', function() {
        leftPanelOpen = !leftPanelOpen;
        if (leftPanelOpen) {
          $('#leftPanel').removeClass('hidden');
          $('#toggleLeftPanel').html('&#10094;');
        } else {
          $('#leftPanel').addClass('hidden');
          $('#toggleLeftPanel').html('&#10095;');
        }
      });
      
      $(document).on('click', '#toggleRightPanel', function() {
        rightPanelOpen = !rightPanelOpen;
        if (rightPanelOpen) {
          $('#rightPanel').removeClass('hidden');
          $('#toggleRightPanel').html('&#10095;');
        } else {
          $('#rightPanel').addClass('hidden');
          $('#toggleRightPanel').html('&#10094;');
        }
      });
      
      // Handle right-click context menu
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'network_rightclick') {
          var data = event.value;
          currentNode = data.node;
          
          // Update menu title
          $('#menuTitle span').first().text('🐾 Animal: ' + data.node);
          
          // Sync menu buttons to current slider value
          var currentGen = parseInt($('#highlight_generations').val()) || 10;
          updateGenButtonStyles(currentGen);
          
          // Position menu below the network visualization
          var networkDiv = $('#pedigree_network');
          var networkOffset = networkDiv.offset();
          var networkHeight = networkDiv.height();
          var networkWidth = networkDiv.width();
          
          // Position at bottom center of network, with small offset down
          var left = networkOffset.left + (networkWidth / 2) - 110; // 110 is half of menu width (220px)
          var top = networkOffset.top + networkHeight + 10; // 10px below network
          
          // Show menu
          $('#contextMenu').css({
            left: left + 'px',
            top: top + 'px',
            display: 'block'
          });
        }
      });
      
      // Handle close button
      $(document).on('click', '#menuClose', function(e) {
        e.stopPropagation();
        $('#contextMenu').hide();
        currentNode = null;
      });
      
      // Make menu draggable (only from title bar, not close button)
      var menuElement = null;
      
      $(document).on('mousedown', '#menuTitle', function(e) {
        // Don't drag when clicking close button or download button
        if ($(e.target).closest('#menuClose').length || $(e.target).closest('#menuDownload').length) {
          return;
        }
        
        isDragging = true;
        menuElement = $('#contextMenu');
        var menuPos = menuElement.offset();
        dragOffsetX = e.pageX - menuPos.left;
        dragOffsetY = e.pageY - menuPos.top;
        
        $(this).css('cursor', 'grabbing');
        e.preventDefault();
        e.stopPropagation();
      });
      
      $(document).on('mousemove', function(e) {
        if (isDragging && menuElement) {
          menuElement.css({
            left: (e.pageX - dragOffsetX) + 'px',
            top: (e.pageY - dragOffsetY) + 'px'
          });
          e.preventDefault();
        }
      });
      
      $(document).on('mouseup', function(e) {
        if (isDragging) {
          isDragging = false;
          $('#menuTitle').css('cursor', 'move');
          e.stopPropagation();
        }
      });
      
      // Handle menu download click
      $(document).on('click', '#menuDownload', function() {
        if (currentNode) {
          Shiny.setInputValue('trigger_download', currentNode, {priority: 'event'});
          $('#contextMenu').hide();
        }
      });
      
      // Handle generation button clicks
      $(document).on('click', '.gen-btn', function(e) {
        e.stopPropagation();
        var generations = parseInt($(this).attr('data-gen'));
        
        // Update slider value in Shiny (this will trigger the input change)
        Shiny.setInputValue('highlight_generations', generations);
        
        // Update visual state of buttons
        updateGenButtonStyles(generations);
        
        // Re-trigger highlight with new generation setting
        if (currentNode) {
          Shiny.setInputValue('selected_node_for_highlight', currentNode, {priority: 'event'});
        }
      });
      
      // Function to update generation button styles
      function updateGenButtonStyles(selectedGen) {
        $('.gen-btn').each(function() {
          var btnGen = parseInt($(this).attr('data-gen'));
          if (btnGen === selectedGen) {
            $(this).css({
              'background': '#CEB888',
              'color': 'white',
              'font-weight': '600',
              'border': '1px solid #B89D5D'
            });
          } else {
            $(this).css({
              'background': 'white',
              'color': 'black',
              'font-weight': 'normal',
              'border': '1px solid #CEB888'
            });
          }
        });
      }
      
      // Initialize button styles on page load and when slider changes
      setTimeout(function() {
        var initialValue = $('#highlight_generations').val();
        if (initialValue) {
          updateGenButtonStyles(parseInt(initialValue));
        }
      }, 500);
      
      // Listen to slider changes via DOM event
      $(document).on('input change', '#highlight_generations', function() {
        var value = parseInt($(this).val());
        updateGenButtonStyles(value);
      });
      
      // Also listen to Shiny input changes as backup
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'highlight_generations') {
          var value = parseInt(event.value);
          updateGenButtonStyles(value);
        }
      });
      
      // Handle download file message from server
      Shiny.addCustomMessageHandler('downloadFile', function(message) {
        var blob = new Blob([message.content], {type: 'text/csv;charset=utf-8;'});
        var link = document.createElement('a');
        if (link.download !== undefined) {
          var url = URL.createObjectURL(blob);
          link.setAttribute('href', url);
          link.setAttribute('download', message.filename);
          link.style.visibility = 'hidden';
          document.body.appendChild(link);
          link.click();
          document.body.removeChild(link);
          URL.revokeObjectURL(url);
        }
      });
      
      // Handle highlighting ancestors
      Shiny.addCustomMessageHandler('highlightDescendants', function(message) {
        var network = $('#pedigree_network').data('visNetwork');
        if (network) {
          // Get all nodes to highlight (includes selected and ancestors)
          var allHighlight = message.all_nodes || [];
          
          // Use visNetwork's selectNodes method to highlight all
          network.selectNodes(allHighlight, true);
        }
      });
      
      // Handle re-trigger highlight when slider changes
      Shiny.addCustomMessageHandler('retriggerHighlight', function(message) {
        var nodeId = message.node;
        // Re-trigger the highlight with new generation setting
        Shiny.setInputValue('selected_node_for_highlight', nodeId, {priority: 'event'});
      });
    });
  "))
)

# --------------- SERVER ----------------
server <- function(input, output, session) {
  # Resolve language passed by Suite (via ?lang=) or default
  current_lang <- reactive({
    if (exists("resolve_suite_lang", mode = "function")) {
      # resolve_suite_lang handles mapping and defaults
      resolve_suite_lang(session)
    } else {
      # fallback: try query param or default to 'en'
      q <- isolate(session$clientData$url_search)
      if (nzchar(q) && grepl("lang=", q)) {
        m <- regmatches(q, regexpr("lang=[^&]*", q))
        sub("lang=", "", m)
      } else {
        "en"
      }
    }
  })

 # Render app title and subtitle using translations
output$app_title_ui <- renderUI({
  # map suite lang for app if helper exists
  lang <- if (exists("map_suite_lang_for_app", mode = "function")) 
    map_suite_lang_for_app(current_lang(), "pediviewer") 
  else 
    current_lang()
  
  title_text <- get_label_local("app_name", lang)

  tags$h1(
    title_text,
    style = "margin:0; color:#000; font-weight:700; 
             font-size:clamp(1.5rem, 3vw, 2.5rem); 
             font-family:'Crimson Text','Noto Sans SC','PingFang SC','Microsoft YaHei','Heiti SC','SimSun','Noto Sans',Arial,sans-serif;"
  )
})

output$app_subtitle_ui <- renderUI({
  lang <- if (exists("map_suite_lang_for_app", mode = "function")) 
    map_suite_lang_for_app(current_lang(), "pediviewer") 
  else 
    current_lang()
  
  subtitle_text <- get_label_local("app_subtitle", lang)

  tags$p(
    subtitle_text,
    style = "margin:5px 0 0 0; font-size:1rem; color:#000;"
  )
})

  # Left panel localized controls
  output$file_upload_input <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    fileInput("file", get_label_local("choose_pedigree_file", lang), accept = c(".csv", ".txt", ".ped"))
  })

  output$sep_selector <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    div(
      radioButtons("sep", get_label_local("separator", lang),
                   choices = c(Comma = ",", Tab = "\t", Space = " "),
                   selected = ",", inline = TRUE),
      actionButton(
        "clear_all",
        "Clear All",
        class = "btn btn-secondary btn-sm",
        style = "margin: 6.5px auto 0; width: 245px; display: block;"
      )
    )
  })

  output$auto_process_checkbox <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    div(
      style = "display: none;",
      checkboxInput("auto_process", get_label_local("auto_process", lang), value = TRUE)
    )
  })

  output$auto_process_help <- renderUI({
    NULL
  })

  output$column_mapping_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("column_mapping", lang)
  })

  output$start_analysis_button <- renderUI({
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) {
      return(NULL)
    }
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    div(
      style = "margin-top: 15px;",
      actionButton("start_analysis", "🚀 Start Analysis", 
                   class = "btn-primary w-100", 
                   style = "font-weight: bold;"),
      tags$small(
        "Click to begin processing and analyzing the pedigree data after selecting column mappings.",
        style = "display: block; margin-top: 8px; color: #666; font-style: italic;"
      )
    )
  })

  output$process_data_button <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    actionButton("process", get_label_local("process_data", lang), class = "btn-primary w-100 mt-2")
  })

  # Network controls & legend
  output$network_controls_ui <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    div(style = "margin-bottom: 15px;",
      textInput("individual_search", get_label_local("search_individual", lang),
                placeholder = get_label_local("search_placeholder", lang), width = "100%"),
      div(style = "margin-top: 10px;",
        sliderInput("search_depth", get_label_local("search_depth", lang), min = 1, max = 10, value = 5, step = 1, width = "100%"),
        actionButton("search_individual", get_label_local("visualize_btn", lang), class = "btn-primary"),
        actionButton("show_highest_f", get_label_local("show_highest_f", lang), class = "btn-secondary"),
        actionButton("refresh_viz", get_label_local("refresh", lang), class = "btn-secondary")
      )
    )
  })

  output$network_legend_ui <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    div(style = "margin-bottom: 15px; padding: 10px; background: #f8f8f8; border-radius: 5px; border: 1px solid #ddd;",
      h5(get_label_local("network_legend", lang), style = "margin-top: 0; color: #B89D5D; font-weight: bold;"),
      div(style = "display: flex; flex-wrap: wrap; gap: 15px;",
        div(style = "display: flex; align-items: center; gap: 5px;",
            div(style = "width: 20px; height: 20px; background: #87CEEB; border: 2px solid #4682B4; border-radius: 50%;"),
            span(get_label_local("legend_male", lang), style = "font-size: 14px;")
        ),
        div(style = "display: flex; align-items: center; gap: 5px;",
            div(style = "width: 20px; height: 20px; background: #FFB6C1; border: 2px solid #DC143C; border-radius: 50%;"),
            span(get_label_local("legend_female", lang), style = "font-size: 14px;")
        ),
        div(style = "display: flex; align-items: center; gap: 5px;",
            div(style = "width: 20px; height: 20px; background: #D3D3D3; border: 2px solid #696969; border-radius: 50%;"),
            span(get_label_local("legend_unknown", lang), style = "font-size: 14px;")
        ),
        div(style = "display: flex; align-items: center; gap: 5px;",
            div(style = "width: 20px; height: 20px; background: #FF0000; border: 2px solid #FF0000; clip-path: polygon(50% 0%, 61% 35%, 98% 35%, 68% 57%, 79% 91%, 50% 70%, 21% 91%, 32% 57%, 2% 35%, 39% 35%);"),
            span(get_label_local("legend_target", lang), style = "font-size: 14px;")
        )
      ),
      div(style = "margin-top: 10px; font-size: 14px; color: #666;",
          get_label_local("legend_hint", lang))
    )
  })

  # Right panel localized pieces
  output$calc_f_button <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    actionButton("calc_f", get_label_local("process_data", lang), class = "btn-primary w-100")
  })

  output$top10_inbred_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("top10_inbred", lang)
  })

output$top10_sire_title <- renderText({
  "Top 10 Most Influential Sires"
})

output$top10_dam_title <- renderText({
  "Top 10 Most Influential Dams"
})

  output$download_all_f_button <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    downloadButton("download_f", get_label_local("download_all_f", lang), class = "btn-sm btn-secondary mt-2 w-100")
  })

  output$selected_animal_export_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("selected_animal_export", lang)
  })

  output$download_selected_range_button <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    downloadButton("download_relatives", get_label_local("download_selected_range", lang), class = "btn-primary w-100 mt-2")
  })

  output$export_scope_help <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    helpText(get_label_local("export_scope_help", lang))
  })

  output$smart_visualization_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("smart_visualization", lang)
  })

  output$node_size_slider <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    sliderInput("node_size", get_label_local("base_node_size", lang), min = 5, max = 50, value = 20, step = 5)
  })

  output$show_labels_checkbox <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    checkboxInput("show_labels", get_label_local("show_labels", lang), value = TRUE)
  })
  
  # Set maximum upload size to 10GB
  options(shiny.maxRequestSize = 10 * 1024^3)
  
  # Storage for fixed/updated raw data
  raw_data_storage <- reactiveVal(NULL)
  # Store last fix summary for QC report
  last_fix_summary <- reactiveVal(NULL)
  
  # Track if analysis has been started manually
  analysis_started <- reactiveVal(FALSE)
  
  # QC Modal System - Track detected issues and pending data
  qc_issues <- reactiveVal(NULL)
  pending_data <- reactiveVal(NULL)
  qc_fix_summary <- reactiveVal(NULL)
  
  # Read raw data
  raw_data <- reactive({
    # If we have stored (fixed) data, use it
    stored_data <- raw_data_storage()
    if (!is.null(stored_data)) {
      return(stored_data)
    }
    
    # Otherwise read from uploaded file
    req(input$file)
    tryCatch({
      cache_base_dir <- "pedigree_cache"
      raw_cache_dir <- file.path(cache_base_dir, "raw_data")
      if (!dir.exists(raw_cache_dir)) {
        dir.create(raw_cache_dir, showWarnings = FALSE, recursive = TRUE)
      }
      file_info <- file.info(input$file$datapath)
      cache_key <- digest::digest(
        list(
          name = input$file$name,
          size = file_info$size,
          mtime = as.numeric(file_info$mtime),
          sep = input$sep
        ),
        algo = "xxhash64"
      )
      cache_path <- file.path(raw_cache_dir, paste0(cache_key, ".rds"))
      
      if (file.exists(cache_path)) {
        return(readRDS(cache_path))
      }
      
      df <- data.table::fread(
        input$file$datapath,
        header = TRUE,
        sep = input$sep,
        data.table = FALSE,
        stringsAsFactors = FALSE,
        quote = "",
        check.names = FALSE,
        encoding = "UTF-8",
        strip.white = TRUE,
        fill = TRUE,
        na.strings = c("", "NA"),
        showProgress = TRUE
      )
      
      tryCatch(saveRDS(df, cache_path), error = function(e) NULL)
      df
    }, error = function(e) {
      showNotification(paste("Error reading file:", e$message), type = "error")
      NULL
    })
  })
  
  # Auto-detect columns from data
  auto_detect_columns <- function(cols) {
    lower_cols <- tolower(cols)
    
    # Detect required columns
    id_col <- cols[which.max(lower_cols %in% c("id", "animal", "indiv", "individual", "label"))]
    sire_col <- cols[which.max(lower_cols %in% c("sire", "father", "dad", "male_parent"))]
    dam_col <- cols[which.max(lower_cols %in% c("dam", "mother", "mom", "female_parent"))]
    
    # Detect optional sex column - only if it exists and has valid values
    sex_col <- NULL
    sex_candidates <- cols[lower_cols %in% c("sex", "gender")]
    
    if (length(sex_candidates) > 0) {
      # Check if the first candidate has meaningful values
      sex_candidate <- sex_candidates[1]
      # We'll let the user decide if they want to use it
      sex_col <- sex_candidate
    }
    
    # Detect optional birthdate column
    birthdate_col <- NULL
    birthdate_candidates <- cols[lower_cols %in% c("birthdate", "birth_date", "birth", "dob", "date_of_birth", "date")]
    
    if (length(birthdate_candidates) > 0) {
      birthdate_col <- birthdate_candidates[1]
    }
    
    list(
      id = id_col,
      sire = sire_col,
      dam = dam_col,
      sex = sex_col,
      birthdate = birthdate_col
    )
  }
  
  # Helper: treat NA/""/"0" as missing parent
  is_missing_parent <- function(x) {
    x_char <- as.character(x)
    is.na(x) | x_char == "" | x_char == "0"
  }

  # Helper: get basic pedigree stats (prefer Rcpp if available)
  get_basic_stats <- function(ped) {
    n <- nrow(ped)
    if (exists("use_rcpp") && use_rcpp && exists("fast_pedigree_qc", mode = "function")) {
      ids_char <- as.character(ped$ID)
      sires_char <- as.character(ifelse(is.na(ped$Sire), "NA", ped$Sire))
      dams_char <- as.character(ifelse(is.na(ped$Dam), "NA", ped$Dam))
      if ("Sex" %in% names(ped) && exists("fast_pedigree_qc_sex", mode = "function")) {
        qc <- fast_pedigree_qc_sex(ids_char, sires_char, dams_char, as.character(ped$Sex))
      } else {
        qc <- fast_pedigree_qc(ids_char, sires_char, dams_char)
      }
      if (!is.null(qc$founders) && !is.null(qc$with_both_parents) &&
          !is.null(qc$only_sire) && !is.null(qc$only_dam)) {
        founders <- as.integer(qc$founders)
        both_parents <- as.integer(qc$with_both_parents)
        only_sire <- as.integer(qc$only_sire)
        only_dam <- as.integer(qc$only_dam)
        return(list(
          founders = founders,
          non_founders = n - founders,
          both_parents = both_parents,
          only_sire = only_sire,
          only_dam = only_dam
        ))
      }
    }

    sire_missing <- is_missing_parent(ped$Sire)
    dam_missing <- is_missing_parent(ped$Dam)
    founders <- sum(sire_missing & dam_missing)
    list(
      founders = founders,
      non_founders = n - founders,
      both_parents = sum(!sire_missing & !dam_missing),
      only_sire = sum(!sire_missing & dam_missing),
      only_dam = sum(sire_missing & !dam_missing)
    )
  }

  # Helper: get extended stats (prefer Rcpp if available)
  get_extended_stats <- function(ped) {
    n <- nrow(ped)
    if (exists("use_rcpp") && use_rcpp && exists("fast_pedigree_qc", mode = "function")) {
      ids_char <- as.character(ped$ID)
      sires_char <- as.character(ifelse(is.na(ped$Sire), "NA", ped$Sire))
      dams_char <- as.character(ifelse(is.na(ped$Dam), "NA", ped$Dam))
      if ("Sex" %in% names(ped) && exists("fast_pedigree_qc_sex", mode = "function")) {
        qc <- fast_pedigree_qc_sex(ids_char, sires_char, dams_char, as.character(ped$Sex))
      } else {
        qc <- fast_pedigree_qc(ids_char, sires_char, dams_char)
      }
      if (!is.null(qc$unique_sires)) {
        return(list(
          unique_sires = as.integer(qc$unique_sires),
          unique_dams = as.integer(qc$unique_dams),
          total_sire_progeny = as.numeric(qc$total_sire_progeny),
          total_dam_progeny = as.numeric(qc$total_dam_progeny),
          individuals_with_progeny = as.integer(qc$individuals_with_progeny),
          individuals_without_progeny = as.integer(qc$individuals_without_progeny),
          founder_sires = as.integer(qc$founder_sires),
          founder_dams = as.integer(qc$founder_dams),
          founder_sire_progeny = as.numeric(qc$founder_sire_progeny),
          founder_dam_progeny = as.numeric(qc$founder_dam_progeny),
          founder_total_progeny = as.numeric(qc$founder_total_progeny),
          founder_no_progeny = as.integer(qc$founder_no_progeny),
          non_founder_sires = as.integer(qc$non_founder_sires),
          non_founder_dams = as.integer(qc$non_founder_dams),
          non_founder_sire_progeny = as.numeric(qc$non_founder_sire_progeny),
          non_founder_dam_progeny = as.numeric(qc$non_founder_dam_progeny)
        ))
      }
    }

    # Fallback to R calculations
    unique_sires <- unique(ped$Sire[!is.na(ped$Sire) & ped$Sire != "" & ped$Sire != "0"])
    unique_dams <- unique(ped$Dam[!is.na(ped$Dam) & ped$Dam != "" & ped$Dam != "0"])
    sire_progeny <- ped %>%
      filter(!is.na(Sire), Sire != "", Sire != "0") %>%
      count(Sire, name = "progeny_count")
    dam_progeny <- ped %>%
      filter(!is.na(Dam), Dam != "", Dam != "0") %>%
      count(Dam, name = "progeny_count")
    total_sire_progeny <- sum(sire_progeny$progeny_count)
    total_dam_progeny <- sum(dam_progeny$progeny_count)
    all_ids <- unique(ped$ID)
    individuals_with_progeny <- length(unique(c(unique_sires, unique_dams))[unique(c(unique_sires, unique_dams)) %in% all_ids])
    individuals_without_progeny <- n - individuals_with_progeny

    sire_missing <- is_missing_parent(ped$Sire)
    dam_missing <- is_missing_parent(ped$Dam)
    founder_ids <- ped$ID[sire_missing & dam_missing]
    founder_ped <- ped %>% filter(ID %in% founder_ids)
    founder_sires <- founder_ped %>% filter(ID %in% unique_sires) %>% pull(ID)
    founder_dams <- founder_ped %>% filter(ID %in% unique_dams) %>% pull(ID)
    founder_sire_progeny <- ped %>% filter(Sire %in% founder_ids) %>% nrow()
    founder_dam_progeny <- ped %>% filter(Dam %in% founder_ids) %>% nrow()
    founder_total_progeny <- ped %>% filter(Sire %in% founder_ids | Dam %in% founder_ids) %>% nrow()
    founder_no_progeny <- sum(sire_missing & dam_missing) - length(unique(c(founder_sires, founder_dams)))

    non_founder_ids <- ped$ID[!(sire_missing & dam_missing)]
    non_founder_sires <- unique_sires[unique_sires %in% non_founder_ids]
    non_founder_dams <- unique_dams[unique_dams %in% non_founder_ids]
    non_founder_sire_progeny <- ped %>% filter(Sire %in% non_founder_ids) %>% nrow()
    non_founder_dam_progeny <- ped %>% filter(Dam %in% non_founder_ids) %>% nrow()

    list(
      unique_sires = length(unique_sires),
      unique_dams = length(unique_dams),
      total_sire_progeny = total_sire_progeny,
      total_dam_progeny = total_dam_progeny,
      individuals_with_progeny = individuals_with_progeny,
      individuals_without_progeny = individuals_without_progeny,
      founder_sires = length(founder_sires),
      founder_dams = length(founder_dams),
      founder_sire_progeny = founder_sire_progeny,
      founder_dam_progeny = founder_dam_progeny,
      founder_total_progeny = founder_total_progeny,
      founder_no_progeny = founder_no_progeny,
      non_founder_sires = length(non_founder_sires),
      non_founder_dams = length(non_founder_dams),
      non_founder_sire_progeny = non_founder_sire_progeny,
      non_founder_dam_progeny = non_founder_dam_progeny
    )
  }

  # Comprehensive QC issue detection function with Rcpp acceleration
  detect_qc_issues <- function(df) {
    issues <- list(
      duplicates = list(),
      missing_ids = list(),
      dual_parent_role = list(),
      sex_mismatch = list(),
      missing_parents = list(),
      self_parenting = list(),
      loops = list(),
      birth_date_order = list(),
      has_errors = FALSE
    )
    
    # Missing or empty ID rows (cannot be used as pedigree nodes)
    missing_id_mask <- is.na(df$ID) | df$ID == "" | trimws(df$ID) == ""
    if (any(missing_id_mask)) {
      issues$missing_ids <- list(
        count = sum(missing_id_mask),
        rows = which(missing_id_mask)
      )
      issues$has_errors <- TRUE
    }
    
    # Try to use fast Rcpp version if available
    if (exists("use_rcpp") && use_rcpp && exists("fast_pedigree_qc") && exists("fast_detect_loops")) {
      tryCatch({
        # Convert to character and handle NAs
        ids_char <- as.character(df$ID)
        sires_char <- as.character(ifelse(is.na(df$Sire), "NA", df$Sire))
        dams_char <- as.character(ifelse(is.na(df$Dam), "NA", df$Dam))
        sex_char <- if ("Sex" %in% names(df)) as.character(df$Sex) else NULL
        
        # Call fast C++ QC function (sex-aware if available)
        if (!is.null(sex_char) && exists("fast_pedigree_qc_sex", mode = "function")) {
          qc_result <- fast_pedigree_qc_sex(ids_char, sires_char, dams_char, sex_char)
        } else {
          qc_result <- fast_pedigree_qc(ids_char, sires_char, dams_char)
        }
        
        # Extract duplicates
        if (length(qc_result$duplicate_ids) > 0) {
          issues$duplicates <- list(
            count = length(qc_result$duplicate_ids),
            ids = qc_result$duplicate_ids
          )
          issues$has_errors <- TRUE
        }
        
        # Extract self-parenting
        if (qc_result$self_parent_count > 0) {
          # Find which IDs have self-parenting
          self_sire <- !is.na(df$Sire) & df$Sire != "" & df$ID == df$Sire
          self_dam <- !is.na(df$Dam) & df$Dam != "" & df$ID == df$Dam
          issues$self_parenting <- list(
            count = qc_result$self_parent_count,
            ids = unique(df$ID[self_sire | self_dam])
          )
          issues$has_errors <- TRUE
        }
        
        # Extract missing parents
        if (length(qc_result$missing_sires) > 0 || length(qc_result$missing_dams) > 0) {
          issues$missing_parents <- list(
            sires = qc_result$missing_sires,
            dams = qc_result$missing_dams,
            total = length(qc_result$missing_sires) + length(qc_result$missing_dams)
          )
          issues$has_errors <- TRUE
        }

        # Check for individuals appearing as both sire and dam
        dual_role_ids <- qc_result$dual_role_ids
        if (length(dual_role_ids) > 0) {
          issues$dual_parent_role <- list(
            count = length(dual_role_ids),
            ids = dual_role_ids
          )
          issues$has_errors <- TRUE
        }

        # Check sex mismatch if Sex column exists
        if (!is.null(sex_char) && !is.null(qc_result$sex_mismatch_sire_count) &&
            (!is.null(qc_result$sex_mismatch_dam_count))) {
          if (qc_result$sex_mismatch_sire_count > 0 || qc_result$sex_mismatch_dam_count > 0) {
            issues$sex_mismatch <- list(
              sire_count = qc_result$sex_mismatch_sire_count,
              dam_count = qc_result$sex_mismatch_dam_count,
              sire_ids = qc_result$sex_mismatch_sire_ids,
              dam_ids = qc_result$sex_mismatch_dam_ids
            )
            issues$has_errors <- TRUE
          }
        }
        
        # Fast loop detection
        loop_result <- fast_detect_loops(ids_char, sires_char, dams_char)
        if (loop_result$count > 0) {
          issues$loops <- list(
            count = loop_result$count,
            cycles = loop_result$cycles
          )
          issues$has_errors <- TRUE
        }
        
        # Birth date order check (if birthdate column is available)
        # Check both original column name and renamed "Birthdate" column
        birthdate_col_name <- NULL
        if ("Birthdate" %in% names(df)) {
          birthdate_col_name <- "Birthdate"
        } else if (!is.null(input$birthdate_col) && input$birthdate_col != "" && input$birthdate_col %in% names(df)) {
          birthdate_col_name <- input$birthdate_col
        }
        
        if (!is.null(birthdate_col_name) && exists("check_birth_date_order", mode = "function")) {
          tryCatch({
            birthdate_vec <- df[[birthdate_col_name]]
            
            # Convert to numeric - handle different formats and invalid dates
            if (inherits(birthdate_vec, "Date") || inherits(birthdate_vec, "POSIXct")) {
              birthdate_numeric <- as.numeric(birthdate_vec)
            } else if (is.character(birthdate_vec)) {
              # Convert character dates one by one to handle invalid dates gracefully
              birthdate_numeric <- sapply(birthdate_vec, function(x) {
                if (is.na(x) || x == "" || x == "NA" || grepl("1900-01-00", x, fixed = TRUE)) {
                  return(NA_real_)
                }
                tryCatch({
                  date_val <- as.Date(x)
                  if (is.na(date_val)) {
                    return(NA_real_)
                  }
                  as.numeric(date_val)
                }, error = function(e) {
                  NA_real_
                })
              })
              # Convert to numeric vector (sapply returns array)
              birthdate_numeric <- as.numeric(birthdate_numeric)
            } else {
              birthdate_numeric <- as.numeric(birthdate_vec)
            }
            
            # Only proceed if we have valid birthdates
            if (sum(!is.na(birthdate_numeric)) > 0) {
              birthdate_result <- check_birth_date_order(ids_char, sires_char, dams_char, birthdate_numeric)
              if (birthdate_result$count > 0) {
                issues$birth_date_order <- list(
                  count = birthdate_result$count,
                  invalid_sire_count = birthdate_result$invalid_sire_count,
                  invalid_dam_count = birthdate_result$invalid_dam_count,
                  invalid_offspring_ids = birthdate_result$invalid_offspring_ids,
                  invalid_sire_ids = birthdate_result$invalid_sire_ids,
                  invalid_dam_ids = birthdate_result$invalid_dam_ids
                )
                issues$has_errors <- TRUE
              }
            }
          }, error = function(e) {
            cat("Birth date order check failed:", e$message, "\n")
          })
        }
        
        return(issues)
      }, error = function(e) {
        cat("Rcpp QC failed, falling back to R version:", e$message, "\n")
        # Fall through to R version below
      })
    }
    
    # Fallback R version (slower but always available)
    # 1. Check for duplicate IDs
    if (anyDuplicated(df$ID)) {
      dup_ids <- df$ID[duplicated(df$ID)]
      issues$duplicates <- list(
        count = length(unique(dup_ids)),
        ids = unique(dup_ids)
      )
      issues$has_errors <- TRUE
    }
    
    # 2. Check for self-parenting
    self_sire <- !is.na(df$Sire) & df$Sire != "" & df$ID == df$Sire
    self_dam <- !is.na(df$Dam) & df$Dam != "" & df$ID == df$Dam
    if (any(self_sire) || any(self_dam)) {
      issues$self_parenting <- list(
        count = sum(self_sire | self_dam),
        ids = unique(df$ID[self_sire | self_dam])
      )
      issues$has_errors <- TRUE
    }
    
    # 3. Check for missing parents (referenced but not in ID column)
    all_ids <- unique(df$ID)
    sires_mentioned <- unique(df$Sire[!is.na(df$Sire) & df$Sire != ""])
    dams_mentioned <- unique(df$Dam[!is.na(df$Dam) & df$Dam != ""])
    
    missing_sires <- setdiff(sires_mentioned, all_ids)
    missing_dams <- setdiff(dams_mentioned, all_ids)
    
    if (length(missing_sires) > 0 || length(missing_dams) > 0) {
      issues$missing_parents <- list(
        sires = missing_sires,
        dams = missing_dams,
        total = length(missing_sires) + length(missing_dams)
      )
      issues$has_errors <- TRUE
    }

    # 3b. Check for individuals appearing as both sire and dam
    dual_role_ids <- intersect(sires_mentioned, dams_mentioned)
    if (length(dual_role_ids) > 0) {
      issues$dual_parent_role <- list(
        count = length(dual_role_ids),
        ids = dual_role_ids
      )
      issues$has_errors <- TRUE
    }

    # 3c. Check sex mismatch if Sex column exists
    if ("Sex" %in% names(df)) {
      normalize_sex <- function(x) {
        x <- tolower(trimws(as.character(x)))
        ifelse(x %in% c("m", "male", "1"), "M",
               ifelse(x %in% c("f", "female", "2"), "F", NA_character_))
      }
      sex_map <- setNames(normalize_sex(df$Sex), df$ID)
      sire_sex <- sex_map[as.character(df$Sire)]
      dam_sex <- sex_map[as.character(df$Dam)]
      mismatch_sire <- !is.na(df$Sire) & df$Sire != "" & !is.na(sire_sex) & sire_sex != "M"
      mismatch_dam <- !is.na(df$Dam) & df$Dam != "" & !is.na(dam_sex) & dam_sex != "F"
      if (any(mismatch_sire) || any(mismatch_dam)) {
        issues$sex_mismatch <- list(
          sire_count = sum(mismatch_sire),
          dam_count = sum(mismatch_dam),
          sire_ids = unique(df$Sire[mismatch_sire]),
          dam_ids = unique(df$Dam[mismatch_dam])
        )
        issues$has_errors <- TRUE
      }
    }
    
    # 4. Check for loops (R version - simplified for speed)
    # Only check if dataset is not too large
    if (nrow(df) > 0 && nrow(df) < 50000) {
      tryCatch({
        # Build parent map using vectors for speed
        id_vec <- df$ID
        sire_vec <- df$Sire
        dam_vec <- df$Dam
        
        # Simple cycle detection: check if any ID appears in its own ancestry chain
        check_ancestry <- function(id, depth = 0, visited = character()) {
          if (depth > 20) return(NULL)  # Limit depth to avoid infinite loops
          if (id %in% visited) return(c(visited, id))  # Found cycle
          
          idx <- which(id_vec == id)
          if (length(idx) == 0) return(NULL)
          
          new_visited <- c(visited, id)
          
          # Check sire
          if (!is.na(sire_vec[idx[1]]) && sire_vec[idx[1]] != "") {
            result <- check_ancestry(sire_vec[idx[1]], depth + 1, new_visited)
            if (!is.null(result)) return(result)
          }
          
          # Check dam
          if (!is.na(dam_vec[idx[1]]) && dam_vec[idx[1]] != "") {
            result <- check_ancestry(dam_vec[idx[1]], depth + 1, new_visited)
            if (!is.null(result)) return(result)
          }
          
          return(NULL)
        }
        
        # Check a sample of IDs for loops (for performance)
        sample_size <- min(1000, nrow(df))
        sample_ids <- sample(id_vec, sample_size)
        
        loops_found <- list()
        for (id in sample_ids) {
          cycle <- check_ancestry(id)
          if (!is.null(cycle)) {
            loops_found[[length(loops_found) + 1]] <- cycle
            if (length(loops_found) >= 10) break  # Limit to 10 loops
          }
        }
        
        if (length(loops_found) > 0) {
          issues$loops <- list(
            count = length(loops_found),
            cycles = loops_found
          )
          issues$has_errors <- TRUE
        }
      }, error = function(e) {
        cat("Loop detection skipped due to error:", e$message, "\n")
      })
    }
    
    # 5. Check birth date order (R version - if birthdate column is available)
    # Check both original column name and renamed "Birthdate" column
    birthdate_col_name <- NULL
    if ("Birthdate" %in% names(df)) {
      birthdate_col_name <- "Birthdate"
    } else if (!is.null(input$birthdate_col) && input$birthdate_col != "" && input$birthdate_col %in% names(df)) {
      birthdate_col_name <- input$birthdate_col
    }
    
    if (!is.null(birthdate_col_name)) {
      tryCatch({
        birthdate_vec <- df[[birthdate_col_name]]
          
          # Convert to numeric - handle different formats and invalid dates
          if (inherits(birthdate_vec, "Date") || inherits(birthdate_vec, "POSIXct")) {
            birthdate_numeric <- as.numeric(birthdate_vec)
          } else if (is.character(birthdate_vec)) {
            # Convert character dates one by one to handle invalid dates gracefully
            birthdate_numeric <- sapply(birthdate_vec, function(x) {
              if (is.na(x) || x == "" || x == "NA" || grepl("1900-01-00", x, fixed = TRUE)) {
                return(NA_real_)
              }
              tryCatch({
                date_val <- as.Date(x)
                if (is.na(date_val)) {
                  return(NA_real_)
                }
                as.numeric(date_val)
              }, error = function(e) {
                NA_real_
              })
            })
            # Convert to numeric vector (sapply returns array)
            birthdate_numeric <- as.numeric(birthdate_numeric)
          } else {
            birthdate_numeric <- as.numeric(birthdate_vec)
          }
          
          # Build ID to birthdate mapping (only include valid dates)
          id_to_birthdate <- setNames(birthdate_numeric, df$ID)
          id_to_birthdate <- id_to_birthdate[!is.na(id_to_birthdate)]
          
          # Only proceed if we have valid birthdates
          if (length(id_to_birthdate) > 0) {
            # Check each individual
            invalid_offspring_ids <- character()
            invalid_sire_ids <- character()
            invalid_dam_ids <- character()
            invalid_sire_count <- 0
            invalid_dam_count <- 0
            
            for (i in seq_len(nrow(df))) {
              id <- df$ID[i]
              sire <- df$Sire[i]
              dam <- df$Dam[i]
              
              # Skip if individual has no birthdate
              if (!id %in% names(id_to_birthdate)) next
              
              offspring_date <- id_to_birthdate[id]
              has_issue <- FALSE
              problem_sire <- ""
              problem_dam <- ""
              
              # Check sire
              if (!is.na(sire) && sire != "" && sire %in% names(id_to_birthdate)) {
                sire_date <- id_to_birthdate[sire]
                if (offspring_date <= sire_date) {
                  problem_sire <- sire
                  invalid_sire_count <- invalid_sire_count + 1
                  has_issue <- TRUE
                }
              }
              
              # Check dam
              if (!is.na(dam) && dam != "" && dam %in% names(id_to_birthdate)) {
                dam_date <- id_to_birthdate[dam]
                if (offspring_date <= dam_date) {
                  problem_dam <- dam
                  invalid_dam_count <- invalid_dam_count + 1
                  has_issue <- TRUE
                }
              }
              
              if (has_issue) {
                invalid_offspring_ids <- c(invalid_offspring_ids, id)
                invalid_sire_ids <- c(invalid_sire_ids, problem_sire)
                invalid_dam_ids <- c(invalid_dam_ids, problem_dam)
              }
            }
            
            if (length(invalid_offspring_ids) > 0) {
              issues$birth_date_order <- list(
                count = length(invalid_offspring_ids),
                invalid_sire_count = invalid_sire_count,
                invalid_dam_count = invalid_dam_count,
                invalid_offspring_ids = invalid_offspring_ids,
                invalid_sire_ids = invalid_sire_ids,
                invalid_dam_ids = invalid_dam_ids
              )
              issues$has_errors <- TRUE
            }
          }
        }, error = function(e) {
          cat("Birth date order check failed:", e$message, "\n")
        })
      }
    
    return(issues)
  }
  
  # Auto-fix QC issues function
  fix_qc_issues <- function(df, issues) {
    fixed_summary <- list()
    
    # Fix 0: Remove rows with missing/empty IDs
    missing_id_mask <- is.na(df$ID) | df$ID == "" | trimws(df$ID) == ""
    if (any(missing_id_mask)) {
      removed_count <- sum(missing_id_mask)
      df <- df[!missing_id_mask, , drop = FALSE]
      fixed_summary$missing_ids <- paste0("Removed ", removed_count, " row(s) with missing or empty ID")
    }

    # Fix 0b: Remove parent IDs that appear as both sire and dam
    sires_mentioned <- unique(df$Sire[!is.na(df$Sire) & df$Sire != ""])
    dams_mentioned <- unique(df$Dam[!is.na(df$Dam) & df$Dam != ""])
    dual_role_ids <- intersect(sires_mentioned, dams_mentioned)
    if (length(dual_role_ids) > 0) {
      df$Sire[df$Sire %in% dual_role_ids] <- NA
      df$Dam[df$Dam %in% dual_role_ids] <- NA
      fixed_summary$dual_parent_role <- paste0("Set ", length(dual_role_ids), " dual-role parent ID(s) to NA")
    }

    # Fix 0c: Enforce sex roles if Sex column exists
    if ("Sex" %in% names(df)) {
      normalize_sex <- function(x) {
        x <- tolower(trimws(as.character(x)))
        ifelse(x %in% c("m", "male", "1"), "M",
               ifelse(x %in% c("f", "female", "2"), "F", NA_character_))
      }
      sex_map <- setNames(normalize_sex(df$Sex), df$ID)
      sire_sex <- sex_map[as.character(df$Sire)]
      dam_sex <- sex_map[as.character(df$Dam)]
      mismatch_sire <- !is.na(df$Sire) & df$Sire != "" & !is.na(sire_sex) & sire_sex != "M"
      mismatch_dam <- !is.na(df$Dam) & df$Dam != "" & !is.na(dam_sex) & dam_sex != "F"
      if (any(mismatch_sire) || any(mismatch_dam)) {
        df$Sire[mismatch_sire] <- NA
        df$Dam[mismatch_dam] <- NA
        fixed_summary$sex_mismatch <- paste0(
          "Set ", sum(mismatch_sire) + sum(mismatch_dam),
          " parent reference(s) with sex mismatch to NA"
        )
      }
    }
    
    # Fix 1: Remove duplicate IDs (keep first occurrence)
    if (length(issues$duplicates) > 0 && issues$duplicates$count > 0) {
      original_count <- nrow(df)
      df <- df[!duplicated(df$ID), ]
      removed_count <- original_count - nrow(df)
      fixed_summary$duplicates <- paste0("Removed ", removed_count, " duplicate record(s)")
    }
    
    # Fix 2: Remove self-parenting (set to NA)
    if (length(issues$self_parenting) > 0 && issues$self_parenting$count > 0) {
      self_sire <- !is.na(df$Sire) & df$Sire != "" & df$ID == df$Sire
      self_dam <- !is.na(df$Dam) & df$Dam != "" & df$ID == df$Dam
      
      df$Sire[self_sire] <- NA
      df$Dam[self_dam] <- NA
      
      fixed_summary$self_parenting <- paste0("Fixed ", issues$self_parenting$count, " self-parenting case(s)")
    }
    
    # Fix 3: Set missing parent references to NA
    if (length(issues$missing_parents) > 0 && issues$missing_parents$total > 0) {
      all_ids <- unique(df$ID)
      
      # Set invalid sire references to NA
      invalid_sires <- !is.na(df$Sire) & df$Sire != "" & !(df$Sire %in% all_ids)
      df$Sire[invalid_sires] <- NA
      
      # Set invalid dam references to NA
      invalid_dams <- !is.na(df$Dam) & df$Dam != "" & !(df$Dam %in% all_ids)
      df$Dam[invalid_dams] <- NA
      
      fixed_summary$missing_parents <- paste0("Set ", issues$missing_parents$total, " missing parent reference(s) to NA")
    }
    
    # Fix 4: Break circular references (loops) by removing oldest parent link in cycle
    if (length(issues$loops) > 0 && issues$loops$count > 0) {
      total_breaks <- 0
      for (cycle in issues$loops$cycles) {
        if (length(cycle) >= 2) {
          # Break the cycle by removing the parent link from the last node to the first
          child <- cycle[length(cycle) - 1]
          parent <- cycle[length(cycle)]
          
          # Find which parent column to clear
          child_idx <- which(df$ID == child)
          if (length(child_idx) > 0) {
            if (!is.na(df$Sire[child_idx]) && df$Sire[child_idx] == parent) {
              df$Sire[child_idx] <- NA
              total_breaks <- total_breaks + 1
            } else if (!is.na(df$Dam[child_idx]) && df$Dam[child_idx] == parent) {
              df$Dam[child_idx] <- NA
              total_breaks <- total_breaks + 1
            }
          }
        }
      }
      fixed_summary$loops <- paste0("Broke ", total_breaks, " circular reference(s) in ", issues$loops$count, " loop(s)")
    }
    
    # Fix 5: Set invalid birth dates to 0 (if birthdate column exists and errors detected)
    if (length(issues$birth_date_order) > 0 && issues$birth_date_order$count > 0) {
      # Check if birthdate column exists (could be original name or already renamed)
      birthdate_col_name <- NULL
      if ("Birthdate" %in% names(df)) {
        birthdate_col_name <- "Birthdate"
      } else if (!is.null(input$birthdate_col) && input$birthdate_col != "" && input$birthdate_col %in% names(df)) {
        birthdate_col_name <- input$birthdate_col
      }
      
      if (!is.null(birthdate_col_name)) {
        invalid_ids <- issues$birth_date_order$invalid_offspring_ids
        if (length(invalid_ids) > 0) {
          # Set invalid birth dates to 0
          invalid_indices <- which(df$ID %in% invalid_ids)
          if (length(invalid_indices) > 0) {
            # Convert to character first if needed, then set to "0"
            if (inherits(df[[birthdate_col_name]], "Date") || inherits(df[[birthdate_col_name]], "POSIXct")) {
              # For Date/POSIXct, convert to character first
              df[[birthdate_col_name]] <- as.character(df[[birthdate_col_name]])
            }
            df[[birthdate_col_name]][invalid_indices] <- "0"
            fixed_summary$birth_date_order <- paste0("Set ", length(invalid_indices), " invalid birth date(s) to 0")
          }
        }
      }
    }
    
    return(list(
      data = df,
      summary = fixed_summary
    ))
  }
  
  # Validate data format before processing
  validate_data_format <- function(df, id_col, sire_col, dam_col) {
    errors <- c()
    
    # Check if columns exist
    if (!id_col %in% names(df)) {
      errors <- c(errors, paste("ID column '", id_col, "' not found in data"))
    }
    if (!sire_col %in% names(df)) {
      errors <- c(errors, paste("Sire column '", sire_col, "' not found in data"))
    }
    if (!dam_col %in% names(df)) {
      errors <- c(errors, paste("Dam column '", dam_col, "' not found in data"))
    }
    
    # If columns exist, check for basic data quality
    if (length(errors) == 0) {
      # Check if ID column has missing values (critical error)
      if (any(is.na(df[[id_col]]) | df[[id_col]] == "")) {
        errors <- c(errors, "ID column contains missing or empty values")
      }
      
      # Check for duplicate IDs but don't stop processing - just warn
      # (duplicates will be handled by keeping first occurrence)
      # Note: This check is removed from errors to allow processing to continue
      
      # Check if sire/dam columns reference valid IDs
      if (sire_col %in% names(df)) {
        valid_sires <- df[[sire_col]][!is.na(df[[sire_col]]) & df[[sire_col]] != "" & df[[sire_col]] != "0"]
        if (length(valid_sires) > 0) {
          missing_sires <- setdiff(unique(valid_sires), unique(df[[id_col]]))
          if (length(missing_sires) > 0) {
            errors <- c(errors, paste("Sire column references", length(missing_sires), "individuals not found in ID column"))
          }
        }
      }
      
      if (dam_col %in% names(df)) {
        valid_dams <- df[[dam_col]][!is.na(df[[dam_col]]) & df[[dam_col]] != "" & df[[dam_col]] != "0"]
        if (length(valid_dams) > 0) {
          missing_dams <- setdiff(unique(valid_dams), unique(df[[id_col]]))
          if (length(missing_dams) > 0) {
            errors <- c(errors, paste("Dam column references", length(missing_dams), "individuals not found in ID column"))
          }
        }
      }
    }
    
    return(errors)
  }
  
  # Reactive value to store auto-detected columns
  detected_cols <- reactiveVal(NULL)
  
  # Reactive value to remember last used column mapping
  saved_mapping <- reactiveVal(list(id = NULL, sire = NULL, dam = NULL, sex = NULL))
  
  # Reactive value to track data validation status
  data_validation_status <- reactiveVal(list(valid = FALSE, errors = c(), warnings = c()))
  
  # Reactive value to track inbreeding calculation status
  f_calculation_status <- reactiveVal(list(
    calculating = FALSE,
    progress = 0,
    message = "",
    n_individuals = 0
  ))
  
  # Cache for F values to avoid repeated calculations
  f_values_cache <- reactiveVal(NULL)
  f_values_cache_hash <- reactiveVal(NULL)
  
  # Track whether the Inbreeding Trend tab is currently inserted
  inb_trend_tab_inserted <- reactiveVal(FALSE)
  
  # Observe file upload and auto-detect
  observe({
    if (is.null(raw_data())) return()
    
    cols <- names(raw_data())
    auto_cols <- auto_detect_columns(cols)
    
    # Try to use saved mapping first, then auto-detect
    last_mapping <- saved_mapping()
    if (!is.null(last_mapping$id) && last_mapping$id %in% cols) {
      detected_cols(last_mapping)
    } else {
      detected_cols(auto_cols)
    }
    
    # Validate the detected columns
    current_cols <- detected_cols()
    if (!is.null(current_cols) && !is.null(current_cols$id) && !is.null(current_cols$sire) && !is.null(current_cols$dam)) {
      validation_errors <- validate_data_format(raw_data(), current_cols$id, current_cols$sire, current_cols$dam)
      
      if (length(validation_errors) == 0) {
        data_validation_status(list(valid = TRUE, errors = c(), warnings = c()))
        showNotification("✅ Data format validated successfully! Auto-processing enabled.", type = "message", duration = 5)
      } else {
        data_validation_status(list(valid = FALSE, errors = validation_errors, warnings = c()))
        showNotification(paste("⚠️ Data format validation failed:", paste(validation_errors, collapse = "; ")), 
                        type = "warning", duration = 10)
      }
    } else {
      data_validation_status(list(valid = FALSE, errors = c("Could not auto-detect required columns"), warnings = c()))
      showNotification("⚠️ Could not auto-detect required columns. Please manually select columns.", 
                      type = "warning", duration = 8)
    }
  })
  
  # Save column mapping when user changes it and re-validate
  observe({
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) return()
    
    saved_mapping(list(
      id = input$id_col,
      sire = input$sire_col,
      dam = input$dam_col,
      sex = input$sex_col,
      birthdate = input$birthdate_col
    ))
    
    # Re-validate when user changes column mapping
    validation_errors <- validate_data_format(raw_data(), input$id_col, input$sire_col, input$dam_col)
    
    if (length(validation_errors) == 0) {
      data_validation_status(list(valid = TRUE, errors = c(), warnings = c()))
      showNotification("✅ Column mapping validated successfully!", type = "message", duration = 3)
    } else {
      data_validation_status(list(valid = FALSE, errors = validation_errors, warnings = c()))
      showNotification(paste("⚠️ Column mapping validation failed:", paste(validation_errors, collapse = "; ")), 
                      type = "warning", duration = 8)
    }
  })
  
  # Conditionally show/hide "Inbreeding Trend" tab
  observe({
    has_data <- !is.null(raw_data())
    
    if (isTRUE(has_data) && !isTRUE(inb_trend_tab_inserted())) {
      insertTab(
        inputId = "mainTabs",
        tab = nav_panel(
          "Inbreeding Trend",
          value = "inb_trend",
          div(style = "margin-top: 10px;",
              uiOutput("inb_trend_controls"),
              uiOutput("inb_trend_hint_ui"),
             plotlyOutput("inb_trend_plot", height = "832px")
          )
        ),
        target = "viz",
        position = "after"
      )
      inb_trend_tab_inserted(TRUE)
    }
    
    if (!isTRUE(has_data) && isTRUE(inb_trend_tab_inserted())) {
      removeTab(inputId = "mainTabs", target = "inb_trend")
      inb_trend_tab_inserted(FALSE)
    }
  })
  
  # Column mapping UI
  output$col_mapping_ui <- renderUI({
    if (is.null(raw_data())) return(NULL)
    
    cols <- names(raw_data())
    auto_cols <- detected_cols()
    
    # Determine sex column selection
    sex_selected <- ""
    if (!is.null(auto_cols) && !is.null(auto_cols$sex) && !is.na(auto_cols$sex)) {
      sex_selected <- auto_cols$sex
    }
    
    # Determine birthdate column selection
    birthdate_selected <- ""
    if (!is.null(auto_cols) && !is.null(auto_cols$birthdate) && !is.na(auto_cols$birthdate)) {
      birthdate_selected <- auto_cols$birthdate
    }
    
    tagList(
      selectInput("id_col", "ID Column:", choices = cols, 
                  selected = if(!is.null(auto_cols)) auto_cols$id else cols[1]),
      selectInput("sire_col", "Sire Column:", choices = cols,
                  selected = if(!is.null(auto_cols)) auto_cols$sire else cols[2]),
      selectInput("dam_col", "Dam Column:", choices = cols,
                  selected = if(!is.null(auto_cols)) auto_cols$dam else cols[3]),
      div(
      selectInput("sex_col", "Sex (optional):", 
                  choices = c("None" = "", cols),
                    selected = sex_selected),
        div(
          style = "background-color: #e7f3ff; padding: 6px; border-radius: 4px; margin-top: 5px; border-left: 3px solid #0066cc;",
          tags$small(
            "📝 Format: 1, 2 or M, F",
            style = "color: #004085; font-size: 0.8rem; font-weight: 500;"
          )
        ),
        if (is.null(auto_cols$sex) || is.na(auto_cols$sex)) {
          div(
            style = "background-color: #fff3cd; padding: 6px; border-radius: 4px; margin-top: 5px; border-left: 3px solid #ffc107;",
            tags$small(
              "💡 No sex column detected. You can manually select one if available, or leave as 'None'.",
              style = "color: #856404; font-size: 0.8rem;"
            )
          )
        }
      ),
      div(
      selectInput("birthdate_col", "Birthdate (optional):", 
                  choices = c("None" = "", cols),
                    selected = birthdate_selected),
        div(
          style = "background-color: #e7f3ff; padding: 6px; border-radius: 4px; margin-top: 5px; border-left: 3px solid #0066cc;",
          tags$small(
            "📝 Format: YYYY-MM-DD (e.g., 2010-02-05)",
            style = "color: #004085; font-size: 0.8rem; font-weight: 500;"
          )
        ),
        if (is.null(auto_cols$birthdate) || is.na(auto_cols$birthdate)) {
          div(
            style = "background-color: #fff3cd; padding: 6px; border-radius: 4px; margin-top: 5px; border-left: 3px solid #ffc107;",
            tags$small(
              "💡 No birthdate column detected. You can manually select one if available, or leave as 'None'.",
              style = "color: #856404; font-size: 0.8rem;"
            )
          )
        }
      )
    )
  })
  
  # Data validation status UI
  # Helper function to get missing IDs from Sire/Dam columns
  get_missing_ids <- reactive({
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) {
      return(list(sire = character(0), dam = character(0)))
    }
    df <- raw_data()
    if (!all(c(input$id_col, input$sire_col, input$dam_col) %in% names(df))) {
      return(list(sire = character(0), dam = character(0)))
    }
    
    valid_ids <- unique(df[[input$id_col]])
    
    # Check Sire column
    valid_sires <- df[[input$sire_col]][!is.na(df[[input$sire_col]]) & df[[input$sire_col]] != "" & df[[input$sire_col]] != "0"]
    missing_sires <- setdiff(unique(valid_sires), valid_ids)
    
    # Check Dam column
    valid_dams <- df[[input$dam_col]][!is.na(df[[input$dam_col]]) & df[[input$dam_col]] != "" & df[[input$dam_col]] != "0"]
    missing_dams <- setdiff(unique(valid_dams), valid_ids)
    
    return(list(sire = as.character(missing_sires), dam = as.character(missing_dams)))
  })
  
  output$data_validation_status_ui <- renderUI({
    validation_status <- data_validation_status()
    
    # Check if validation_status is NULL or invalid
    if (is.null(validation_status) || !is.list(validation_status)) {
      return(div(class = "alert alert-warning", style = "padding: 8px; font-size: 0.85rem; margin-top: 10px;",
                 "⚠️ Validation status not available..."))
    }
    
    if (validation_status$valid) {
      div(class = "alert alert-success", style = "padding: 8px; font-size: 0.85rem; margin-top: 10px;",
          "✅ Data format validation passed! Ready for processing.")
    } else if (!is.null(validation_status$errors) && length(validation_status$errors) > 0) {
      # Check if errors are about missing Sire/Dam references
      has_sire_error <- any(grepl("Sire column references.*individuals not found", validation_status$errors))
      has_dam_error <- any(grepl("Dam column references.*individuals not found", validation_status$errors))
      
      if (has_sire_error || has_dam_error) {
        div(class = "alert alert-warning", style = "padding: 8px; font-size: 0.85rem; margin-top: 10px;",
            tags$div(strong("⚠️ Validation Warnings:"), 
                     tags$ul(
                       lapply(validation_status$errors, function(error) {
                         tags$li(error, style = "margin: 2px 0;")
                       })
                     ))
        )
      } else {
        div(class = "alert alert-danger", style = "padding: 8px; font-size: 0.85rem; margin-top: 10px;",
            tags$div(strong("❌ Validation Errors:"), 
                     tags$ul(
                       lapply(validation_status$errors, function(error) {
                         tags$li(error, style = "margin: 2px 0;")
                       })
                     )))
      }
    } else {
      div(class = "alert alert-warning", style = "padding: 8px; font-size: 0.85rem; margin-top: 10px;",
          "⚠️ Please select columns and wait for validation...")
    }
  })
  
  # Reset storage when new file is uploaded
  observeEvent(input$file, {
    raw_data_storage(NULL)
    f_values_cache(NULL)  # Clear inbreeding cache when new file uploaded
    f_values_cache_hash(NULL)
    analysis_started(FALSE)  # Reset analysis started flag
    qc_issues(NULL)
    pending_data(NULL)
    qc_fix_summary(NULL)
    last_fix_summary(NULL)
  })
  
  # Clear all data, caches, and inputs
  observeEvent(input$clear_all, {
    raw_data_storage(NULL)
    f_values_cache(NULL)
    f_values_cache_hash(NULL)
    analysis_started(FALSE)
    qc_issues(NULL)
    pending_data(NULL)
    qc_fix_summary(NULL)
    last_fix_summary(NULL)
    detected_cols(NULL)
    saved_mapping(list(id = NULL, sire = NULL, dam = NULL, sex = NULL))
    data_validation_status(list(valid = FALSE, errors = c(), warnings = c()))
    selected_individual(NULL)
    highlighted_individuals(character(0))
    
    cache_base_dir <- "pedigree_cache"
    if (dir.exists(cache_base_dir)) {
      unlink(cache_base_dir, recursive = TRUE, force = TRUE)
    }
    
    shinyjs::reset("file")
    updateTextInput(session, "individual_search", value = "")
    updateSelectInput(session, "id_col", choices = character(0), selected = character(0))
    updateSelectInput(session, "sire_col", choices = character(0), selected = character(0))
    updateSelectInput(session, "dam_col", choices = character(0), selected = character(0))
    updateSelectInput(session, "sex_col", choices = c("None" = ""), selected = "")
    updateSelectInput(session, "birthdate_col", choices = c("None" = ""), selected = "")
    
    showNotification("Cleared all data and caches. You can upload a new file.", type = "message", duration = 4)
  })
  
  # Handle Start Analysis button click
  observeEvent(input$start_analysis, {
    # Validate column mapping first
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) {
      showNotification("Please select all required columns (ID, Sire, Dam) before starting analysis.", 
                      type = "warning", duration = 5)
      return()
    }
    
    # Mark analysis as started
    analysis_started(TRUE)
    
    # Trigger validation and update validation status
    validation_errors <- validate_data_format(raw_data(), input$id_col, input$sire_col, input$dam_col)
    if (length(validation_errors) > 0) {
      # Filter out missing parent reference errors - these will be handled by QC auto-fix
      critical_errors <- validation_errors[!grepl("Sire column references.*individuals not found|Dam column references.*individuals not found|ID column contains missing or empty values", validation_errors)]
      if (length(critical_errors) > 0) {
        data_validation_status(list(valid = FALSE, errors = validation_errors, warnings = c()))
        showNotification(paste("❌ Data validation failed:", paste(critical_errors, collapse = "; ")), 
                        type = "error", duration = 8)
        analysis_started(FALSE)  # Reset if validation failed
        return()
      } else {
        # Only missing parent errors - allow processing (will be fixed by QC)
        data_validation_status(list(valid = TRUE, errors = NULL, warnings = validation_errors))
      }
    } else {
      # No errors - validation passed
      data_validation_status(list(valid = TRUE, errors = NULL, warnings = NULL))
    }
    
    # Show notification that analysis is starting
    showNotification("🚀 Starting pedigree analysis...", type = "message", duration = 3)
    
    # Note: Setting analysis_started(TRUE) will trigger ped_data() to process
    # regardless of auto_process setting, so no need to manually trigger process button
  })
  
  # Trigger inbreeding recalculation when data is fixed and auto_process is enabled
  observe({
    # Watch for changes in raw_data_storage and auto_process status
    stored_data <- raw_data_storage()
    auto_enabled <- isTRUE(input$auto_process)
    validation_status <- data_validation_status()
    
    # If we have fixed data stored, validation passed, and auto_process is enabled
    # wait a moment for ped_data() to reprocess, then ensure cache is cleared
    if (!is.null(stored_data) && 
        !is.null(validation_status) && 
        isTRUE(validation_status$valid) && 
        auto_enabled) {
      # Clear cache only if pedigree actually changed
      ped <- isolate(ped_data())
      if (!is.null(ped) && all(c("ID", "Sire", "Dam") %in% names(ped))) {
        current_hash <- digest::digest(ped[, c("ID", "Sire", "Dam")], algo = "xxhash64")
        cached_hash <- f_values_cache_hash()
        if (!is.null(cached_hash) && !identical(current_hash, cached_hash)) {
          f_values_cache(NULL)
          f_values_cache_hash(NULL)
        }
      } else {
        f_values_cache(NULL)
        f_values_cache_hash(NULL)
      }
    }
  })
  
  # Fix Pedigree button handler
  # NOTE: This functionality has been integrated into the Auto-Fix QC system.
  # The UI button has been removed, but keeping this handler for backward compatibility.
  # Missing parent references are now automatically detected and fixed via the QC modal's Auto-Fix button.
  observeEvent(input$fix_pedigree, {
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) {
      showNotification("Cannot fix pedigree: Data or column selections missing", type = "error")
      return()
    }
    
    df <- raw_data()
    missing_ids <- get_missing_ids()
    valid_ids <- unique(df[[input$id_col]])
    
    total_fixed <- 0
    
    # Fix Sire column
    if (length(missing_ids$sire) > 0) {
      sire_mask <- df[[input$sire_col]] %in% missing_ids$sire
      fixed_count <- sum(sire_mask, na.rm = TRUE)
      df[[input$sire_col]][sire_mask] <- NA
      total_fixed <- total_fixed + fixed_count
    }
    
    # Fix Dam column
    if (length(missing_ids$dam) > 0) {
      dam_mask <- df[[input$dam_col]] %in% missing_ids$dam
      fixed_count <- sum(dam_mask, na.rm = TRUE)
      df[[input$dam_col]][dam_mask] <- NA
      total_fixed <- total_fixed + fixed_count
    }
    
    # Store fixed data
    raw_data_storage(df)
    # Write fix summary for QC report
    last_fix_summary(list(
      fixed_sire = length(missing_ids$sire),
      fixed_dam = length(missing_ids$dam),
      total_fixed_cells = total_fixed,
      when = Sys.time()
    ))
    
    # Re-validate data
    validation_errors <- validate_data_format(df, input$id_col, input$sire_col, input$dam_col)
    if (length(validation_errors) == 0) {
      data_validation_status(list(valid = TRUE, errors = c(), warnings = c()))
      
      # Clear inbreeding cache to trigger recalculation with fixed data
    f_values_cache(NULL)
    f_values_cache_hash(NULL)
      
      # If auto_process is enabled, data will be automatically reprocessed
      # and inbreeding will be recalculated
      showNotification(paste("✅ Pedigree fixed! Removed", total_fixed, "invalid references. Data validation passed. ", 
                            if(isTRUE(input$auto_process)) "Inbreeding coefficients will be recalculated automatically." else ""), 
                      type = "message", duration = 6)
    } else {
      data_validation_status(list(valid = FALSE, errors = validation_errors, warnings = c()))
      showNotification(paste("✅ Fixed", total_fixed, "invalid references. ⚠️ Some validation errors remain."), 
                      type = "warning", duration = 5)
    }
  })
  
  # QC Modal Event Handlers
  
  # Auto-fix button - apply fixes and update data
  observeEvent(input$fix_qc, {
    issues <- qc_issues()
    pend_data <- pending_data()
    
    if (is.null(issues) || is.null(pend_data)) {
      return()
    }
    
    # Apply fixes
    fix_result <- fix_qc_issues(pend_data, issues)
    
    # Store fixed data for use by ped_data reactive
    raw_data_storage(fix_result$data)
    qc_fix_summary(fix_result$summary)
    
    # Clear QC state
    qc_issues(NULL)
    pending_data(NULL)
    
    # Close modal
    removeModal()
    
    # Show success notification with summary
    summary_text <- paste(unlist(fix_result$summary), collapse = "; ")
    showNotification(
      paste0("✅ Auto-fix completed! ", summary_text),
      type = "message",
      duration = 10
    )
    
    # Trigger reprocessing by clearing and re-setting validation
    data_validation_status(list(valid = TRUE, errors = NULL))
  })
  
  # Ignore button - proceed with data as-is
  observeEvent(input$ignore_qc, {
    pend_data <- pending_data()
    
    if (is.null(pend_data)) {
      return()
    }
    
    # Store data without fixes
    raw_data_storage(pend_data)
    
    # Clear QC state
    qc_issues(NULL)
    pending_data(NULL)
    
    # Close modal
    removeModal()
    
    # Show warning notification
    showNotification(
      "⚠️ Proceeding with unresolved QC issues. This may cause errors in analysis.",
      type = "warning",
      duration = 8
    )
    
    # Allow processing to continue
    data_validation_status(list(valid = TRUE, errors = NULL))
  })
  
  # Cancel button - reject data and return to upload
  observeEvent(input$cancel_qc, {
    # Clear all QC state
    qc_issues(NULL)
    pending_data(NULL)
    raw_data_storage(NULL)
    
    # Close modal
    removeModal()
    
    # Show info notification
    showNotification(
      "Upload cancelled. Please upload a different file or fix issues manually.",
      type = "default",
      duration = 5
    )
  })
  
  # Download QC report button
  output$download_qc_report <- downloadHandler(
    filename = function() {
      paste0("pedigree_qc_issues_", Sys.Date(), ".txt")
    },
    content = function(file) {
      issues <- qc_issues()
      
      if (is.null(issues)) {
        writeLines("No QC issues recorded.", file)
        return()
      }
      
      report_lines <- c(
        "========================================",
        "PEDIGREE QC ISSUES REPORT",
        paste("Generated:", Sys.time()),
        "========================================",
        ""
      )
      
      # Duplicate IDs section
      if (length(issues$duplicates) > 0 && issues$duplicates$count > 0) {
        report_lines <- c(
          report_lines,
          "--- DUPLICATE IDs ---",
          paste("Total unique duplicated IDs:", issues$duplicates$count),
          "Duplicated IDs:",
          paste("  ", issues$duplicates$ids),
          ""
        )
      }
      
      # Self-parenting section
      if (length(issues$self_parenting) > 0 && issues$self_parenting$count > 0) {
        report_lines <- c(
          report_lines,
          "--- SELF-PARENTING ---",
          paste("Total cases:", issues$self_parenting$count),
          "Affected IDs:",
          paste("  ", issues$self_parenting$ids),
          ""
        )
      }

      # Dual-role parents section
      if (length(issues$dual_parent_role) > 0 && issues$dual_parent_role$count > 0) {
        report_lines <- c(
          report_lines,
          "--- DUAL-ROLE PARENTS (SIRE & DAM) ---",
          paste("Total IDs:", issues$dual_parent_role$count),
          "Affected IDs:",
          paste("  ", issues$dual_parent_role$ids),
          ""
        )
      }
      
      # Missing parents section
      if (length(issues$missing_parents) > 0 && issues$missing_parents$total > 0) {
        report_lines <- c(
          report_lines,
          "--- MISSING PARENT REFERENCES ---",
          paste("Total missing references:", issues$missing_parents$total)
        )
        
        if (length(issues$missing_parents$sires) > 0) {
          report_lines <- c(
            report_lines,
            "",
            paste("Missing Sires (", length(issues$missing_parents$sires), "):"),
            paste("  ", issues$missing_parents$sires)
          )
        }
        
        if (length(issues$missing_parents$dams) > 0) {
          report_lines <- c(
            report_lines,
            "",
            paste("Missing Dams (", length(issues$missing_parents$dams), "):"),
            paste("  ", issues$missing_parents$dams)
          )
        }
        
        report_lines <- c(report_lines, "")
      }

      # Sex mismatch section
      if (length(issues$sex_mismatch) > 0 &&
          ((issues$sex_mismatch$sire_count %||% 0) > 0 ||
           (issues$sex_mismatch$dam_count %||% 0) > 0)) {
        report_lines <- c(
          report_lines,
          "--- SEX MISMATCH ---",
          paste("Sire mismatches:", issues$sex_mismatch$sire_count %||% 0),
          paste("Dam mismatches:", issues$sex_mismatch$dam_count %||% 0)
        )
        if ((issues$sex_mismatch$sire_count %||% 0) > 0) {
          report_lines <- c(report_lines, "Sire IDs:", paste("  ", issues$sex_mismatch$sire_ids))
        }
        if ((issues$sex_mismatch$dam_count %||% 0) > 0) {
          report_lines <- c(report_lines, "Dam IDs:", paste("  ", issues$sex_mismatch$dam_ids))
        }
        report_lines <- c(report_lines, "")
      }
      
      # Birth date order section
      if (length(issues$birth_date_order) > 0 && issues$birth_date_order$count > 0) {
        report_lines <- c(
          report_lines,
          "--- BIRTH DATE ORDER ISSUES ---",
          paste("Total cases:", issues$birth_date_order$count),
          paste("Invalid sire cases:", issues$birth_date_order$invalid_sire_count),
          paste("Invalid dam cases:", issues$birth_date_order$invalid_dam_count),
          "",
          "Affected offspring and parents:"
        )
        
        for (i in seq_along(issues$birth_date_order$invalid_offspring_ids)) {
          offspring_id <- issues$birth_date_order$invalid_offspring_ids[i]
          sire_id <- issues$birth_date_order$invalid_sire_ids[i]
          dam_id <- issues$birth_date_order$invalid_dam_ids[i]
          problem_line <- paste("  Offspring:", offspring_id)
          if (sire_id != "" && sire_id != "NA") {
            problem_line <- paste0(problem_line, " - Sire: ", sire_id, " (born after or same date)")
          }
          if (dam_id != "" && dam_id != "NA") {
            problem_line <- paste0(problem_line, " - Dam: ", dam_id, " (born after or same date)")
          }
          report_lines <- c(report_lines, problem_line)
        }
        
        report_lines <- c(report_lines, "")
      }
      
      # Circular references (loops) section
      if (length(issues$loops) > 0 && issues$loops$count > 0) {
        report_lines <- c(
          report_lines,
          "--- CIRCULAR REFERENCES (LOOPS) ---",
          paste("Total loops detected:", issues$loops$count),
          ""
        )
        
        for (i in seq_along(issues$loops$cycles)) {
          cycle <- issues$loops$cycles[[i]]
          report_lines <- c(
            report_lines,
            paste("Loop", i, ":"),
            paste("  ", paste(cycle, collapse = " -> ")),
            ""
          )
        }
      }
      
      report_lines <- c(
        report_lines,
        "========================================",
        "END OF REPORT",
        "========================================"
      )
      
      writeLines(report_lines, file)
    }
  )

  # Download fixed pedigree handler
  # Download comprehensive QC report
  output$download_qc_full_report <- downloadHandler(
    filename = function() {
      paste0("pedigree_qc_full_report_", Sys.Date(), ".txt")
    },
    content = function(file) {
      ped <- ped_data()
      if (is.null(ped) || nrow(ped) == 0) {
        writeLines("No processed pedigree available", file)
        return()
      }
      
      # Run comprehensive QC detection
      qc_results <- detect_qc_issues(ped)
      
      # Basic statistics
      n <- nrow(ped)
      stats <- get_basic_stats(ped)
      founders <- stats$founders
      both_parents <- stats$both_parents
      
      report_lines <- c(
        "========================================",
        "COMPREHENSIVE PEDIGREE QC REPORT",
        paste("Generated:", Sys.time()),
        if (exists("use_rcpp") && use_rcpp) "(Using Rcpp-accelerated QC)" else "(Using R-based QC)",
        "========================================",
        "",
        "--- BASIC STATISTICS ---",
        paste("Total individuals:", format(n, big.mark = ",")),
        paste("Founders:", format(founders, big.mark = ",")),
        paste("With both parents:", format(both_parents, big.mark = ",")),
        ""
      )
      
      # Duplicate IDs
      if (length(qc_results$duplicates) > 0 && qc_results$duplicates$count > 0) {
        report_lines <- c(
          report_lines,
          "--- DUPLICATE IDs ---",
          paste("Total unique duplicated IDs:", qc_results$duplicates$count),
          "Duplicated IDs:",
          paste("  ", qc_results$duplicates$ids),
          ""
        )
      } else {
        report_lines <- c(report_lines, "✔ No duplicate IDs", "")
      }
      
      # Self-parenting
      if (length(qc_results$self_parenting) > 0 && qc_results$self_parenting$count > 0) {
        report_lines <- c(
          report_lines,
          "--- SELF-PARENTING ---",
          paste("Total cases:", qc_results$self_parenting$count),
          "Affected IDs:",
          paste("  ", qc_results$self_parenting$ids),
          ""
        )
      } else {
        report_lines <- c(report_lines, "✔ No self-parenting issues", "")
      }
      
      # Missing parents
      if (length(qc_results$missing_parents) > 0 && qc_results$missing_parents$total > 0) {
        report_lines <- c(
          report_lines,
          "--- MISSING PARENT REFERENCES ---",
          paste("Total missing references:", qc_results$missing_parents$total)
        )
        
        if (length(qc_results$missing_parents$sires) > 0) {
          report_lines <- c(
            report_lines,
            "",
            paste("Missing Sires (", length(qc_results$missing_parents$sires), "):"),
            paste("  ", qc_results$missing_parents$sires)
          )
        }
        
        if (length(qc_results$missing_parents$dams) > 0) {
          report_lines <- c(
            report_lines,
            "",
            paste("Missing Dams (", length(qc_results$missing_parents$dams), "):"),
            paste("  ", qc_results$missing_parents$dams)
          )
        }
        
        report_lines <- c(report_lines, "")
      } else {
        report_lines <- c(report_lines, "✔ All sires found", "✔ All dams found", "")
      }
      
      # Birth date order section
      if (length(qc_results$birth_date_order) > 0 && qc_results$birth_date_order$count > 0) {
        report_lines <- c(
          report_lines,
          "--- BIRTH DATE ORDER ISSUES ---",
          paste("Total cases:", qc_results$birth_date_order$count),
          paste("Invalid sire cases:", qc_results$birth_date_order$invalid_sire_count),
          paste("Invalid dam cases:", qc_results$birth_date_order$invalid_dam_count),
          "",
          "Affected offspring and parents:"
        )
        
        for (i in seq_along(qc_results$birth_date_order$invalid_offspring_ids)) {
          offspring_id <- qc_results$birth_date_order$invalid_offspring_ids[i]
          sire_id <- qc_results$birth_date_order$invalid_sire_ids[i]
          dam_id <- qc_results$birth_date_order$invalid_dam_ids[i]
          problem_line <- paste("  Offspring:", offspring_id)
          if (sire_id != "" && sire_id != "NA") {
            problem_line <- paste0(problem_line, " - Sire: ", sire_id, " (born after or same date)")
          }
          if (dam_id != "" && dam_id != "NA") {
            problem_line <- paste0(problem_line, " - Dam: ", dam_id, " (born after or same date)")
          }
          report_lines <- c(report_lines, problem_line)
        }
        
        report_lines <- c(report_lines, "")
      }
      
      # Circular references (loops)
      if (length(qc_results$loops) > 0 && qc_results$loops$count > 0) {
        report_lines <- c(
          report_lines,
          "--- CIRCULAR REFERENCES (LOOPS) ---",
          paste("Total loops detected:", qc_results$loops$count),
          "",
          "Detected ancestry cycles:"
        )
        
        for (i in seq_along(qc_results$loops$cycles)) {
          cycle <- qc_results$loops$cycles[[i]]
          report_lines <- c(
            report_lines,
            paste("  Loop", i, ":", paste(cycle, collapse = " -> "))
          )
        }
        
        report_lines <- c(report_lines, "")
      } else {
        report_lines <- c(report_lines, "✔ No circular references detected", "")
      }
      
      # Auto-fix summary if available
      fix_info <- qc_fix_summary()
      if (!is.null(fix_info) && length(fix_info) > 0) {
        report_lines <- c(
          report_lines,
          "--- AUTO-FIX SUMMARY ---",
          "The following fixes were applied:"
        )
        
        for (fix_type in names(fix_info)) {
          report_lines <- c(report_lines, paste("  •", fix_info[[fix_type]]))
        }
        
        report_lines <- c(report_lines, "")
      }
      
      # Overall status
      report_lines <- c(
        report_lines,
        "========================================",
        "OVERALL STATUS",
        "========================================",
        if (qc_results$has_errors) {
          "⚠️ Issues detected in pedigree data"
        } else {
          "✅ All QC checks passed!"
        },
        "",
        "========================================",
        "END OF REPORT",
        "========================================"
      )
      
      writeLines(report_lines, file)
    }
  )
  
  output$download_fixed_pedigree <- downloadHandler(
    filename = function() {
      paste0("fixed_pedigree_", Sys.Date(), ".txt")
    },
    content = function(file) {
      ped <- ped_data()
      if (is.null(ped) || nrow(ped) == 0) {
        writeLines("No processed pedigree available", file)
        return()
      }
      # Export key columns; include Sex and Birthdate if present
      cols <- c("ID","Sire","Dam")
      if ("Sex" %in% names(ped)) cols <- c(cols, "Sex")
      if ("Birthdate" %in% names(ped)) cols <- c(cols, "Birthdate")
      out <- ped[, cols, drop = FALSE]
      
      # Replace NA/empty with 0 for missing parents
      out$Sire[is.na(out$Sire) | out$Sire == ""] <- "0"
      out$Dam[is.na(out$Dam) | out$Dam == ""] <- "0"
      
      # Replace NA/empty birthdates with 0 (invalid dates are already set to 0 by fix_qc_issues)
      if ("Birthdate" %in% names(out)) {
        # Convert to character if it's Date/POSIXct
        if (inherits(out$Birthdate, "Date") || inherits(out$Birthdate, "POSIXct")) {
          out$Birthdate <- as.character(out$Birthdate)
        }
        out$Birthdate[is.na(out$Birthdate) | out$Birthdate == ""] <- "0"
      }
      
      # Write space-delimited with column names (so it can be re-imported correctly)
      write.table(out, file, 
                  sep = " ",           # 空格分隔
                  quote = FALSE,       # 不加引号
                  row.names = FALSE,   # 无行号
                  col.names = TRUE,    # 包含列名（重要：便于重新导入）
                  na = "0")            # NA显示为0
    }
  )
  
  # Download Missing IDs handler
  output$download_missing_ids <- downloadHandler(
    filename = function() {
      paste0("missing_ids_", Sys.Date(), ".csv")
    },
    content = function(file) {
      missing_ids <- get_missing_ids()
      
      if (length(missing_ids$sire) == 0 && length(missing_ids$dam) == 0) {
        write.csv(data.frame(Message = "No missing IDs found"), file, row.names = FALSE)
        return()
      }
      
      # Create export data
      export_data <- data.frame(
        Missing_ID = c(missing_ids$sire, missing_ids$dam),
        Source_Column = c(rep("Sire", length(missing_ids$sire)), 
                          rep("Dam", length(missing_ids$dam))),
        stringsAsFactors = FALSE
      )
      
      export_data <- export_data[order(export_data$Source_Column, export_data$Missing_ID), ]
      
      write.csv(export_data, file, row.names = FALSE)
    }
  )
  
  # Inbreeding calculation progress UI
  output$f_calculation_progress <- renderUI({
    status <- f_calculation_status()
    
    if (!status$calculating) {
      return(NULL)
    }
    
    div(
      style = "margin-bottom: 10px;",
      div(
        style = "background-color: #e3f2fd; padding: 8px; border-radius: 4px; border-left: 4px solid #2196f3;",
        tags$div(
          style = "display: flex; align-items: center; margin-bottom: 5px;",
          tags$div(
            style = "flex: 1;",
            tags$small(
              paste0("🔄 ", status$message),
              style = "color: #1976d2; font-weight: 500;"
            )
          ),
          tags$div(
            style = "margin-left: 10px;",
            tags$small(
              paste0(format(status$n_individuals, big.mark = ","), " individuals"),
              style = "color: #666;"
            )
          )
        ),
        div(
          style = "background-color: #f5f5f5; border-radius: 10px; height: 8px; overflow: hidden;",
          div(
            style = paste0("background-color: #2196f3; height: 100%; width: ", status$progress * 100, "%; transition: width 0.3s ease;"),
            ""
          )
        )
      )
    )
  })
  
  # Process data (manual mode only - requires user action)
  ped_data <- reactive({
    # Always require user action: either process button click or start_analysis button click
    # auto_process only controls inbreeding calculation, not data processing
    if (!isTRUE(analysis_started())) {
      req(input$process)
    }
    
    # Both modes: require data and column mapping
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) {
      return(NULL)
    }
    
    # Check if data validation passed
    # Note: Missing parent references (Sire/Dam not in ID column) are allowed to pass
    # They will be detected and fixed by QC detection and auto-fix
    validation_status <- data_validation_status()
    if (is.null(validation_status) || !validation_status$valid) {
      # If validation status is not available, perform basic validation here
      basic_validation_errors <- validate_data_format(raw_data(), input$id_col, input$sire_col, input$dam_col)
      if (length(basic_validation_errors) > 0) {
        # Filter out missing parent reference errors - these will be handled by QC auto-fix
        critical_errors <- basic_validation_errors[!grepl("Sire column references.*individuals not found|Dam column references.*individuals not found|ID column contains missing or empty values", basic_validation_errors)]
        if (length(critical_errors) > 0) {
          showNotification(paste("❌ Data validation failed:", paste(critical_errors, collapse = "; ")), type = "error", duration = 8)
          return(NULL)
        }
        # If only missing parent errors, allow processing to continue (will be fixed by auto-fix)
      }
    }
    
    df <- raw_data()
    
    # Check if data is already processed (from QC fix or manual fix)
    # Already processed data will have ID, Sire, Dam columns
    already_processed <- all(c("ID", "Sire", "Dam") %in% names(df))
    
    if (!already_processed) {
      # Original data - need to check and rename columns
      if (!input$id_col %in% names(df)) {
        showNotification(paste("Error: Column", input$id_col, "not found in data"), type = "error")
        return(NULL)
      }
      if (!input$sire_col %in% names(df)) {
        showNotification(paste("Error: Column", input$sire_col, "not found in data"), type = "error")
        return(NULL)
      }
      if (!input$dam_col %in% names(df)) {
        showNotification(paste("Error: Column", input$dam_col, "not found in data"), type = "error")
        return(NULL)
      }
      
      df <- df %>%
        rename(
          ID = !!sym(input$id_col),
          Sire = !!sym(input$sire_col),
          Dam = !!sym(input$dam_col)
        )
      
      # Verify that rename was successful
      if (!"ID" %in% names(df) || !"Sire" %in% names(df) || !"Dam" %in% names(df)) {
        showNotification("Error: Column renaming failed. Please check your column selections.", type = "error")
        return(NULL)
      }
      
      # Handle Birthdate column - preserve if user selected it
      if (!is.null(input$birthdate_col) && input$birthdate_col != "" && 
          input$birthdate_col %in% names(df)) {
        # Rename birthdate column to a standard name for consistency
        birthdate_col_name <- "Birthdate"
        if (input$birthdate_col != birthdate_col_name && !birthdate_col_name %in% names(df)) {
          df <- df %>% rename(!!sym(birthdate_col_name) := !!sym(input$birthdate_col))
        }
        # If Birthdate already exists (from QC fix), keep it
      } else if ("Birthdate" %in% names(df)) {
        # Birthdate column already exists (from QC fix), keep it
      }
      
      # Standardize data format
      df <- df %>%
        mutate(
          ID = as.character(ID),
          Sire = as.character(Sire),
          Dam = as.character(Dam),
          Sire = ifelse(Sire %in% c("0", "NA", "", " ", "NULL"), NA, Sire),
          Dam = ifelse(Dam %in% c("0", "NA", "", " ", "NULL"), NA, Dam)
        )
      
      # Run comprehensive QC detection with progress indicator
      n_rows <- nrow(df)
      qc_method <- if (exists("use_rcpp") && use_rcpp) "Rcpp-accelerated" else "R-based"
      
      detected_issues <- withProgress(
        message = paste0("Running QC checks (", qc_method, ")..."),
        detail = paste("Analyzing", format(n_rows, big.mark = ","), "records"),
        value = 0.5,
        {
          detect_qc_issues(df)
        }
      )
    } else {
      # Data already processed - skip QC detection (already fixed)
      detected_issues <- list(has_errors = FALSE)
    }
    
    # If issues found, show modal and store pending data
    if (detected_issues$has_errors) {
      qc_issues(detected_issues)
      pending_data(df)
      
      # Build detailed issue summary for modal
      issue_html <- tags$div(
        style = "max-height: 400px; overflow-y: auto;",
        
        # Duplicate IDs section
        if (length(detected_issues$duplicates) > 0 && detected_issues$duplicates$count > 0) {
          tags$div(
            class = "alert alert-warning",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "❌ Duplicate IDs: ", detected_issues$duplicates$count, " unique ID(s) duplicated"
            ),
            tags$p(
              "Examples: ",
              tags$code(paste(head(detected_issues$duplicates$ids, 5), collapse = ", "))
            ),
            tags$p(
              style = "margin-bottom: 0; font-size: 0.9em;",
              "Auto-fix will keep the first occurrence of each ID."
            )
          )
        },
        
        # Missing/empty ID rows section
        if (length(detected_issues$missing_ids) > 0 && detected_issues$missing_ids$count > 0) {
          tags$div(
            class = "alert alert-danger",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "❌ Missing/Empty IDs: ", detected_issues$missing_ids$count, " row(s)"
            ),
            tags$p(
              "Rows with missing or empty IDs cannot be used in the pedigree."
            ),
            tags$p(
              "Example row indices: ",
              tags$code(paste(head(detected_issues$missing_ids$rows, 5), collapse = ", ")),
              if (detected_issues$missing_ids$count > 5) "..."
            ),
            tags$p(
              style = "margin-bottom: 0; font-size: 0.9em;",
              "Auto-fix will remove these rows."
            )
          )
        },
        
        # Self-parenting section
        if (length(detected_issues$self_parenting) > 0 && detected_issues$self_parenting$count > 0) {
          tags$div(
            class = "alert alert-warning",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "⚠️ Self-Parenting: ", detected_issues$self_parenting$count, " case(s)"
            ),
            tags$p(
              "IDs that list themselves as parent: ",
              tags$code(paste(head(detected_issues$self_parenting$ids, 5), collapse = ", "))
            ),
            tags$p(
              style = "margin-bottom: 0; font-size: 0.9em;",
              "Auto-fix will set these parent references to NA."
            )
          )
        },
        
        # Dual-role parent section (same ID used as both sire and dam)
        if (length(detected_issues$dual_parent_role) > 0 && detected_issues$dual_parent_role$count > 0) {
          tags$div(
            class = "alert alert-danger",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "❌ Dual-Role Parents: ", detected_issues$dual_parent_role$count, " ID(s)"
            ),
            tags$p(
              "These IDs appear as both Sire and Dam."
            ),
            tags$p(
              "Examples: ",
              tags$code(paste(head(detected_issues$dual_parent_role$ids, 5), collapse = ", ")),
              if (detected_issues$dual_parent_role$count > 5) "..."
            ),
            tags$p(
              style = "margin-bottom: 0; font-size: 0.9em;",
              "Auto-fix will set these parent references to NA."
            )
          )
        },
        
        # Sex mismatch section (requires Sex column)
        if (length(detected_issues$sex_mismatch) > 0 &&
            ((detected_issues$sex_mismatch$sire_count %||% 0) > 0 ||
             (detected_issues$sex_mismatch$dam_count %||% 0) > 0)) {
          tags$div(
            class = "alert alert-warning",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "⚠️ Sex Mismatch: ",
              (detected_issues$sex_mismatch$sire_count %||% 0) + (detected_issues$sex_mismatch$dam_count %||% 0),
              " reference(s)"
            ),
            if ((detected_issues$sex_mismatch$sire_count %||% 0) > 0) {
              tags$p(
                "Sire sex mismatches (", detected_issues$sex_mismatch$sire_count, "): ",
                tags$code(paste(head(detected_issues$sex_mismatch$sire_ids, 5), collapse = ", ")),
                if (length(detected_issues$sex_mismatch$sire_ids) > 5) "..."
              )
            },
            if ((detected_issues$sex_mismatch$dam_count %||% 0) > 0) {
              tags$p(
                "Dam sex mismatches (", detected_issues$sex_mismatch$dam_count, "): ",
                tags$code(paste(head(detected_issues$sex_mismatch$dam_ids, 5), collapse = ", ")),
                if (length(detected_issues$sex_mismatch$dam_ids) > 5) "..."
              )
            },
            tags$p(
              style = "margin-bottom: 0; font-size: 0.9em;",
              "Auto-fix will set these parent references to NA."
            )
          )
        },
        
        # Missing parents section
        if (length(detected_issues$missing_parents) > 0 && detected_issues$missing_parents$total > 0) {
          tags$div(
            class = "alert alert-info",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "ℹ️ Missing Parents: ", detected_issues$missing_parents$total, " reference(s)"
            ),
            if (length(detected_issues$missing_parents$sires) > 0) {
              tags$p(
                "Missing sires (", length(detected_issues$missing_parents$sires), "): ",
                tags$code(paste(head(detected_issues$missing_parents$sires, 5), collapse = ", ")),
                if (length(detected_issues$missing_parents$sires) > 5) "..."
              )
            },
            if (length(detected_issues$missing_parents$dams) > 0) {
              tags$p(
                "Missing dams (", length(detected_issues$missing_parents$dams), "): ",
                tags$code(paste(head(detected_issues$missing_parents$dams, 5), collapse = ", ")),
                if (length(detected_issues$missing_parents$dams) > 5) "..."
              )
            },
            tags$p(
              style = "margin-bottom: 0; font-size: 0.9em;",
              tags$strong("Auto-Fix: "), "Will automatically remove invalid parent references by setting them to NA. ",
              tags$strong("Download Report: "), "Exports all missing Sire/Dam IDs for your reference."
            )
          )
        },
        
        # Birth date order section
        if (length(detected_issues$birth_date_order) > 0 && detected_issues$birth_date_order$count > 0) {
          tags$div(
            class = "alert alert-warning",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "📅 Birth Date Order Issues: ", detected_issues$birth_date_order$count, " case(s)"
            ),
            tags$p(
              "Offspring born before or same date as parents:"
            ),
            tags$div(
              style = "max-height: 150px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px;",
              lapply(head(seq_along(detected_issues$birth_date_order$invalid_offspring_ids), 10), function(i) {
                offspring_id <- detected_issues$birth_date_order$invalid_offspring_ids[i]
                sire_id <- detected_issues$birth_date_order$invalid_sire_ids[i]
                dam_id <- detected_issues$birth_date_order$invalid_dam_ids[i]
                problem_text <- paste0(offspring_id, " (offspring)")
                if (sire_id != "" && sire_id != "NA") {
                  problem_text <- paste0(problem_text, " - Sire: ", sire_id)
                }
                if (dam_id != "" && dam_id != "NA") {
                  problem_text <- paste0(problem_text, " - Dam: ", dam_id)
                }
                tags$p(
                  style = "margin: 5px 0; font-family: monospace; font-size: 0.9em;",
                  tags$code(problem_text)
                )
              })
            ),
            if (detected_issues$birth_date_order$count > 10) {
              tags$p(
                style = "margin-top: 5px; font-size: 0.9em;",
                "... and ", detected_issues$birth_date_order$count - 10, " more case(s)"
              )
            },
            tags$p(
              style = "margin-bottom: 0; margin-top: 10px; font-size: 0.9em;",
              tags$strong("Auto-Fix: "), "Will set invalid birth dates to 0. ",
              tags$strong("Note: "), "Parents must be born before their offspring. Please review and correct birth dates."
            )
          )
        },
        
        # Circular references (loops) section
        if (length(detected_issues$loops) > 0 && detected_issues$loops$count > 0) {
          tags$div(
            class = "alert alert-danger",
            style = "margin: 10px 0;",
            tags$h5(
              style = "margin-top: 0;",
              "🔄 Circular References (Loops): ", detected_issues$loops$count, " loop(s) detected"
            ),
            tags$p(
              "Circular ancestry chains found in pedigree:"
            ),
            tags$div(
              style = "max-height: 150px; overflow-y: auto; background: #f8f9fa; padding: 10px; border-radius: 4px;",
              lapply(head(detected_issues$loops$cycles, 3), function(cycle) {
                tags$div(
                  style = "margin-bottom: 8px;",
                  tags$code(
                    style = "font-size: 0.85em;",
                    paste(cycle, collapse = " → ")
                  )
                )
              }),
              if (detected_issues$loops$count > 3) {
                tags$p(
                  style = "margin-top: 8px; margin-bottom: 0; font-style: italic;",
                  "... and ", detected_issues$loops$count - 3, " more loop(s)"
                )
              }
            ),
            tags$p(
              style = "margin-bottom: 0; margin-top: 10px; font-size: 0.9em;",
              "Auto-fix will break loops by removing one parent link in each cycle."
            )
          )
        }
      )
      
      # Show modal with QC issues and action buttons
      showModal(modalDialog(
        title = tags$h3(
          style = "margin: 0; color: #856404;",
          "⚠️ Pedigree Quality Issues Detected"
        ),
        tags$div(
          tags$p(
            style = "font-size: 1.1em; margin-bottom: 20px;",
            "The following issues were found in your pedigree data:"
          ),
          issue_html,
          tags$hr(),
          tags$p(
            style = "font-weight: bold; margin-top: 20px;",
            "What would you like to do?"
          ),
          tags$ul(
            style = "list-style-type: none; padding-left: 0;",
            tags$li("🛠 ", tags$strong("Auto-Fix:"), " Automatically resolve all issues listed above"),
            tags$li("⚠️ ", tags$strong("Ignore & Continue:"), " Proceed with data as-is (may cause errors)"),
            tags$li("📥 ", tags$strong("Download Report:"), " Export detailed issue list for manual review"),
            tags$li("❌ ", tags$strong("Cancel:"), " Return to file upload")
          )
        ),
        size = "l",
        easyClose = FALSE,
        footer = tagList(
          downloadButton("download_qc_report", "📥 Download Report", class = "btn-info"),
          actionButton("cancel_qc", "❌ Cancel", class = "btn-secondary"),
          actionButton("ignore_qc", "⚠️ Ignore & Continue", class = "btn-warning"),
          actionButton("fix_qc", "🛠 Auto-Fix", class = "btn-success")
        )
      ))
      
      return(NULL)  # Don't process data yet, wait for user action
    }
    
    # Handle Sex column - only add if not already present
    if (!"Sex" %in% names(df)) {
      if (!is.null(input$sex_col) && input$sex_col != "" && input$sex_col %in% names(df)) {
        df <- df %>% mutate(Sex = !!sym(input$sex_col))
      } else {
        df$Sex <- NA
      }
    }
    
    # Handle Birthdate column - preserve if user selected it (already renamed to "Birthdate" above)
    # Birthdate column is already handled in the rename section above, so it should already be present
    # if user selected it. If not present and user selected it, add it here as fallback.
    if (!"Birthdate" %in% names(df)) {
      if (!is.null(input$birthdate_col) && input$birthdate_col != "" && input$birthdate_col %in% names(df)) {
        df <- df %>% mutate(Birthdate = !!sym(input$birthdate_col))
      }
    }

    # Infer Sex when missing/unknown from parental roles (Sire/Dam)
    # Normalize Sex values to characters and keep existing non-missing entries
    df$Sex <- as.character(df$Sex)
    df$Sex[df$Sex %in% c("", " ", "NA", "NULL")] <- NA
    sire_ids <- unique(na.omit(df$Sire))
    dam_ids  <- unique(na.omit(df$Dam))
    # IDs that appear only as sire => Male; only as dam => Female; both => keep as is
    only_sire <- setdiff(sire_ids, dam_ids)
    only_dam  <- setdiff(dam_ids, sire_ids)
    # Fill only where Sex is NA
    df$Sex[is.na(df$Sex) & df$ID %in% only_sire] <- "M"
    df$Sex[is.na(df$Sex) & df$ID %in% only_dam]  <- "F"
    # For those appearing in both roles and missing Sex, leave as NA (Unknown)
    
    showNotification("✅ Data processed successfully!", type = "message", duration = 3)
    
    df
  })
  
  # Quick stats
  output$quick_stats <- renderPrint({
    req(ped_data())
    ped <- ped_data()
    
    # Check if ped_data returned NULL (due to column mapping errors)
    if (is.null(ped)) {
      cat("Error: Data processing failed. Please check column mapping.\n")
      return()
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      cat("Error: Required columns (ID, Sire, Dam) not found in processed data\n")
      return()
    }
    
    stats <- get_basic_stats(ped)
    cat("Total individuals:", nrow(ped), "\n")
    cat("Founders:", stats$founders, "\n")
    cat("Non-founders:", stats$non_founders, "\n")
  })
  
  # Auto-switch to visualization tab when data is processed (with delay for large datasets)
  observe({
    req(ped_data())
    # Only trigger when analysis has been started manually (via Start Analysis or Process button)
    if (isTRUE(analysis_started()) || !is.null(input$process)) {
      # Check if data validation passed
      validation_status <- data_validation_status()
      if (!validation_status$valid) {
        return()  # Don't switch tabs if validation failed
      }
      
      n_rows <- nrow(ped_data())
      
      # Auto-select individual with highest inbreeding coefficient (if available)
      f_vals_auto <- f_values_cache()
      if (!is.null(f_vals_auto) && nrow(f_vals_auto) > 0 && "ID" %in% names(f_vals_auto) && "F" %in% names(f_vals_auto)) {
        highest_f <- f_vals_auto %>% arrange(desc(F)) %>% slice(1)
        if (nrow(highest_f) > 0) {
          selected_individual(highest_f$ID)
          showNotification(
            paste0("🎯 Auto-selected highest inbreeding individual: ", highest_f$ID,
                   " (F = ", round(highest_f$F, 4), ")"),
            type = "message",
            duration = 8
          )
        }
      } else {
        # F values not available yet; keep previous behavior as fallback
        deepest_info <- find_deepest_ancestor_individual(ped_data())
        
        if (!is.null(deepest_info)) {
          selected_individual(deepest_info$id)
          showNotification(
            paste0("🎯 Auto-selected individual with deepest pedigree: ", 
                   deepest_info$id, " (", deepest_info$depth, " generations)"),
            type = "message",
            duration = 8
          )
        }
      }
      
      # For large datasets, stay on data tab to avoid rendering freeze
      if (n_rows > 5000) {
        showNotification(
          paste0("✅ Data loaded (", format(n_rows, big.mark = ","), " individuals). ",
                 "Switch to Network Visualization tab when ready."),
          type = "message",
          duration = 8
        )
      } else {
        updateTabsetPanel(session, "mainTabs", selected = "viz")
      }
    }
  })

  # Selected individual for visualization
  selected_individual <- reactiveVal(NULL)
  
  # Highlighted individuals for export (subset of visualized network)
  highlighted_individuals <- reactiveVal(character(0))
  
  # Info display for Selected Animal Export (match visualization exactly)
  # Create a reactive value to track selected individual info to prevent state conflicts
  # Use isolate() for network_data and search_depth to prevent rapid re-triggers
  selected_node_info_data <- reactive({
    # Use req() to ensure dependencies exist before proceeding
    ped <- req(ped_data())
    target_id <- req(selected_individual())
    
    # Validate target_id is in pedigree
    if (!target_id %in% ped$ID) {
      return(NULL)
    }
    
    # Include search_depth as a dependency but isolate network_data to reduce conflicts
    depth <- input$search_depth %||% 1
    if (is.na(depth) || depth < 0) depth <- 1
    
    # Use isolate for network_data to prevent it from triggering rapid recalculations
    # Network data changes are handled separately through other reactives
    net <- isolate(network_data())
    if (!is.null(net) && !is.null(net$nodes) && nrow(net$nodes) > 0) {
      # Count nodes currently visualized
      related_ids <- net$nodes$id
      # Derive ancestors set to present breakdown (computed with same depth)
      ancestors <- get_ancestors(ped, target_id, max_depth = depth)
      # Intersect with visualized nodes to avoid mismatch
      n_ancestors <- length(intersect(related_ids, ancestors))
      total_export <- length(unique(related_ids))
    } else {
      # Fallback to recompute when network not yet built
      ancestors <- get_ancestors(ped, target_id, max_depth = depth)
      selected_ids <- unique(c(target_id, ancestors))
      n_ancestors <- length(ancestors)
      total_export <- length(selected_ids)
    }
    
    return(list(
      target_id = target_id,
      depth = depth,
      n_ancestors = n_ancestors,
      total_export = total_export
    ))
  })
  
  output$selected_node_info <- renderUI({
    # Add error handling and state management
    tryCatch({
      # Check if we have a selected individual first
      target_id <- selected_individual()
      if (is.null(target_id)) {
        return(div(style = "color: #666; font-style: italic; padding: 8px; background: #f8f8f8; border-radius: 4px;",
                   "💡 Select an individual (via search or by clicking a node) to enable export"))
      }
      
      # Get computed data
      info <- selected_node_info_data()
      if (is.null(info)) {
        return(div(style = "color: #666; font-style: italic; padding: 8px; background: #f8f8f8; border-radius: 4px;",
                   "💡 Select an individual (via search or by clicking a node) to enable export"))
      }
      
      div(style = "color: #B89D5D; font-weight: bold; margin-bottom: 10px; padding: 8px; background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%); border-left: 4px solid #B89D5D; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
          div(style = "font-size: 1.1em; margin-bottom: 6px;",
              paste0("🎯 Selected Individual: ", info$target_id)),
          div(style = "font-size: 0.95em; color: #666;",
              paste0("📊 Search Depth: ", info$depth, " generation", ifelse(info$depth > 1, "s", ""))),
          div(style = "font-size: 0.95em; color: #666; margin-top: 4px;",
              paste0("📈 Export Summary: ", info$total_export, " individual", ifelse(info$total_export > 1, "s", ""),
                     " (", info$n_ancestors, " ancestor", ifelse(info$n_ancestors != 1, "s", ""), " + 1 selected)")))
    }, error = function(e) {
      # Return a safe fallback UI on error
      return(div(style = "color: #666; font-style: italic; padding: 8px; background: #f8f8f8; border-radius: 4px;",
                 "💡 Select an individual (via search or by clicking a node) to enable export"))
    })
  })
  
  # Set output options to prevent state conflicts
  outputOptions(output, "selected_node_info", suspendWhenHidden = FALSE, priority = 10)
  
  # Whether a selected individual exists (controls export button visibility)
  output$has_selected_individual <- reactive({
    !is.null(selected_individual())
  })
  outputOptions(output, "has_selected_individual", suspendWhenHidden = FALSE)
  
  # Info display for visualization
  # Reactive to compute viz_info data separately to prevent state conflicts
  viz_info_data <- reactive({
    # Use req() to ensure dependencies exist before proceeding
    target_id <- selected_individual()
    if (is.null(target_id)) {
      return(NULL)
    }
    
    ped <- req(ped_data())
    
    # Get search depth
    search_depth <- input$search_depth %||% 5
    
    # Get F value if available (use isolate to reduce reactivity)
    f_val <- NULL
    f_vals <- isolate(f_values())
    if (!is.null(f_vals) && nrow(f_vals) > 0) {
      f_data <- f_vals %>% filter(ID == target_id)
      if (nrow(f_data) > 0) {
        f_val <- f_data$F[1]
      }
    }
    
    # Get highlighted count (use isolate to reduce reactivity)
    highlighted_count <- length(isolate(highlighted_individuals()))
    
    return(list(
      target_id = target_id,
      search_depth = search_depth,
      f_val = f_val,
      highlighted_count = highlighted_count
    ))
  })
  
  output$viz_info <- renderUI({
    # Add error handling and state management
    tryCatch({
      # Get computed data
      info <- viz_info_data()
      
      if (is.null(info)) {
        return(div(style = "color: #666; font-style: italic; margin-bottom: 10px;",
                   "💡 Enter an individual ID above to visualize their pedigree network"))
      }
      
      f_text <- if (!is.null(info$f_val)) paste0(" (F = ", round(info$f_val, 4), ")") else ""
      highlight_text <- if (info$highlighted_count > 0) paste0(" | ", info$highlighted_count, " highlighted") else ""
      
      div(style = "color: #B89D5D; font-weight: bold; margin-bottom: 10px; padding: 8px; background: #f8f8f8; border-radius: 4px;",
          paste0("🎯 Showing pedigree network for: ", info$target_id, f_text, 
                 " (Depth: ", info$search_depth, " generations)", highlight_text))
    }, error = function(e) {
      # Return a safe fallback UI on error
      return(div(style = "color: #666; font-style: italic; margin-bottom: 10px;",
                 "💡 Enter an individual ID above to visualize their pedigree network"))
    })
  })
  
  # Set output options to prevent state conflicts
  outputOptions(output, "viz_info", suspendWhenHidden = FALSE, priority = 10)
  
  # Event handlers for individual search and visualization
  observeEvent(input$search_individual, {
    search_id <- trimws(input$individual_search)
    if (search_id != "") {
      ped <- ped_data()
      if (!is.null(ped) && search_id %in% ped$ID) {
        selected_individual(search_id)
        showNotification(paste("Showing pedigree for:", search_id), type = "message")
      } else {
        showNotification("Individual ID not found in pedigree data", type = "error")
      }
    }
  })
  
  observeEvent(input$show_highest_f, {
    f_vals <- f_values()
    if (!is.null(f_vals) && nrow(f_vals) > 0) {
      highest_f <- f_vals %>% arrange(desc(F)) %>% slice(1)
      selected_individual(highest_f$ID)
      showNotification(paste("Showing highest inbreeding individual:", highest_f$ID, 
                             "(F =", round(highest_f$F, 4), ")"), type = "message")
    } else {
      showNotification("No inbreeding coefficients available. Please run inbreeding analysis first.", type = "warning")
    }
  })
  
  observeEvent(input$refresh_viz, {
    selected_individual(NULL)
    highlighted_individuals(character(0))  # Clear highlighted individuals
    showNotification("Network refreshed", type = "message")
  })
  
  # Trigger download event handler
  observeEvent(input$trigger_download, {
    node_id <- input$trigger_download
    if (!is.null(node_id) && node_id != "") {
      # Update highlighted individuals with the clicked node
      highlighted_individuals(c(node_id))
      showNotification(paste("Highlighted individual:", node_id), type = "message")
    }
  })
  
  # Download handler: export selected individual's range (Search Depth)
  output$download_relatives <- downloadHandler(
    filename = function() {
      paste0("selected_range_", Sys.Date(), ".csv")
    },
    content = function(file) {
      tryCatch({
        # Safety check: Ensure pedigree data is available
        ped <- ped_data()
        if (is.null(ped)) {
          write.csv(data.frame(Message = "No pedigree data available"), file, row.names = FALSE)
          return()
        }
        # Ensure required columns exist
        for (col in c("ID","Sire","Dam","Sex")) {
          if (!col %in% names(ped)) ped[[col]] <- NA
        }
        ped$ID <- as.character(ped$ID)

        # Determine selected individual and depth
        target_id <- selected_individual()
        if (is.null(target_id) || !target_id %in% ped$ID) {
          write.csv(data.frame(Message = "No selected individual to export"), file, row.names = FALSE)
          return()
        }
        depth <- input$search_depth %||% 1
        if (is.na(depth) || depth < 0) depth <- 1

        # Compute relatives within depth with generation tracking
        ancestors <- get_ancestors(ped, target_id, max_depth = depth)
        selected_ids <- unique(c(target_id, ancestors))

        # Filter pedigree to selection
        export_data <- ped %>%
          dplyr::filter(ID %in% selected_ids) %>%
          dplyr::arrange(ID)
        if (nrow(export_data) == 0) {
          write.csv(data.frame(Message = "No individuals found for selected range"), file, row.names = FALSE)
          return()
        }

        # Calculate generation level for each individual relative to target
        # Helper function to calculate generation level
        calc_generation <- function(id) {
          if (id == target_id) return(0)
          if (id %in% ancestors) {
            # For ancestors, calculate upward generations
            gen <- 0
            current <- target_id
            visited <- character(0)
            
            # Trace back through parents
            while (gen < depth && !id %in% visited) {
              ind <- ped %>% filter(ID == current)
              if (nrow(ind) == 0) break
              
              visited <- c(visited, current)
              gen <- gen + 1
              
              # Check if id is parent
              if (!is.na(ind$Sire[1]) && ind$Sire[1] == id) return(-gen)
              if (!is.na(ind$Dam[1]) && ind$Dam[1] == id) return(-gen)
              
              # Continue with one parent (prefer sire)
              if (!is.na(ind$Sire[1]) && ind$Sire[1] != "") {
                current <- ind$Sire[1]
              } else if (!is.na(ind$Dam[1]) && ind$Dam[1] != "") {
                current <- ind$Dam[1]
              } else {
                break
              }
            }
            return(-gen)  # Negative for ancestors
          }
          return(NA)  # Should not reach here
        }

        # Enhanced relationship classification with generation info
        export_data <- export_data %>%
          mutate(
            Generation = sapply(ID, calc_generation),
            Relationship = dplyr::case_when(
              ID == target_id ~ "Selected Individual",
              ID %in% ancestors & Generation == -1 ~ "Parent",
              ID %in% ancestors & Generation == -2 ~ "Grandparent",
              ID %in% ancestors & Generation < -2 ~ paste0("Ancestor (G", abs(Generation), ")"),
              TRUE ~ "Relative"
            )
          )

        # Add F values if available
        if (!is.null(f_values()) && nrow(f_values()) > 0) {
          export_data <- export_data %>%
            left_join(f_values(), by = "ID")
        } else {
          export_data$F <- NA
        }
        
        # Select and arrange columns with complete pedigree info
        final_data <- export_data %>%
          dplyr::select(ID, Sire, Dam, Sex, F, Generation, Relationship) %>%
          mutate(
            Sire = ifelse(is.na(Sire) | Sire == "", "Unknown", as.character(Sire)),
            Dam = ifelse(is.na(Dam) | Dam == "", "Unknown", as.character(Dam)),
            Sex = ifelse(is.na(Sex), "Unknown", as.character(Sex)),
            F = ifelse(is.na(F), "", round(F, 4)),
            Generation = ifelse(is.na(Generation), "", Generation)
          ) %>%
          arrange(Generation, ID)  # Sort by generation, then ID
        
        write.csv(final_data, file, row.names = FALSE)
      }, error = function(e) {
        # Fail-safe: write error message instead of crashing the UI
        try(write.csv(data.frame(Error = e$message), file, row.names = FALSE), silent = TRUE)
        showNotification(paste("Export failed:", e$message), type = "error", duration = 5)
      })
    }
  )
  
  # Handle node highlighting for ancestor generations
  observeEvent(input$selected_node_for_highlight, {
    node_id <- input$selected_node_for_highlight
    if (!is.null(node_id) && node_id != "") {
      # Also treat a click as selecting this individual for export
      selected_individual(node_id)

      # Get the highlight generations setting with proper defaults
      highlight_gens <- input$search_depth %||% 1
      
      # Handle NULL, NA, or empty values - default to 1 generation
      if (is.null(highlight_gens) || is.na(highlight_gens) || highlight_gens == "") {
        highlight_gens <- 1
      }
      
      # Get current visualized network data
      net <- network_data()
      if (is.null(net) || nrow(net$nodes) == 0) {
        showNotification("Please first search and visualize an individual", type = "warning")
        return()
      }
      
      # Check if the clicked node exists in current network
      if (!node_id %in% net$nodes$id) {
        showNotification("Selected node not found in current visualization", type = "warning")
        return()
      }
      
      # Get pedigree data
      ped <- ped_data()
      if (is.null(ped)) return()
      
      # Get ancestors within specified generations
      ancestors <- get_ancestors(ped, node_id, max_depth = highlight_gens)
      all_relatives <- unique(c(node_id, ancestors))
      
      # Accumulate highlighted individuals (add to existing highlights)
      current_highlighted <- highlighted_individuals()
      new_highlighted <- unique(c(node_id, ancestors))
      
      # Combine with existing highlights to avoid duplicates
      all_highlighted <- unique(c(current_highlighted, new_highlighted))
      highlighted_individuals(all_highlighted)
      
      # Send custom message to frontend JavaScript for visual highlighting
      session$sendCustomMessage("highlightDescendants", list(
        all_nodes = all_relatives,
        selected_node = node_id,
        generations = highlight_gens,
        total_count = length(all_relatives)
      ))
      
      # Show notification with current total count
      showNotification(paste("Added", length(new_highlighted), "individuals to highlights:", 
                            node_id, "and", highlight_gens, "generations of ancestors. Total:", length(all_highlighted)), 
                      type = "message", duration = 4)
    }
  })
  
  # Data preview - highly optimized for large datasets, shows all data with inbreeding if available
  output$data_preview <- renderDT({
    req(ped_data())
    ped <- ped_data()
    
    # Check if ped_data returned NULL (due to column mapping errors)
    if (is.null(ped)) {
      return(datatable(
        data.frame(Message = "Error: Data processing failed. Please check column mapping."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      return(datatable(
        data.frame(Message = "Error: Required columns (ID, Sire, Dam) not found in processed data."),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Merge inbreeding coefficients if available
    display_data <- ped
    has_inbreeding <- FALSE
    f_tbl <- f_values_cache()
    if (!is.null(f_tbl) && nrow(f_tbl) > 0 && "ID" %in% names(f_tbl) && "F" %in% names(f_tbl)) {
      display_data <- display_data %>%
        left_join(f_tbl %>% mutate(ID = as.character(ID)), by = "ID") %>%
        mutate(F = round(F, 6))  # Round to 6 decimal places for display
      has_inbreeding <- TRUE
    }
    
    n <- nrow(display_data)
    
    # Determine optimal rendering based on dataset size
    if (n > 5000) {
      # Optimized rendering for large datasets - show all data with virtual scrolling
      dt <- datatable(
        display_data,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color: #666; font-weight: normal; padding: 10px;',
          paste0("📊 Displaying all ", format(n, big.mark = ","), 
                 " rows. Use filters and search to navigate.")
        ),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "500px",
          deferRender = TRUE,
          scroller = TRUE,
          dom = 'Bfrtip',
          buttons = list('copy', 'csv', 'excel', 'pdf'),
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
        ),
        filter = "top",
        rownames = FALSE,
        extensions = c('Buttons', 'Scroller')
      )
      
      # Apply inbreeding styling if available
      if (has_inbreeding && "F" %in% names(display_data)) {
        dt <- dt %>%
          formatStyle("F", 
                     backgroundColor = styleInterval(c(0, 0.0625, 0.125), 
                                                    c("white", "#fff9e6", "#ffe6cc", "#ffcccc")),
                     fontWeight = styleInterval(0.125, c("normal", "bold")))
      }
      return(dt)
    } else {
      # Normal rendering for smaller datasets
      dt <- datatable(
        display_data,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = if(n > 100) "500px" else NULL,
          deferRender = if(n > 100) TRUE else FALSE,
          dom = 'Bfrtip',
          buttons = list('copy', 'csv', 'excel', 'pdf'),
          lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All'))
        ),
        filter = "top",
        rownames = FALSE,
        extensions = 'Buttons'
      )
      
      # Apply inbreeding styling if available
      if (has_inbreeding && "F" %in% names(display_data)) {
        dt <- dt %>%
          formatStyle("F", 
                     backgroundColor = styleInterval(c(0, 0.0625, 0.125), 
                                                    c("white", "#fff9e6", "#ffe6cc", "#ffcccc")),
                     fontWeight = styleInterval(0.125, c("normal", "bold")))
      }
      return(dt)
    }
  }, server = TRUE)
  
  # QC Report - comprehensive with all issue detection
  output$qc_report <- renderPrint({
    req(ped_data())
    ped <- ped_data()
    
    # Check if ped_data returned NULL (due to column mapping errors)
    if (is.null(ped)) {
      cat("Error: Data processing failed. Please check column mapping.\n")
      return()
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      cat("Error: Required columns (ID, Sire, Dam) not found in processed data\n")
      return()
    }
    
    n <- nrow(ped)
    
    cat("=== PEDIGREE QC REPORT ===\n")
    if (exists("use_rcpp") && use_rcpp) {
      cat("(Using Rcpp-accelerated QC)\n")
    } else {
      cat("(Using optimized R functions)\n")
    }
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")
    
    # Run comprehensive QC detection
    qc_results <- detect_qc_issues(ped)
    
    # Basic statistics
    stats <- get_basic_stats(ped)
    founders <- stats$founders
    both_parents <- stats$both_parents
    
    cat("Total individuals:", format(n, big.mark = ","), "\n")
    cat("Founders:", format(founders, big.mark = ","), "\n")
    cat("With both parents:", format(both_parents, big.mark = ","), "\n\n")
    
    # Duplicate IDs
    if (length(qc_results$duplicates) > 0 && qc_results$duplicates$count > 0) {
      cat("❌ Duplicate IDs:", qc_results$duplicates$count, "unique ID(s) duplicated\n")
      cat("   Examples:", paste(head(qc_results$duplicates$ids, 5), collapse = ", "), "\n")
      if (qc_results$duplicates$count > 5) {
        cat("   ... and", qc_results$duplicates$count - 5, "more\n")
      }
      cat("\n")
    } else {
      cat("✔ No duplicate IDs\n\n")
    }
    
    # Self-parenting
    if (length(qc_results$self_parenting) > 0 && qc_results$self_parenting$count > 0) {
      cat("⚠️ Self-Parenting:", qc_results$self_parenting$count, "case(s)\n")
      cat("   Affected IDs:", paste(head(qc_results$self_parenting$ids, 5), collapse = ", "), "\n")
      if (qc_results$self_parenting$count > 5) {
        cat("   ... and", qc_results$self_parenting$count - 5, "more\n")
      }
      cat("\n")
    } else {
      cat("✔ No self-parenting issues\n\n")
    }
    
    # Missing parents
    if (length(qc_results$missing_parents) > 0 && qc_results$missing_parents$total > 0) {
      cat("ℹ️ Missing Parents:", qc_results$missing_parents$total, "reference(s)\n")
      
      if (length(qc_results$missing_parents$sires) > 0) {
        cat("   Missing sires (", length(qc_results$missing_parents$sires), "):", 
            paste(head(qc_results$missing_parents$sires, 5), collapse = ", "), "\n")
        if (length(qc_results$missing_parents$sires) > 5) {
          cat("   ... and", length(qc_results$missing_parents$sires) - 5, "more\n")
        }
      }
      
      if (length(qc_results$missing_parents$dams) > 0) {
        cat("   Missing dams (", length(qc_results$missing_parents$dams), "):", 
            paste(head(qc_results$missing_parents$dams, 5), collapse = ", "), "\n")
        if (length(qc_results$missing_parents$dams) > 5) {
          cat("   ... and", length(qc_results$missing_parents$dams) - 5, "more\n")
        }
      }
      cat("\n")
    } else {
      cat("✔ All sires found\n")
      cat("✔ All dams found\n\n")
    }
    
    # Birth date order issues
    if (length(qc_results$birth_date_order) > 0 && qc_results$birth_date_order$count > 0) {
      cat("📅 Birth Date Order Issues:", qc_results$birth_date_order$count, "case(s)\n")
      cat("   Invalid sire cases:", qc_results$birth_date_order$invalid_sire_count, "\n")
      cat("   Invalid dam cases:", qc_results$birth_date_order$invalid_dam_count, "\n")
      cat("   Affected offspring:\n")
      for (i in seq_along(qc_results$birth_date_order$invalid_offspring_ids)) {
        offspring_id <- qc_results$birth_date_order$invalid_offspring_ids[i]
        sire_id <- qc_results$birth_date_order$invalid_sire_ids[i]
        dam_id <- qc_results$birth_date_order$invalid_dam_ids[i]
        cat("     ", offspring_id)
        if (sire_id != "" && sire_id != "NA") {
          cat(" - Sire:", sire_id, "(born after or same date)")
        }
        if (dam_id != "" && dam_id != "NA") {
          cat(" - Dam:", dam_id, "(born after or same date)")
        }
        cat("\n")
      }
      if (qc_results$birth_date_order$count > 10) {
        cat("   ... and", qc_results$birth_date_order$count - 10, "more case(s)\n")
      }
      cat("\n")
    }
    
    # Circular references (loops)
    if (length(qc_results$loops) > 0 && qc_results$loops$count > 0) {
      cat("🔄 Circular References (Loops):", qc_results$loops$count, "loop(s) detected\n")
      cat("   Ancestry chains that form cycles:\n")
      
      for (i in seq_along(head(qc_results$loops$cycles, 3))) {
        cycle <- qc_results$loops$cycles[[i]]
        cat("   Loop", i, ":", paste(cycle, collapse = " → "), "\n")
      }
      
      if (qc_results$loops$count > 3) {
        cat("   ... and", qc_results$loops$count - 3, "more loop(s)\n")
      }
      cat("\n")
    } else {
      cat("✔ No circular references detected\n\n")
    }
    
    # Show last fix summary if available
    fix_info <- qc_fix_summary()
    if (!is.null(fix_info) && length(fix_info) > 0) {
      cat("--- Auto-Fix Summary ---\n")
      for (fix_type in names(fix_info)) {
        cat("  •", fix_info[[fix_type]], "\n")
      }
      cat("\n")
    }
    
    # Overall status
    if (qc_results$has_errors) {
      cat("⚠️ STATUS: Issues detected. Use 'Download QC Report' for full details.\n")
    } else {
      cat("✅ STATUS: All QC checks passed!\n")
    }
  })
  
  # Pedigree Structure Report
  output$pedigree_structure_report <- renderPrint({
    req(ped_data())
    ped <- ped_data()
    
    if (is.null(ped)) {
      cat("Error: Data processing failed. Please check column mapping.\n")
      return()
    }
    
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      cat("Error: Required columns (ID, Sire, Dam) not found in processed data\n")
      return()
    }
    
    cat("========================================\n")
    cat("PEDIGREE STRUCTURE REPORT\n")
    cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    cat("========================================\n\n")
    
    n <- nrow(ped)
    
    # Basic statistics
    cat("--- BASIC STATISTICS ---\n")
    cat("Individuals in total:", format(n, big.mark = ","), "\n")
    
    # Founders and non-founders
    stats <- get_basic_stats(ped)
    founders <- stats$founders
    non_founders <- stats$non_founders
    cat("Founders:", format(founders, big.mark = ","), "\n")
    cat("Non-founders:", format(non_founders, big.mark = ","), "\n")
    
    # Individuals with both parents
    both_parents <- stats$both_parents
    only_sire <- stats$only_sire
    only_dam <- stats$only_dam
    cat("With both parents:", format(both_parents, big.mark = ","), "\n")
    cat("Only with known sire:", format(only_sire, big.mark = ","), "\n")
    cat("Only with known dam:", format(only_dam, big.mark = ","), "\n\n")
    
  # Parent statistics
  stats_ext <- get_extended_stats(ped)
    cat("--- PARENT STATISTICS ---\n")
    # Get unique sires and dams
    all_ids <- unique(ped$ID)
    unique_sires <- unique(ped$Sire[!is.na(ped$Sire) & ped$Sire != "" & ped$Sire != "0"])
    unique_dams <- unique(ped$Dam[!is.na(ped$Dam) & ped$Dam != "" & ped$Dam != "0"])
    
    # Count progeny for each sire
    sire_progeny <- ped %>% 
      filter(!is.na(Sire), Sire != "", Sire != "0") %>%
      count(Sire, name = "progeny_count")
    total_sire_progeny <- sum(sire_progeny$progeny_count)
    
    # Count progeny for each dam
    dam_progeny <- ped %>% 
      filter(!is.na(Dam), Dam != "", Dam != "0") %>%
      count(Dam, name = "progeny_count")
    total_dam_progeny <- sum(dam_progeny$progeny_count)
    
  cat("Sires in total:", format(stats_ext$unique_sires, big.mark = ","), "\n")
  cat("   -Progeny:", format(stats_ext$total_sire_progeny, big.mark = ","), "\n")
  cat("Dams in total:", format(stats_ext$unique_dams, big.mark = ","), "\n")
  cat("   -Progeny:", format(stats_ext$total_dam_progeny, big.mark = ","), "\n")
    
    # Individuals with progeny
    individuals_with_progeny <- unique(c(unique_sires, unique_dams))
    individuals_with_progeny <- individuals_with_progeny[individuals_with_progeny %in% all_ids]
    individuals_without_progeny <- n - length(individuals_with_progeny)
  cat("Individuals with progeny:", format(stats_ext$individuals_with_progeny, big.mark = ","), "\n")
  cat("Individuals with no progeny:", format(stats_ext$individuals_without_progeny, big.mark = ","), "\n\n")
    
    # Founder statistics
    cat("--- FOUNDER STATISTICS ---\n")
    sire_missing <- is_missing_parent(ped$Sire)
    dam_missing <- is_missing_parent(ped$Dam)
    founder_ids <- ped$ID[sire_missing & dam_missing]
    founder_ped <- ped %>% filter(ID %in% founder_ids)
    
    # Founder sires and dams
    founder_sires <- founder_ped %>% 
      filter(ID %in% unique_sires) %>%
      pull(ID)
    founder_dams <- founder_ped %>% 
      filter(ID %in% unique_dams) %>%
      pull(ID)
    
    # Count founder progeny
    founder_sire_progeny <- ped %>% 
      filter(Sire %in% founder_ids) %>%
      nrow()
    founder_dam_progeny <- ped %>% 
      filter(Dam %in% founder_ids) %>%
      nrow()
    founder_total_progeny <- ped %>% 
      filter(Sire %in% founder_ids | Dam %in% founder_ids) %>%
      nrow()
    
    founder_no_progeny <- founders - length(unique(c(founder_sires, founder_dams)))
    
  cat("Founders:", format(founders, big.mark = ","), "\n")
  cat("   -Progeny:", format(stats_ext$founder_total_progeny, big.mark = ","), "\n")
  cat("   -Sires:", format(stats_ext$founder_sires, big.mark = ","), "\n")
  cat("       -Progeny:", format(stats_ext$founder_sire_progeny, big.mark = ","), "\n")
  cat("   -Dams:", format(stats_ext$founder_dams, big.mark = ","), "\n")
  cat("       -Progeny:", format(stats_ext$founder_dam_progeny, big.mark = ","), "\n")
  cat("   -With no progeny:", format(stats_ext$founder_no_progeny, big.mark = ","), "\n\n")
    
    # Non-founder statistics
    cat("--- NON-FOUNDER STATISTICS ---\n")
    non_founder_ped <- ped %>% filter(!ID %in% founder_ids)
    non_founder_sires <- non_founder_ped %>% 
      filter(ID %in% unique_sires) %>%
      pull(ID) %>%
      unique()
    non_founder_dams <- non_founder_ped %>% 
      filter(ID %in% unique_dams) %>%
      pull(ID) %>%
      unique()
    
    non_founder_sire_progeny <- ped %>% 
      filter(Sire %in% non_founder_ped$ID) %>%
      nrow()
    non_founder_dam_progeny <- ped %>% 
      filter(Dam %in% non_founder_ped$ID) %>%
      nrow()
    
  cat("Non-founders:", format(non_founders, big.mark = ","), "\n")
  cat("   -Sires:", format(stats_ext$non_founder_sires, big.mark = ","), "\n")
  cat("       -Progeny:", format(stats_ext$non_founder_sire_progeny, big.mark = ","), "\n")
  cat("   -Dams:", format(stats_ext$non_founder_dams, big.mark = ","), "\n")
  cat("       -Progeny:", format(stats_ext$non_founder_dam_progeny, big.mark = ","), "\n")
    cat("   -Only with known sire:", format(only_sire, big.mark = ","), "\n")
    cat("   -Only with known dam:", format(only_dam, big.mark = ","), "\n")
    cat("   -With known sire and dam:", format(both_parents, big.mark = ","), "\n\n")
    
    # Full-sib groups
    cat("--- FULL-SIB GROUPS ---\n")
    full_sibs <- ped %>%
      filter(!is.na(Sire), !is.na(Dam), Sire != "", Dam != "", Sire != "0", Dam != "0") %>%
      group_by(Sire, Dam) %>%
      summarise(family_size = n(), .groups = "drop") %>%
      filter(family_size >= 2)
    
    if (nrow(full_sibs) > 0) {
      cat("Full-sib groups:", format(nrow(full_sibs), big.mark = ","), "\n")
      cat("   -Average family size:", format(mean(full_sibs$family_size), digits = 4), "\n")
      cat("       -Maximum:", max(full_sibs$family_size), "\n")
      cat("       -Minimum:", min(full_sibs$family_size), "\n\n")
    } else {
      cat("Full-sib groups: 0\n\n")
    }
    
    # Inbreeding statistics (if available)
    f_vals <- f_values()
    if (!is.null(f_vals) && nrow(f_vals) > 0 && "F" %in% names(f_vals)) {
      cat("--- INBREEDING STATISTICS ---\n")
      f_numeric <- f_vals$F[!is.na(f_vals$F)]
      inbreds <- sum(f_numeric > 0)
      
      cat("Evaluated individuals:", format(length(f_numeric), big.mark = ","), "\n")
      cat("Inbreds in total:", format(inbreds, big.mark = ","), "\n")
      cat("Inbreds in evaluated:", format(inbreds, big.mark = ","), "\n\n")
      
      # Inbreeding coefficient distribution
      cat("Distribution of inbreeding coefficients\n")
      cat("-----------------------------------------------------------\n")
      breaks <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 
                  0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)
      labels <- c("0.00 < F <= 0.05", "0.05 < F <= 0.10", "0.10 < F <= 0.15", 
                  "0.15 < F <= 0.20", "0.20 < F <= 0.25", "0.25 < F <= 0.30",
                  "0.30 < F <= 0.35", "0.35 < F <= 0.40", "0.40 < F <= 0.45",
                  "0.45 < F <= 0.50", "0.50 < F <= 0.55", "0.55 < F <= 0.60",
                  "0.60 < F <= 0.65", "0.65 < F <= 0.70", "0.70 < F <= 0.75",
                  "0.75 < F <= 0.80", "0.80 < F <= 0.85", "0.85 < F <= 0.90",
                  "0.90 < F <= 0.95", "0.95 < F <= 1.00")
      
      f_zero <- sum(f_numeric == 0, na.rm = TRUE)
      f_numeric_pos <- f_numeric[f_numeric > 0]
      f_cut <- cut(f_numeric_pos, breaks = breaks, labels = labels, include.lowest = FALSE)
      f_table <- table(f_cut)
      
      cat(sprintf("%30s %20s\n", "F = 0", format(f_zero, big.mark = ",")))
      for (i in seq_along(labels)) {
        count <- if (labels[i] %in% names(f_table)) f_table[labels[i]] else 0
        cat(sprintf("%30s %20s\n", labels[i], format(count, big.mark = ",")))
      }
      cat("-----------------------------------------------------------\n\n")
      
      # Summary statistics
      cat("--- SUMMARY STATISTICS ---\n")
      cat("A: Number of individuals:", format(n, big.mark = ","), "\n")
      cat("B: Number of inbreds:", format(inbreds, big.mark = ","), "\n")
      cat("C: Number of founders:", format(founders, big.mark = ","), "\n")
      cat("D: Number of individuals with both known parents:", format(both_parents, big.mark = ","), "\n")
      cat("E: Number of individuals with no progeny:", format(individuals_without_progeny, big.mark = ","), "\n\n")
      
      cat("G: Average inbreeding coefficients:", format(mean(f_numeric), digits = 7), "\n")
      inbred_f <- f_numeric[f_numeric > 0]
      if (length(inbred_f) > 0) {
        cat("H: Average inbreeding coefficients in the inbreds:", format(mean(inbred_f), digits = 7), "\n")
      } else {
        cat("H: Average inbreeding coefficients in the inbreds: N/A\n")
      }
      cat("I: Maximum of inbreeding coefficients:", format(max(f_numeric), digits = 7), "\n")
      cat("J: Minimum of inbreeding coefficients:", format(min(f_numeric), digits = 7), "\n")
    } else {
      cat("--- INBREEDING STATISTICS ---\n")
      cat("No inbreeding coefficients calculated.\n")
      cat("Inbreeding coefficients are calculated automatically after data processing.\n\n")
    }
    
    # Longest ancestral path (LAP) - calculate depth distribution
    cat("\n--- LONGEST ANCESTRAL PATH (LAP) ---\n")
    tryCatch({
      # Use Rcpp function if available, otherwise fall back to R version
      if (exists("use_rcpp") && use_rcpp && exists("fast_lap_distribution", mode = "function")) {
        # Convert to character vectors for Rcpp
        ids_char <- as.character(ped$ID)
        sires_char <- as.character(ifelse(is.na(ped$Sire), "", ped$Sire))
        dams_char <- as.character(ifelse(is.na(ped$Dam), "", ped$Dam))
        
        # Determine sample size
        sample_size <- if (n > 10000) min(5000, n) else n
        
        
        # Call Rcpp function
        lap_distribution <- fast_lap_distribution(ids_char, sires_char, dams_char, sample_size, 20)
      } else {
        # Fallback to R version
        if (n > 10000) {
          sample_size <- min(5000, n)
          sample_ids <- sample(ped$ID, sample_size)
        } else {
          sample_ids <- ped$ID
          sample_size <- n
        }
        
        # Initialize distribution
        lap_distribution <- numeric(20)  # 0-19 generations
        names(lap_distribution) <- 0:19
        
        # Calculate depth for each individual
        for (id in sample_ids) {
          depth <- 0
          visited <- character()
          current_gen <- id
          
          while (length(current_gen) > 0 && depth < 20) {
            next_gen <- character()
            for (ind in current_gen) {
              if (ind %in% visited) next
              visited <- c(visited, ind)
              
              idx <- which(ped$ID == ind)
              if (length(idx) > 0) {
                sire <- ped$Sire[idx[1]]
                dam <- ped$Dam[idx[1]]
                
                if (!is.na(sire) && sire != "" && sire != "0") {
                  next_gen <- c(next_gen, sire)
                }
                if (!is.na(dam) && dam != "" && dam != "0") {
                  next_gen <- c(next_gen, dam)
                }
              }
            }
            
            if (length(next_gen) > 0) {
              depth <- depth + 1
              current_gen <- unique(next_gen)
            } else {
              break
            }
          }
          
          if (depth < 20) {
            lap_distribution[as.character(depth)] <- lap_distribution[as.character(depth)] + 1
          }
        }
        
        # Scale to total population if sampled
        if (n > sample_size) {
          lap_distribution <- round(lap_distribution * (n / sample_size))
        }
      }
      
      # Display distribution
      for (i in 0:19) {
        count <- lap_distribution[as.character(i)]
        if (count > 0 || i <= 5) {  # Show at least first few generations
          cat(sprintf("%20s %20s\n", i, format(count, big.mark = ",")))
        }
      }
      
      # Calculate and display mean generation depth
      total_count <- sum(lap_distribution, na.rm = TRUE)
      if (total_count > 0) {
        weighted_sum <- sum(as.numeric(names(lap_distribution)) * lap_distribution, na.rm = TRUE)
        mean_generation_depth <- weighted_sum / total_count
        cat("\nMean generation depth:", format(mean_generation_depth, digits = 4), "\n")
      }
    }, error = function(e) {
      cat("LAP calculation error:", e$message, "\n")
      # Fallback: show founders only
      founders_count <- sum(is_missing_parent(ped$Sire) & is_missing_parent(ped$Dam))
      cat("Founders (LAP = 0):", format(founders_count, big.mark = ","), "\n")
    })
  })
  
  # Download structure report
  output$download_structure_report <- downloadHandler(
    filename = function() {
      paste0("pedigree_structure_", Sys.Date(), ".txt")
    },
    content = function(file) {
      req(ped_data())
      ped <- ped_data()
      
      if (is.null(ped)) {
        writeLines("Error: Data processing failed.", file)
        return()
      }
      
      # Capture the output
      report_lines <- capture.output({
        # Basic statistics
        n <- nrow(ped)
        stats <- get_basic_stats(ped)
        founders <- stats$founders
        non_founders <- stats$non_founders
        both_parents <- stats$both_parents
        only_sire <- stats$only_sire
        only_dam <- stats$only_dam
        stats_ext <- get_extended_stats(ped)
        
        cat("========================================\n")
        cat("PEDIGREE STRUCTURE REPORT\n")
        cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
        cat("========================================\n\n")
        
        cat("--- BASIC STATISTICS ---\n")
        cat("Individuals in total:", format(n, big.mark = ","), "\n")
        cat("Founders:", format(founders, big.mark = ","), "\n")
        cat("Non-founders:", format(non_founders, big.mark = ","), "\n")
        cat("With both parents:", format(both_parents, big.mark = ","), "\n")
        cat("Only with known sire:", format(only_sire, big.mark = ","), "\n")
        cat("Only with known dam:", format(only_dam, big.mark = ","), "\n\n")
        
        # Parent statistics
        sire_missing <- is_missing_parent(ped$Sire)
        dam_missing <- is_missing_parent(ped$Dam)
        all_ids <- unique(ped$ID)
        unique_sires <- unique(ped$Sire[!is.na(ped$Sire) & ped$Sire != "" & ped$Sire != "0"])
        unique_dams <- unique(ped$Dam[!is.na(ped$Dam) & ped$Dam != "" & ped$Dam != "0"])
        
        sire_progeny <- ped %>% 
          filter(!is.na(Sire), Sire != "", Sire != "0") %>%
          count(Sire, name = "progeny_count")
        total_sire_progeny <- sum(sire_progeny$progeny_count)
        
        dam_progeny <- ped %>% 
          filter(!is.na(Dam), Dam != "", Dam != "0") %>%
          count(Dam, name = "progeny_count")
        total_dam_progeny <- sum(dam_progeny$progeny_count)
        
        individuals_with_progeny <- unique(c(unique_sires, unique_dams))
        individuals_with_progeny <- individuals_with_progeny[individuals_with_progeny %in% all_ids]
        individuals_without_progeny <- n - length(individuals_with_progeny)
        
        cat("--- PARENT STATISTICS ---\n")
        cat("Sires in total:", format(stats_ext$unique_sires, big.mark = ","), "\n")
        cat("   -Progeny:", format(stats_ext$total_sire_progeny, big.mark = ","), "\n")
        cat("Dams in total:", format(stats_ext$unique_dams, big.mark = ","), "\n")
        cat("   -Progeny:", format(stats_ext$total_dam_progeny, big.mark = ","), "\n")
        cat("Individuals with progeny:", format(stats_ext$individuals_with_progeny, big.mark = ","), "\n")
        cat("Individuals with no progeny:", format(stats_ext$individuals_without_progeny, big.mark = ","), "\n\n")
        
        # Founder statistics
    founder_ids <- ped$ID[sire_missing & dam_missing]
        founder_ped <- ped %>% filter(ID %in% founder_ids)
        founder_sires <- founder_ped %>% filter(ID %in% unique_sires) %>% pull(ID)
        founder_dams <- founder_ped %>% filter(ID %in% unique_dams) %>% pull(ID)
        
        founder_sire_progeny <- ped %>% filter(Sire %in% founder_ids) %>% nrow()
        founder_dam_progeny <- ped %>% filter(Dam %in% founder_ids) %>% nrow()
        founder_total_progeny <- ped %>% filter(Sire %in% founder_ids | Dam %in% founder_ids) %>% nrow()
        founder_no_progeny <- founders - length(unique(c(founder_sires, founder_dams)))
        
        cat("--- FOUNDER STATISTICS ---\n")
        cat("Founders:", format(founders, big.mark = ","), "\n")
        cat("   -Progeny:", format(stats_ext$founder_total_progeny, big.mark = ","), "\n")
        cat("   -Sires:", format(stats_ext$founder_sires, big.mark = ","), "\n")
        cat("       -Progeny:", format(stats_ext$founder_sire_progeny, big.mark = ","), "\n")
        cat("   -Dams:", format(stats_ext$founder_dams, big.mark = ","), "\n")
        cat("       -Progeny:", format(stats_ext$founder_dam_progeny, big.mark = ","), "\n")
        cat("   -With no progeny:", format(stats_ext$founder_no_progeny, big.mark = ","), "\n\n")
        
        # Non-founder statistics
        non_founder_ped <- ped %>% filter(!ID %in% founder_ids)
        non_founder_sires <- non_founder_ped %>% filter(ID %in% unique_sires) %>% pull(ID) %>% unique()
        non_founder_dams <- non_founder_ped %>% filter(ID %in% unique_dams) %>% pull(ID) %>% unique()
        non_founder_sire_progeny <- ped %>% filter(Sire %in% non_founder_ped$ID) %>% nrow()
        non_founder_dam_progeny <- ped %>% filter(Dam %in% non_founder_ped$ID) %>% nrow()
        
        cat("--- NON-FOUNDER STATISTICS ---\n")
        cat("Non-founders:", format(non_founders, big.mark = ","), "\n")
        cat("   -Sires:", format(stats_ext$non_founder_sires, big.mark = ","), "\n")
        cat("       -Progeny:", format(stats_ext$non_founder_sire_progeny, big.mark = ","), "\n")
        cat("   -Dams:", format(stats_ext$non_founder_dams, big.mark = ","), "\n")
        cat("       -Progeny:", format(stats_ext$non_founder_dam_progeny, big.mark = ","), "\n")
        cat("   -Only with known sire:", format(only_sire, big.mark = ","), "\n")
        cat("   -Only with known dam:", format(only_dam, big.mark = ","), "\n")
        cat("   -With known sire and dam:", format(both_parents, big.mark = ","), "\n\n")
        
        # Full-sib groups
        full_sibs <- ped %>%
          filter(!is.na(Sire), !is.na(Dam), Sire != "", Dam != "", Sire != "0", Dam != "0") %>%
          group_by(Sire, Dam) %>%
          summarise(family_size = n(), .groups = "drop") %>%
          filter(family_size >= 2)
        
        cat("--- FULL-SIB GROUPS ---\n")
        if (nrow(full_sibs) > 0) {
          cat("Full-sib groups:", format(nrow(full_sibs), big.mark = ","), "\n")
          cat("   -Average family size:", format(mean(full_sibs$family_size), digits = 4), "\n")
          cat("       -Maximum:", max(full_sibs$family_size), "\n")
          cat("       -Minimum:", min(full_sibs$family_size), "\n\n")
        } else {
          cat("Full-sib groups: 0\n\n")
        }
        
        # Inbreeding statistics
        f_vals <- f_values()
        if (!is.null(f_vals) && nrow(f_vals) > 0 && "F" %in% names(f_vals)) {
          f_numeric <- f_vals$F[!is.na(f_vals$F)]
          inbreds <- sum(f_numeric > 0)
          
          cat("--- INBREEDING STATISTICS ---\n")
          cat("Evaluated individuals:", format(length(f_numeric), big.mark = ","), "\n")
          cat("Inbreds in total:", format(inbreds, big.mark = ","), "\n")
          cat("Inbreds in evaluated:", format(inbreds, big.mark = ","), "\n\n")
          
          cat("Distribution of inbreeding coefficients\n")
          cat("-----------------------------------------------------------\n")
          breaks <- c(0, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 
                      0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 1.00)
          labels <- c("0.00 < F <= 0.05", "0.05 < F <= 0.10", "0.10 < F <= 0.15", 
                      "0.15 < F <= 0.20", "0.20 < F <= 0.25", "0.25 < F <= 0.30",
                      "0.30 < F <= 0.35", "0.35 < F <= 0.40", "0.40 < F <= 0.45",
                      "0.45 < F <= 0.50", "0.50 < F <= 0.55", "0.55 < F <= 0.60",
                      "0.60 < F <= 0.65", "0.65 < F <= 0.70", "0.70 < F <= 0.75",
                      "0.75 < F <= 0.80", "0.80 < F <= 0.85", "0.85 < F <= 0.90",
                      "0.90 < F <= 0.95", "0.95 < F <= 1.00")
          
          f_zero <- sum(f_numeric == 0, na.rm = TRUE)
          f_numeric_pos <- f_numeric[f_numeric > 0]
          f_cut <- cut(f_numeric_pos, breaks = breaks, labels = labels, include.lowest = FALSE)
          f_table <- table(f_cut)
          
          cat(sprintf("%30s %20s\n", "F = 0", format(f_zero, big.mark = ",")))
          for (i in seq_along(labels)) {
            count <- if (labels[i] %in% names(f_table)) f_table[labels[i]] else 0
            cat(sprintf("%30s %20s\n", labels[i], format(count, big.mark = ",")))
          }
          cat("-----------------------------------------------------------\n\n")
          
          cat("--- SUMMARY STATISTICS ---\n")
          cat("A: Number of individuals:", format(n, big.mark = ","), "\n")
          cat("B: Number of inbreds:", format(inbreds, big.mark = ","), "\n")
          cat("C: Number of founders:", format(founders, big.mark = ","), "\n")
          cat("D: Number of individuals with both known parents:", format(both_parents, big.mark = ","), "\n")
          cat("E: Number of individuals with no progeny:", format(individuals_without_progeny, big.mark = ","), "\n\n")
          
          cat("G: Average inbreeding coefficients:", format(mean(f_numeric), digits = 7), "\n")
          inbred_f <- f_numeric[f_numeric > 0]
          if (length(inbred_f) > 0) {
            cat("H: Average inbreeding coefficients in the inbreds:", format(mean(inbred_f), digits = 7), "\n")
          }
          cat("I: Maximum of inbreeding coefficients:", format(max(f_numeric), digits = 7), "\n")
          cat("J: Minimum of inbreeding coefficients:", format(min(f_numeric), digits = 7), "\n")
        }
        
        # Longest ancestral path (LAP)
        cat("\n--- LONGEST ANCESTRAL PATH (LAP) ---\n")
        tryCatch({
          # Use Rcpp function if available, otherwise fall back to R version
          if (exists("use_rcpp") && use_rcpp && exists("fast_lap_distribution", mode = "function")) {
            # Convert to character vectors for Rcpp
            ids_char <- as.character(ped$ID)
            sires_char <- as.character(ifelse(is.na(ped$Sire), "", ped$Sire))
            dams_char <- as.character(ifelse(is.na(ped$Dam), "", ped$Dam))
            
            # Determine sample size
            sample_size <- if (n > 10000) min(5000, n) else n
            
            
            # Call Rcpp function
            lap_distribution <- fast_lap_distribution(ids_char, sires_char, dams_char, sample_size, 20)
          } else {
            # Fallback to R version
            if (n > 10000) {
              sample_size <- min(5000, n)
              sample_ids <- sample(ped$ID, sample_size)
            } else {
              sample_ids <- ped$ID
              sample_size <- n
            }
            
            lap_distribution <- numeric(20)
            names(lap_distribution) <- 0:19
            
            for (id in sample_ids) {
              depth <- 0
              visited <- character()
              current_gen <- id
              
              while (length(current_gen) > 0 && depth < 20) {
                next_gen <- character()
                for (ind in current_gen) {
                  if (ind %in% visited) next
                  visited <- c(visited, ind)
                  
                  idx <- which(ped$ID == ind)
                  if (length(idx) > 0) {
                    sire <- ped$Sire[idx[1]]
                    dam <- ped$Dam[idx[1]]
                    
                    if (!is.na(sire) && sire != "" && sire != "0") {
                      next_gen <- c(next_gen, sire)
                    }
                    if (!is.na(dam) && dam != "" && dam != "0") {
                      next_gen <- c(next_gen, dam)
                    }
                  }
                }
                
                if (length(next_gen) > 0) {
                  depth <- depth + 1
                  current_gen <- unique(next_gen)
                } else {
                  break
                }
              }
              
              if (depth < 20) {
                lap_distribution[as.character(depth)] <- lap_distribution[as.character(depth)] + 1
              }
            }
            
            if (n > sample_size) {
              lap_distribution <- round(lap_distribution * (n / sample_size))
            }
          }
          
          for (i in 0:19) {
            count <- lap_distribution[as.character(i)]
            if (count > 0 || i <= 5) {
              cat(sprintf("%20s %20s\n", i, format(count, big.mark = ",")))
            }
          }
          
          # Calculate and display mean generation depth
          total_count <- sum(lap_distribution, na.rm = TRUE)
          if (total_count > 0) {
            weighted_sum <- sum(as.numeric(names(lap_distribution)) * lap_distribution, na.rm = TRUE)
            mean_generation_depth <- weighted_sum / total_count
            cat("\nMean generation depth:", format(mean_generation_depth, digits = 4), "\n")
          }
        }, error = function(e) {
          cat("LAP calculation error:", e$message, "\n")
        })
      })
      
      writeLines(report_lines, file)
    }
  )
  
  # Helper function to calculate inbreeding using fast C++ method (Rcpp)
  calculate_inbreeding_cpp <- function(ped_clean, progress_callback = NULL, status_callback = NULL) {
    tryCatch({
      if (!is.null(progress_callback)) progress_callback(0.1, "Preparing pedigree data for C++ inbreeding...")
      if (!is.null(status_callback)) {
        status_callback(list(
          calculating = TRUE,
          progress = 0.1,
          message = "Preparing pedigree data for C++ inbreeding...",
          n_individuals = nrow(ped_clean)
        ))
      }

      ids <- as.character(ped_clean$ID)
      sires <- ifelse(is.na(ped_clean$Sire) | ped_clean$Sire == "" | ped_clean$Sire == "0", "0", as.character(ped_clean$Sire))
      dams <- ifelse(is.na(ped_clean$Dam) | ped_clean$Dam == "" | ped_clean$Dam == "0", "0", as.character(ped_clean$Dam))

      if (!is.null(progress_callback)) progress_callback(0.5, "Computing inbreeding coefficients (C++ method)...")
      if (!is.null(status_callback)) {
        status_callback(list(
          calculating = TRUE,
          progress = 0.5,
          message = "Computing inbreeding coefficients (C++ method)...",
          n_individuals = nrow(ped_clean)
        ))
      }

      f_vec <- fast_inbreeding_cpp(ids, sires, dams)
      if (is.null(f_vec) || length(f_vec) == 0) {
        stop("C++ inbreeding calculation returned empty results.")
      }
      if (length(f_vec) != nrow(ped_clean)) {
        stop("C++ inbreeding result length mismatch.")
      }

      result <- tibble(ID = as.character(ped_clean$ID), F = as.numeric(f_vec))
      return(result)
    }, error = function(e) {
      error_msg <- paste("Error in C++ inbreeding calculation:", e$message)
      cat(error_msg, "\n")
      return(NULL)
    })
  }

  # Helper function to calculate inbreeding using inbupgf90 (faster method)
  calculate_inbreeding_inbupgf90 <- function(ped_clean, progress_callback = NULL, status_callback = NULL) {
    # Create temporary directory in app directory
    temp_dir <- file.path(getwd(), "temp_inbreeding")
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Ensure cleanup on exit
    on.exit({
      # Clean up temporary files (including pedigree.txt, pedigree.txt.solinb, pedigree.txt.errors)
      temp_files <- list.files(temp_dir, pattern = "^(pedigree|pedigree\\.txt|.*\\.solinb|.*\\.errors)$", full.names = TRUE)
      tryCatch({
        if (length(temp_files) > 0) {
          file.remove(temp_files)
        }
      }, error = function(e) {
        cat("Warning: Could not clean up all temporary files:", e$message, "\n")
      })
    }, add = TRUE)
    
    tryCatch({
      # Update progress
      if (!is.null(progress_callback)) progress_callback(0.1, "Preparing pedigree file for inbupgf90...")
      if (!is.null(status_callback)) {
        status_callback(list(
          calculating = TRUE,
          progress = 0.1,
          message = "Preparing pedigree file for inbupgf90...",
          n_individuals = nrow(ped_clean)
        ))
      }
      
      # Prepare pedigree data for inbupgf90 format (progeny sire dam, space-separated)
      # Replace NA/empty/"0" with "0" for inbupgf90
      ped_for_file <- ped_clean %>%
        mutate(
          Sire = ifelse(is.na(Sire) | Sire == "" | Sire == "0", "0", as.character(Sire)),
          Dam = ifelse(is.na(Dam) | Dam == "" | Dam == "0", "0", as.character(Dam))
        )
      
      # Write pedigree.txt file (progeny sire dam format, space-separated)
      pedigree_file <- file.path(temp_dir, "pedigree.txt")
      write.table(
        ped_for_file %>% select(ID, Sire, Dam),
        file = pedigree_file,
        sep = " ",
        quote = FALSE,
        row.names = FALSE,
        col.names = FALSE
      )
      
      # Update progress
      if (!is.null(progress_callback)) progress_callback(0.3, "Running inbupgf90...")
      if (!is.null(status_callback)) {
        status_callback(list(
          calculating = TRUE,
          progress = 0.3,
          message = "Running inbupgf90 (fast method)...",
          n_individuals = nrow(ped_clean)
        ))
      }
      
      # Run inbupgf90 (it will look for pedigree.txt in current directory)
      # Temporarily change to temp directory for execution
      old_wd <- getwd()
      tryCatch({
        setwd(temp_dir)
        exit_code <- linkbreedeR::run_inbupgf90("--pedfile pedigree.txt")
      }, finally = {
        setwd(old_wd)
      })
      
      # Check for errors (after restoring working directory)
      # inbupgf90 creates error file as pedigree.txt.errors (based on input filename)
      errors_file <- file.path(temp_dir, "pedigree.txt.errors")
      if (file.exists(errors_file)) {
        error_content <- readLines(errors_file, warn = FALSE)
        if (length(error_content) > 0 && any(nchar(trimws(error_content)) > 0)) {
          # Non-empty error file - fall back to pedigreeTools
          warning_msg <- paste("inbupgf90 reported errors, falling back to pedigreeTools. Errors:", 
                              paste(error_content, collapse = "; "))
          cat(warning_msg, "\n")
          return(NULL)  # Signal to use fallback
        }
      }
      
      # Check exit code
      if (!is.null(exit_code) && exit_code != 0) {
        warning_msg <- paste("inbupgf90 exited with code", exit_code, ", falling back to pedigreeTools")
        cat(warning_msg, "\n")
        return(NULL)  # Signal to use fallback
      }
      
      # Update progress
      if (!is.null(progress_callback)) progress_callback(0.6, "Parsing inbupgf90 results...")
      if (!is.null(status_callback)) {
        status_callback(list(
          calculating = TRUE,
          progress = 0.6,
          message = "Parsing inbupgf90 results...",
          n_individuals = nrow(ped_clean)
        ))
      }
      
      # Parse .solinb file (two columns: ID and F, space-separated)
      # inbupgf90 creates output file as pedigree.txt.solinb (based on input filename)
      solinb_file <- file.path(temp_dir, "pedigree.txt.solinb")
      if (!file.exists(solinb_file)) {
        warning_msg <- "inbupgf90 output file (.solinb) not found, falling back to pedigreeTools"
        cat(warning_msg, "\n")
        return(NULL)  # Signal to use fallback
      }
      
      # Read .solinb file
      # Format: fixed-width with ID and F coefficient, separated by multiple spaces
      solinb_data <- tryCatch({
        # Read with flexible whitespace separator (handles multiple spaces)
        raw_data <- read.table(
          solinb_file,
          sep = "",  # Any whitespace (space, tab, etc.) - handles multiple spaces
          header = FALSE,
          stringsAsFactors = FALSE,
          fill = TRUE,  # Allow rows with different numbers of columns
          strip.white = TRUE  # Remove leading/trailing whitespace
        )
        
        # Check if we have at least 2 columns
        if (ncol(raw_data) < 2) {
          stop("Expected at least 2 columns in .solinb file, found ", ncol(raw_data))
        }
        
        # Extract first two columns (ID and F)
        solinb_data <- data.frame(
          ID = as.character(raw_data[, 1]),
          F = as.numeric(raw_data[, 2]),
          stringsAsFactors = FALSE
        )
        
        # Remove any rows with NA F values (invalid data)
        solinb_data <- solinb_data[!is.na(solinb_data$F), ]
        
        if (nrow(solinb_data) == 0) {
          stop("No valid inbreeding coefficients found in .solinb file")
        }
        
        solinb_data
      }, error = function(e) {
        warning_msg <- paste("Error parsing .solinb file:", e$message, ", falling back to pedigreeTools")
        cat(warning_msg, "\n")
        return(NULL)
      })
      
      if (is.null(solinb_data)) {
        return(NULL)  # Signal to use fallback
      }
      
      # Update progress
      if (!is.null(progress_callback)) progress_callback(0.8, "Merging results...")
      if (!is.null(status_callback)) {
        status_callback(list(
          calculating = TRUE,
          progress = 0.8,
          message = "Merging results...",
          n_individuals = nrow(ped_clean)
        ))
      }
      
      # Merge with all individuals - set F=0 for individuals not in .solinb
      all_ids <- tibble(ID = as.character(ped_clean$ID))
      result <- all_ids %>%
        left_join(solinb_data, by = "ID") %>%
        mutate(F = ifelse(is.na(F), 0, F)) %>%
        select(ID, F)
      
      # Ensure result matches input order
      result <- result[match(ped_clean$ID, result$ID), ]
      
      return(result)
      
    }, error = function(e) {
      error_msg <- paste("Error in inbupgf90 calculation:", e$message, ", falling back to pedigreeTools")
      cat(error_msg, "\n")
      return(NULL)  # Signal to use fallback
    })
  }
  
  # Calculate F (automatically when auto_process is enabled, or manually)
  f_values <- reactive({
    # Manual mode: requires button click
    if (!isTRUE(input$auto_process)) {
      req(input$calc_f)
    }
    
    req(ped_data())
    ped <- ped_data()
    
    # Check if we have cached values for this exact pedigree
    cached_f <- f_values_cache()
    cached_hash <- f_values_cache_hash()
    if (!is.null(cached_f) && all(c("ID", "Sire", "Dam") %in% names(ped))) {
      current_hash <- digest::digest(ped[, c("ID", "Sire", "Dam")], algo = "xxhash64")
      if (!is.null(cached_hash) && identical(current_hash, cached_hash)) {
        return(cached_f)
      }
    }
    
    # Check if ped_data returned NULL (due to column mapping errors or validation failure)
    if (is.null(ped)) {
      return(tibble(ID = character(), F = numeric()))
    }
    
    # Check if data validation passed
    validation_status <- data_validation_status()
    if (!validation_status$valid) {
      showNotification("❌ Data validation failed. Cannot calculate inbreeding coefficients.", type = "error", duration = 5)
      return(tibble(ID = character(), F = numeric()))
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      showNotification("Error: Required columns (ID, Sire, Dam) not found in processed data", type = "error")
      return(tibble(ID = character(), F = numeric()))
    }
    
    n_individuals <- nrow(ped)
    
    # Update calculation status
    f_calculation_status(list(
      calculating = TRUE,
      progress = 0,
      message = "Starting calculation...",
      n_individuals = n_individuals
    ))
    
    withProgress(message = paste0("Calculating inbreeding coefficients for ", format(n_individuals, big.mark = ","), " individuals..."), {
      incProgress(0.1, detail = "Preparing pedigree data...")
      
      # Update progress status
      f_calculation_status(list(
        calculating = TRUE,
        progress = 0.1,
        message = "Preparing pedigree data...",
        n_individuals = n_individuals
      ))
      
      tryCatch({
        # Check if we have enough individuals with parent information
        sire_missing <- is_missing_parent(ped$Sire)
        dam_missing <- is_missing_parent(ped$Dam)
        n_with_parents <- sum(!sire_missing | !dam_missing)
        n_founders <- sum(sire_missing & dam_missing)
        
        if (n_with_parents == 0) {
          stop("No individuals have parent information (Sire or Dam). Cannot calculate inbreeding coefficients.")
        }
        
        if (n_founders == n_individuals) {
          stop("All individuals are founders (no parents). Cannot calculate inbreeding coefficients.")
        }
        
        incProgress(0.2, detail = "Building pedigree object...")
        
        # Update progress status
        f_calculation_status(list(
          calculating = TRUE,
          progress = 0.2,
          message = paste0("Building pedigree object... (", n_with_parents, " with parents)"),
          n_individuals = n_individuals
        ))
        
        # Prepare pedigree data - replace empty strings and ensure proper NA handling
        ped_clean <- ped %>%
          mutate(
            ID = as.character(ID),
            Sire = ifelse(is.na(Sire) | Sire == "" | Sire == "0", NA_character_, as.character(Sire)),
            Dam = ifelse(is.na(Dam) | Dam == "" | Dam == "0", NA_character_, as.character(Dam))
          )
        
        # Check for duplicate IDs
        if (anyDuplicated(ped_clean$ID)) {
          stop("Duplicate IDs found in pedigree. Each individual must have a unique ID.")
        }
        
        # Check if Sire/Dam IDs exist in ID column
        all_ids <- unique(ped_clean$ID)
        valid_sires <- ped_clean$Sire[!is.na(ped_clean$Sire)]
        valid_dams <- ped_clean$Dam[!is.na(ped_clean$Dam)]
        missing_sires <- setdiff(valid_sires, all_ids)
        missing_dams <- setdiff(valid_dams, all_ids)
        
        # Try to use fast C++ method if available (skip editPed for C++ method)
        if (use_fast_inbreeding_cpp) {
          # C++ method treats missing parents as founders
          if (length(missing_sires) > 0 || length(missing_dams) > 0) {
            cat("Note: Some parent references not found in ID column. C++ method will treat them as founders.\n")
          }
          incProgress(0.4, detail = "Computing inbreeding coefficients (using fast C++ method)...")
          
          # Update progress status
          f_calculation_status(list(
            calculating = TRUE,
            progress = 0.4,
            message = "Computing inbreeding coefficients (using fast C++ method)...",
            n_individuals = n_individuals
          ))
          
          # Try fast C++ method first
          result_fast <- calculate_inbreeding_cpp(
            ped_clean,
            progress_callback = function(progress, detail) {
              incProgress(progress - 0.4, detail = detail)
            },
            status_callback = function(status) {
              f_calculation_status(status)
            }
          )
          
          # If fast method succeeded, use it; otherwise fall back to pedigreeTools
          if (!is.null(result_fast) && nrow(result_fast) > 0) {
            incProgress(0.8, detail = "Finalizing results...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.8,
              message = "Finalizing results...",
              n_individuals = n_individuals
            ))
            
            result <- result_fast
          } else {
            if (!pedigreeTools_available) {
              stop("pedigreeTools is not installed; cannot fall back from fast method. Please install pedigreeTools or enable inbupgf90.")
            }
            # Fall back to pedigreeTools - need to do editPed and create ped_obj
            cat("Falling back to pedigreeTools method...\n")
            
            # Use editPed() to complete and sort pedigree (ensures ancestors precede progeny)
            incProgress(0.25, detail = "Completing and sorting pedigree...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.25,
              message = "Completing and sorting pedigree...",
              n_individuals = n_individuals
            ))
            
            # Check for missing parent references (pedigreeTools requires them)
            if (length(missing_sires) > 0 || length(missing_dams) > 0) {
              error_msg <- paste0("Invalid parent references found. ",
                                 if(length(missing_sires) > 0) paste0(length(missing_sires), " missing Sire(s)") else "",
                                 if(length(missing_sires) > 0 && length(missing_dams) > 0) ", " else "",
                                 if(length(missing_dams) > 0) paste0(length(missing_dams), " missing Dam(s)") else "",
                                 ". Please use 'Fix Pedigree' button to correct.")
              stop(error_msg)
            }
            
            # Use editPed() to complete missing ancestors and sort (ancestors before progeny)
            ped_edited <- pedigreeTools::editPed(
              sire = ped_clean$Sire,
              dam = ped_clean$Dam,
              label = ped_clean$ID
            )
            
            incProgress(0.3, detail = "Building pedigree object...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.3,
              message = "Building pedigree object...",
              n_individuals = n_individuals
            ))
            
            # Convert edited pedigree to pedigree object
            ped_obj <- with(ped_edited, pedigreeTools::pedigree(
              label = label,
              sire = sire,
              dam = dam
            ))
            
            incProgress(0.4, detail = "Computing inbreeding coefficients (using pedigreeTools method)...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.4,
              message = "Computing inbreeding coefficients (using pedigreeTools method)...",
              n_individuals = n_individuals
            ))
            
            # Use inbreeding() function instead of getF()
            f_vec <- pedigreeTools::inbreeding(ped_obj)
            
            # Check if result is valid
            if (is.null(f_vec) || length(f_vec) == 0 || all(is.na(f_vec))) {
              stop("Inbreeding calculation returned empty or all-NA results. Please check pedigree structure.")
            }
            
            incProgress(0.8, detail = "Finalizing results...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.8,
              message = "Finalizing results...",
              n_individuals = n_individuals
            ))
            
            result <- tibble(ID = as.character(ped_clean$ID), F = f_vec)
          }
        } else if (use_linkbreedeR) {
          # inbupgf90 doesn't need editPed, so we can use ped_clean directly
          # But we still need to check for missing parent references for validation
          if (length(missing_sires) > 0 || length(missing_dams) > 0) {
            # For inbupgf90, missing parents are allowed (they will be treated as founders)
            # So we just log a warning but continue
            cat("Note: Some parent references not found in ID column. inbupgf90 will treat them as founders.\n")
          }
          incProgress(0.4, detail = "Computing inbreeding coefficients (using fast inbupgf90 method)...")
          
          # Update progress status
          f_calculation_status(list(
            calculating = TRUE,
            progress = 0.4,
            message = "Computing inbreeding coefficients (using fast inbupgf90 method)...",
            n_individuals = n_individuals
          ))
          
          # Try fast method first
          result_fast <- calculate_inbreeding_inbupgf90(
            ped_clean,
            progress_callback = function(progress, detail) {
              incProgress(progress - 0.4, detail = detail)
            },
            status_callback = function(status) {
              f_calculation_status(status)
            }
          )
          
          # If fast method succeeded, use it; otherwise fall back to pedigreeTools
          if (!is.null(result_fast) && nrow(result_fast) > 0) {
            incProgress(0.8, detail = "Finalizing results...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.8,
              message = "Finalizing results...",
              n_individuals = n_individuals
            ))
            
            result <- result_fast
          } else {
            if (!pedigreeTools_available) {
              stop("pedigreeTools is not installed; cannot fall back from inbupgf90. Please install pedigreeTools.")
            }
            # Fall back to pedigreeTools - need to do editPed and create ped_obj
            cat("Falling back to pedigreeTools method...\n")
            
            # Use editPed() to complete and sort pedigree (ensures ancestors precede progeny)
            incProgress(0.25, detail = "Completing and sorting pedigree...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.25,
              message = "Completing and sorting pedigree...",
              n_individuals = n_individuals
            ))
            
            # Check for missing parent references (pedigreeTools requires them)
            if (length(missing_sires) > 0 || length(missing_dams) > 0) {
              error_msg <- paste0("Invalid parent references found. ",
                                 if(length(missing_sires) > 0) paste0(length(missing_sires), " missing Sire(s)") else "",
                                 if(length(missing_sires) > 0 && length(missing_dams) > 0) ", " else "",
                                 if(length(missing_dams) > 0) paste0(length(missing_dams), " missing Dam(s)") else "",
                                 ". Please use 'Fix Pedigree' button to correct.")
              stop(error_msg)
            }
            
            # Use editPed() to complete missing ancestors and sort (ancestors before progeny)
            ped_edited <- pedigreeTools::editPed(
              sire = ped_clean$Sire,
              dam = ped_clean$Dam,
              label = ped_clean$ID
            )
            
            incProgress(0.3, detail = "Building pedigree object...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.3,
              message = "Building pedigree object...",
              n_individuals = n_individuals
            ))
            
            # Convert edited pedigree to pedigree object
            ped_obj <- with(ped_edited, pedigreeTools::pedigree(
              label = label,
              sire = sire,
              dam = dam
            ))
            
            incProgress(0.4, detail = "Computing inbreeding coefficients (using pedigreeTools method)...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.4,
              message = "Computing inbreeding coefficients (using pedigreeTools method)...",
              n_individuals = n_individuals
            ))
            
            # Use inbreeding() function instead of getF()
            f_vec <- pedigreeTools::inbreeding(ped_obj)
            
            # Check if result is valid
            if (is.null(f_vec) || length(f_vec) == 0 || all(is.na(f_vec))) {
              stop("Inbreeding calculation returned empty or all-NA results. Please check pedigree structure.")
            }
            
            incProgress(0.8, detail = "Finalizing results...")
            
            # Update progress status
            f_calculation_status(list(
              calculating = TRUE,
              progress = 0.8,
              message = "Finalizing results...",
              n_individuals = n_individuals
            ))
            
            result <- tibble(ID = as.character(ped_clean$ID), F = f_vec)
          }
        } else {
          if (!pedigreeTools_available) {
            stop("pedigreeTools is not installed; cannot compute inbreeding with the fallback method.")
          }
          # Use pedigreeTools method (original implementation)
          # Check for missing parent references (pedigreeTools requires them)
          if (length(missing_sires) > 0 || length(missing_dams) > 0) {
            error_msg <- paste0("Invalid parent references found. ",
                               if(length(missing_sires) > 0) paste0(length(missing_sires), " missing Sire(s)") else "",
                               if(length(missing_sires) > 0 && length(missing_dams) > 0) ", " else "",
                               if(length(missing_dams) > 0) paste0(length(missing_dams), " missing Dam(s)") else "",
                               ". Please use 'Fix Pedigree' button to correct.")
            stop(error_msg)
          }
          
          # Use editPed() to complete and sort pedigree (ensures ancestors precede progeny)
          incProgress(0.25, detail = "Completing and sorting pedigree...")
          
          # Update progress status
          f_calculation_status(list(
            calculating = TRUE,
            progress = 0.25,
            message = "Completing and sorting pedigree...",
            n_individuals = n_individuals
          ))
          
          # Use editPed() to complete missing ancestors and sort (ancestors before progeny)
          ped_edited <- pedigreeTools::editPed(
            sire = ped_clean$Sire,
            dam = ped_clean$Dam,
            label = ped_clean$ID
          )
          
          incProgress(0.3, detail = "Building pedigree object...")
          
          # Update progress status
          f_calculation_status(list(
            calculating = TRUE,
            progress = 0.3,
            message = "Building pedigree object...",
            n_individuals = n_individuals
          ))
          
          # Convert edited pedigree to pedigree object
          ped_obj <- with(ped_edited, pedigreeTools::pedigree(
            label = label,
            sire = sire,
            dam = dam
          ))
          
          incProgress(0.4, detail = "Computing inbreeding coefficients (using pedigreeTools method)...")
          
          # Update progress status
          f_calculation_status(list(
            calculating = TRUE,
            progress = 0.4,
            message = "Computing inbreeding coefficients (using pedigreeTools method)...",
            n_individuals = n_individuals
          ))
          
          # Use inbreeding() function instead of getF()
          f_vec <- pedigreeTools::inbreeding(ped_obj)
          
          # Check if result is valid
          if (is.null(f_vec) || length(f_vec) == 0 || all(is.na(f_vec))) {
            stop("Inbreeding calculation returned empty or all-NA results. Please check pedigree structure.")
          }
          
          incProgress(0.8, detail = "Finalizing results...")
          
          # Update progress status
          f_calculation_status(list(
            calculating = TRUE,
            progress = 0.8,
            message = "Finalizing results...",
            n_individuals = n_individuals
          ))
          
          result <- tibble(ID = as.character(ped_clean$ID), F = f_vec)
        }
        
        incProgress(1.0, detail = "Complete!")
        
        # Update progress status - calculation complete
        f_calculation_status(list(
          calculating = FALSE,
          progress = 1.0,
          message = "Calculation complete!",
          n_individuals = n_individuals
        ))
        
        # Cache the result
        f_values_cache(result)
        if (all(c("ID", "Sire", "Dam") %in% names(ped))) {
          f_values_cache_hash(digest::digest(ped[, c("ID", "Sire", "Dam")], algo = "xxhash64"))
        } else {
          f_values_cache_hash(NULL)
        }
        
        # Hide progress bar after a short delay
        later::later(function() {
          f_calculation_status(list(
            calculating = FALSE,
            progress = 0,
            message = "",
            n_individuals = 0
          ))
        }, delay = 2.0)
        
        showNotification("✅ Inbreeding coefficients calculated!", type = "message", duration = 3)
        result
      }, error = function(e) {
        # Update progress status - error
        f_calculation_status(list(
          calculating = FALSE,
          progress = 0,
          message = "Calculation failed",
          n_individuals = n_individuals
        ))
        
        error_msg <- paste("⚠️ Error calculating F:", e$message)
        showNotification(error_msg, type = "error", duration = 8)
        cat("Inbreeding calculation error:", e$message, "\n")
        tibble(ID = character(), F = numeric())
      })
    })
  })

  # ---- Inbreeding Trend (requires Birthdate mapping + F values) ----
  output$inb_trend_hint_ui <- renderUI({
    # If plotly isn't available, show a clear hint
    if (!requireNamespace("plotly", quietly = TRUE)) {
      return(div(class = "alert alert-warning", style = "padding: 8px; font-size: 0.9rem;",
                 tags$strong("Plotly not installed. "),
                 "Please install it to view the interactive Inbreeding Trend plot: ",
                 tags$code("install.packages('plotly')")))
    }
    
    # If inbreeding hasn't been calculated yet, guide the user
    cached_f <- f_values_cache()
    if (is.null(cached_f) || nrow(cached_f) == 0) {
      return(div(class = "alert alert-info", style = "padding: 8px; font-size: 0.9rem;",
                 "Inbreeding coefficients are calculated automatically after data processing.",
                 " Please wait for completion."))
    }
    
    NULL
  })

  output$inb_trend_controls <- renderUI({
    has_birthdate_mapping <- !is.null(input$birthdate_col) && nzchar(input$birthdate_col)
    if (isTRUE(has_birthdate_mapping)) {
      div(style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap; margin-bottom:10px;",
          tags$strong("Granularity:"),
          selectInput(
            "inb_trend_granularity",
            label = NULL,
            choices = c("Week" = "week", "Month" = "month", "Year" = "year", "Generation" = "generation"),
            selected = "month",
            width = "220px"
          )
      )
    } else {
      div(style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap; margin-bottom:10px;",
          tags$strong("Granularity:"),
          span("Generation (LAP)")
      )
    }
  })

  generation_depths <- reactive({
    req(ped_data())
    ped <- ped_data()
    ids <- as.character(ped$ID)
    depths <- NULL
    if (exists("use_rcpp") && use_rcpp && exists("fast_lap_depths", mode = "function")) {
      ids_char <- as.character(ped$ID)
      sires_char <- as.character(ifelse(is.na(ped$Sire), "NA", ped$Sire))
      dams_char <- as.character(ifelse(is.na(ped$Dam), "NA", ped$Dam))
      depths <- tryCatch({
        as.integer(fast_lap_depths(ids_char, sires_char, dams_char))
      }, error = function(e) {
        NULL
      })
    }
    if (is.null(depths) || length(depths) != length(ids)) {
      depths <- sapply(ids, function(id) get_ancestor_depth(ped, id))
    }
    tibble(ID = ids, Generation = as.integer(depths))
  })
  
  inb_trend_data <- reactive({
    req(ped_data())
    
    ped <- ped_data()
    if (is.null(ped) || nrow(ped) == 0) return(NULL)
    
    # Need F values (from cache or calculation)
    f_tbl <- f_values_cache()
    if (is.null(f_tbl) || nrow(f_tbl) == 0) return(NULL)
    
    has_birthdate_mapping <- !is.null(input$birthdate_col) && nzchar(input$birthdate_col)
    gran <- input$inb_trend_granularity %||% "generation"
    if (!isTRUE(has_birthdate_mapping)) {
      gran <- "generation"
    }
    
    if (identical(gran, "generation")) {
      gen_tbl <- generation_depths()
      df <- gen_tbl %>%
        left_join(f_tbl %>% mutate(ID = as.character(ID)), by = "ID") %>%
        filter(!is.na(F))
      if (nrow(df) == 0) return(NULL)
      
      out <- df %>%
        group_by(Generation) %>%
        summarise(
          mean_F = mean(F, na.rm = TRUE),
          sd_F = sd(F, na.rm = TRUE),
          n_animals = dplyr::n(),
          .groups = "drop"
        ) %>%
        arrange(Generation) %>%
        mutate(x = Generation)
      attr(out, "x_label") <- "Generation (LAP)"
      return(out)
    }
    
    # Need Birthdate in processed data (ped_data standardizes to "Birthdate")
    if (!"Birthdate" %in% names(ped)) return(NULL)
    
    # Parse Birthdate robustly (allow Date, POSIXct, character; ignore 0/NA)
    bd_raw <- ped$Birthdate
    bd <- suppressWarnings({
      if (inherits(bd_raw, "Date")) {
        bd_raw
      } else if (inherits(bd_raw, "POSIXct") || inherits(bd_raw, "POSIXt")) {
        as.Date(bd_raw)
      } else {
        # Treat "0" and empty as NA
        bd_chr <- as.character(bd_raw)
        bd_chr[bd_chr %in% c("0", "", "NA", "NULL")] <- NA_character_
        as.Date(bd_chr)
      }
    })
    
    df <- tibble(
      ID = as.character(ped$ID),
      Birthdate = bd
    ) %>%
      left_join(f_tbl %>% mutate(ID = as.character(ID)), by = "ID") %>%
      filter(!is.na(Birthdate), !is.na(F))
    
    if (nrow(df) == 0) return(NULL)
    
    period_start <- switch(
      gran,
      week = as.Date(cut(df$Birthdate, breaks = "week", start.on.monday = TRUE)),
      month = as.Date(paste0(format(df$Birthdate, "%Y-%m"), "-01")),
      year = as.Date(paste0(format(df$Birthdate, "%Y"), "-01-01")),
      as.Date(paste0(format(df$Birthdate, "%Y-%m"), "-01"))
    )
    
    out <- df %>%
      mutate(period = period_start) %>%
      group_by(period) %>%
      summarise(
        mean_F = mean(F, na.rm = TRUE),
        sd_F = sd(F, na.rm = TRUE),
        n_animals = dplyr::n(),
        .groups = "drop"
      ) %>%
      arrange(period) %>%
      mutate(x = period)
    attr(out, "x_label") <- "Birthdate"
    out
  })
  
  output$inb_trend_plot <- renderPlotly({
    req(requireNamespace("plotly", quietly = TRUE))
    
    dat <- inb_trend_data()
    validate(need(!is.null(dat) && nrow(dat) > 1, "Not enough data to plot a trend."))
    
    # Purdue gold
    gold <- "#CEB888"
    ribbon_fill <- "rgba(206,184,136,0.25)"
    
    # SD ribbon bounds (handle sd_F NA for singletons)
    sd_val <- ifelse(is.na(dat$sd_F), 0, dat$sd_F)
    y0 <- dat$mean_F - sd_val
    y1 <- dat$mean_F + sd_val
    
    x_label <- attr(dat, "x_label") %||% "X"
    x_display <- if (inherits(dat$x, "Date")) format(dat$x) else dat$x
    hover_txt <- paste0(
      "<b>", x_display, "</b>",
      "<br>Animals: ", dat$n_animals,
      "<br>Mean inbreeding (F): ", signif(dat$mean_F, 5),
      "<br>SD: ", signif(sd_val, 5)
    )
    
    plotly::plot_ly(dat, x = ~x) %>%
      plotly::add_ribbons(
        ymin = ~y0, ymax = ~y1,
        line = list(color = "rgba(0,0,0,0)"),
        fillcolor = ribbon_fill,
        name = "±1 SD",
        hoverinfo = "skip"
      ) %>%
      plotly::add_lines(
        y = ~mean_F,
        line = list(color = gold, width = 3),
        name = "Mean F",
        hoverinfo = "skip"
      ) %>%
      plotly::add_markers(
        y = ~mean_F,
        marker = list(color = gold, size = 7),
        text = hover_txt,
        hoverinfo = "text",
        name = "Mean F"
      ) %>%
      plotly::layout(
        xaxis = list(title = x_label),
        yaxis = list(title = "Inbreeding (F)"),
        hovermode = "closest",
        legend = list(orientation = "h", x = 0, y = -0.15)
      )
  })
  
  # F summary
  output$f_summary <- renderPrint({
    req(f_values())
    f_vals <- f_values()$F
    ped <- ped_data()
    
    # Check if there are any valid F values
    if (length(f_vals) == 0 || all(is.na(f_vals))) {
      cat("No F values calculated.\n\n")
      cat("Possible reasons:\n")
      cat("1. All individuals are founders (no parents)\n")
      if (!is.null(ped)) {
        sire_missing <- is_missing_parent(ped$Sire)
        dam_missing <- is_missing_parent(ped$Dam)
        n_founders <- sum(sire_missing & dam_missing)
        n_with_parents <- sum(!sire_missing | !dam_missing)
        cat("   - Founders in your data:", n_founders, "\n")
        cat("   - Individuals with parents:", n_with_parents, "\n")
      }
      cat("2. Invalid parent references (Sire/Dam not in ID column)\n")
      if (!is.null(ped)) {
        all_ids <- unique(ped$ID)
        valid_sires <- ped$Sire[!is.na(ped$Sire) & ped$Sire != ""]
        valid_dams <- ped$Dam[!is.na(ped$Dam) & ped$Dam != ""]
        missing_sires <- setdiff(valid_sires, all_ids)
        missing_dams <- setdiff(valid_dams, all_ids)
        if (length(missing_sires) > 0) {
          cat("   - Missing Sire IDs:", length(missing_sires), "\n")
        }
        if (length(missing_dams) > 0) {
          cat("   - Missing Dam IDs:", length(missing_dams), "\n")
        }
        if (length(missing_sires) > 0 || length(missing_dams) > 0) {
          cat("   → Use 'Fix Pedigree' button to correct invalid references\n")
        }
      }
      cat("3. Data validation failed\n")
      validation_status <- data_validation_status()
      if (!is.null(validation_status) && !validation_status$valid) {
        cat("   - Validation errors exist. Please fix them first.\n")
      }
      cat("\nPlease check your pedigree structure and ensure:\n")
      cat("- At least some individuals have parent information\n")
      cat("- All Sire/Dam IDs exist in the ID column\n")
      cat("- No duplicate IDs\n")
      return()
    }
    
    f_all <- f_vals[!is.na(f_vals)]
    # Treat very small values as zero to avoid precision artifacts
    f_pos <- f_all[f_all > .Machine$double.eps]
    
    cat("All individuals (F including 0)\n")
    cat("n =", length(f_all), "\n")
    cat("Mean:", round(mean(f_all, na.rm = TRUE), 4), "\n")
    cat("SD:", round(stats::sd(f_all, na.rm = TRUE), 4), "\n")
    cat("Min:", round(min(f_all, na.rm = TRUE), 4), "\n")
    cat("Max:", round(max(f_all, na.rm = TRUE), 4), "\n\n")
    
    cat("Inbred individuals (F > 0)\n")
    cat("n =", length(f_pos), "\n")
    if (length(f_pos) > 0) {
      cat("Mean:", round(mean(f_pos, na.rm = TRUE), 4), "\n")
      cat("SD:", round(stats::sd(f_pos, na.rm = TRUE), 4), "\n")
      cat("Min:", signif(min(f_pos, na.rm = TRUE), 1), "\n")
      cat("Max:", round(max(f_pos, na.rm = TRUE), 4), "\n\n")
    } else {
      cat("Mean: N/A\n")
      cat("SD: N/A\n")
      cat("Min: N/A\n")
      cat("Max: N/A\n")
    }
  })
  
  # Top 10 table
  output$f_table_top <- renderDT({
    req(f_values())
    
    if (nrow(f_values()) == 0) {
      return(datatable(
        data.frame(Message = "No F values available"),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    datatable(
      f_values() %>% arrange(desc(F)) %>% head(10),
      options = list(
        pageLength = 10,
        dom = 't',
        columnDefs = list(list(className = 'dt-id-cell', targets = 0))
      ),
      rownames = FALSE
    ) %>% formatRound("F", 4)
  })

  compute_generation_counts <- function(child_map, root_id, max_depth = 50) {
    visited <- character(0)
    current <- child_map[[root_id]]
    if (is.null(current) || length(current) == 0) {
      return(list(total = 0L, counts = integer(0)))
    }
    counts <- integer(0)
    depth <- 1
    while (length(current) > 0 && depth <= max_depth) {
      current <- unique(as.character(current))
      current <- setdiff(current, visited)
      if (length(current) == 0) break
      counts[depth] <- length(current)
      visited <- c(visited, current)
      current <- unlist(child_map[current], use.names = FALSE)
      depth <- depth + 1
    }
    list(total = length(visited), counts = counts)
  }

  build_descendant_summary <- function(parent_col, id_label) {
    ped <- ped_data()
    if (is.null(ped) || !"ID" %in% names(ped) || !parent_col %in% names(ped)) {
      return(data.frame())
    }
    parent_vals <- ped[[parent_col]]
    valid <- !is_missing_parent(parent_vals)
    if (!any(valid)) return(data.frame())

    if (exists("use_rcpp") && use_rcpp && exists("fast_descendant_summary", mode = "function")) {
      res <- fast_descendant_summary(
        ids = as.character(ped$ID),
        parent_vals = as.character(parent_vals),
        max_depth = 50
      )
      if (is.null(res) || length(res$parents) == 0) return(data.frame())
      counts <- res$counts
      max_gen <- ncol(counts)
      df <- data.frame(
        id = as.character(res$parents),
        total = as.integer(res$totals),
        counts,
        stringsAsFactors = FALSE
      )
      colnames(df) <- c(id_label, "Total_Descendants", paste0("Gen", seq_len(max_gen)))
      return(df[order(-df$Total_Descendants, df[[id_label]]), , drop = FALSE])
    }

    child_map <- split(as.character(ped$ID[valid]), as.character(parent_vals[valid]))
    parent_ids <- names(child_map)
    if (length(parent_ids) == 0) return(data.frame())

    summaries <- lapply(parent_ids, function(pid) {
      res <- compute_generation_counts(child_map, pid)
      list(id = pid, total = res$total, counts = res$counts)
    })
    max_gen <- max(lengths(lapply(summaries, `[[`, "counts")), 0L)
    rows <- lapply(summaries, function(s) {
      counts <- s$counts
      if (length(counts) < max_gen) {
        counts <- c(counts, rep(0L, max_gen - length(counts)))
      }
      data.frame(
        id = s$id,
        total = s$total,
        t(counts),
        stringsAsFactors = FALSE
      )
    })
    df <- do.call(rbind, rows)
    if (is.null(df) || nrow(df) == 0) return(data.frame())
    colnames(df) <- c(id_label, "Total_Descendants", paste0("Gen", seq_len(max_gen)))
    df[order(-df$Total_Descendants, df[[id_label]]), , drop = FALSE]
  }

  sire_descendants <- reactive({
    build_descendant_summary("Sire", "Sire_ID")
  })

  dam_descendants <- reactive({
    build_descendant_summary("Dam", "Dam_ID")
  })

  output$sire_top_table <- renderDT({
    df <- sire_descendants()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No sire data available"), options = list(dom = "t"), rownames = FALSE))
    }
    df_display <- df[, 1:2, drop = FALSE]
    datatable(
      head(df_display, 10),
      options = list(
        pageLength = 10,
        dom = "t",
        columnDefs = list(list(className = 'dt-id-cell', targets = 0))
      ),
      rownames = FALSE
    )
  })

  output$dam_top_table <- renderDT({
    df <- dam_descendants()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Message = "No dam data available"), options = list(dom = "t"), rownames = FALSE))
    }
    df_display <- df[, 1:2, drop = FALSE]
    datatable(
      head(df_display, 10),
      options = list(
        pageLength = 10,
        dom = "t",
        columnDefs = list(list(className = 'dt-id-cell', targets = 0))
      ),
      rownames = FALSE
    )
  })

  output$download_sire_descendants <- downloadHandler(
    filename = function() { paste0("sire_descendants_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- sire_descendants()
      if (nrow(df) == 0) {
        write.csv(data.frame(Sire_ID = character(), Total_Descendants = integer()), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )

  output$download_dam_descendants <- downloadHandler(
    filename = function() { paste0("dam_descendants_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- dam_descendants()
      if (nrow(df) == 0) {
        write.csv(data.frame(Dam_ID = character(), Total_Descendants = integer()), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  # Download F
  output$download_f <- downloadHandler(
    filename = function() { paste0("inbreeding_", Sys.Date(), ".csv") },
    content = function(file) {
      req(f_values())
      if (nrow(f_values()) > 0) {
        write.csv(f_values(), file, row.names = FALSE)
      } else {
        # Create empty file with headers
        write.csv(data.frame(ID = character(), F = numeric()), file, row.names = FALSE)
      }
    }
  )
  
  # Cache directory for layout data
  cache_dir <- "pedigree_cache"
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, showWarnings = FALSE)
  }
  
  # Helper function to generate cache key
  generate_cache_key <- function(data, level, params) {
    data_hash <- digest::digest(data)
    params_hash <- digest::digest(params)
    paste0(data_hash, "_", level, "_", params_hash)
  }
  
  # Helper function to load cached layout
  load_cached_layout <- function(cache_key) {
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    if (file.exists(cache_file)) {
      tryCatch({
        readRDS(cache_file)
      }, error = function(e) {
        NULL
      })
      } else {
      NULL
    }
  }
  
  # Helper function to save cached layout
  save_cached_layout <- function(cache_key, layout_data) {
    cache_file <- file.path(cache_dir, paste0(cache_key, ".rds"))
    tryCatch({
      saveRDS(layout_data, cache_file)
    }, error = function(e) {
      # Silently fail if cache save fails
    })
  }
  
  # Function to compute layout using igraph
  compute_layout <- function(nodes, edges, method = "fr", iterations = 1000) {
    if (nrow(edges) == 0) {
      # If no edges, return random layout
      return(data.frame(
        id = nodes$id,
        x = runif(nrow(nodes), -50, 50),
        y = runif(nrow(nodes), -50, 50)
      ))
    }
    
    # Filter edges to only include nodes that exist in the nodes data frame
    valid_nodes <- nodes$id
    edges_filtered <- edges %>%
      filter(from %in% valid_nodes & to %in% valid_nodes)
    
    if (nrow(edges_filtered) == 0) {
      # If no valid edges, return random layout
      return(data.frame(
        id = nodes$id,
        x = runif(nrow(nodes), -50, 50),
        y = runif(nrow(nodes), -50, 50)
      ))
    }
    
    # Create igraph object
    g <- igraph::graph_from_data_frame(edges_filtered, vertices = nodes)
    
    # Compute layout based on method
    if (method == "fr") {
      layout_coords <- igraph::layout_with_fr(g, niter = iterations)
    } else if (method == "kk") {
      layout_coords <- igraph::layout_with_kk(g)
    } else if (method == "lgl") {
      layout_coords <- igraph::layout_with_lgl(g)
    } else {
      layout_coords <- igraph::layout_with_fr(g, niter = iterations)
    }
    
    # Scale coordinates for better visualization
    if (nrow(layout_coords) > 0) {
      layout_coords[, 1] <- layout_coords[, 1] * 100
      layout_coords[, 2] <- layout_coords[, 2] * 100
    }
    
    data.frame(
      id = nodes$id,
      x = layout_coords[, 1],
      y = layout_coords[, 2]
    )
  }
  
  # Build network with smart aggregation
  
  # Topological sort for pedigree: parents must precede offspring
  sort_pedigree_topological <- function(ped) {
    if (nrow(ped) == 0) return(ped)
    
    # Create a mapping of ID to row index for quick lookup
    id_to_index <- setNames(1:nrow(ped), ped$ID)
    
    # Track visited nodes and build dependency graph
    visited <- logical(nrow(ped))
    sorted_indices <- integer(0)
    
    # Helper function to get parents of an individual
    get_parents <- function(id) {
      row_idx <- id_to_index[[id]]
      if (is.na(row_idx)) return(character(0))
      
      parents <- character(0)
      if (!is.na(ped$Sire[row_idx]) && ped$Sire[row_idx] != "") {
        parents <- c(parents, ped$Sire[row_idx])
      }
      if (!is.na(ped$Dam[row_idx]) && ped$Dam[row_idx] != "") {
        parents <- c(parents, ped$Dam[row_idx])
      }
      return(parents)
    }
    
    # DFS-based topological sort
    visit <- function(idx) {
      if (visited[idx]) return()
      
      id <- ped$ID[idx]
      parents <- get_parents(id)
      
      # Visit all parents first
      for (parent_id in parents) {
        parent_idx <- id_to_index[[parent_id]]
        if (!is.na(parent_idx) && !visited[parent_idx]) {
          visit(parent_idx)
        }
      }
      
      # Mark current node as visited and add to sorted list
      visited[idx] <<- TRUE
      sorted_indices <<- c(sorted_indices, idx)
    }
    
    # Visit all nodes starting from those with no parents (founders)
    # Then visit others
    for (i in 1:nrow(ped)) {
      if (!visited[i]) {
        visit(i)
      }
    }
    
    # Return sorted pedigree
    ped[sorted_indices, ]
  }
  
  # Function to get all ancestors of an individual
  get_ancestors <- function(ped, individual_id, max_depth = NULL, current_depth = 0) {
    if (is.null(max_depth)) max_depth <- input$search_depth %||% 5
    if (current_depth >= max_depth) return(character(0))
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
    individual <- ped %>% filter(ID == individual_id)
    if (nrow(individual) == 0) return(character(0))
    
    ancestors <- character(0)
    
    if (!is.na(individual$Sire) && individual$Sire != "") {
      ancestors <- c(ancestors, individual$Sire)
      ancestors <- c(ancestors, get_ancestors(ped, individual$Sire, max_depth, current_depth + 1))
    }
    
    if (!is.na(individual$Dam) && individual$Dam != "") {
      ancestors <- c(ancestors, individual$Dam)
      ancestors <- c(ancestors, get_ancestors(ped, individual$Dam, max_depth, current_depth + 1))
    }
    
    unique(ancestors)
  }
  
  # Function to calculate ancestor depth for each individual (with cycle detection)
  get_ancestor_depth <- function(ped, individual_id, visited = character(0), max_iterations = 100) {
    if (is.null(individual_id) || is.na(individual_id) || individual_id == "") return(0)
    
    # Prevent infinite recursion (cycle detection)
    if (individual_id %in% visited) return(0)
    if (length(visited) > max_iterations) return(length(visited))
    
    individual <- ped %>% filter(ID == individual_id)
    if (nrow(individual) == 0) return(0)
    
    # If both parents are NA, this is a founder (depth 0)
    if ((is.na(individual$Sire) || individual$Sire == "") && 
        (is.na(individual$Dam) || individual$Dam == "")) return(0)
    
    # Add current individual to visited set
    new_visited <- c(visited, individual_id)
    
    max_parent_depth <- 0
    
    # Get depth from sire
    if (!is.na(individual$Sire) && individual$Sire != "") {
      sire_depth <- get_ancestor_depth(ped, individual$Sire, new_visited, max_iterations)
      max_parent_depth <- max(max_parent_depth, sire_depth)
    }
    
    # Get depth from dam
    if (!is.na(individual$Dam) && individual$Dam != "") {
      dam_depth <- get_ancestor_depth(ped, individual$Dam, new_visited, max_iterations)
      max_parent_depth <- max(max_parent_depth, dam_depth)
    }
    
    return(max_parent_depth + 1)
  }
  
  # Function to find individual with deepest ancestors
  # Uses Rcpp version if available, otherwise falls back to R version
  find_deepest_ancestor_individual <- function(ped) {
    if (is.null(ped) || nrow(ped) == 0) return(NULL)
    
    # Try Rcpp version first if available
    if (exists("use_rcpp") && use_rcpp && exists("fast_find_deepest_ancestor")) {
      tryCatch({
        # Convert to character vectors
        ids_char <- as.character(ped$ID)
        sires_char <- as.character(ifelse(is.na(ped$Sire), "NA", ped$Sire))
        dams_char <- as.character(ifelse(is.na(ped$Dam), "NA", ped$Dam))
        
        # Determine sample size based on dataset size
        sample_size <- if (nrow(ped) > 1000) 200 else 1000
        
        # Call fast C++ function
        result <- fast_find_deepest_ancestor(ids_char, sires_char, dams_char, sample_size)
        
        if (length(result$id) > 0 && result$depth > 0) {
          return(list(id = as.character(result$id), depth = as.integer(result$depth)))
        }
        return(NULL)
      }, error = function(e) {
        warning("Rcpp version failed, falling back to R version: ", e$message)
        # Fall through to R version below
      })
    }
    
    # Fallback to R version
    tryCatch({
      # For large datasets, sample to avoid performance issues
      if (nrow(ped) > 1000) {
        # Sample 200 non-founders for depth calculation (reduced for performance)
        non_founders <- ped %>% filter(!is.na(Sire) | !is.na(Dam))
        if (nrow(non_founders) == 0) return(NULL)
        
        if (nrow(non_founders) > 200) {
          sample_ids <- sample(non_founders$ID, 200)
        } else {
          sample_ids <- non_founders$ID
        }
      } else {
        # For small datasets, check all non-founders
        non_founders <- ped %>% filter(!is.na(Sire) | !is.na(Dam))
        if (nrow(non_founders) == 0) return(NULL)
        sample_ids <- non_founders$ID
      }
      
      # Calculate depth for sampled individuals with error handling
      depths <- numeric(length(sample_ids))
      for (i in seq_along(sample_ids)) {
        depths[i] <- tryCatch({
          get_ancestor_depth(ped, sample_ids[i])
        }, error = function(e) {
          0
        })
      }
      
      # Find individual with maximum depth
      if (all(depths == 0)) return(NULL)
      
      max_depth <- max(depths, na.rm = TRUE)
      if (is.na(max_depth) || max_depth == 0) return(NULL)
      
      deepest_id <- sample_ids[which.max(depths)]
      
      return(list(id = deepest_id, depth = max_depth))
    }, error = function(e) {
      warning("Error in find_deepest_ancestor_individual: ", e$message)
      return(NULL)
    })
  }
  
  # Function to get ancestors within specified generations
  get_all_relatives <- function(ped, individual_id, max_depth = 1) {
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
    # Get ancestors
    ancestors <- get_ancestors(ped, individual_id, max_depth = max_depth)
    # Combine ancestors (including the individual itself)
    return(unique(c(individual_id, ancestors)))
  }
  
  # Advanced function using pedigreeTools for ancestor analysis
  get_relatives_with_pedigreeTools <- function(ped, individual_id, max_depth = 1) {
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    if (!pedigreeTools_available) {
      return(get_all_relatives(ped, individual_id, max_depth))
    }
    
    tryCatch({
      # Create pedigree object using pedigreeTools
      ped_obj <- pedigreeTools::pedigree(
        id = ped$ID,
        dadid = ped$Sire,
        momid = ped$Dam,
        sex = if("Sex" %in% names(ped)) ped$Sex else NULL
      )
      
      # Get all individuals in the pedigree
      all_ids <- ped$ID
      
      # Find relatives using pedigreeTools functions
      # Get ancestors using kinship matrix
      kin_matrix <- kinship(ped_obj)
      
      # Find individuals with kinship > 0 (related individuals)
      related_indices <- which(kin_matrix[individual_id, ] > 0)
      related_ids <- all_ids[related_indices]
      
      # Filter by generation depth if needed
      if (max_depth > 0) {
        ancestors <- get_ancestors(ped, individual_id, max_depth = max_depth)
        # Combine with pedigreeTools results
        all_relatives <- unique(c(individual_id, ancestors, related_ids))
      } else {
        all_relatives <- unique(c(individual_id, related_ids))
      }
      
      return(all_relatives)
      
    }, error = function(e) {
      # Fallback to our custom functions if pedigreeTools fails
      warning("pedigreeTools analysis failed, using custom functions: ", e$message)
      return(get_all_relatives(ped, individual_id, max_depth))
    })
  }
  
  # Function using igraph for network-based ancestor analysis
  get_relatives_with_igraph <- function(ped, individual_id, max_depth = 1) {
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
    tryCatch({
      # Create edges from pedigree data
      edges <- bind_rows(
        ped %>% filter(!is.na(Sire)) %>% transmute(from = as.character(Sire), to = as.character(ID)),
        ped %>% filter(!is.na(Dam)) %>% transmute(from = as.character(Dam), to = as.character(ID))
      )
      
      if (nrow(edges) == 0) return(character(0))
      
      # Create igraph object
      g <- igraph::graph_from_data_frame(edges, vertices = ped$ID)
      
      # Find all nodes within max_depth steps from the individual
      if (max_depth > 0) {
        # Get neighbors within specified distance
        distances <- igraph::distances(g, v = individual_id, mode = "all")
        reachable_nodes <- names(which(distances[1, ] <= max_depth & distances[1, ] > 0))
        
        # Include the individual itself
        all_relatives <- unique(c(individual_id, reachable_nodes))
        } else {
        # Get all connected components
        components <- igraph::components(g)
        individual_component <- components$membership[individual_id]
        all_relatives <- names(components$membership)[components$membership == individual_component]
      }
      
      return(all_relatives)
      
    }, error = function(e) {
      # Fallback to our custom functions if igraph fails
      warning("igraph analysis failed, using custom functions: ", e$message)
      return(get_all_relatives(ped, individual_id, max_depth))
    })
  }

  # Helper: compute relationship codes for ancestors (1=father, 2=mother)
  compute_ancestor_codes <- function(ped, root_id, max_depth) {
    if (is.null(root_id) || !root_id %in% ped$ID || max_depth < 1) return(setNames(character(0), character(0)))
    # queue holds list of list(id, code, depth)
    queue <- list(list(id = root_id, code = "", depth = 0))
    seen <- setNames(character(0), character(0))
    codes <- setNames(character(0), character(0))
    while (length(queue) > 0) {
      current <- queue[[1]]; queue <- queue[-1]
      if (current$depth >= max_depth) next
      # find current person's parents (safe subset)
      row <- ped[ped$ID == current$id, c("Sire","Dam"), drop = FALSE]
      if (nrow(row) == 0) next
      # father
      if (!is.null(row$Sire) && !is.na(row$Sire) && row$Sire != "") {
        nid <- as.character(row$Sire)
        code <- paste0(current$code, "1")
        if (is.null(seen[[nid]]) || nchar(code) < nchar(seen[[nid]])) {
          seen[[nid]] <- code
          codes[[nid]] <- code
          queue[[length(queue) + 1]] <- list(id = nid, code = code, depth = current$depth + 1)
        }
      }
      # mother
      if (!is.null(row$Dam) && !is.na(row$Dam) && row$Dam != "") {
        nid <- as.character(row$Dam)
        code <- paste0(current$code, "2")
        if (is.null(seen[[nid]]) || nchar(code) < nchar(seen[[nid]])) {
          seen[[nid]] <- code
          codes[[nid]] <- code
          queue[[length(queue) + 1]] <- list(id = nid, code = code, depth = current$depth + 1)
        }
      }
    }
    return(codes)
  }

  # Helper: compute relationship codes for descendants (1=male child, 2=female child, 0=unknown)
  compute_descendant_codes <- function(ped, root_id, max_depth) {
    if (is.null(root_id) || !root_id %in% ped$ID || max_depth < 1) return(setNames(character(0), character(0)))
    # precompute children map
    # list of children IDs per parent id
    children_of <- function(pid) ped$ID[which(ped$Sire == pid | ped$Dam == pid)]
    get_sex_digit <- function(id) {
      if (!"Sex" %in% names(ped)) return("0")
      sx <- ped$Sex[match(id, ped$ID)]
      if (is.na(sx)) return("0")
      sx <- as.character(sx)
      if (sx %in% c("M","Male","m","male")) return("1")
      if (sx %in% c("F","Female","f","female")) return("2")
      return("0")
    }
    queue <- list(list(id = root_id, code = "", depth = 0))
    seen <- setNames(character(0), character(0))
    codes <- setNames(character(0), character(0))
    while (length(queue) > 0) {
      current <- queue[[1]]; queue <- queue[-1]
      if (current$depth >= max_depth) next
      kids <- children_of(current$id)
      if (length(kids) == 0) next
      for (kid in kids) {
        digit <- get_sex_digit(kid)
        code <- paste0(current$code, digit)
        if (is.null(seen[[kid]]) || nchar(code) < nchar(seen[[kid]])) {
          seen[[kid]] <- code
          codes[[kid]] <- code
          queue[[length(queue) + 1]] <- list(id = kid, code = code, depth = current$depth + 1)
        }
      }
    }
    return(codes)
  }
  
  # Function to get all descendants of an individual
  get_descendants <- function(ped, individual_id, max_depth = NULL, current_depth = 0) {
    # Default to 1 generation if max_depth is NULL, NA, or 0
    if (is.null(max_depth) || is.na(max_depth) || max_depth == 0) {
      max_depth <- 1
    }
    
    if (current_depth >= max_depth) return(character(0))
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
    # Get direct descendants (children)
    descendants <- ped %>%
      filter(Sire == individual_id | Dam == individual_id) %>%
      pull(ID)
    
    if (length(descendants) == 0) return(character(0))
    
    # If max_depth is 1, only return direct descendants
    if (max_depth == 1) {
      return(descendants)
    }
    
    # For deeper generations, recursively get descendants
    all_descendants <- descendants
    for (desc in descendants) {
      all_descendants <- c(all_descendants, get_descendants(ped, desc, max_depth, current_depth + 1))
    }
    
    unique(all_descendants)
  }
  
  # Build network for a specific individual
  build_individual_network <- function(target_id) {
    ped <- ped_data()
    if (is.null(ped) || is.null(target_id)) return(list(nodes = tibble(), edges = tibble()))
    
    # Get search depth from user input (ancestors only)
    search_depth <- input$search_depth %||% 5
    
    # Get related individuals (ancestors only)
    ancestors <- get_ancestors(ped, target_id, max_depth = search_depth)
    related_ids <- unique(c(target_id, ancestors))
    
    # Filter pedigree to related individuals
    related_ped <- ped %>% filter(ID %in% related_ids)
    
    if (nrow(related_ped) == 0) return(list(nodes = tibble(), edges = tibble()))

    top_contrib_text <- ""
    if (exists("fast_top_contrib_cpp", mode = "function")) {
      f_vals <- f_values_cache()
      cat("[TopContrib] target=", target_id, " f_cache_rows=", ifelse(is.null(f_vals), 0, nrow(f_vals)), "\n", sep = "")
      if (!is.null(f_vals) && nrow(f_vals) > 0) {
      f_joined <- ped %>%
        transmute(
          ID = as.character(ID),
          Sire = as.character(Sire),
          Dam = as.character(Dam)
        ) %>%
        left_join(f_vals %>% transmute(ID = as.character(ID), F = as.numeric(F)), by = "ID") %>%
        mutate(F = ifelse(is.na(F), 0, F))

      top_contrib <- tryCatch({
        fast_top_contrib_cpp(
          f_joined$ID,
          f_joined$Sire,
          f_joined$Dam,
          f_joined$F,
          target_id,
          max_depth = search_depth,
          top_k = 5
        )
      }, error = function(e) {
        cat("[TopContrib] error:", e$message, "\n")
        NULL
      })

      if (!is.null(top_contrib) && nrow(top_contrib) > 0) {
        cat("[TopContrib] rows=", nrow(top_contrib), "\n", sep = "")
        lines <- sprintf(
          "%s (C=%.4f, P=%.1f%%)",
          top_contrib$ancestor_id,
          top_contrib$contribution,
          top_contrib$proportion * 100
        )
        top_contrib_text <- paste0("<br><br><strong>Top 5 Inbreeding Contributors:</strong><br>",
                                   paste(lines, collapse = "<br>"))
      } else {
        cat("[TopContrib] empty result\n")
      }
      }
    }
    
    # Build nodes
    nodes <- related_ped %>%
      transmute(
        id = ID,
        label = if(input$show_labels) ID else "",
        group = ifelse(!is.na(Sex), as.character(Sex), "Unknown"),
        title = paste0("ID: ", ID, 
                       ifelse(!is.na(Sire), paste0("<br>Sire: ", Sire), ""),
                       ifelse(!is.na(Dam), paste0("<br>Dam: ", Dam), ""),
                       ifelse(!is.na(Sex), paste0("<br>Sex: ", Sex), "")),
        value = input$node_size,
        level = "individual",
        # Set default colors based on sex
        color = case_when(
          Sex == "M" | Sex == "Male" | Sex == "m" ~ "#87CEEB",  # Sky blue for males
          Sex == "F" | Sex == "Female" | Sex == "f" ~ "#FFB6C1",  # Light pink for females
          TRUE ~ "#D3D3D3"         # Light gray for unknown
        ),
        borderWidth = 2
      )
    
    # Add F values and adjust node size based on inbreeding
    if (!is.null(f_values()) && nrow(f_values()) > 0) {
      nodes <- nodes %>%
        left_join(f_values(), by = c("id" = "ID")) %>%
        mutate(
          title = paste0(title, ifelse(!is.na(F), paste0("<br>F: ", round(F, 4)), "")),
          # Scale node size based on inbreeding coefficient (F)
          # Base size + F * scaling factor, with minimum size
          value = ifelse(!is.na(F), 
                        pmax(input$node_size * 0.5, input$node_size + F * 300), 
                        input$node_size)
        )
    }
    
    # Highlight target individual
    nodes <- nodes %>%
      mutate(
        title = ifelse(id == target_id, paste0(title, "<br><br>🎯 Target Individual", top_contrib_text), title),
        shape = ifelse(id == target_id, "star", "dot"),
        color = ifelse(id == target_id, "#FF0000", color),
        borderWidth = ifelse(id == target_id, 4, borderWidth),
        # Make target individual slightly larger
        value = ifelse(id == target_id, value * 1.2, value)
      )
    
    # Build edges
    edges <- bind_rows(
      related_ped %>% filter(!is.na(Sire)) %>% transmute(from = as.character(Sire), to = as.character(ID)),
      related_ped %>% filter(!is.na(Dam)) %>% transmute(from = as.character(Dam), to = as.character(ID))
    ) %>%
      # Only keep edges where both nodes exist in the related_ped
      filter(from %in% related_ped$ID & to %in% related_ped$ID)
    
    
    # Compute layout
    layout_data <- compute_layout(nodes, edges, method = "fr", iterations = 800)
    
    # Add layout coordinates
    nodes <- nodes %>%
      left_join(layout_data, by = "id") %>%
      mutate(
        x = ifelse(is.na(x), runif(n(), -100, 100), x),
        y = ifelse(is.na(y), runif(n(), -100, 100), y)
      )
    
    list(nodes = nodes, edges = edges, layout = "precomputed")
  }
  
  # Simplified network data reactive
  network_data <- reactive({
    # Check if raw data is available first
    if (is.null(raw_data())) {
      return(list(nodes = tibble(), edges = tibble(), status = "waiting"))
    }
    
    # Check if ped_data is available
    ped <- ped_data()
    if (is.null(ped)) {
      return(list(nodes = tibble(), edges = tibble(), status = "error"))
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      showNotification("Error: Required columns (ID, Sire, Dam) not found in processed data", type = "error")
      return(list(nodes = tibble(), edges = tibble(), status = "error"))
    }
    
    # Get target individual
    target_id <- selected_individual()
    
    if (!is.null(target_id)) {
      # Build individual network
      result <- build_individual_network(target_id)
      result$status <- "success"
      return(result)
    } else {
      # Return empty network (data loaded but no individual selected)
      return(list(nodes = tibble(), edges = tibble(), layout = "precomputed", status = "no_selection"))
    }
  })
  
  # Network visualization output
  output$pedigree_network <- renderVisNetwork({
    # Get network data with proper error handling
    tryCatch({
      net <- network_data()
      
      # Check status first
      status <- net$status %||% "unknown"
      
      # If waiting for data (raw_data is NULL)
      if (status == "waiting") {
        return(visNetwork(data.frame(id = 1, label = "Waiting for data"), data.frame()) %>%
                 visNodes(shape = "text", color = list(background = "white")) %>%
                 visOptions(manipulation = FALSE))
      }
      
      # If error occurred (ped_data is NULL or columns missing)
      if (status == "error") {
        return(visNetwork(data.frame(id = 1, label = "Error: Data processing failed. Please check column mapping."), data.frame()) %>%
                 visNodes(shape = "text", color = list(background = "white")) %>%
                 visOptions(manipulation = FALSE))
      }
      
      # If no individual selected (data loaded but no selection)
      if (status == "no_selection") {
        return(visNetwork(data.frame(id = 1, label = "💡 Enter an individual ID above to visualize their pedigree network"), data.frame()) %>%
                 visNodes(shape = "text", color = list(background = "white")) %>%
                 visOptions(manipulation = FALSE))
      }
      
      # Validate network data
      if (is.null(net) || !is.list(net) || 
          !"nodes" %in% names(net) || 
          !is.data.frame(net$nodes) || 
          nrow(net$nodes) == 0) {
        return(visNetwork(data.frame(id = 1, label = "No data"), data.frame()) %>%
                 visNodes(shape = "text", color = list(background = "white")) %>%
                 visOptions(manipulation = FALSE))
      }
    
    # Check if we have precomputed layout
    if (!is.null(net$layout) && net$layout == "precomputed" && "x" %in% names(net$nodes) && "y" %in% names(net$nodes)) {
      # Use precomputed layout
      visNetwork(net$nodes, net$edges) %>%
        visNodes(
          font = list(size = 12, color = "#333333"),
          shadow = list(enabled = TRUE, size = 10),
          scaling = list(min = 10, max = 50)
        ) %>%
        visEdges(
          arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
          color = list(color = "#B89D5D", highlight = "#FFD700"),
          width = 2,
          smooth = TRUE
        ) %>%
        visGroups(groupname = "M", color = list(background = "#87CEEB", border = "#4682B4", highlight = "#FFD700")) %>%
        visGroups(groupname = "F", color = list(background = "#FFB6C1", border = "#DC143C", highlight = "#FFD700")) %>%
        visGroups(groupname = "Unknown", color = list(background = "#D3D3D3", border = "#696969", highlight = "#FFD700")) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = input$search_depth %||% 1, hover = TRUE),
          nodesIdSelection = list(enabled = TRUE, style = "width: 200px; height: 26px"),
          manipulation = FALSE
        ) %>%
        visInteraction(
          dragNodes = TRUE,
          dragView = TRUE,
          zoomView = TRUE,
          hover = TRUE,
          tooltipDelay = 200
        ) %>%
        visEvents(
          oncontext = "function(params) {
            if (params.nodes.length > 0) {
              var nodeId = params.nodes[0];
              var nodeData = this.body.data.nodes.get(nodeId);
              if (nodeData && !nodeData.id.startsWith('Cluster_')) {
                showContextMenu(params.event, nodeData);
              }
            }
          }"
        ) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var network = this;
            
            // Wait for network to be fully initialized
            if (network && typeof network.on === 'function') {
              // Add click event listener
              network.on('click', function(params) {
                if (params.nodes.length > 0) {
                  var nodeId = params.nodes[0];
                  if (network.body && network.body.data && network.body.data.nodes) {
                    var nodeData = network.body.data.nodes.get(nodeId);
                    if (nodeData && !nodeData.id.startsWith('Cluster_')) {
                      // Trigger highlight event
                      Shiny.setInputValue('selected_node_for_highlight', nodeId, {priority: 'event'});
                      // Also trigger download event for export functionality
                      Shiny.setInputValue('trigger_download', nodeId, {priority: 'event'});
                    }
                  }
                }
              });
            }
          }
        ")
        } else {
      # Fallback to dynamic layout
      visNetwork(net$nodes, net$edges) %>%
        visNodes(
          font = list(size = 12, color = "#333333"),
          shadow = list(enabled = TRUE, size = 10),
          scaling = list(min = 10, max = 50)
        ) %>%
        visEdges(
          arrows = list(to = list(enabled = TRUE, scaleFactor = 1)),
          color = list(color = "#B89D5D", highlight = "#FFD700"),
          width = 2,
          smooth = TRUE
        ) %>%
        visGroups(groupname = "M", color = list(background = "#87CEEB", border = "#4682B4", highlight = "#FFD700")) %>%
        visGroups(groupname = "F", color = list(background = "#FFB6C1", border = "#DC143C", highlight = "#FFD700")) %>%
        visGroups(groupname = "Unknown", color = list(background = "#D3D3D3", border = "#696969", highlight = "#FFD700")) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = input$search_depth %||% 1, hover = TRUE),
          nodesIdSelection = list(enabled = TRUE, style = "width: 200px; height: 26px"),
          manipulation = FALSE
        ) %>%
        visInteraction(
          dragNodes = TRUE,
          dragView = TRUE,
          zoomView = TRUE,
          hover = TRUE,
          tooltipDelay = 200
        ) %>%
        visPhysics(
          enabled = TRUE,
          stabilization = list(enabled = TRUE, iterations = 100),
          barnesHut = list(
            gravitationalConstant = -2000,
            centralGravity = 0.1,
            springLength = 95,
            springConstant = 0.04,
            damping = 0.09,
            avoidOverlap = 0.1
          )
        ) %>%
        visEvents(
          oncontext = "function(params) {
            if (params.nodes.length > 0) {
              var nodeId = params.nodes[0];
              var nodeData = this.body.data.nodes.get(nodeId);
              if (nodeData && !nodeData.id.startsWith('Cluster_')) {
                showContextMenu(params.event, nodeData);
              }
            }
          }"
        ) %>%
        htmlwidgets::onRender("
          function(el, x) {
            var network = this;
            
            // Wait for network to be fully initialized
            if (network && typeof network.on === 'function') {
              // Add click event listener
              network.on('click', function(params) {
                if (params.nodes.length > 0) {
                  var nodeId = params.nodes[0];
                  if (network.body && network.body.data && network.body.data.nodes) {
                    var nodeData = network.body.data.nodes.get(nodeId);
                    if (nodeData && !nodeData.id.startsWith('Cluster_')) {
                      // Trigger highlight event
                      Shiny.setInputValue('selected_node_for_highlight', nodeId, {priority: 'event'});
                      // Also trigger download event for export functionality
                      Shiny.setInputValue('trigger_download', nodeId, {priority: 'event'});
                    }
                  }
                }
              });
            }
          }
        ")
      }
    }, error = function(e) {
      # Check if error is due to missing raw_data
      if (is.null(raw_data())) {
        return(visNetwork(data.frame(id = 1, label = "Waiting for data"), data.frame()) %>%
                 visNodes(shape = "text", color = list(background = "white")) %>%
                 visOptions(manipulation = FALSE))
      }
      # Return error message for other errors
      return(visNetwork(data.frame(id = 1, label = "Error loading network"), data.frame()) %>%
               visNodes(shape = "text", color = list(background = "white")) %>%
               visOptions(manipulation = FALSE))
    })
  })
  
  # Set output options to prevent state conflicts
  # Use suspendWhenHidden = TRUE to prevent updates when tab is hidden
  # This helps prevent state conflicts when switching tabs or during rapid updates
  outputOptions(output, "pedigree_network", suspendWhenHidden = TRUE, priority = 5)

  # Dynamic localized titles for three-panel UI and tabs
  output$left_upload_title <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    h4(get_label_local("data_upload", lang), class = "section-title")
  })

  output$quick_stats_title <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    h4(get_label_local("quick_stats", lang), class = "section-title")
  })

  output$inbreeding_analysis_title <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    h4(get_label_local("inbreeding_analysis", lang), class = "section-title")
  })

  output$tab_network_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("tab_network", lang)
  })

  output$tab_data_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("tab_data_preview", lang)
  })

  output$tab_qc_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("tab_qc_report", lang)
  })
  
  output$tab_structure_title <- renderText({
    "Pedigree Structure"
  })
}

shinyApp(ui, server)
