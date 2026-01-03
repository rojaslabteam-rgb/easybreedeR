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
library(pedigreeTools)
library(visNetwork)
library(igraph)
library(digest)

# Try to load Rcpp QC function
use_rcpp <- FALSE
tryCatch({
  library(Rcpp)
  if (file.exists("pedigree_qc.cpp")) {
    sourceCpp("pedigree_qc.cpp")
    use_rcpp <- TRUE
    cat("✓ Rcpp QC functions loaded successfully\n")
  }
}, error = function(e) {
  cat("Note: Rcpp not available, using optimized R functions\n")
  use_rcpp <- FALSE
})

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
      .section-title {
        font-size: 1.1rem;
        font-weight: 700;
        color: #2c3e50;
        margin-top: 0;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 2px solid #CEB888;
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
              uiOutput("data_validation_status_ui"),
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
              uiOutput("download_all_f_button")
          ),
          
          div(class = "panel-section",
              h4(textOutput("selected_animal_export_title"), class = "section-title"),
              uiOutput("selected_node_info"),
              uiOutput("download_selected_range_button"),
              uiOutput("export_scope_help")
          ),
          
          div(class = "panel-section",
              h4(textOutput("smart_visualization_title"), class = "section-title"),
              uiOutput("aggregation_level_selector"),
              uiOutput("drill_down_navigation"),
              hr(),
              uiOutput("node_size_slider"),
              uiOutput("show_labels_checkbox")
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
      
      // Handle highlighting descendants
      Shiny.addCustomMessageHandler('highlightDescendants', function(message) {
        var network = $('#pedigree_network').data('visNetwork');
        if (network) {
          // Get all nodes to highlight (includes selected, parents, and descendants)
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
    radioButtons("sep", get_label_local("separator", lang),
                 choices = c(Comma = ",", Tab = "\t", Space = " "),
                 selected = ",", inline = TRUE)
  })

  output$auto_process_checkbox <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    checkboxInput("auto_process", get_label_local("auto_process", lang), value = TRUE)
  })

  output$auto_process_help <- renderUI({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    helpText(get_label_local("auto_process_help", lang))
  })

  output$column_mapping_title <- renderText({
    lang <- if (exists("map_suite_lang_for_app", mode = "function")) map_suite_lang_for_app(current_lang(), "pediviewer") else current_lang()
    get_label_local("column_mapping", lang)
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
      read.table(input$file$datapath, 
                 header = TRUE, 
                 sep = input$sep, 
                 stringsAsFactors = FALSE,
                 quote = "",           # 禁用引号解析，支持ID中包含 ' 和 "
                 comment.char = "",    # 禁用注释符，支持ID中包含 #
                 check.names = FALSE,  # 保留原始列名，不自动修正
                 encoding = "UTF-8")   # 明确编码，支持中文等字符
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
    
    list(
      id = id_col,
      sire = sire_col,
      dam = dam_col,
      sex = sex_col
    )
  }
  
  # Comprehensive QC issue detection function with Rcpp acceleration
  detect_qc_issues <- function(df) {
    issues <- list(
      duplicates = list(),
      missing_parents = list(),
      self_parenting = list(),
      loops = list(),
      has_errors = FALSE
    )
    
    # Try to use fast Rcpp version if available
    if (exists("use_rcpp") && use_rcpp && exists("fast_pedigree_qc") && exists("fast_detect_loops")) {
      tryCatch({
        # Convert to character and handle NAs
        ids_char <- as.character(df$ID)
        sires_char <- as.character(ifelse(is.na(df$Sire), "NA", df$Sire))
        dams_char <- as.character(ifelse(is.na(df$Dam), "NA", df$Dam))
        
        # Call fast C++ QC function
        qc_result <- fast_pedigree_qc(ids_char, sires_char, dams_char)
        
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
        
        # Fast loop detection
        loop_result <- fast_detect_loops(ids_char, sires_char, dams_char)
        if (loop_result$count > 0) {
          issues$loops <- list(
            count = loop_result$count,
            cycles = loop_result$cycles
          )
          issues$has_errors <- TRUE
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
    
    return(issues)
  }
  
  # Auto-fix QC issues function
  fix_qc_issues <- function(df, issues) {
    fixed_summary <- list()
    
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
  
  # Smart aggregation system for large datasets
  smart_aggregation_system <- function(ped, max_nodes_per_level = 200) {
    n_individuals <- nrow(ped)
    
    # Helper function to safely count clusters
    safe_cluster_count <- function(clustered_data) {
      if (is.null(clustered_data) || nrow(clustered_data) == 0 || !"cluster" %in% names(clustered_data)) {
        return(1)
      }
      unique_clusters <- unique(clustered_data$cluster)
      if (length(unique_clusters) == 0) {
        return(1)
      }
      return(length(unique_clusters))
    }
    
    # Determine optimal aggregation strategy based on data size
    if (n_individuals <= 100) {
      return(list(
        strategy = "individual",
        levels = list(individual = ped),
        level_info = list(individual = list(name = "Individual View", count = n_individuals))
      ))
      } else if (n_individuals <= 1000) {
        family_data <- create_family_clusters(ped, k = 15, method = "louvain", force_k = FALSE)
        return(list(
          strategy = "family",
          levels = list(family = family_data),
          level_info = list(family = list(name = "Family Clusters", count = safe_cluster_count(family_data)))
        ))
      } else if (n_individuals <= 10000) {
        super_family_data <- create_super_family_clusters(ped, max_size = 500)
        family_data <- create_family_clusters(ped, k = 20, method = "louvain", force_k = FALSE)
        return(list(
          strategy = "multi_level",
          levels = list(
            super_family = super_family_data,
            family = family_data,
            individual = ped
          ),
          level_info = list(
            super_family = list(name = "Super Families", count = safe_cluster_count(super_family_data)),
            family = list(name = "Families", count = safe_cluster_count(family_data)),
            individual = list(name = "Individuals", count = n_individuals)
          )
        ))
      } else {
        mega_family_data <- create_mega_family_clusters(ped, max_size = 2000)
        super_family_data <- create_super_family_clusters(ped, max_size = 500)
        family_data <- create_family_clusters(ped, k = 25, method = "louvain", force_k = FALSE)
        return(list(
          strategy = "hierarchical",
          levels = list(
            mega_family = mega_family_data,
            super_family = super_family_data,
            family = family_data,
            individual = ped
          ),
          level_info = list(
            mega_family = list(name = "Mega Families", count = safe_cluster_count(mega_family_data)),
            super_family = list(name = "Super Families", count = safe_cluster_count(super_family_data)),
            family = list(name = "Families", count = safe_cluster_count(family_data)),
            individual = list(name = "Individuals", count = n_individuals)
          )
        ))
      }
  }
  
  # Create family clusters with robust strategies for highly related populations
  create_family_clusters <- function(ped, 
                                     k = 30, 
                                     method = c("auto", "louvain", "spectral", "threshold"),
                                     tau = 0.0625,          # kinship-like threshold if using "threshold" mode
                                     max_gen = 5,           # depth for approximate relatedness search (unused here but reserved)
                                     force_k = FALSE) {     # force exactly k clusters when TRUE
    method <- match.arg(method)

    # Fast path: if input is empty, return as-is with cluster=1
    if (is.null(ped) || nrow(ped) == 0) {
      return(tibble::as_tibble(ped) %>% dplyr::mutate(cluster = 1L))
    }

    # Build edges (parent-child), drop NAs/empties
    edges_all <- dplyr::bind_rows(
      ped %>% dplyr::filter(!is.na(Sire), Sire != "", Sire != "0") %>% dplyr::transmute(from = Sire, to = ID),
      ped %>% dplyr::filter(!is.na(Dam),  Dam  != "", Dam  != "0") %>% dplyr::transmute(from = Dam,  to = ID)
    )

    # If there are no edges, each node is its own singleton
    if (nrow(edges_all) == 0) {
      out <- ped %>% dplyr::transmute(ID, cluster = dplyr::row_number())
      return(out)
    }

    # Undirected pedigree graph
    g <- igraph::graph_from_data_frame(edges_all, directed = FALSE)

    # Check connected components but always use community detection for better clustering
    comp <- igraph::components(g, mode = "weak")
    comp_membership <- comp$membership
    
    # Ensure comp_membership has names
    if (is.null(names(comp_membership))) {
      names(comp_membership) <- igraph::V(g)$name
    }
    
    # Only use component membership if we have many small components (not one giant component)
    if (comp$no > 5 && !isTRUE(force_k)) {
      # Multiple small components - use them directly
      result <- tibble::tibble(ID = names(comp_membership), cluster = as.integer(comp_membership))
      result <- ped %>% dplyr::left_join(result, by = "ID")
      renum <- result %>% dplyr::distinct(cluster) %>% dplyr::arrange(cluster) %>% dplyr::mutate(new_cluster = dplyr::row_number())
      result <- result %>% dplyr::left_join(renum, by = "cluster") %>% dplyr::select(-cluster) %>% dplyr::rename(cluster = new_cluster)
      return(result)
    }
    
    # For single large component or when force_k=TRUE, use community detection

    # Otherwise the whole graph is one giant component (or user wants fixed K).
    # Choose a community strategy - prioritize methods that create more clusters
    choose_method <- function() {
      if (method != "auto") return(method)
      # For highly connected pedigrees, use spectral clustering to force more clusters
      if (isTRUE(force_k)) return("spectral")
      # For large connected components, use Louvain with resolution tuning
      if (igraph::vcount(g) > 100) return("louvain") else return("louvain")
    }
    mth <- choose_method()

    # --- Community discovery helpers ----
    run_louvain <- function(graph) {
      # Try multiple resolution parameters to get more communities
      resolutions <- c(0.5, 1.0, 1.5, 2.0)
      best_comm <- NULL
      best_modularity <- -Inf
      
      for (res in resolutions) {
        tryCatch({
          # Use edge betweenness for weighted clustering
          comm <- igraph::cluster_louvain(graph, resolution = res)
          mod <- igraph::modularity(comm)
          if (mod > best_modularity) {
            best_modularity <- mod
            best_comm <- comm
          }
        }, error = function(e) {
          # Fallback to default Louvain
          if (is.null(best_comm)) {
            best_comm <<- igraph::cluster_louvain(graph)
          }
        })
      }
      
      if (is.null(best_comm)) {
        best_comm <- igraph::cluster_louvain(graph)
      }
      
      membership <- igraph::membership(best_comm)
      # Preserve names when converting to integer
      result <- as.integer(membership)
      names(result) <- names(membership)
      result
    }

    run_spectral_k <- function(graph, k_target) {
      # Spectral embedding + kmeans for an exact K-way partition.
      # Compute first k eigenvectors of normalized Laplacian.
      # For stability, cap k_target at (number of vertices)
      nV <- igraph::vcount(graph)
      k_use <- max(1, min(k_target, nV))
      # Use igraph's Laplacian
      L <- igraph::laplacian_matrix(graph, normalized = TRUE, sparse = TRUE)
      # Compute k smallest eigenvectors (excluding trivial if any)
      # Convert to dense (small k), fallback if fails
      emb <- try({
        # RSpectra may not be available here; use base eigen on small dense backup
        if (inherits(L, "dgCMatrix")) {
          # As a fallback, convert a small dense
          if (nV <= 5000) {
            Ld <- as.matrix(L)
            ev <- eigen(Ld, symmetric = TRUE)
            # Take the last k_use columns of eigenvectors with smallest eigenvalues
            U <- ev$vectors[, (ncol(ev$vectors) - k_use + 1):ncol(ev$vectors), drop = FALSE]
          } else {
            # For very large graphs, approximate with Louvain then merge to K
            return(NULL)
          }
        } else {
          ev <- eigen(L, symmetric = TRUE)
          U <- ev$vectors[, (ncol(ev$vectors) - k_use + 1):ncol(ev$vectors), drop = FALSE]
        }
        U
      }, silent = TRUE)

      if (is.null(emb) || inherits(emb, "try-error")) return(NULL)

      set.seed(1L)
      km <- stats::kmeans(emb, centers = k_use, iter.max = 100, nstart = 5)
      # Preserve vertex names from graph
      result <- as.integer(km$cluster)
      names(result) <- igraph::V(graph)$name
      result
    }

    merge_to_exact_k <- function(labels, k_target) {
      # Preserve names from input
      label_names <- names(labels)
      # If already equal to K, return (preserving names)
      labs <- as.integer(labels)
      n_groups <- length(unique(labs))
      if (n_groups == k_target) {
        names(labs) <- label_names
        return(labs)
      }
      # Greedy round-robin merge of communities by descending size to exactly K bins
      sizes <- sort(table(labs), decreasing = TRUE)
      bins <- rep(0L, length(sizes))
      names(bins) <- names(sizes)
      # Assign initial groups to bins in round-robin
      bin_id <- rep(1:k_target, length.out = length(sizes))
      target_map <- setNames(bin_id, names(sizes))
      # Map original labels to merged bins
      merged <- target_map[as.character(labs)]
      result <- as.integer(merged)
      names(result) <- label_names
      result
    }

    # --- Run the chosen method ---
    membership <- NULL
    if (mth == "louvain") {
      membership <- run_louvain(g)
      if (isTRUE(force_k)) {
        membership <- merge_to_exact_k(membership, k)
      }
    } else if (mth == "spectral") {
      membership <- run_spectral_k(g, k)
      if (is.null(membership)) {
        # Fallback: Louvain then merge to K
        membership <- run_louvain(g)
        membership <- merge_to_exact_k(membership, k)
      }
    } else if (mth == "threshold") {
      # Build a similarity graph based on simple shared-parent tie-strength
      # (Here we keep original edges; in future we could expand with cousin ties)
      # Then prune weak bridges using edge betweenness percentile.
      eb <- igraph::edge_betweenness(g)
      cutoff <- stats::quantile(eb, probs = 0.90, na.rm = TRUE)
      g2 <- igraph::delete_edges(g, which(eb >= cutoff))
      membership <- igraph::membership(igraph::clusters(g2))
      # Ensure membership has names (should already have them from clusters)
      if (is.null(names(membership))) {
        names(membership) <- igraph::V(g2)$name
      }
      if (isTRUE(force_k)) {
        membership <- merge_to_exact_k(membership, k)
      }
    } else {
      membership <- run_louvain(g)
    }
    
    # Output as tibble in original ped order with consecutive 1..C labels
    # Ensure membership has names
    if (is.null(names(membership)) || length(names(membership)) == 0) {
      names(membership) <- igraph::V(g)$name
    }
    mem_tbl <- tibble::tibble(ID = names(membership), cluster = as.integer(membership))
    out <- ped %>% dplyr::left_join(mem_tbl, by = "ID")
    
    # Renumber clusters to 1..C by appearance order
    renum <- out %>% dplyr::distinct(cluster) %>% dplyr::arrange(cluster) %>% dplyr::mutate(new_cluster = dplyr::row_number())
    out <- out %>% dplyr::left_join(renum, by = "cluster") %>% dplyr::select(-cluster) %>% dplyr::rename(cluster = new_cluster)
    
    out
  }
  
  # Create super family clusters (larger groupings)
  create_super_family_clusters <- function(ped, max_size = 500) {
    family_clusters <- create_family_clusters(ped)
    
    # If no clusters or only one cluster, return as is
    if (nrow(family_clusters) == 0 || length(unique(family_clusters$cluster)) <= 1) {
      return(family_clusters)
    }
    
    # Group small families together
    cluster_sizes <- family_clusters %>% 
      group_by(cluster) %>% 
      summarise(size = n(), .groups = "drop")
    
    # Create super clusters using a simpler approach
    super_cluster_id <- 1
    current_size <- 0
    super_cluster_mapping <- numeric()
    
    for (i in 1:nrow(cluster_sizes)) {
      cluster_id <- cluster_sizes$cluster[i]
      cluster_size <- cluster_sizes$size[i]
      
      if (current_size + cluster_size > max_size && current_size > 0) {
        super_cluster_id <- super_cluster_id + 1
        current_size <- 0
      }
      
      super_cluster_mapping[cluster_id] <- super_cluster_id
      current_size <- current_size + cluster_size
    }
    
    # Apply mapping using simple vector indexing
    family_clusters %>%
      mutate(super_cluster = super_cluster_mapping[cluster]) %>%
      mutate(super_cluster = ifelse(is.na(super_cluster), 1, super_cluster)) %>%
      select(-cluster) %>%
      rename(cluster = super_cluster)
  }
  
  # Create mega family clusters (even larger groupings)
  create_mega_family_clusters <- function(ped, max_size = 2000) {
    super_family_clusters <- create_super_family_clusters(ped, max_size = 500)
    
    # If no clusters or only one cluster, return as is
    if (nrow(super_family_clusters) == 0 || length(unique(super_family_clusters$cluster)) <= 1) {
      return(super_family_clusters)
    }
    
    # Group super families together
    cluster_sizes <- super_family_clusters %>% 
      group_by(cluster) %>% 
      summarise(size = n(), .groups = "drop")
    
    # Create mega clusters using a simpler approach
    mega_cluster_id <- 1
    current_size <- 0
    mega_cluster_mapping <- numeric()
    
    for (i in 1:nrow(cluster_sizes)) {
      cluster_id <- cluster_sizes$cluster[i]
      cluster_size <- cluster_sizes$size[i]
      
      if (current_size + cluster_size > max_size && current_size > 0) {
        mega_cluster_id <- mega_cluster_id + 1
        current_size <- 0
      }
      
      mega_cluster_mapping[cluster_id] <- mega_cluster_id
      current_size <- current_size + cluster_size
    }
    
    # Apply mapping using simple vector indexing
    super_family_clusters %>%
      mutate(mega_cluster = mega_cluster_mapping[cluster]) %>%
      mutate(mega_cluster = ifelse(is.na(mega_cluster), 1, mega_cluster)) %>%
      select(-cluster) %>%
      rename(cluster = mega_cluster)
  }
  
  # Reactive value to store auto-detected columns
  detected_cols <- reactiveVal(NULL)
  
  # Reactive value to remember last used column mapping
  saved_mapping <- reactiveVal(list(id = NULL, sire = NULL, dam = NULL, sex = NULL))
  
  # Reactive value to track data validation status
  data_validation_status <- reactiveVal(list(valid = FALSE, errors = c(), warnings = c()))
  
  # Reactive values for smart aggregation system
  aggregation_system <- reactiveVal(NULL)
  current_aggregation_level <- reactiveVal("auto")
  current_drill_down_path <- reactiveVal(list())
  
  # Reactive value to track inbreeding calculation status
  f_calculation_status <- reactiveVal(list(
    calculating = FALSE,
    progress = 0,
    message = "",
    n_individuals = 0
  ))
  
  # Cache for F values to avoid repeated calculations
  f_values_cache <- reactiveVal(NULL)
  
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
      sex = input$sex_col
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
        if (is.null(auto_cols$sex) || is.na(auto_cols$sex)) {
          div(
            style = "background-color: #fff3cd; padding: 6px; border-radius: 4px; margin-top: 5px; border-left: 3px solid #ffc107;",
            tags$small(
              "💡 No sex column detected. You can manually select one if available, or leave as 'None'.",
              style = "color: #856404; font-size: 0.8rem;"
            )
          )
        } else {
          div(
            style = "background-color: #d1ecf1; padding: 6px; border-radius: 4px; margin-top: 5px; border-left: 3px solid #17a2b8;",
            tags$small(
              paste0("✓ Sex column '", auto_cols$sex, "' detected automatically."),
              style = "color: #0c5460; font-size: 0.8rem;"
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
        div(class = "alert alert-danger", style = "padding: 8px; font-size: 0.85rem; margin-top: 10px;",
            tags$div(strong("❌ Validation Errors:"), 
                     tags$ul(
                       lapply(validation_status$errors, function(error) {
                         tags$li(error, style = "margin: 2px 0;")
                       })
                     )),
            tags$hr(style = "margin: 10px 0;"),
            tags$div(
              actionButton("fix_pedigree", "🔧 Fix Pedigree", 
                           class = "btn btn-warning btn-sm", 
                           style = "margin-right: 8px; margin-top: 5px;"),
              downloadButton("download_missing_ids", "📥 Download Miss ID", 
                            class = "btn btn-info btn-sm", 
                            style = "margin-top: 5px;"),
              tags$small(tags$br(), "Fix Pedigree: Automatically removes invalid parent references. Download Miss ID: Exports missing Sire/Dam IDs.", 
                        style = "display: block; margin-top: 8px; color: #666;")
            )
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
      # Clear cache to force recalculation
      f_values_cache(NULL)
    }
  })
  
  # Fix Pedigree button handler
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
      founders <- sum(is.na(ped$Sire) & is.na(ped$Dam))
      both_parents <- sum(!is.na(ped$Sire) & !is.na(ped$Dam))
      
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
      # Export key columns; include Sex if present
      cols <- c("ID","Sire","Dam")
      if ("Sex" %in% names(ped)) cols <- c(cols, "Sex")
      out <- ped[, cols, drop = FALSE]
      
      # Replace NA/empty with 0 for missing parents
      out$Sire[is.na(out$Sire) | out$Sire == ""] <- "0"
      out$Dam[is.na(out$Dam) | out$Dam == ""] <- "0"
      
      # Write space-delimited without quotes, no row names, no header
      write.table(out, file, 
                  sep = " ",           # 空格分隔
                  quote = FALSE,       # 不加引号
                  row.names = FALSE,   # 无行号
                  col.names = FALSE,   # 无列名
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
  
  # Aggregation level selector UI
  output$aggregation_level_selector <- renderUI({
    # Non-interactive info: Auto mode only
    div(
      h5("Visualization Level: Auto (Recommended)", style = "margin: 0 0 6px 0; font-weight: 600;"),
      tags$small("Auto mode selects optimal visualization for your data size.")
    )
  })
  
  # Drill down navigation UI
  output$drill_down_navigation <- renderUI({
    drill_path <- current_drill_down_path()
    
    if (is.null(drill_path) || length(drill_path) == 0) {
      return(NULL)
    }
    
    div(
      h6("📍 Current Path:", style = "font-weight: 600; margin-bottom: 8px;"),
      div(
        style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; margin-bottom: 8px;",
        tags$small(
          paste(drill_path, collapse = " → "),
          style = "color: #6c757d;"
        )
      ),
      actionButton("reset_drill_down", "🏠 Back to Top Level", 
                   class = "btn btn-sm btn-outline-secondary",
                   style = "width: 100%;")
    )
  })
  
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
  
  # Process data (manual or auto)
  ped_data <- reactive({
    # Manual mode: requires button click
    if (!isTRUE(input$auto_process)) {
      req(input$process)
    }
    
    # Both modes: require data and column mapping
    if (is.null(raw_data()) || is.null(input$id_col) || is.null(input$sire_col) || is.null(input$dam_col)) {
      return(NULL)
    }
    
    # Check if data validation passed
    validation_status <- data_validation_status()
    if (is.null(validation_status) || !validation_status$valid) {
      # If validation status is not available, perform basic validation here
      basic_validation_errors <- validate_data_format(raw_data(), input$id_col, input$sire_col, input$dam_col)
      if (length(basic_validation_errors) > 0) {
        showNotification(paste("❌ Data validation failed:", paste(basic_validation_errors, collapse = "; ")), type = "error", duration = 8)
        return(NULL)
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
              "Auto-fix will set these missing references to NA."
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
    
    # Clear cache when new data is processed
    # family_clusters_cache(NULL)  # Removed - no longer needed
    
    # Initialize smart aggregation system with detailed progress
    n_individuals <- nrow(df)
    
    if (n_individuals > 1000) {
      withProgress(message = "Initializing smart aggregation system...", value = 0, {
        incProgress(0.1, detail = "Analyzing data structure...")
        
        incProgress(0.2, detail = "Creating family clusters...")
        agg_system <- smart_aggregation_system(df)
        
        incProgress(0.3, detail = "Setting up aggregation levels...")
        aggregation_system(agg_system)
        current_aggregation_level("auto")
        current_drill_down_path(list())
        
        incProgress(0.4, detail = "Finalizing system...")
        
        showNotification(
          paste0("✅ Data processed! Smart aggregation enabled for ", format(n_individuals, big.mark = ","), 
                 " individuals. Use level selector to navigate."),
          type = "message", duration = 8
        )
      })
    } else {
      withProgress(message = "Processing data...", value = 0.5, {
        agg_system <- smart_aggregation_system(df)
        aggregation_system(agg_system)
        current_aggregation_level("auto")
        current_drill_down_path(list())
    
    showNotification("✅ Data processed successfully!", type = "message", duration = 3)
      })
    }
    
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
    
    cat("Total individuals:", nrow(ped), "\n")
    cat("Founders:", sum(is.na(ped$Sire) & is.na(ped$Dam)), "\n")
    cat("Non-founders:", sum(!is.na(ped$Sire) | !is.na(ped$Dam)), "\n")
  })
  
  # Auto-switch to visualization tab when data is processed (with delay for large datasets)
  observe({
    req(ped_data())
    if (isTRUE(input$auto_process)) {
      # Check if data validation passed
      validation_status <- data_validation_status()
      if (!validation_status$valid) {
        return()  # Don't switch tabs if validation failed
      }
      
      n_rows <- nrow(ped_data())
      
      # Auto-select individual with deepest ancestors
      deepest_info <- find_deepest_ancestor_individual(ped_data())
      
      if (!is.null(deepest_info)) {
        # Set the individual with deepest ancestors as selected
        selected_individual(deepest_info$id)
        
        # Show notification
        showNotification(
          paste0("🎯 Auto-selected individual with deepest pedigree: ", 
                 deepest_info$id, " (", deepest_info$depth, " generations)"),
          type = "message",
          duration = 8
        )
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
  
  # Reactive value to store current drill-down state
  current_family <- reactiveVal(NULL)
  
  # Selected individual for visualization
  selected_individual <- reactiveVal(NULL)
  
  # Highlighted individuals for export (subset of visualized network)
  highlighted_individuals <- reactiveVal(character(0))
  
  # Info display for Selected Animal Export (match visualization exactly)
  output$selected_node_info <- renderUI({
    ped <- ped_data()
    target_id <- selected_individual()
    if (is.null(ped) || is.null(target_id) || !target_id %in% ped$ID) {
      return(div(style = "color: #666; font-style: italic; padding: 8px; background: #f8f8f8; border-radius: 4px;",
                 "💡 Select an individual (via search or by clicking a node) to enable export"))
    }

    depth <- input$search_depth %||% 1
    if (is.na(depth) || depth < 0) depth <- 1

    # Prefer using current visualization network to ensure counts match
    net <- network_data()
    if (!is.null(net) && !is.null(net$nodes) && nrow(net$nodes) > 0) {
      # Count nodes currently visualized
      related_ids <- net$nodes$id
      # Derive ancestors/descendants sets to present breakdown (computed with same depth)
      ancestors <- get_ancestors(ped, target_id, max_depth = depth)
      descendants <- get_descendants(ped, target_id, max_depth = depth)
      # Intersect with visualized nodes to avoid mismatch
      n_ancestors <- length(intersect(related_ids, ancestors))
      n_descendants <- length(intersect(related_ids, descendants))
      total_export <- length(unique(related_ids))
    } else {
      # Fallback to recompute when network not yet built
      ancestors <- get_ancestors(ped, target_id, max_depth = depth)
      descendants <- get_descendants(ped, target_id, max_depth = depth)
      selected_ids <- unique(c(target_id, ancestors, descendants))
      n_ancestors <- length(ancestors)
      n_descendants <- length(descendants)
      total_export <- length(selected_ids)
    }

    div(style = "color: #B89D5D; font-weight: bold; margin-bottom: 10px; padding: 8px; background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%); border-left: 4px solid #B89D5D; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
        div(style = "font-size: 1.1em; margin-bottom: 6px;",
            paste0("🎯 Selected Individual: ", target_id)),
        div(style = "font-size: 0.95em; color: #666;",
            paste0("📊 Search Depth: ", depth, " generation", ifelse(depth > 1, "s", ""))),
        div(style = "font-size: 0.95em; color: #666; margin-top: 4px;",
            paste0("📈 Export Summary: ", total_export, " individual", ifelse(total_export > 1, "s", ""),
                   " (", n_ancestors, " ancestor", ifelse(n_ancestors != 1, "s", ""),
                   " + 1 selected + ", n_descendants, " descendant", ifelse(n_descendants != 1, "s", ""), ")")))
  })
  
  # Whether a selected individual exists (controls export button visibility)
  output$has_selected_individual <- reactive({
    !is.null(selected_individual())
  })
  outputOptions(output, "has_selected_individual", suspendWhenHidden = FALSE)
  
  # Info display for visualization
  output$viz_info <- renderUI({
    target_id <- selected_individual()
    if (is.null(target_id)) {
      return(div(style = "color: #666; font-style: italic; margin-bottom: 10px;",
                 "💡 Enter an individual ID above to visualize their pedigree network"))
    }
    
    ped <- ped_data()
    if (is.null(ped)) return(NULL)
    
    # Get search depth
    search_depth <- input$search_depth %||% 5
    
    # Get F value if available
    f_val <- NULL
    if (!is.null(f_values()) && nrow(f_values()) > 0) {
      f_data <- f_values() %>% filter(ID == target_id)
      if (nrow(f_data) > 0) {
        f_val <- f_data$F[1]
      }
    }
    
    f_text <- if (!is.null(f_val)) paste0(" (F = ", round(f_val, 4), ")") else ""
    
    # Get highlighted count
    highlighted_count <- length(highlighted_individuals())
    highlight_text <- if (highlighted_count > 0) paste0(" | ", highlighted_count, " highlighted") else ""
    
    div(style = "color: #B89D5D; font-weight: bold; margin-bottom: 10px; padding: 8px; background: #f8f8f8; border-radius: 4px;",
        paste0("🎯 Showing pedigree network for: ", target_id, f_text, 
               " (Depth: ", search_depth, " generations)", highlight_text))
  })
  
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
        descendants <- get_descendants(ped, target_id, max_depth = depth)
        selected_ids <- unique(c(target_id, ancestors, descendants))

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
          } else if (id %in% descendants) {
            # For descendants, calculate downward generations
            gen <- 0
            current <- id
            visited <- character(0)
            
            # Trace back to target
            while (gen < depth && current != target_id && !current %in% visited) {
              ind <- ped %>% filter(ID == current)
              if (nrow(ind) == 0) break
              
              visited <- c(visited, current)
              gen <- gen + 1
              
              # Check parents
              if (!is.na(ind$Sire[1]) && ind$Sire[1] == target_id) return(gen)
              if (!is.na(ind$Dam[1]) && ind$Dam[1] == target_id) return(gen)
              
              # Continue with one parent
              if (!is.na(ind$Sire[1]) && ind$Sire[1] != "") {
                current <- ind$Sire[1]
              } else if (!is.na(ind$Dam[1]) && ind$Dam[1] != "") {
                current <- ind$Dam[1]
              } else {
                break
              }
            }
            return(gen)  # Positive for descendants
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
              ID %in% descendants & Generation == 1 ~ "Offspring",
              ID %in% descendants & Generation == 2 ~ "Grandoffspring",
              ID %in% descendants & Generation > 2 ~ paste0("Descendant (G", Generation, ")"),
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
  
  # Handle node highlighting for offspring generations
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
      
      # Get all relatives (ancestors + descendants) within specified generations
      # Try different methods in order of preference
      all_relatives <- tryCatch({
        # First try igraph method (most comprehensive for network analysis)
        get_relatives_with_igraph(ped, node_id, max_depth = highlight_gens)
      }, error = function(e) {
        # Fallback to pedigreeTools method
        tryCatch({
          get_relatives_with_pedigreeTools(ped, node_id, max_depth = highlight_gens)
        }, error = function(e2) {
          # Final fallback to custom method
          get_all_relatives(ped, node_id, max_depth = highlight_gens)
        })
      })
      
      # Get descendants based on highlight_gens setting (for backward compatibility)
      descendants <- get_descendants(ped, node_id, max_depth = highlight_gens)
      
      # Accumulate highlighted individuals (add to existing highlights)
      current_highlighted <- highlighted_individuals()
      new_highlighted <- unique(c(node_id, descendants))
      
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
                            node_id, "and", highlight_gens, "generations of offspring. Total:", length(all_highlighted)), 
                      type = "message", duration = 4)
    }
  })
  
  # Reactive to determine visualization mode based on data size and user choice
  actual_viz_mode <- reactive({
    req(ped_data())
    n_individuals <- nrow(ped_data())
    
    # Auto only (UI no longer selectable)
    viz_level <- "auto"
    
    if (viz_level == "auto") {
      if (n_individuals > 100) {
        return("family")
      } else {
        return("individual")
      }
    } else if (viz_level == "family") {
      return("family")
    } else {
      return("individual")
    }
  })
  
  # Update viz mode radio buttons based on data size (optional)
  observe({
    req(ped_data())
    n_individuals <- nrow(ped_data())
    
    # Only suggest cluster mode for large datasets
    if (n_individuals > 100) {
      # Don't force change, just show info
      showNotification(
        paste0("💡 Tip: For ", format(n_individuals, big.mark = ","), 
               " individuals, 'Family Clusters' mode may provide better performance"),
        type = "message",
        duration = 5
      )
    }
  })
  
  # Show drill-down info
  output$drill_down_info <- renderUI({
    req(ped_data())
    ped <- ped_data()
    
    # Check if ped_data returned NULL (due to column mapping errors)
    if (is.null(ped)) {
      return(div(class = "alert alert-danger", style = "padding: 8px; font-size: 0.85rem; margin-bottom: 10px;",
                 strong("❌ Error: "), "Data processing failed. Please check column mapping."))
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      return(div(class = "alert alert-danger", style = "padding: 8px; font-size: 0.85rem; margin-bottom: 10px;",
                 strong("❌ Error: "), "Required columns (ID, Sire, Dam) not found in processed data."))
    }
    
    n <- nrow(ped)
    mode <- actual_viz_mode()
    drilled <- current_family()
    
    if (!is.null(drilled) && grepl("^Family_", drilled)) {
      # Drilling into specific family
      family_num <- gsub("Family_", "", drilled)
      
      # Get member count for this family
      edges_all <- bind_rows(
        ped_data() %>% filter(!is.na(Sire)) %>% transmute(from = Sire, to = ID),
        ped_data() %>% filter(!is.na(Dam)) %>% transmute(from = Dam, to = ID)
      )
      
      if (nrow(edges_all) > 0) {
        g <- graph_from_data_frame(edges_all, directed = FALSE)
        comp <- components(g, mode = "weak")
        clusters <- tibble(ID = names(comp$membership), cluster = comp$membership)
        
        family_count <- sum(clusters$cluster == as.numeric(family_num))
        
        div(class = "alert alert-info", style = "padding: 8px; font-size: 0.85rem; margin-bottom: 10px;",
            strong("📍 Drill-Down View: "), 
            paste0("Family ", family_num, " (", family_count, " members)"))
      } else {
        div(class = "alert alert-info", style = "padding: 8px; font-size: 0.85rem; margin-bottom: 10px;",
            strong("📍 Drill-Down View: "), paste0("Family ", family_num))
      }
    } else if (mode == "family") {
      div(class = "alert alert-info", style = "padding: 8px; font-size: 0.85rem; margin-bottom: 10px;",
          strong("👨‍👩‍👧‍👦 Family Clusters View: "), 
          paste0(format(n, big.mark = ","), " individuals grouped into families based on parental relationships. "),
          "Families are defined by siblings (same parents) and half-siblings (same sire or dam). ",
          "Double-click any family node to drill down and see individual family members.")
    } else {
      div(class = "alert alert-success", style = "padding: 8px; font-size: 0.85rem; margin-bottom: 10px;",
          strong("👤 Full Pedigree View: "), paste0("Showing all ", n, " individuals"))
    }
  })
  
  # Data preview - highly optimized for large datasets
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
    
    n <- nrow(ped)
    
    # For very large datasets, show only a sample
    if (n > 10000) {
      # Show first 5000 rows with warning
      display_data <- head(ped, 5000)
      
      datatable(
        display_data,
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color: #d9534f; font-weight: bold; padding: 10px;',
          paste0("⚠️ Displaying first 5,000 of ", format(n, big.mark = ","), 
                 " rows for performance. Download full data if needed.")
        ),
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "400px",
          deferRender = TRUE,
          dom = 'Bfrtip',
          buttons = list('copy', 'csv', 'excel')
        ),
        filter = "top",
        rownames = FALSE
      )
    } else if (n > 5000) {
      # Optimized rendering for large datasets
      datatable(
        ped,
        options = list(
          pageLength = 25,
          scrollX = TRUE,
          scrollY = "400px",
          deferRender = TRUE,
          scroller = TRUE,
          dom = 'Bfrtip'
        ),
        filter = "top",
        rownames = FALSE
      )
    } else {
      # Normal rendering for small datasets
      datatable(
        ped,
        options = list(
          pageLength = 15,
          scrollX = TRUE
        ),
        filter = "top",
        rownames = FALSE
      )
    }
  })
  
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
    founders <- sum(is.na(ped$Sire) & is.na(ped$Dam))
    both_parents <- sum(!is.na(ped$Sire) & !is.na(ped$Dam))
    
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
  
  # Calculate F (automatically when auto_process is enabled, or manually)
  f_values <- reactive({
    # Manual mode: requires button click
    if (!isTRUE(input$auto_process)) {
      req(input$calc_f)
    }
    
    req(ped_data())
    ped <- ped_data()
    
    # Check if we have cached values
    cached_f <- f_values_cache()
    if (!is.null(cached_f)) {
      return(cached_f)
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
        n_with_parents <- sum(!is.na(ped$Sire) | !is.na(ped$Dam))
        n_founders <- sum(is.na(ped$Sire) & is.na(ped$Dam))
        
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
        
        incProgress(0.4, detail = "Computing inbreeding coefficients...")
        
        # Update progress status
        f_calculation_status(list(
          calculating = TRUE,
          progress = 0.4,
          message = "Computing inbreeding coefficients...",
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
        n_founders <- sum(is.na(ped$Sire) & is.na(ped$Dam))
        n_with_parents <- sum(!is.na(ped$Sire) | !is.na(ped$Dam))
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
    
    cat("n =", length(f_vals), "\n")
    cat("Mean:", round(mean(f_vals, na.rm = TRUE), 4), "\n")
    cat("Median:", round(median(f_vals, na.rm = TRUE), 4), "\n")
    cat("Min:", round(min(f_vals, na.rm = TRUE), 4), "\n")
    cat("Max:", round(max(f_vals, na.rm = TRUE), 4), "\n")
    cat("\nInbred (F>0):", sum(f_vals > 0, na.rm = TRUE))
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
      options = list(pageLength = 10, dom = 't'),
      rownames = FALSE
    ) %>% formatRound("F", 4)
  })
  
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
  find_deepest_ancestor_individual <- function(ped) {
    if (is.null(ped) || nrow(ped) == 0) return(NULL)
    
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
  
  # Function to get all relatives (ancestors + descendants) within specified generations
  get_all_relatives <- function(ped, individual_id, max_depth = 1) {
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
    # Get ancestors
    ancestors <- get_ancestors(ped, individual_id, max_depth = max_depth)
    
    # Get descendants  
    descendants <- get_descendants(ped, individual_id, max_depth = max_depth)
    
    # Combine all relatives (including the individual itself)
    all_relatives <- unique(c(individual_id, ancestors, descendants))
    
    return(all_relatives)
  }
  
  # Advanced function using pedigreeTools for comprehensive relative analysis
  get_relatives_with_pedigreeTools <- function(ped, individual_id, max_depth = 1) {
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
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
        # For now, use our custom functions for depth filtering
        # This could be enhanced with more sophisticated pedigreeTools analysis
        ancestors <- get_ancestors(ped, individual_id, max_depth = max_depth)
        descendants <- get_descendants(ped, individual_id, max_depth = max_depth)
        
        # Combine with pedigreeTools results
        all_relatives <- unique(c(individual_id, ancestors, descendants, related_ids))
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
  
  # Function using igraph for network-based relative analysis
  get_relatives_with_igraph <- function(ped, individual_id, max_depth = 1) {
    if (is.null(individual_id) || is.na(individual_id)) return(character(0))
    
    tryCatch({
      # Create edges from pedigree data
      edges <- bind_rows(
        ped %>% filter(!is.na(Sire)) %>% transmute(from = Sire, to = ID),
        ped %>% filter(!is.na(Dam)) %>% transmute(from = Dam, to = ID)
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
    
    # Get search depth from user input (for both ancestors and descendants)
    search_depth <- input$search_depth %||% 5
    
    # Get all related individuals
    ancestors <- get_ancestors(ped, target_id, max_depth = search_depth)
    descendants <- get_descendants(ped, target_id, max_depth = search_depth)
    related_ids <- unique(c(target_id, ancestors, descendants))
    
    # Filter pedigree to related individuals
    related_ped <- ped %>% filter(ID %in% related_ids)
    
    if (nrow(related_ped) == 0) return(list(nodes = tibble(), edges = tibble()))
    
    # Build nodes
    nodes <- related_ped %>%
      transmute(
        id = ID,
        label = if(input$show_labels) ID else "",
        group = ifelse(!is.na(Sex), as.character(Sex), "Unknown"),
        title = paste0("ID: ", ID, 
                       ifelse(!is.na(Sire), paste0("\nSire: ", Sire), ""),
                       ifelse(!is.na(Dam), paste0("\nDam: ", Dam), ""),
                       ifelse(!is.na(Sex), paste0("\nSex: ", Sex), "")),
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
          title = paste0(title, ifelse(!is.na(F), paste0("\nF: ", round(F, 4)), "")),
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
        title = ifelse(id == target_id, paste0(title, "\n\n🎯 Target Individual"), title),
        shape = ifelse(id == target_id, "star", "dot"),
        color = ifelse(id == target_id, "#FF0000", color),
        borderWidth = ifelse(id == target_id, 4, borderWidth),
        # Make target individual slightly larger
        value = ifelse(id == target_id, value * 1.2, value)
      )
    
    # Build edges
    edges <- bind_rows(
      related_ped %>% filter(!is.na(Sire)) %>% transmute(from = Sire, to = ID),
      related_ped %>% filter(!is.na(Dam)) %>% transmute(from = Dam, to = ID)
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
    req(ped_data())
      ped <- ped_data()
    
    # Check if ped_data returned NULL (due to column mapping errors)
    if (is.null(ped)) {
      return(list(nodes = tibble(), edges = tibble()))
    }
    
    # Check if required columns exist
    if (!"ID" %in% names(ped) || !"Sire" %in% names(ped) || !"Dam" %in% names(ped)) {
      showNotification("Error: Required columns (ID, Sire, Dam) not found in processed data", type = "error")
      return(list(nodes = tibble(), edges = tibble()))
    }
    
    # Get target individual
    target_id <- selected_individual()
    
    if (!is.null(target_id)) {
      # Build individual network
      return(build_individual_network(target_id))
      } else {
      # Return empty network
      return(list(nodes = tibble(), edges = tibble(), layout = "precomputed"))
    }
  })
  
  # Network visualization output
  output$pedigree_network <- renderVisNetwork({
    net <- network_data()
    
    if (is.null(net) || nrow(net$nodes) == 0) {
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
            
            // Add click event listener
            network.on('click', function(params) {
              if (params.nodes.length > 0) {
                var nodeId = params.nodes[0];
                var nodeData = network.body.data.nodes.get(nodeId);
                if (nodeData && !nodeData.id.startsWith('Cluster_')) {
                  // Trigger highlight event
                  Shiny.setInputValue('selected_node_for_highlight', nodeId, {priority: 'event'});
                  // Also trigger download event for export functionality
                  Shiny.setInputValue('trigger_download', nodeId, {priority: 'event'});
                }
              }
            });
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
            
            // Add click event listener
            network.on('click', function(params) {
              if (params.nodes.length > 0) {
                var nodeId = params.nodes[0];
                var nodeData = network.body.data.nodes.get(nodeId);
                if (nodeData && !nodeData.id.startsWith('Cluster_')) {
                  // Trigger highlight event
                  Shiny.setInputValue('selected_node_for_highlight', nodeId, {priority: 'event'});
                  // Also trigger download event for export functionality
                  Shiny.setInputValue('trigger_download', nodeId, {priority: 'event'});
                }
              }
            });
          }
        ")
    }
  })

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
}

shinyApp(ui, server)
