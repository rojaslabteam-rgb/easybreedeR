# ================================================
# easyblup - Interactive BLUPF90 Parameter Generator  
# ================================================

library(shiny)
library(bslib)

# Declare stub globals to silence R CMD check when loaded before modules
utils::globalVariables(c("aiAssistantUI", "aiAssistantServer", "language_code"))

# Declare externally provided helpers to silence lintr visibility warnings
utils::globalVariables(c("resolve_suite_lang", "map_suite_lang_for_app"))

# Source shared language helpers (export into global env so get_label is visible)
try(source(normalizePath(file.path("..", "Language.R"), winslash = "/", mustWork = FALSE), local = FALSE), silent = TRUE)

# Capture shared get_label before defining local alias
shared_get_label <- if (exists("get_label", mode = "function")) get_label else NULL

# Use shared get_label, but wrap it to add app prefix and handle EN/ZH uppercase
get_label_local <- function(key, lang = "EN") {
  # Normalize lang to lowercase for lookup, but remember original case
  lang_lower <- tolower(lang)
  
  # Use shared get_label if available
  gl <- shared_get_label
  if (is.null(gl) || !is.function(gl)) return(key)
  
  # Only prefix if not already prefixed
  effective_key <- if (startsWith(key, "easyblup_")) key else paste0("easyblup_", key)
  result <- try(gl(effective_key, lang_lower, app = "easyblup"), silent = TRUE)
  if (inherits(result, "try-error")) {
    # Fallback to direct key lookup
    result <- try(gl(key, lang_lower, app = "easyblup"), silent = TRUE)
    if (inherits(result, "try-error")) return(key)
  }
  result
}

# Alias for compatibility (safe: uses captured shared_get_label inside)
get_label <- get_label_local

# small helper
`%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x

# Determine PLINK prefix from uploaded PED/MAP pair.
derive_plink_prefix <- function(file_names) {
  if (is.null(file_names) || length(file_names) == 0) return(NULL)
  norm_names <- basename(file_names)
  ped_idx <- grepl("\\.ped$", norm_names, ignore.case = TRUE)
  map_idx <- grepl("\\.map$", norm_names, ignore.case = TRUE)
  candidate <- if (any(ped_idx)) {
    norm_names[ped_idx][1]
  } else if (any(map_idx)) {
    norm_names[map_idx][1]
  } else {
    norm_names[1]
  }
  tools::file_path_sans_ext(candidate)
}

# ====== AI Assistant (optional) ======
load_app_rules <- function(app_name) {
  rules_path <- file.path("apps", app_name, "ai_rules.json")
  # Fallback to local app folder (easybreedeR/easyblup)
  if (!file.exists(rules_path)) {
    rules_path <- file.path(".", "ai_rules.json")
  }
  if (file.exists(rules_path)) {
    tryCatch(jsonlite::fromJSON(rules_path, simplifyVector = TRUE), error = function(e) list())
  } else list()
}

  # Look for ai_assistant module in either ../modules/ (shared) or local app folder (easyblup)
  ai_module_candidates <- c(
    normalizePath(file.path("..", "modules", "ai_assistant.R"), winslash = "/", mustWork = FALSE),
    normalizePath(file.path(".", "ai_assistant.R"), winslash = "/", mustWork = FALSE)
  )
  ai_modules_path <- NULL
  for (p in ai_module_candidates) {
    if (file.exists(p)) { ai_modules_path <- p; break }
  }
  if (!is.null(ai_modules_path)) {
    # Load assistant module into global env so aiAssistantUI/Server are visible to exists()
    try(source(ai_modules_path, local = FALSE), silent = TRUE)
    # If language_code is still missing but TRANSLATIONS loaded, define a light fallback
    if (!exists("language_code", mode = "function")) {
      language_code <- function(name) {
        if (is.null(name)) return("en")
        nm <- tolower(trimws(as.character(name)))
        if (grepl("zh", nm)) return("zh")
        if (grepl("portugu", nm) || grepl("pt", nm)) return("pt")
        "en"
      }
    }
    # Provide lightweight fallbacks for UI/server to avoid "no visible global" errors during static checks
    if (!exists("aiAssistantUI", mode = "function")) {
      aiAssistantUI <- function(id) { shiny::div(id = shiny::NS(id, "ai_placeholder"), "AI module not loaded") }
    }
    if (!exists("aiAssistantServer", mode = "function")) {
      aiAssistantServer <- function(id, ...) { shiny::moduleServer(id, function(input, output, session) { }) }
    }
  }

  # MCP-style tool definitions for easyblup (optional)
  mcp_tools_path <- normalizePath(file.path(".", "mcp_tools.R"), winslash = "/", mustWork = FALSE)
  if (file.exists(mcp_tools_path)) {
    try(source(mcp_tools_path, local = FALSE), silent = TRUE)
  }

# ====== UI Definition ======
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#CEB888",
    base_font = font_google("Crimson Text")
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
    html, body { height: 100%; margin: 0; padding: 0; }
    .title-bar {
      background: linear-gradient(135deg, #CEB888 0%, #B89D5D 100%);
      padding: 15px 20px; text-align: center;
      box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      border-bottom: 3px solid #CFB991;
      flex-shrink: 0;
    }
    .title-bar h1 {
      margin: 0; color: #000; font-weight: 700;
      font-size: clamp(1.5rem, 3vw, 2.5rem);
      font-family: 'Crimson Text', 'Noto Sans SC', 'PingFang SC', 'Microsoft YaHei', 'Heiti SC', 'SimSun', 'Noto Sans', Arial, sans-serif;
    }
    .title-bar p { margin: 5px 0 0 0; font-size: 1rem; color: #000; opacity: .9; 
      font-family: 'Crimson Text', 'Noto Sans SC', 'PingFang SC', 'Microsoft YaHei', 'Heiti SC', 'SimSun', 'Noto Sans', Arial, sans-serif; }
    .three-panel-container {
      display: flex; height: calc(100vh - 72px);
      overflow: hidden; position: relative;
    }
    .left-panel, .right-panel {
      width: 320px; background: #FEFEFE;
      overflow-y: auto; overflow-x: hidden;
      padding: 20px; flex-shrink: 0;
      transition: all .3s ease;
    }
    /* hide scrollbars but keep scrolling */
    .left-panel, .right-panel { -ms-overflow-style: none; scrollbar-width: none; }
    .left-panel::-webkit-scrollbar, .right-panel::-webkit-scrollbar { display: none; }
    .left-panel { border-right: 2px solid #CFB991; }
    .right-panel { border-left: 2px solid #CFB991; }
    .left-panel.hidden, .right-panel.hidden { width: 0; padding: 0; overflow: hidden; }

    .center-panel {
      flex: 1; overflow-y: auto; overflow-x: hidden;
      padding: 20px; background: #FFFFFF;
      display: flex; flex-direction: column;
    }

    .panel-section {
      background: linear-gradient(135deg,#FFFFFF 0%,#F8F9FA 100%);
      border: 2px solid #CFB991; border-radius: 8px;
      padding: 15px; margin-bottom: 20px;
      box-shadow: 0 2px 8px rgba(206,184,136,.15);
    }
    /* Button styles aligned with PedivieweR 2.0 */
    .btn-primary, .btn.btn-primary { background-color:#CEB888 !important; border-color:#CEB888 !important; color:#000000 !important; font-weight:600; }
    .btn-primary:hover, .btn.btn-primary:hover { background-color:#B89D5D !important; border-color:#B89D5D !important; }
    .btn-secondary { background-color:#95A5A6 !important; border-color:#95A5A6 !important; color:#fff !important; }
    .btn-secondary:hover { background-color:#7F8C8D !important; border-color:#7F8C8D !important; }
    .section-title {
      font-size: 1.1rem; font-weight: 700; color: #2c3e50;
      margin: 0 0 12px 0; padding-bottom: 8px; border-bottom: 2px solid #CEB888;
    }

    /* Toggle buttons */
    .toggle-btn-left, .toggle-btn-right {
      position: fixed; top: 50%; transform: translateY(-50%);
      z-index: 1100;
    }
    .toggle-btn-left { left: 8px; }
    .toggle-btn-right { right: 8px; }
    .toggle-btn-left .btn, .toggle-btn-right .btn {
      width: 14px; height: 64px; padding: 0; line-height: 1;
      border-radius: 8px; border: 2px solid #CFB991;
      background-color: #CEB888; color: #000; font-weight: 700;
      box-shadow: 0 4px 12px rgba(0,0,0,.2);
      display: flex; align-items: center; justify-content: center;
    }
    .toggle-btn-left .btn:hover, .toggle-btn-right .btn:hover {
      background-color: #B89D5D;
      box-shadow: 0 6px 16px rgba(0,0,0,.3);
      transform: translateY(-1px);
    }

    /* Button theme */
    .btn-primary, .btn.btn-primary {
      background-color: #CEB888 !important;
      border-color: #CEB888 !important;
      color: #000 !important; font-weight: 600;
    }
    .btn-primary:hover, .btn.btn-primary:hover {
      background-color: #B89D5D !important;
      border-color: #B89D5D !important;
      color: #000 !important;
    }

    /* Parameter editor */
    #param_editor {
      flex: 1; width: 100%; min-height: 400px;
      resize: vertical; border: 1px solid #E0E0E0;
      border-radius: 6px; padding: 10px;
      font-family: 'Courier New', monospace;
      background: #fff;
    }

    /* Responsive */
    @media (max-width: 1200px) {
      .left-panel, .right-panel { width: 280px; }
    }
    @media (max-width: 992px) {
      .left-panel, .right-panel { width: 260px; }
    }
    @media (max-width: 768px) {
      .left-panel.hidden, .right-panel.hidden { display: none; }
    }
    /* AI floating assistant */
    .ai-fab { position: fixed; right: 20px; bottom: 30px; z-index: 1200; }
    .ai-fab .btn { border-radius: 50%; width: 56px; height: 56px; font-size: 24px; padding: 0; display:flex; align-items:center; justify-content:center; }
    #aiFabBox { cursor: pointer; }
    #aiFabBox.dragging { cursor: grabbing; }
    .ai-panel { position: fixed; right: 20px; bottom: 88px; width: 360px; max-width: 92vw; height: 60vh; max-height: 78vh; background:#fff; border:2px solid #CFB991; border-radius: 10px; box-shadow:0 8px 24px rgba(0,0,0,.2); z-index: 1199; display:none; overflow:hidden; }
    .ai-panel-header { padding:10px 12px; border-bottom:2px solid #CFB991; background:linear-gradient(135deg,#FFF9F0 0%,#FFF5E6 100%); font-weight:700; }
    .ai-panel-body { padding:10px; height: calc(100% - 48px); overflow:auto; }
    .ai-gear { float:right; margin-top:-2px; }
    .ai-gear .btn { border-radius:4px; padding:4px 8px; font-size:14px; }
    
    /* Genotype Format Helper Modal Styles */
    .geno-helper-modal .modal-header {
      background: linear-gradient(135deg, #CEB888 0%, #B89D5D 100%);
      border-bottom: 3px solid #CFB991;
      padding: 20px 25px;
      border-radius: 8px 8px 0 0;
    }
    .geno-helper-modal .modal-title {
      color: #000;
      font-weight: 700;
      font-size: 1.4rem;
      display: flex;
      align-items: center;
      gap: 10px;
    }
    .geno-helper-modal .modal-title::before {
      content: '🔄';
      font-size: 1.6rem;
    }
    .geno-helper-modal .modal-body {
      padding: 25px;
      background: #FAFAFA;
    }
    .geno-helper-info-box {
      background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%);
      border: 2px solid #CEB888;
      border-radius: 8px;
      padding: 15px 18px;
      margin-bottom: 20px;
      box-shadow: 0 2px 6px rgba(206,184,136,.1);
    }
    .geno-helper-info-box p {
      margin: 0;
      color: #333;
      font-size: 0.95rem;
      line-height: 1.6;
    }
    .geno-helper-info-box p::before {
      content: '💡 ';
      margin-right: 5px;
    }
    .geno-helper-direction-card {
      background: #FFFFFF;
      border: 2px solid #E0E0E0;
      border-radius: 10px;
      padding: 20px;
      margin-bottom: 20px;
      transition: all 0.3s ease;
      box-shadow: 0 2px 4px rgba(0,0,0,.05);
    }
    .geno-helper-direction-card:hover {
      border-color: #CEB888;
      box-shadow: 0 4px 12px rgba(206,184,136,.15);
    }
    .geno-helper-direction-card h4 {
      color: #2c3e50;
      font-weight: 700;
      margin-top: 0;
      margin-bottom: 12px;
      font-size: 1.2rem;
      display: flex;
      align-items: center;
      gap: 8px;
    }
    .geno-helper-direction-card h4::before {
      content: '📁';
      font-size: 1.3rem;
    }
    .geno-helper-direction-card p {
      color: #666;
      font-size: 0.9rem;
      margin-bottom: 15px;
      line-height: 1.5;
    }
    .geno-helper-file-input-wrapper {
      background: #F8F9FA;
      border: 2px dashed #CFB991;
      border-radius: 8px;
      padding: 15px;
      margin-bottom: 12px;
      transition: all 0.2s ease;
    }
    .geno-helper-file-input-wrapper:hover {
      border-color: #CEB888;
      background: #FFF9F0;
    }
    .geno-helper-file-input-wrapper label {
      font-weight: 600;
      color: #2c3e50;
      margin-bottom: 8px;
      display: block;
    }
    .geno-helper-file-input-wrapper .form-group {
      margin-bottom: 0;
    }
    .geno-helper-file-input-wrapper .input-group {
      display: flex;
      align-items: stretch;
    }
    .geno-helper-file-input-wrapper .btn-file {
      height: 38px;
      line-height: 38px;
      padding: 0 15px;
      display: flex;
      align-items: center;
      justify-content: center;
      border-radius: 4px 0 0 4px;
    }
    .geno-helper-file-input-wrapper .form-control {
      height: 38px;
      line-height: 38px;
      padding: 0 12px;
      border-radius: 0 4px 4px 0;
    }
    .geno-helper-file-input-wrapper .input-group-btn,
    .geno-helper-file-input-wrapper .input-group-prepend {
      display: flex;
      align-items: stretch;
    }
    .geno-helper-modal .modal-footer {
      background: #FAFAFA;
      border-top: 2px solid #E0E0E0;
      padding: 15px 25px;
      border-radius: 0 0 8px 8px;
    }
    .geno-helper-modal .btn {
      padding: 10px 24px;
      font-weight: 600;
      border-radius: 6px;
      transition: all 0.2s ease;
    }
    .geno-helper-modal .btn-primary {
      background-color: #CEB888 !important;
      border-color: #CEB888 !important;
      color: #000 !important;
    }
    .geno-helper-modal .btn-primary:hover {
      background-color: #B89D5D !important;
      border-color: #B89D5D !important;
      transform: translateY(-1px);
      box-shadow: 0 4px 8px rgba(206,184,136,.3);
    }
    .geno-helper-modal .btn-secondary {
      background-color: #95A5A6 !important;
      border-color: #95A5A6 !important;
    }
    .geno-helper-modal .btn-secondary:hover {
      background-color: #7F8C8D !important;
      border-color: #7F8C8D !important;
    }
    #download_blup_map {
      background-color: rgba(206, 184, 136, 1) !important;
      border-color: rgba(206, 184, 136, 1) !important;
    }
    #download_blup_txt {
      background-color: rgba(206, 184, 136, 1) !important;
      border-color: rgba(206, 184, 136, 1) !important;
      width: 212px;
    }
    /* Align heights for folder selection input */
    #geno_convert_output_dir,
    #geno_convert_output_dir_blup {
      height: 45px !important;
      box-sizing: border-box !important;
      line-height: 45px !important;
      padding: 0 12px !important;
    }
    /* Reposition progress bars for file inputs - place them below span and input */
    .geno-helper-file-input-wrapper .form-group {
      display: flex !important;
      flex-direction: column !important;
    }
    .geno-helper-file-input-wrapper .form-group .input-group {
      order: 1 !important;
      width: 100% !important;
    }
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_ped_progress,
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_map_progress {
      order: 2 !important;
      width: 100% !important;
      margin-top: 8px !important;
      margin-bottom: 0 !important;
      display: block !important;
    }
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_ped_progress .progress-bar,
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_map_progress .progress-bar {
      width: 100% !important;
    }
    /* Ensure the textInput wrapper container aligns properly */
    .geno-helper-file-input-wrapper .shiny-input-container {
      margin-bottom: 0 !important;
      display: flex !important;
      align-items: stretch !important;
      height: 100% !important;
    }
    .geno-helper-file-input-wrapper .form-group {
      margin-bottom: 0 !important;
      display: flex !important;
      align-items: stretch !important;
      flex: 1 !important;
    }
    .geno-helper-file-input-wrapper .shiny-input-container .form-control.shiny-input-text {
      height: 45px !important;
      line-height: 45px !important;
      box-sizing: border-box !important;
    }
    .geno-helper-arrow {
      display: inline-block;
      margin: 0 8px;
      color: #CEB888;
      font-weight: bold;
      font-size: 1.2rem;
    }
    
    /* Genotype Format Label Alignment */
    #geno_format-label {
      display: flex;
      align-items: center;
    }
    #geno_format_label_ui {
      display: inline-flex;
      align-items: center;
      gap: 6px;
    }
    #geno_format_label_ui > span {
      display: inline-flex;
      align-items: center;
      line-height: 1.5;
    }
    #geno_format_help {
      display: inline-flex;
      align-items: center;
      padding: 0;
      text-decoration: none;
      vertical-align: middle;
    }
    #geno_format_help:hover {
      text-decoration: none;
    }
    #geno_format_help .action-label {
      display: inline-flex;
      align-items: center;
    }
  ")),
    tags$script(HTML("
      // Handle text download
      Shiny.addCustomMessageHandler('download_text', function(message) {
        var blob = new Blob([message.content], { type: 'text/plain' });
        var url = window.URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = message.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);
      });
      
      // Handle folder selection
      window.selectOutputFolder = function() {
        var outputDirId = 'geno_convert_output_dir_blup';
        
        function setOutputDirValue(id, value) {
          var el = document.getElementById(id);
          if (el) {
            el.value = value;
            el.dispatchEvent(new Event('change', { bubbles: true }));
          }
          Shiny.setInputValue(id, value, {priority: 'event'});
        }
        
        // Try File System Access API (Chrome/Edge 86+, Safari 15.2+)
        if (window.showDirectoryPicker) {
          window.showDirectoryPicker().then(function(handle) {
            // Get the folder name
            var folderName = handle.name;
            
            // Try to get the full path using the handle
            // Note: File System Access API doesn't directly expose full paths for security reasons
            // We'll use the folder name and let the user see it, or they can manually enter the full path
            var currentValue = document.getElementById(outputDirId).value || '';
            var basePath = currentValue || '';
            
            // If current value looks like a path, try to extract the directory part
            if (basePath && basePath.indexOf('/') >= 0) {
              var lastSlash = basePath.lastIndexOf('/');
              basePath = basePath.substring(0, lastSlash + 1);
            } else if (!basePath) {
              basePath = './';
            }
            
            var newPath = basePath + folderName;
            setOutputDirValue(outputDirId, newPath);
          }).catch(function(err) {
            // User cancelled or error occurred
            if (err.name !== 'AbortError') {
              console.log('Directory picker error:', err);
              alert('无法选择文件夹。请手动输入路径，或使用支持文件夹选择的浏览器（Chrome/Edge 86+）。');
            }
          });
        } else {
          // Fallback: use a file input with webkitdirectory (older browsers)
          var input = document.createElement('input');
          input.type = 'file';
          input.webkitdirectory = true;
          input.style.display = 'none';
          input.onchange = function(e) {
            if (e.target.files.length > 0) {
              var firstFile = e.target.files[0];
              var relativePath = firstFile.webkitRelativePath || '';
              var folderName = relativePath.split('/')[0] || firstFile.name;
              
              // Try to get full path (works in Electron/Node.js environments)
              var fullPath = '';
              try {
                if (firstFile.path) {
                  var filePath = firstFile.path;
                  var lastSlash = filePath.lastIndexOf(folderName);
                  if (lastSlash >= 0) {
                    fullPath = filePath.substring(0, lastSlash + folderName.length);
                  }
                }
              } catch(ex) {}
              
              if (fullPath) {
                setOutputDirValue(outputDirId, fullPath);
              } else {
                // Fallback: use folder name
                var currentValue = document.getElementById(outputDirId).value || './';
                var newPath = currentValue + (currentValue.endsWith('/') ? '' : '/') + folderName;
                setOutputDirValue(outputDirId, newPath);
              }
            }
            document.body.removeChild(input);
          };
          document.body.appendChild(input);
          input.click();
        }
      };
      
      // Reposition progress bars for file inputs - ensure they appear below span and input
      function repositionFileInputProgressBars() {
        var progressBarIds = ['geno_help_plink_ped_progress', 'geno_help_plink_map_progress'];
        progressBarIds.forEach(function(progressId) {
          var progressDiv = document.getElementById(progressId);
          if (progressDiv) {
            var formGroup = progressDiv.closest('.form-group');
            var inputGroup = formGroup ? formGroup.querySelector('.input-group') : null;
            if (formGroup && inputGroup) {
              // Ensure progress bar is after input-group in DOM order
              if (progressDiv.previousElementSibling !== inputGroup) {
                // Progress bar is already correctly positioned by CSS flexbox order
                // Just ensure it's visible and styled correctly
                progressDiv.style.display = 'block';
                progressDiv.style.width = '100%';
                progressDiv.style.marginTop = '8px';
              }
            }
          }
        });
      }
      
      // Run on Shiny connection and after updates
      $(document).on('shiny:connected', function() {
        setTimeout(repositionFileInputProgressBars, 100);
      });
      $(document).on('shiny:value', function() {
        setTimeout(repositionFileInputProgressBars, 100);
      });
      
      // Handle batch downloads with delays between files
      Shiny.addCustomMessageHandler('download_text_batch', function(message) {
        var files = message.files || [];
        if (files.length === 0) return;
        
        function downloadFile(index) {
          if (index >= files.length) return;
          
          var file = files[index];
          var blob = new Blob([file.content], { type: 'text/plain' });
          var url = window.URL.createObjectURL(blob);
          var a = document.createElement('a');
          a.href = url;
          a.download = file.filename;
          document.body.appendChild(a);
          a.click();
          document.body.removeChild(a);
          
          // Delay before next download to ensure browser processes each file
          setTimeout(function() {
            window.URL.revokeObjectURL(url);
            downloadFile(index + 1);
          }, 300);
        }
        
        downloadFile(0);
      });
      
      // Handle getting textarea content
      Shiny.addCustomMessageHandler('get_textarea_content', function(message) {
        var content = document.getElementById('param_editor').value;
        Shiny.setInputValue('textarea_content', content);
      });
      
      // Handle updating textarea content
      Shiny.addCustomMessageHandler('update_textarea', function(message) {
        document.getElementById('param_editor').value = message.content;
      });
      
      // Toggle panel functionality
      $(function () {
        var leftOpen = true, rightOpen = true;
        function toggleLeft()  { $('#leftPanel').toggleClass('hidden'); leftOpen  = !leftOpen; }
        function toggleRight() { $('#rightPanel').toggleClass('hidden'); rightOpen = !rightOpen; }
        $('#toggleLeftPanel').on('click', toggleLeft);
        $('#toggleRightPanel').on('click', toggleRight);
      });
      function initAIFabInteractions(){
        var box = document.getElementById('aiFabBox');
        if (!box || box.__fabInit) return; box.__fabInit = true;

        // Unified behavior: RIGHT-CLICK opens (or closes) the AI assistant panel + settings on ALL platforms.
        // Left click is reserved purely for dragging (handled below). This makes UX consistent.
        $(document).off('click.aiFab');      // remove any prior left-click toggles
        $(document).off('dblclick.aiFabWin');
        $(document).off('contextmenu.aiFabUnified').on('contextmenu.aiFabUnified', '#aiFabToggle, #aiFabBox', function(e){
          e.preventDefault(); e.stopPropagation();
          var p = document.getElementById('aiPanel');
          if (!p) return;
          if (p.style.display === 'block') { p.style.display = 'none'; return; }
          p.style.display = 'block';
          alignPanelToFab();
          // Always show settings when opened via right-click
          setTimeout(function(){
            try {
              var s = document.getElementById('aiSettings');
              if (s) s.style.display = 'block';
              if (typeof loadAiSettings === 'function') loadAiSettings();
            } catch(_){ }
          }, 20);
        });

  var startX=0, startY=0, origLeft=0, origTop=0, dragging=false, moved=false;
  // On some Windows touchpads/mice there is tiny jitter during click; use a larger threshold
  // to avoid treating normal clicks as drags which suppresses the toggle click.
  var dragThreshold = 8; // pixels (was 2)
        function alignPanelToFab(){
          var p = document.getElementById('aiPanel');
          if (!p || p.style.display !== 'block') return;
          var rect = box.getBoundingClientRect();
          var px = rect.left;
          var py = rect.top - p.offsetHeight - 12; // show above
          if (py < 8) py = rect.bottom + 12; // otherwise below
          var vw = window.innerWidth, pw = p.offsetWidth;
          if (px + pw + 8 > vw) px = Math.max(8, vw - pw - 8);
          p.style.left = px + 'px'; p.style.top = py + 'px';
          p.style.right = 'auto'; p.style.bottom = 'auto';
        }
        function pointerDown(e){
          var isOnButton = false;
          try {
            var t = e.target;
            isOnButton = !!(t && (t.id === 'aiFabToggle' || (t.closest && t.closest('#aiFabToggle'))));
          } catch(_) { isOnButton = false; }

          // Only left button initiates drag; allow right click to open settings on Windows
          if (e.button !== 0) { return; }

          // For clicks starting on the button, don't immediately prevent default so the click can fire
          // if there is no drag. For drags starting elsewhere, prevent default to avoid text selection.
          if (!isOnButton) {
            e.preventDefault();
            e.stopPropagation();
          }
          var rect = box.getBoundingClientRect();
          startX = e.clientX; startY = e.clientY;
          box.style.left = rect.left + 'px'; box.style.top = rect.top + 'px';
          box.style.right = 'auto'; box.style.bottom = 'auto';
          origLeft = rect.left; origTop = rect.top; dragging = true; moved = false;
          box.classList.add('dragging');
          try { box.setPointerCapture && box.setPointerCapture(e.pointerId); } catch(_){ }
          document.addEventListener('pointermove', pointerMove);
          document.addEventListener('pointerup', pointerUp, { once: true });
        }
        function pointerMove(e){
          if (!dragging) return;
          e.preventDefault();
          var dx = e.clientX - startX; var dy = e.clientY - startY;
          if (Math.abs(dx) + Math.abs(dy) > dragThreshold) moved = true;
          var nx = origLeft + dx; var ny = origTop + dy;
          var vw = window.innerWidth, vh = window.innerHeight, bw = box.offsetWidth, bh = box.offsetHeight;
          nx = Math.max(8, Math.min(nx, vw - bw - 8));
          ny = Math.max(8, Math.min(ny, vh - bh - 8));
          box.style.left = nx + 'px'; box.style.top = ny + 'px';
          alignPanelToFab();
        }
        function pointerUp(e){
          if (!dragging) return;
          e.preventDefault();
          e.stopPropagation();
          dragging = false; box.classList.remove('dragging');
          try { box.releasePointerCapture && box.releasePointerCapture(e.pointerId); } catch(_){ }
          document.removeEventListener('pointermove', pointerMove);
          // Only suppress the immediate click if a genuine drag happened (beyond threshold)
          if (moved) {
            window.__aiFabSuppressClick = true;
            // Keep suppression window short to avoid swallowing real clicks on slower browsers
            setTimeout(function(){ window.__aiFabSuppressClick = false; }, 80);
          }
        }
        box.addEventListener('pointerdown', pointerDown, { passive: false });
        window.addEventListener('resize', alignPanelToFab);

        // Settings gear toggle
        $(document).off('click.aiGear').on('click.aiGear', '#aiSettingsGear', function(){
          var s = document.getElementById('aiSettings'); if (!s) return; s.style.display = (s.style.display==='block'?'none':'block');
          if (s.style.display === 'block') setTimeout(loadAiSettings, 10);
        });
        // Save settings to localStorage
        $(document).off('click.aiSave').on('click.aiSave', '#aiSaveSettings', function(){
          try {
            var b = $('#ai_api_base').val()||'';
            var k = $('#ai_api_key').val()||'';
            localStorage.setItem('ai_base', b);
            localStorage.setItem('ai_key', k);
            localStorage.setItem('ai_model', $('#ai_model').val()||'');
            localStorage.setItem('ai_temperature', $('#ai_temperature').val()||'');
            localStorage.setItem('ai_max_tokens', $('#ai_max_tokens').val()||'');
            localStorage.setItem('ai_system_prompt', $('#ai_system_prompt').val()||'');
            // Propagate values to Shiny so server-side inputs reflect saved settings
            try { Shiny.setInputValue('ai_api_base', b, {priority: 'event'}); } catch(e) {}
            try { Shiny.setInputValue('ai_api_key', k, {priority: 'event'}); } catch(e) {}
            try { Shiny.setInputValue('ai_model', $('#ai_model').val()||'', {priority: 'event'}); } catch(e) {}
            try { Shiny.setInputValue('ai_temperature', $('#ai_temperature').val()||'', {priority: 'event'}); } catch(e) {}
            try { Shiny.setInputValue('ai_max_tokens', $('#ai_max_tokens').val()||'', {priority: 'event'}); } catch(e) {}
            try { Shiny.setInputValue('ai_system_prompt', $('#ai_system_prompt').val()||'', {priority: 'event'}); } catch(e) {}
            alert('AI settings saved');
          } catch(e) { console.warn('save ai settings failed', e); }
        });
        // Reset to defaults
        $(document).off('click.aiReset').on('click.aiReset', '#aiResetSettings', function(){
          $('#ai_api_base').val('');
          $('#ai_api_key').val('');
          // Ensure Shiny server sees the reset values
          try { Shiny.setInputValue('ai_api_base', '', {priority: 'event'}); } catch(e) {}
          try { Shiny.setInputValue('ai_api_key', '', {priority: 'event'}); } catch(e) {}
          $('#ai_model').val('gpt-4o-mini');
          $('#ai_temperature').val(0.2);
          $('#ai_max_tokens').val(2048);
          $('#ai_system_prompt').val('');
        });
        // Close panel
        $(document).off('click.aiClose').on('click.aiClose', '#aiCloseSettings', function(){
          var s = document.getElementById('aiSettings'); if (!s) return; s.style.display = 'none';
        });
        function loadAiSettings(){
          try {
            var b = localStorage.getItem('ai_base')||'';
            var k = localStorage.getItem('ai_key')||'';
            var m = localStorage.getItem('ai_model')||'gpt-4o-mini';
            var t = localStorage.getItem('ai_temperature');
            var x = localStorage.getItem('ai_max_tokens');
            var s = localStorage.getItem('ai_system_prompt')||'';
            if ($('#ai_api_base').length) { $('#ai_api_base').val(b); try { Shiny.setInputValue('ai_api_base', b, {priority: 'event'}); } catch(e) {} }
            if ($('#ai_api_key').length) { $('#ai_api_key').val(k); try { Shiny.setInputValue('ai_api_key', k, {priority: 'event'}); } catch(e) {} }
            if ($('#ai_model').length) $('#ai_model').val(m);
            if ($('#ai_temperature').length) $('#ai_temperature').val(t || 0.2);
            if ($('#ai_max_tokens').length) $('#ai_max_tokens').val(x || 2048);
            if ($('#ai_system_prompt').length) $('#ai_system_prompt').val(s);
          } catch(e) {}
        }
      }

      // initialize after DOM exists and whenever overlay is (re)rendered
      setTimeout(initAIFabInteractions, 300);
      $(document).on('shiny:value', function(ev){ if (ev.name === 'ai_overlay_ui') setTimeout(initAIFabInteractions, 50); });

      // Forward module test button clicks to Shiny as a robust fallback in case native
      // click events are swallowed by overlay dragging/pointer capture. This listens for
      // clicks on the namespaced button id '#ai_blup-test' and explicitly sets the
      // corresponding Shiny input value (timestamp) so the server observeEvent fires.
      $(document).off('click.aiModuleTest').on('click.aiModuleTest', '#ai_blup-test', function(e){
        try {
          Shiny.setInputValue('ai_blup-test', Math.floor(Date.now()/1000), {priority: 'event'});
        } catch (err) { /* ignore */ }
      });
      // Forward Ask button clicks and sync the user prompt value in case native clicks are blocked
      $(document).off('click.aiModuleAsk').on('click.aiModuleAsk', '#ai_blup-ask', function(e){
        try {
          var p = $('#ai_blup-user_prompt').val() || '';
          try { Shiny.setInputValue('ai_blup-user_prompt', p, {priority: 'event'}); } catch(_) {}
          Shiny.setInputValue('ai_blup-ask', Math.floor(Date.now()/1000), {priority: 'event'});
        } catch (err) { /* ignore */ }
      });
      // Forward Apply button clicks so server apply callback triggers reliably
      $(document).off('click.aiModuleApply').on('click.aiModuleApply', '#ai_blup-apply', function(e){
        try {
          Shiny.setInputValue('ai_blup-apply', Math.floor(Date.now()/1000), {priority: 'event'});
        } catch (err) { /* ignore */ }
      });
    "))
  ),
  
  # Title Bar
  div(class = "title-bar", 
      uiOutput("app_title_ui"),
      uiOutput("app_subtitle_ui")
  ),

  # Toggle Buttons
  div(id = "toggleLeftBtn", class = "toggle-btn-left",
      actionButton("toggleLeftPanel", HTML("&#10094;"), class = "btn btn-sm", title = "Toggle left panel")),
  div(id = "toggleRightBtn", class = "toggle-btn-right",
      actionButton("toggleRightPanel", HTML("&#10095;"), class = "btn btn-sm", title = "Toggle right panel")),

  # Floating AI assistant (overlay)
  uiOutput("ai_overlay_ui"),

  # Three-Panel Container
  div(class = "three-panel-container",
    
    # ====== Left Panel: Data Upload & Model Builder ======
    div(id = "leftPanel", class = "left-panel",
        
        # File Upload Section
        div(class = "panel-section",
          uiOutput("upload_title_ui"),
          fileInput("pheno_file", uiOutput("pheno_label_ui", inline = TRUE), 
                   accept = c(".csv", ".txt", ".dat", ".xlsx", ".xls"),
                   buttonLabel = "Browse...",
                   placeholder = "No file selected"),
          fileInput("ped_file", uiOutput("ped_label_ui", inline = TRUE),
                   accept = c(".ped", ".txt"),
                   buttonLabel = "Browse...",
                   placeholder = "Optional"),
          div(
            selectizeInput(
              inputId = "geno_format",
              label = uiOutput("geno_format_label_ui", inline = TRUE),
              choices = c(
                "PLINK (.ped/.map)" = "plink",
                "BLUPF90 (.txt)" = "blupf90"
              ),
              selected = "plink",
              multiple = FALSE
            )
          ),
          fileInput("geno_file", uiOutput("geno_label_ui", inline = TRUE),
                   accept = NULL,
                   multiple = TRUE,
                   buttonLabel = "Browse...",
                   placeholder = "Optional"),
          uiOutput("geno_accept_ui"),
          uiOutput("clear_all_btn_ui")
        ),
        
        # Model Builder Section
        div(class = "panel-section",
          uiOutput("model_builder_title_ui"),
          selectizeInput(
            inputId = "traits_select",
            label = uiOutput("traits_label_ui", inline = TRUE),
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select trait columns")
          ),
          selectizeInput(
            inputId = "fixed_select",
            label = uiOutput("fixed_label_ui", inline = TRUE),
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select fixed-effect columns")
          ),
          selectizeInput(
            inputId = "animal_select",
            label = uiOutput("animal_label_ui", inline = TRUE),
            choices = NULL,
            multiple = FALSE,
            options = list(placeholder = "Select animal ID column")
          ),
          selectizeInput(
            inputId = "random_select",
            label = uiOutput("random_label_ui", inline = TRUE),
            choices = NULL,
            multiple = TRUE,
            options = list(placeholder = "Select random-effect columns")
          )
        ),
        
        # Optional Effects
        div(class = "panel-section",
          uiOutput("optional_title_ui"),
          checkboxInput("opt_pe", uiOutput("pe_label_ui", inline = TRUE), value = FALSE),
          checkboxInput("opt_mat", uiOutput("mat_label_ui", inline = TRUE), value = FALSE),
          checkboxInput("opt_mpe", uiOutput("mpe_label_ui", inline = TRUE), value = FALSE)
        )
      ),
    
    # ====== Center Panel: Parameter File Preview ======
    div(class = "center-panel",
        uiOutput("param_title_ui"),
        
        # Action Buttons
        div(
          style = "display: flex; gap: 10px; margin-bottom: 15px;",
          uiOutput("reset_btn_ui"),
          uiOutput("download_btn_ui")
        ),
        
        # Parameter Editor Textarea
        tags$textarea(
          id = "param_editor",
          placeholder = "Parameter file will appear here..."
        )
      ),
    
    # ====== Right Panel: BLUP Options ======
    div(id = "rightPanel", class = "right-panel",
        uiOutput("right_basic_options_ui"),
        uiOutput("right_analysis_method_ui"),
        uiOutput("right_solution_output_ui"),
        uiOutput("right_accuracy_ui"),
        uiOutput("right_genomic_ui"),
        uiOutput("right_hetres_ui"),
        uiOutput("right_renumf90_ui"),
        uiOutput("right_ai_ui")
      )
  )
)

# ====== Server Logic ======
server <- function(input, output, session) {
  
  # Language state (from shared resolver)
  lang <- reactiveVal("en")
  observe({
    resolved <- try({
      if (exists("resolve_suite_lang", mode = "function")) {
        get("resolve_suite_lang", mode = "function")(session, default = "en")
      } else {
        "en"
      }
    }, silent = TRUE)
    if (!inherits(resolved, "try-error")) {
      mapped <- try({
        if (exists("map_suite_lang_for_app", mode = "function")) {
          get("map_suite_lang_for_app", mode = "function")(resolved, app = "easyblup")
        } else {
          resolved
        }
      }, silent = TRUE)
      if (!inherits(mapped, "try-error")) lang(mapped)
    }
  })
  
  # ====== Right panel dynamic sections (language-aware) ======
  output$right_basic_options_ui <- renderUI({
    l <- lang()
    div(class = "panel-section",
      h4(get_label("basic_options", l), class = "section-title"),
      checkboxInput("opt_remove_all_missing", get_label("remove_all_missing", l), value = TRUE),
      checkboxInput("opt_missing_in_weights", get_label("missing_in_weights", l), value = FALSE),
      checkboxInput("opt_no_basic_statistics", get_label("no_basic_statistics", l), value = FALSE),
      textInput("opt_missing_value", get_label("missing_value_symbol", l), value = "-999", width = "200px")
    )
  })

  output$right_analysis_method_ui <- renderUI({
    l <- lang()
    div(class = "panel-section",
      h4(get_label("analysis_method_options", l), class = "section-title"),
      selectInput("opt_method", get_label("method_label", l),
                 choices = c(setNames("BLUP", get_label("method_blup", l)),
                             setNames("VCE",  get_label("method_vce",  l))),
                 selected = "VCE", width = "200px"),
      checkboxInput("opt_sol_se", get_label("sol_se", l), value = TRUE),
      textInput("opt_conv_crit_val", get_label("conv_crit", l), value = "1d-12", width = "200px"),
      numericInput("opt_em_reml_rounds", get_label("em_reml_rounds", l), value = 100, min = 1, step = 1, width = "200px"),
      checkboxInput("opt_em_reml_pure", get_label("em_reml_pure", l), value = FALSE),
      checkboxInput("opt_em_reml_ai_conv", get_label("em_reml_ai_conv", l), value = FALSE),
      checkboxInput("opt_use_yams", get_label("use_yams", l), value = TRUE),
      checkboxInput("opt_tuned_g2", get_label("tuned_g2", l), value = FALSE),
      numericInput("opt_maxrounds_val", get_label("maxrounds", l), value = 1000000, min = 1, step = 1000, width = "200px"),
      selectInput("opt_solv_method", get_label("solv_method", l), choices = c("PCG", "FSPAK", "SOR"), selected = "PCG", width = "200px"),
      numericInput("opt_r_factor", get_label("r_factor", l), value = 1.6, step = 0.1, width = "200px"),
      numericInput("opt_blksize", get_label("blksize_traits", l), value = 1, min = 1, step = 1, width = "200px"),
      checkboxInput("opt_residual_out", get_label("residual_output", l), value = FALSE),
      checkboxInput("opt_stdresidual_out", get_label("stdresidual_output", l), value = FALSE),
      checkboxInput("opt_prior_solutions", get_label("prior_solutions", l), value = FALSE),
      textInput("opt_set_eig", get_label("set_eig", l), value = "1d-12", width = "200px"),
      checkboxInput("opt_auto_se_covar", get_label("auto_se_covar", l), value = TRUE)
    )
  })

  output$right_solution_output_ui <- renderUI({
    l <- lang()
    div(class = "panel-section",
      h4(get_label("solution_output_options", l), class = "section-title"),
      checkboxInput("opt_origID", get_label("origID_store_solutions", l), value = TRUE)
    )
  })

  output$right_accuracy_ui <- renderUI({
    l <- lang()
    div(class = "panel-section",
      h4(get_label("accuracy_reliability", l), class = "section-title"),
      uiOutput("store_accuracy_ui"),
      uiOutput("store_accuracy_orig_ui"),
      selectInput("opt_acctype", get_label("acctype", l), choices = c("1.0", "0.5"), selected = "1.0", width = "120px"),
      checkboxInput("opt_correct_acc_inb_direct0", get_label("correct_accuracy_inb_direct0", l), value = FALSE)
    )
  })

  output$right_genomic_ui <- renderUI({
    l <- lang()
    div(class = "panel-section",
      h4(get_label("genomic_ssgblup", l), class = "section-title"),
      checkboxInput("opt_snp_p_value", get_label("snp_p_value", l), value = FALSE),
      checkboxInput("opt_omit_ainv", get_label("omit_ainv", l), value = FALSE),
      textInput("opt_TauOmega", get_label("tauomega", l), value = "1.0 0.0", width = "200px"),
      textInput("opt_AlphaBeta", get_label("alphabeta", l), value = "0.95 0.05", width = "200px")
    )
  })

  output$right_hetres_ui <- renderUI({
    l <- lang()
    div(class = "panel-section",
      h4(get_label("het_res_weights", l), class = "section-title"),
      selectInput("opt_hetres_pos", get_label("hetres_pos", l), choices = NULL, multiple = TRUE, width = "240px"),
      selectInput("opt_hetres_pol_preset", get_label("hetres_pol_preset_label", l),
                 choices = c(setNames("0.1", get_label("hetres_pol_constant", l)),
                             setNames("0.1 0.01", get_label("hetres_pol_linear", l)),
                             setNames("0.1 0.01 0.001", get_label("hetres_pol_quadratic", l))),
                 selected = "0.1 0.01", width = "260px")
    )
  })

  output$right_renumf90_ui <- renderUI({
    div(class = "panel-section",
      h4("RENUMF90: Pedigree & Inbreeding", class = "section-title"),
      checkboxInput("opt_ped_search_complete", "OPTION ped_search complete", value = FALSE),
      checkboxInput("opt_use_ped_depth", "Use PED_DEPTH", value = FALSE),
      conditionalPanel(
        condition = "input.opt_use_ped_depth == true",
        numericInput("opt_ped_depth", "PED_DEPTH", value = 0, min = 0, step = 1, width = "140px")
      ),
      selectInput("opt_inbreeding_method", "OPTION inbreeding_method",
                 choices = c(
                   "1: Meuwissen and Luo (1992)" = "1",
                   "2: Modified Meuwissen & Luo by Sargolzaei & Iwaisaki (2004)" = "2",
                   "3: Modified Colleau by Sargolzaei et al. (2005)" = "3",
                   "4: recursive tabular method" = "4",
                   "5: method of Tier (1990)" = "5",
                   "6: Hybrid parallel computing (OMP) version of Meuwissen and Luo (1992)" = "6",
                   "7: Recursive tabular with self-breeding generations (selfing, e.g., wheat)" = "7"
                 ), selected = "1", width = "200px")
    )
  })

  # ====== AI Assistant (easyblup) - floating overlay ======
  output$ai_overlay_ui <- renderUI({
    if (!exists("aiAssistantUI")) return(NULL)
    tagList(
      # include assistant CSS (served from easybreedeR/easyblup/www/ai_assistant.css)
      tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ai_assistant.css")),
  div(id = "aiFabBox", class = "ai-fab", actionButton("aiFabToggle", label = HTML('<img src="ai_icon.png" class="ai-fab-icon" alt="assistant"/>'), class = "btn btn-primary", title = "AI Assistant")),
      div(id = "aiPanel", class = "ai-panel",
          div(class = "ai-panel-header",
              span(class = "ai-panel-title", "🐰 AI Assistant"),
              span(class = "ai-gear", actionButton("aiSettingsGear", label = HTML("⚙️"), class = "btn btn-secondary btn-sm", title = "Settings"))
          ),
          div(class = "ai-panel-body",
              # Settings panel (hidden by default)
              div(id = "aiSettings", style = "display:none; margin-bottom:10px;",
                  fluidRow(
                    column(
                      12,
                      tags$input(id = "ai_provider", type = "hidden", value = "openai"),
            div(style = "margin-top:-6px;color:#6c757d;font-size:12px;",
              "Provider: OpenAI (other providers removed). Configure the OpenAI Base URL and API Key here.")
                    ),
                    column(
                      12,
                      selectizeInput(
                        "ai_model", "Model",
                        choices = c(
                          "gpt-4o-mini", "gpt-4o", "gpt-4.1-mini", "gpt-4.1",
                          "o3-mini", "o4-mini",
                          "deepseek-chat",
                          "gemini-2.0-flash", "gemini-2.5-flash", "gemini-2.5-flash-lite-preview-06-17"
                        ),
                        selected = "gpt-4o-mini",
                        options = list(create = TRUE, placeholder = "Enter model or deployment ID")
                      ),
            div(style = "margin-top:-6px;color:#6c757d;font-size:12px;",
              "You can enter a custom model or deployment ID (e.g., Claude, qwen) compatible with OpenAI.")
                    ),
                    column(
                      12,
                      textInput("ai_api_base", "API Base URL", value = "", width = "100%"),
            div(style = "margin-top:-6px;color:#6c757d;font-size:12px;",
              "Defaults: OpenAI: https://api.openai.com/v1; DeepSeek: https://api.deepseek.com/v1; Gemini endpoints may include /v1beta.")
                    ),
                    column(
                      12,
                      passwordInput("ai_api_key", "API Key", value = "", width = "100%"),
                      fileInput("ai_key_file", "Import API Key (.txt)", multiple = FALSE,
                                accept = c("text/plain", ".txt"), buttonLabel = "Browse", width = "100%"),
            div(style = "margin-top:-6px;color:#6c757d;font-size:12px;",
              "API Key is stored in browser localStorage and is not uploaded to the server.")
                    ),
                    column(
                      12,
                      textInput("ai_organization", "OpenAI Organization (optional)", value = "", width = "100%"),
            div(style = "margin-top:-6px;color:#6c757d;font-size:12px;",
              "Optional: enter your OpenAI Organization ID here if applicable.")
                    ),
                    column(6, numericInput("ai_temperature", "Temperature", value = 0.2, min = 0, max = 2, step = 0.1, width = "100%")),
                    column(6, numericInput("ai_max_tokens", "Max Tokens", value = 2048, min = 128, max = 32768, step = 128, width = "100%")),
                    column(12, textAreaInput("ai_system_prompt", "System Prompt", value = "", resize = "vertical", width = "100%", height = "90px")),
                    column(12,
                      div(style = "display:flex; gap:8px; flex-wrap:wrap;",
                          actionButton("aiSaveSettings", "Save", class = "btn-primary btn-sm"),
                          actionButton("aiResetSettings", "Reset", class = "btn-secondary btn-sm"),
                          actionButton("aiCloseSettings", "Close", class = "btn-light btn-sm")
                      )
                    )
          )
        ),
        # Chat wrapper (default visible area) — styled by ai_assistant.css to match provided mock
        div(id = "aiChatWrapper", class = "ai-chat-wrapper",
          # render the assistant module UI here so it's visible when settings hidden
          get("aiAssistantUI")("ai_blup")
        )
          )
      ),
      # JS handler to let server request returning to chat after settings saved
      tags$script(HTML(
        "Shiny.addCustomMessageHandler('ai_show_chat', function(msg){ try{ $('#aiSettings').hide(); $('#aiChatWrapper').show(); $('#aiPanel .ai-panel-body').show(); } catch(e){} });"
      ))
    )
  })

  # ====== Dynamic UI Elements (Language-aware) ======
  
  # Title bar
  output$app_title_ui <- renderUI({
    h1(get_label("easyblup_app_name", lang()))
  })
  output$app_subtitle_ui <- renderUI({
    p(get_label("easyblup_app_subtitle", lang()))
  })
  
  # Left Panel - Upload section
  output$upload_title_ui <- renderUI({
    h4(get_label("upload_data", lang()), class = "section-title")
  })
  
  output$pheno_label_ui <- renderUI({
    get_label("phenotype", lang())
  })
  
  output$ped_label_ui <- renderUI({
    get_label("pedigree", lang())
  })
  
  output$geno_label_ui <- renderUI({
    format <- input$geno_format %||% "plink"
    if (format == "blupf90") {
      get_label("snp_file", lang())
    } else {
      get_label("genotype", lang())
    }
  })
  
  output$geno_format_label_ui <- renderUI({
    # If optional dependency plinkR is available, show a small helper "?" icon
    # that opens a modal for PLINK <-> BLUPF90 format conversion.
    label_text <- get_label("genotype_format", lang())
    has_plinkr <- requireNamespace("plinkR", quietly = TRUE)
    
    if (!has_plinkr) {
      return(label_text)
    }
    
    help_title <- if (tolower(lang()) == "zh") {
      "Need help? Genotype format 转换 (PLINK → BLUPF90)"
    } else {
      "Need help? Genotype format translate (PLINK → BLUPF90)"
    }
    
    # Use actionLink so clicks are observable on the server
    tagList(
      span(
        style = "display:inline-flex;align-items:center;gap:6px;vertical-align:middle;",
        span(label_text, style = "line-height:1.5;"),
        actionLink(
          inputId = "geno_format_help",
          label = tags$span(
            "?",
            style = "display:inline-flex;align-items:center;justify-content:center;
                     width:18px;height:18px;border-radius:50%;border:1px solid #999;
                     line-height:1;font-size:11px;font-weight:bold;
                     cursor:pointer;color:#555;background-color:#f8f9fa;
                     flex-shrink:0;",
            title = help_title
          ),
          style = "padding:0;text-decoration:none;display:inline-flex;align-items:center;"
        )
      )
    )
  })
  
  output$geno_accept_ui <- renderUI({
    # Hidden helper to update file input accept attribute dynamically
    format <- input$geno_format %||% "plink"
    # Note: Shiny's fileInput doesn't support dynamic accept, so we validate on server side
    # This is a UI message to guide users
    if (format == "blupf90") {
      div(style = "font-size: 0.85rem; color: #666; margin-top: 5px;",
          "📝 ", "Accepted format: .txt files")
    } else {
      div(style = "font-size: 0.85rem; color: #666; margin-top: 5px;",
          "📝 ", "Accepted formats: .ped and .map files")
    }
  })
  
  output$clear_all_btn_ui <- renderUI({
    actionButton("clear_all", get_label("clear_all", lang()), 
                class = "btn btn-primary btn-sm", 
                style = "width: 100%;")
  })
  
  # Left Panel - Model Builder section
  output$model_builder_title_ui <- renderUI({
    h4(get_label("model_builder", lang()), class = "section-title")
  })
  
  output$traits_label_ui <- renderUI({
    get_label("traits", lang())
  })
  
  output$fixed_label_ui <- renderUI({
    get_label("fixed_effects", lang())
  })
  
  output$animal_label_ui <- renderUI({
    get_label("animal_id", lang())
  })
  
  output$random_label_ui <- renderUI({
    get_label("random_effects", lang())
  })
  
  # Left Panel - Optional Effects section
  output$optional_title_ui <- renderUI({
    h4(get_label("optional_effects", lang()), class = "section-title")
  })
  
  output$pe_label_ui <- renderUI({
    span("PE - ", if(tolower(lang()) == "zh") "永久环境效应" else "Permanent Environmental")
  })
  
  output$mat_label_ui <- renderUI({
    span("MAT - ", if(tolower(lang()) == "zh") "母体效应" else "Maternal Effect")
  })
  
  output$mpe_label_ui <- renderUI({
    span("MPE - ", if(tolower(lang()) == "zh") "母体永久环境" else "Maternal Permanent Env.")
  })
  
  # Center Panel - Parameter file section
  output$param_title_ui <- renderUI({
    h2(get_label("parameter_file", lang()), style = "text-align: center; margin-bottom: 20px;")
  })
  
  output$reset_btn_ui <- renderUI({
    actionButton("reset_param", get_label("reset_param", lang()), 
                class = "btn-primary", style = "flex: 1;")
  })
  
  output$download_btn_ui <- renderUI({
    actionButton("download_param", get_label("download_param", lang()), 
                class = "btn-primary", style = "flex: 1;")
  })
  
  # ====== End Dynamic UI ======
  
  # Set file size limit
  options(shiny.maxRequestSize = 10 * 1024^3)
  
  # Reactive values for selected variables
  values <- reactiveValues(
    traits = c(),
    fixed = c(),
    animal = c(),
    random = c(),
    default_param = "",
    current_param = "",
    ai_applied = FALSE
  )
  
  conversion_files <- reactiveValues(
    blup_txt = NULL,
    blup_map = NULL
  )
  
  cleanup_conversion_files <- function() {
    if (!is.null(conversion_files$blup_txt) && file.exists(conversion_files$blup_txt)) {
      unlink(conversion_files$blup_txt, force = TRUE)
    }
    if (!is.null(conversion_files$blup_map) && file.exists(conversion_files$blup_map)) {
      unlink(conversion_files$blup_map, force = TRUE)
    }
    conversion_files$blup_txt <- NULL
    conversion_files$blup_map <- NULL
  }
  
  # Calculate animal effect number reactively
  animal_effect_number <- reactive({
    n_fixed <- length(values$fixed)
    n_random <- length(values$random)
    animal_effect_num <- n_fixed + n_random + 1
    return(animal_effect_num)
  })
  
  # Dynamic UI: show animal effect number next to store_accuracy checkboxes
  output$store_accuracy_ui <- renderUI({
    an <- animal_effect_number()
    checkboxInput("opt_store_accuracy", paste0("store_accuracy (Animal effect: ", an, ")"), value = FALSE)
  })
  output$store_accuracy_orig_ui <- renderUI({
    an <- animal_effect_number()
    checkboxInput("opt_store_accuracy_orig", paste0("store_accuracy with original ID (Animal effect: ", an, ")"), value = TRUE)
  })
  
  # Robust file reader for phenotype file
  data <- reactive({
    req(input$pheno_file)
    
    path <- input$pheno_file$datapath
    ext  <- tolower(tools::file_ext(input$pheno_file$name))
    
    safe_read <- function(expr) {
      tryCatch(expr, error = function(e) {
        showNotification(paste0("File read failed: ", e$message), type = "error", duration = 8)
        NULL
      })
    }
    
    # Excel support if readxl is available
    if (ext %in% c("xlsx", "xls")) {
      if (requireNamespace("readxl", quietly = TRUE)) {
        df <- safe_read(readxl::read_excel(path))
        if (!is.null(df)) return(as.data.frame(df, check.names = FALSE))
      } else {
        showNotification("readxl package not installed. Please install readxl or upload CSV/TXT.", type = "warning", duration = 8)
        return(NULL)
      }
    }
    
    # CSV
    if (ext == "csv") {
      return(safe_read(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)))
    }
    
    # TXT/DAT or unknown: detect delimiter from first line
    first_line <- safe_read(readLines(path, n = 1L, warn = FALSE))
    if (is.null(first_line)) return(NULL)
    
    delim <- if (grepl(",", first_line, fixed = TRUE)) "," 
             else if (grepl("\t", first_line)) "\t" 
             else if (grepl(";", first_line, fixed = TRUE)) ";" 
             else ""
    
    if (delim == ",") {
      return(safe_read(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)))
    } else if (delim == "\t") {
      return(safe_read(utils::read.table(path, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)))
    } else if (delim == ";") {
      return(safe_read(utils::read.table(path, header = TRUE, sep = ";", stringsAsFactors = FALSE, check.names = FALSE)))
    } else {
      # Fallback: whitespace delimited
      return(safe_read(utils::read.table(path, header = TRUE, sep = "", stringsAsFactors = FALSE, check.names = FALSE)))
    }
  })
  
  # Update select choices when data is available
  observe({
    req(data())
    vars <- colnames(data())
    updateSelectizeInput(session, "traits_select", choices = vars, server = TRUE)
    updateSelectizeInput(session, "fixed_select", choices = vars, server = TRUE)
    updateSelectizeInput(session, "animal_select", choices = vars, server = TRUE)
    updateSelectizeInput(session, "random_select", choices = vars, server = TRUE)
    updateSelectInput(session, "opt_hetres_pos", choices = vars)
  })
  
  # Map select inputs to reactive values
  observeEvent(input$traits_select, {
    values$traits <- if (is.null(input$traits_select)) character(0) else input$traits_select
  }, ignoreNULL = FALSE)
  
  observeEvent(input$fixed_select, {
    values$fixed <- if (is.null(input$fixed_select)) character(0) else input$fixed_select
  }, ignoreNULL = FALSE)
  
  observeEvent(input$animal_select, {
    values$animal <- if (is.null(input$animal_select)) character(0) else input$animal_select
  }, ignoreNULL = FALSE)
  
  observeEvent(input$random_select, {
    values$random <- if (is.null(input$random_select)) character(0) else input$random_select
  }, ignoreNULL = FALSE)
  
  # Clear all
  observeEvent(input$clear_all, {
    updateSelectizeInput(session, "traits_select", selected = character(0))
    updateSelectizeInput(session, "fixed_select", selected = character(0))
    updateSelectizeInput(session, "animal_select", selected = character(0))
    updateSelectizeInput(session, "random_select", selected = character(0))
  }, ignoreInit = TRUE)
  
  # Helper function to generate covariance matrix
  generate_covariance_matrix <- function(n_traits) {
    if (n_traits <= 0) return("0.1")
    if (n_traits == 1) return("0.1")
    
    matrix_rows <- c()
    for (i in 1:n_traits) {
      row_values <- c()
      for (j in 1:n_traits) {
        row_values <- c(row_values, if (i == j) "1" else "0.01")
      }
      matrix_rows <- c(matrix_rows, paste(row_values, collapse = " "))
    }
    return(paste(matrix_rows, collapse = "\n"))
  }
  
  # (Removed old combined h2+rg generator to avoid duplicate rg lines in h2 section)
  
  # Precise strategy: parse renf90.par to detect indices
  parse_renf90_par <- function(txt) { # nolint object_usage_linter
    if (is.null(txt) || !nzchar(txt)) return(NULL)
    lines <- unlist(strsplit(txt, "\n"))
    animal_idx <- NULL
    maternal_idx <- NULL
    rand_indices <- integer(0)
    i <- 1
    while (i <= length(lines)) {
      ln <- trimws(lines[i])
      if (identical(toupper(ln), "RANDOM_GROUP")) {
        # collect following numeric lines until next keyword or empty
        i <- i + 1
        nums <- c()
        while (i <= length(lines)) {
          cur <- trimws(lines[i])
          if (cur == "" || grepl("^[A-Z_]+$", cur)) break
          # extract integers present in line
          found <- gregexpr("[0-9]+", cur)
          vals <- regmatches(cur, found)[[1]]
          if (length(vals) > 0) nums <- c(nums, as.integer(vals))
          i <- i + 1
        }
        # find next non-empty line for RANDOM_TYPE
        rt <- NULL
        j <- i
        while (j <= length(lines)) {
          cur <- trimws(lines[j])
          if (cur != "") { rt <- cur; break }
          j <- j + 1
        }
        if (!is.null(rt) && identical(toupper(rt), "RANDOM_TYPE")) {
          # next non-empty line is the type label
          k <- j + 1
          type_label <- NULL
          while (k <= length(lines)) {
            cur <- trimws(lines[k])
            if (cur != "") { type_label <- cur; break }
            k <- k + 1
          }
          if (!is.null(type_label)) {
            if (grepl("add_animal", type_label, ignore.case = TRUE)) {
              if (length(nums) >= 1) animal_idx <- nums[1]
              if (length(nums) >= 2) maternal_idx <- nums[2]
              rand_indices <- unique(c(rand_indices, nums))
            } else {
              rand_indices <- unique(c(rand_indices, nums))
            }
          }
          i <- k
          next
        } else {
          # No RANDOM_TYPE found; treat as diagonal randoms
          rand_indices <- unique(c(rand_indices, nums))
        }
      }
      i <- i + 1
    }
    list(animal = animal_idx, maternal = maternal_idx, random_indices = sort(unique(rand_indices)))
  }
  
  # Build denominator string for one trait t
  build_denominator <- function(random_idx_vec, t) {
    if (length(random_idx_vec) == 0) return(sprintf("R_%d_%d", t, t))
    parts <- paste0("G_", random_idx_vec, "_", random_idx_vec, "_", t, "_", t)
    paste0(paste(parts, collapse = "+"), "+R_", t, "_", t)
  }

  # Infer effect indices according to the BLUPF90+ (v2.60+) effect order
  # animal 后接 mat, pe 始终在 animal/mat 之后, mpe 永远最后（可以独立于 mat 存在）
  # 返回索引顺序 random → animal → mat → pe → mpe
  infer_effect_indices <- function(n_fixed, n_user_random, opt_mat, opt_pe, opt_mpe) {
    # 基础编号：animal 紧随所有随机效应之后
    base_animal <- n_fixed + n_user_random + 1
    idx <- list(a = base_animal, m = NULL, p = NULL, mp = NULL)
    
    # mat 紧跟 animal
    if (opt_mat) idx$m <- idx$a + 1
    
    # pe 始终在 animal 或 mat 之后
    if (opt_pe) {
      last_idx <- if (!is.null(idx$m)) idx$m else idx$a
      idx$p <- last_idx + 1
    }
    
    # mpe 永远在最后，可以在没有 mat 的情况下存在
    if (opt_mpe) {
      # MPE can exist without MAT - it follows after pe or animal/mat
      last_idx <- if (!is.null(idx$p)) idx$p else if (!is.null(idx$m)) idx$m else idx$a
      idx$mp <- last_idx + 1
    }
    
    # 汇总所有随机效应编号
    user_rand_idx <- if (n_user_random > 0) (seq_len(n_user_random) + n_fixed) else integer(0)
    idx$random_indices <- sort(c(user_rand_idx, unlist(idx)))
    
    message(sprintf(
      "Effect index mapping → fixed=%d, random=%d, animal=%d, mat=%s, pe=%s, mpe=%s",
      n_fixed, n_user_random, idx$a,
      ifelse(is.null(idx$m), "NA", idx$m),
      ifelse(is.null(idx$p), "NA", idx$p),
      ifelse(is.null(idx$mp), "NA", idx$mp)
    ))
    
    return(idx)
  }
  
  emit_h2_no_mat <- function(a_idx, rand_idx, t) {
    sprintf("OPTION se_covar_function H2_%d G_%d_%d_%d_%d/(%s)", t, a_idx, a_idx, t, t, build_denominator(rand_idx, t))
  }
  emit_h2_with_mat <- function(a_idx, m_idx, rand_idx, t) {
    # 分子：G_aa + 1.5*G_am + 0.5*G_mm
    num_t <- sprintf("(G_%d_%d_%d_%d+1.5*G_%d_%d_%d_%d+0.5*G_%d_%d_%d_%d)",
                     a_idx, a_idx, t, t,
                     a_idx, m_idx, t, t,
                     m_idx, m_idx, t, t)
    # 分母：∑随机效应 + 2*G_am + R
    rand_terms <- paste0("G_", rand_idx, "_", rand_idx, "_", t, "_", t)
    denom <- paste(c(rand_terms,
                     sprintf("2*G_%d_%d_%d_%d", a_idx, m_idx, t, t),
                     sprintf("R_%d_%d", t, t)), collapse = "+")
    paste(
      sprintf("OPTION se_covar_function H2t_%d %s/(%s)", t, num_t, denom),
      sprintf("OPTION se_covar_function H2d_%d G_%d_%d_%d_%d/(%s)", t, a_idx, a_idx, t, t, denom),
      sep = "\n"
    )
  }
  
  # Build genetic correlation block (additive and optional maternal)
  generate_rg_block <- function(n_traits, a_idx, m_idx = NULL, include_maternal = FALSE) {
    if (n_traits <= 1 || is.null(a_idx)) return("")
    lines <- c()
    for (i in 1:(n_traits-1)) {
      for (j in (i+1):n_traits) {
        lines <- c(lines, sprintf(
          "OPTION se_covar_function rg%d%d G_%d_%d_%d_%d/(G_%d_%d_%d_%d*G_%d_%d_%d_%d)**0.5",
          i, j,
          a_idx, a_idx, i, j,
          a_idx, a_idx, i, i,
          a_idx, a_idx, j, j
        ))
      }
    }
    if (include_maternal && !is.null(m_idx)) {
      for (i in 1:(n_traits-1)) {
        for (j in (i+1):n_traits) {
          lines <- c(lines, sprintf(
            "OPTION se_covar_function rgm%d%d G_%d_%d_%d_%d/(G_%d_%d_%d_%d*G_%d_%d_%d_%d)**0.5",
            i, j,
            m_idx, m_idx, i, j,
            m_idx, m_idx, i, i,
            m_idx, m_idx, j, j
          ))
          lines <- c(lines, sprintf(
            "OPTION se_covar_function rgdm%d%d G_%d_%d_%d_%d/(G_%d_%d_%d_%d*G_%d_%d_%d_%d)**0.5",
            i, j,
            a_idx, m_idx, i, j,
            a_idx, a_idx, i, i,
            m_idx, m_idx, j, j
          ))
        }
      }
    }
    paste(lines, collapse = "\n")
  }

  # Build phenotypic correlation block (rp) across traits
  generate_rp_block <- function(n_traits, a_idx, m_idx = NULL, p_idx = NULL,
                                include_maternal = FALSE, include_pe = FALSE) {
    if (n_traits <= 1) return("")
    lines <- c()
    for (i in 1:(n_traits - 1)) {
      for (j in (i + 1):n_traits) {
        # 分子：包含 A-A, 可选 M-M/PE-PE，若含M则加上 A-M 与 M-A 跨项，再加 R_ij
        num_terms <- c(sprintf("G_%d_%d_%d_%d", a_idx, a_idx, i, j))
        if (include_maternal && !is.null(m_idx))
          num_terms <- c(num_terms, sprintf("G_%d_%d_%d_%d", m_idx, m_idx, i, j))
        if (include_pe && !is.null(p_idx))
          num_terms <- c(num_terms, sprintf("G_%d_%d_%d_%d", p_idx, p_idx, i, j))
        if (include_maternal && !is.null(m_idx))
          num_terms <- c(num_terms,
                         sprintf("G_%d_%d_%d_%d", a_idx, m_idx, i, j),
                         sprintf("G_%d_%d_%d_%d", m_idx, a_idx, i, j))
        num_terms <- c(num_terms, sprintf("R_%d_%d", i, j))
        
        # 分母：每个性状的表型方差，含可选 M-M/PE-PE，若含M则加 2*G_am
        denom1_terms <- c(sprintf("G_%d_%d_%d_%d", a_idx, a_idx, i, i))
        if (include_maternal && !is.null(m_idx))
          denom1_terms <- c(denom1_terms,
                            sprintf("G_%d_%d_%d_%d", m_idx, m_idx, i, i),
                            sprintf("2*G_%d_%d_%d_%d", a_idx, m_idx, i, i))
        if (include_pe && !is.null(p_idx))
          denom1_terms <- c(denom1_terms, sprintf("G_%d_%d_%d_%d", p_idx, p_idx, i, i))
        denom1_terms <- c(denom1_terms, sprintf("R_%d_%d", i, i))
        
        denom2_terms <- c(sprintf("G_%d_%d_%d_%d", a_idx, a_idx, j, j))
        if (include_maternal && !is.null(m_idx))
          denom2_terms <- c(denom2_terms,
                            sprintf("G_%d_%d_%d_%d", m_idx, m_idx, j, j),
                            sprintf("2*G_%d_%d_%d_%d", a_idx, m_idx, j, j))
        if (include_pe && !is.null(p_idx))
          denom2_terms <- c(denom2_terms, sprintf("G_%d_%d_%d_%d", p_idx, p_idx, j, j))
        denom2_terms <- c(denom2_terms, sprintf("R_%d_%d", j, j))
        
        num <- paste(num_terms, collapse = "+")
        denom1 <- paste(denom1_terms, collapse = "+")
        denom2 <- paste(denom2_terms, collapse = "+")
        denom <- sprintf("((%s)*(%s))**0.5", denom1, denom2)
        
        lines <- c(lines, sprintf("OPTION se_covar_function rp%d%d (%s)/%s", i, j, num, denom))
      }
    }
    paste(lines, collapse = "\n")
  }
  
  # Helpers to map selected variables to column indices
  get_col_num_int <- function(vars) {
    if (length(vars) == 0 || is.null(data())) return(integer(0))
    which(colnames(data()) %in% vars)
  }
  
  get_col_num <- function(vars) {
    cols <- get_col_num_int(vars)
    if (length(cols) == 0) return("")
    paste(cols, collapse = " ")
  }
  
  format_effect_cols <- function(vars, n_traits) {
    cols <- get_col_num_int(vars)
    if (length(cols) == 0 || n_traits == 0) return("")
    if (length(cols) == 1) {
      cols <- rep(cols, n_traits)
    } else if (length(cols) != n_traits) {
      cols <- rep(cols, length.out = n_traits)
    }
    paste(cols, collapse = " ")
  }
  
  # Generate and update parameter file in textarea
  observe({
    if (is.null(data())) {
      param_text <- "Please upload a phenotype file first"
    } else {
      # Check pedigree/genotype files
      has_ped <- !is.null(input$ped_file) && 
                 is.data.frame(input$ped_file) && 
                 nrow(input$ped_file) > 0 &&
                 !is.null(input$ped_file$datapath) &&
                 nzchar(input$ped_file$datapath[1])
      has_geno <- !is.null(input$geno_file) && 
                  is.data.frame(input$geno_file) && 
                  nrow(input$geno_file) > 0 &&
                  !is.null(input$geno_file$datapath) &&
                  nzchar(input$geno_file$datapath[1])
    
    # Build parameter file
    param_text <- paste0(
      "# PARAMETER FILE for renumf90\n#\nDATAFILE\n",
      basename(input$pheno_file$name), "\n",
      "SKIP_HEADER\n1\nTRAITS # Specify trait columns\n",
      if (length(values$traits) > 0) get_col_num(values$traits) else "# Add trait column numbers here",
      "\nFIELDS_PASSED TO OUTPUT\n\nWEIGHT(S)\n\nRESIDUAL_VARIANCE\n",
      generate_covariance_matrix(length(values$traits)), "\n"
    )
    
    # Fixed effects
    if (length(values$fixed) > 0) {
      for (eff in values$fixed) {
        col_data <- data()[[eff]]
        effect_type <- "cross"
        if (!is.null(col_data) && length(col_data) > 0) {
          n_unique <- length(unique(col_data))
          n_total <- length(col_data)
          unique_ratio <- if (n_total > 0) n_unique / n_total else 0
          if (!is.character(col_data) && ((!is.na(n_unique) && n_unique > 20) || (!is.na(unique_ratio) && unique_ratio > 0.05))) {
            effect_type <- "cov"
          }
        }
        
        n_traits <- length(values$traits)
        effect_cols <- format_effect_cols(eff, n_traits)
        if (nzchar(effect_cols)) {
          param_text <- paste0(param_text, "EFFECT\n", effect_cols, " ", effect_type)
          if (effect_type == "cross") {
            param_text <- paste0(param_text, " alpha")
          }
          param_text <- paste0(param_text, " # ", eff, " fixed effect\n")
        }
      }
    }
    
    # Random effects
    if (length(values$random) > 0) {
      for (eff in values$random) {
        n_traits <- length(values$traits)
        effect_cols <- format_effect_cols(eff, n_traits)
        if (nzchar(effect_cols)) {
          param_text <- paste0(param_text,
                               "EFFECT\n",
                               effect_cols, " cross alpha # ", eff, " random effect\n",
                               "RANDOM\n",
                               "diagonal # Random effects section\n")
        }
      }
    }
    
    # Animal effect
    if (length(values$animal) > 0) {
      n_traits <- length(values$traits)
      effect_cols <- format_effect_cols(values$animal, n_traits)
      if (nzchar(effect_cols)) {
        param_text <- paste0(param_text, "EFFECT\n", effect_cols, " cross alpha # Animal ID effect\n")
      }
      param_text <- paste0(param_text, "RANDOM\nanimal # Animal random effect\n")
      
      # Add OPTIONAL effects if selected
      optional_effects <- c()
      if (input$opt_pe) optional_effects <- c(optional_effects, "pe")
      if (input$opt_mat) optional_effects <- c(optional_effects, "mat")
      if (input$opt_mpe) optional_effects <- c(optional_effects, "mpe")
      
      if (length(optional_effects) > 0) {
        param_text <- paste0(param_text, "OPTIONAL\n", paste(optional_effects, collapse = " "), "\n")
      }
      
      # Add pedigree file information (and immediately follow with PLINK/SNP block when present)
      pedigree_filename <- if (has_ped) basename(input$ped_file$name) else "pedigree.txt"
      param_text <- paste0(param_text, "FILE\n", pedigree_filename, "\nFILE_POS\n1 2 3 # Progeny Sire Dam\n")
      if (has_geno) {
        geno_format <- input$geno_format %||% "plink"
        
        if (geno_format == "blupf90") {
          # BLUPF90 format: use SNP_FILE
          snp_filename <- if (!is.null(input$geno_file$name) && length(input$geno_file$name) > 0) {
            basename(input$geno_file$name[1])
          } else {
            "snp_marker.txt"
          }
          param_text <- paste0(param_text, "SNP_FILE\n", snp_filename, " ## SNP marker file\n")
        } else {
          # PLINK format: use PLINK_FILE
          plink_prefix <- derive_plink_prefix(input$geno_file$name)
          if (!is.null(plink_prefix) && nzchar(plink_prefix)) {
            param_text <- paste0(param_text, "PLINK_FILE\n", plink_prefix, " # Genotype file name\n")
          }
        }
      }
        
        # RENUMF90 pedigree/inbreeding controls (omit INBREEDING/UPG_TYPE per workflow)
        # Include PED_DEPTH only when enabled and not using complete search
        if (!isTRUE(input$opt_ped_search_complete)) {
          if (isTRUE(input$opt_use_ped_depth) && !is.null(input$opt_ped_depth) && !is.na(input$opt_ped_depth)) {
            param_text <- paste0(param_text, "PED_DEPTH\n", input$opt_ped_depth, "\n")
          }
        }
        # INBREEDING and UPG_TYPE are handled by renumf90 automatically after run
    }
    
    # Options
    # Note: Covariance matrices are generated here regardless of animal selection
    # (CO)VARIANCES: output n x n matrix (like RESIDUAL_VARIANCE)
    # When mat is included, matrix dimension should be 2n x 2n (animal + mat for each trait)
    # This section always executes (outside the animal check block)
    n_traits <- length(values$traits)
    has_mat <- isTRUE(input$opt_mat)
    # Calculate number of effects: n_traits if no mat, 2*n_traits if has mat
    n_effects <- n_traits * (1 + as.integer(has_mat))
    
    if (n_traits > 0) {
      cov_lines <- c()
      for (i in 1:n_effects) {
        row_vals <- rep("1", n_effects)
        if (n_effects > 1) {
          for (j in 1:n_effects) {
            if (i != j) {
              # Set off-diagonal values for animal-mat, mat-animal, and mat-mat covariances
              row_vals[j] <- "0.01"
            }
          }
        }
        cov_lines <- c(cov_lines, paste(row_vals, collapse = " "))
      }
      param_text <- paste0(param_text, "(CO)VARIANCES\n", paste(cov_lines, collapse = "\n"), "\n")
    } else {
      param_text <- paste0(param_text, "(CO)VARIANCES\n1\n")
    }

    # PE (Permanent Environmental) covariance matrix - works for single or multiple traits
    if (input$opt_pe) {
      if (n_traits > 0) {
        if (n_traits == 1) {
          # Single trait: output single value (not matrix)
          param_text <- paste0(param_text, "(CO)VARIANCES_PE\n0.001\n")
        } else {
          # Multiple traits: generate matrix
          pe_lines <- c()
          for (i in 1:n_traits) {
            row_vals <- rep("0.001", n_traits)
            row_vals[i] <- "1"
            pe_lines <- c(pe_lines, paste(row_vals, collapse = " "))
          }
          param_text <- paste0(param_text, "(CO)VARIANCES_PE\n", paste(pe_lines, collapse = "\n"), "\n")
        }
      } else {
        # Fallback when no traits selected
        param_text <- paste0(param_text, "(CO)VARIANCES_PE\n0.001\n")
      }
    }
    # MPE (Maternal Permanent Environmental) covariance matrix - works for single or multiple traits
    if (input$opt_mpe) {
      if (n_traits > 0) {
        if (n_traits == 1) {
          # Single trait: output single value (not matrix)
          param_text <- paste0(param_text, "(CO)VARIANCES_MPE\n0.003\n")
        } else {
          # Multiple traits: generate matrix
          mpe_lines <- c()
          for (i in 1:n_traits) {
            row_vals <- rep("0.001", n_traits)
            row_vals[i] <- "1"
            mpe_lines <- c(mpe_lines, paste(row_vals, collapse = " "))
          }
          param_text <- paste0(param_text, "(CO)VARIANCES_MPE\n", paste(mpe_lines, collapse = "\n"), "\n")
        }
      } else {
        # Fallback when no traits selected
        param_text <- paste0(param_text, "(CO)VARIANCES_MPE\n0.003\n")
      }
    }
    
    param_text <- paste0(param_text, "\n")
    
    # Add user-selected OPTION parameters
    options <- c()
    if (input$opt_remove_all_missing) options <- c(options, "OPTION remove_all_missing")
    if (input$opt_missing_in_weights) options <- c(options, "OPTION missing_in_weights")
    if (input$opt_no_basic_statistics) options <- c(options, "OPTION no_basic_statistics")
    # Missing value symbol
    mv <- input$opt_missing_value
    if (is.null(mv) || !nzchar(mv)) mv <- "-999"
    options <- c(options, paste("OPTION missing", mv))
    
    # Method: BLUP or VCE
    if (!is.null(input$opt_method) && input$opt_method == "VCE") {
      options <- c(options, "OPTION method VCE")
    } else {
      options <- c(options, "OPTION method BLUP")
    }
    if (input$opt_sol_se) options <- c(options, "OPTION sol se")
    if (!is.null(input$opt_conv_crit_val) && nzchar(input$opt_conv_crit_val)) options <- c(options, paste("OPTION conv_crit", input$opt_conv_crit_val))
    if (!is.null(input$opt_em_reml_rounds) && !is.na(input$opt_em_reml_rounds)) options <- c(options, paste("OPTION EM-REML", input$opt_em_reml_rounds))
    if (isTRUE(input$opt_em_reml_pure)) options <- c(options, "OPTION EM-REML pure")
    if (isTRUE(input$opt_em_reml_ai_conv)) options <- c(options, "OPTION EM-REML AI conv")
    # Use YAMS and Tuned G2 options (user can enable even without genotype file)
    if (isTRUE(input$opt_use_yams)) options <- c(options, "OPTION use_yams")
    if (isTRUE(input$opt_tuned_g2)) options <- c(options, "OPTION tunedG2")
    if (!is.null(input$opt_maxrounds_val) && !is.na(input$opt_maxrounds_val)) options <- c(options, paste("OPTION maxrounds", input$opt_maxrounds_val))
    # Removed from default generation: OPTION solv_method
    # if (!is.null(input$opt_solv_method) && nzchar(input$opt_solv_method)) options <- c(options, paste("OPTION solv_method", tolower(input$opt_solv_method)))
    # Removed from default generation: OPTION r_factor
    # if (!is.null(input$opt_r_factor) && !is.na(input$opt_r_factor)) options <- c(options, paste("OPTION r_factor", input$opt_r_factor))
    # Removed from default generation: OPTION blksize
    # if (!is.null(input$opt_blksize) && !is.na(input$opt_blksize)) options <- c(options, paste("OPTION blksize", input$opt_blksize))
    if (isTRUE(input$opt_residual_out)) options <- c(options, "OPTION residual")
    if (isTRUE(input$opt_stdresidual_out)) options <- c(options, "OPTION stdresidual")
    if (isTRUE(input$opt_prior_solutions)) options <- c(options, "OPTION prior_solutions")
    # Removed from default generation: OPTION set_eig
    # if (!is.null(input$opt_set_eig) && nzchar(input$opt_set_eig)) options <- c(options, paste("OPTION set_eig", input$opt_set_eig))
    if (input$opt_origID) options <- c(options, "OPTION origID")
    
    if (input$opt_store_accuracy || input$opt_store_accuracy_orig) {
      an <- animal_effect_number()
      line <- paste0("OPTION store_accuracy ", an)
      if (input$opt_store_accuracy_orig) line <- paste(line, "orig")
      options <- c(options, line)
    }
    # Removed from default generation: OPTION acctype
    # if (!is.null(input$opt_acctype) && nzchar(input$opt_acctype)) options <- c(options, paste("OPTION acctype", input$opt_acctype))
    if (isTRUE(input$opt_correct_acc_inb_direct0)) options <- c(options, "OPTION correct_accuracy_by_inbreeding_direct 0")
    
    # Heterogeneous residuals
    if (!is.null(input$opt_hetres_pos) && length(input$opt_hetres_pos) > 0) {
      pos_cols <- which(colnames(data()) %in% input$opt_hetres_pos)
      if (length(pos_cols) > 0) options <- c(options, paste("OPTION hetres_pos", paste(pos_cols, collapse = " ")))
    }
    if (!is.null(input$opt_hetres_pol_preset) && nzchar(input$opt_hetres_pol_preset)) {
      # Only include hetres_pol if user changed from default ("0.1 0.01")
      if (input$opt_hetres_pol_preset != "0.1 0.01") {
        options <- c(options, paste("OPTION hetres_pol", input$opt_hetres_pol_preset))
      }
    }
    
    # Genomic / ssGBLUP
    if (isTRUE(input$opt_snp_p_value)) options <- c(options, "OPTION snp_p_value")
    if (isTRUE(input$opt_omit_ainv)) options <- c(options, "OPTION omit_ainv")
    # Removed from default generation: OPTION TauOmega
    # if (!is.null(input$opt_TauOmega) && nzchar(input$opt_TauOmega)) options <- c(options, paste("OPTION TauOmega", input$opt_TauOmega))
    # Removed from default generation: OPTION AlphaBeta
    # if (!is.null(input$opt_AlphaBeta) && nzchar(input$opt_AlphaBeta)) options <- c(options, paste("OPTION AlphaBeta", input$opt_AlphaBeta))
    
    # RENUMF90 options
    if (isTRUE(input$opt_ped_search_complete)) options <- c(options, "OPTION ped_search complete")
    if (!is.null(input$opt_inbreeding_method) && nzchar(input$opt_inbreeding_method)) {
      # Append human-readable label after the option
      label_map <- c(
        "1" = "Meuwissen and Luo (1992)",
        "2" = "Modified Meuwissen & Luo by Sargolzaei & Iwaisaki (2004)",
        "3" = "Modified Colleau by Sargolzaei et al. (2005)",
        "4" = "recursive tabular method",
        "5" = "method of Tier (1990)",
        "6" = "Hybrid parallel computing (OMP) version of Meuwissen and Luo (1992)",
        "7" = "Recursive tabular with self-breeding generations (selfing, e.g., wheat)"
      )
      method_label <- label_map[[as.character(input$opt_inbreeding_method)]]
      if (is.null(method_label) || !nzchar(method_label)) method_label <- as.character(input$opt_inbreeding_method)
      options <- c(options, paste("OPTION inbreeding_method", input$opt_inbreeding_method, "#", method_label))
    }
    
    param_text <- paste0(param_text, paste(options, collapse = "\n"))

    # Append se_covar_function block automatically for VCE
    if (!is.null(input$opt_method) && input$opt_method == "VCE" && isTRUE(input$opt_auto_se_covar)) {
      n_traits <- length(values$traits)
      if (n_traits > 0) {
        # Infer indices based on optional effects ordering
        n_fixed <- length(values$fixed)
        n_user_random <- length(values$random)
        
        # Infer effect indices (MPE can exist without MAT)
        idx_result <- tryCatch({
          idx <- infer_effect_indices(n_fixed, n_user_random, isTRUE(input$opt_mat), isTRUE(input$opt_pe), isTRUE(input$opt_mpe))
          list(success = TRUE, idx = idx)
        }, error = function(e) {
          # Handle any other errors
          showNotification(paste("警告: 效应索引推断失败:", e$message), type = "warning", duration = 5)
          # Return failure status
          list(success = FALSE, idx = NULL)
        })
        
        # Only proceed if index inference was successful
        if (idx_result$success) {
          idx <- idx_result$idx
          a_idx <- idx$a
          m_idx <- idx$m
          rand_idx <- idx$random_indices
          # include any user diagonal randoms before optional effects
          if (n_user_random > 0) rand_idx <- sort(unique(c(rand_idx, (n_fixed + 1):(n_fixed + n_user_random))))
          
          # Build lines for each trait
          out_lines <- c()
          for (t in seq_len(n_traits)) {
            if (!is.null(m_idx) && isTRUE(input$opt_mat)) {
              out_lines <- c(out_lines, emit_h2_with_mat(a_idx, m_idx, rand_idx, t))
            } else {
              out_lines <- c(out_lines, emit_h2_no_mat(a_idx, rand_idx, t))
            }
          }
          # Pairwise rg across traits will be appended by the dedicated generator below
          # Append heritability block
          param_text <- paste0(param_text, "\n\n# === Heritability Calculation (auto) ===\n", paste(out_lines, collapse = "\n"), "\n")
          
          # Append genetic correlation block
          rg_block <- generate_rg_block(n_traits, a_idx, m_idx, include_maternal = !is.null(m_idx) && isTRUE(input$opt_mat))
          if (nzchar(rg_block)) {
            param_text <- paste0(param_text, "\n# === Genetic Correlation Calculation (auto) ===\n", rg_block, "\n")
          }
          
          # Append phenotypic correlation block
          rp_block <- generate_rp_block(
            n_traits = n_traits,
            a_idx = a_idx,
            m_idx = m_idx,
            p_idx = idx$p,
            include_maternal = !is.null(m_idx) && isTRUE(input$opt_mat),
            include_pe = !is.null(idx$p) && isTRUE(input$opt_pe)
          )
          if (nzchar(rp_block)) {
            param_text <- paste0(param_text, "\n# === Phenotypic Correlation Calculation (auto) ===\n", rp_block, "\n")
          }
          
          # Always add sampling/output controls (once at the end)
          param_text <- paste0(param_text, "\nOPTION samples_se_covar_function 10000\nOPTION out_se_covar_function\n")
        }
      }
    }
    
      values$default_param <- param_text
      # If an AI suggestion was just applied, skip overwriting current_param once
      if (isTRUE(values$ai_applied)) {
        values$ai_applied <- FALSE
      } else {
        values$current_param <- param_text
      }
    }
    
    session$sendCustomMessage("update_textarea", list(content = values$current_param))
  })
  
  # Reset button
  observeEvent(input$reset_param, {
    values$current_param <- values$default_param
    session$sendCustomMessage("update_textarea", list(content = values$current_param))
    showNotification("Parameter file reset to default", type = "message", duration = 2)
  })

  # When enabling PED_DEPTH, initialize value to 0
  observeEvent(input$opt_use_ped_depth, {
    if (isTRUE(input$opt_use_ped_depth)) {
      updateNumericInput(session, "opt_ped_depth", value = 0)
    }
  }, ignoreInit = TRUE)
  
  # MPE and MAT are independent - no validation needed
  
  # Handle download button click
  observeEvent(input$download_param, {
    showModal(
      modalDialog(
        title = "Download Confirmation",
        p("Please carefully review the parameter file before using it."),
        p("This parameter file will be used for BLUPF90 analysis."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_download", "Confirm Download", class = "btn-primary")
        )
      )
    )
  })
  
  # Handle download confirmation
  observeEvent(input$confirm_download, {
    removeModal()
    session$sendCustomMessage("get_textarea_content", list())
  })
  
  # Listen for textarea content from JavaScript
  observeEvent(input$textarea_content, {
    if (!is.null(input$textarea_content) && nchar(input$textarea_content) > 0) {
      session$sendCustomMessage("download_text", list(
        content = input$textarea_content,
        filename = "easyblup.par"
      ))
    }
  })
  
  # ====== Genotype format help: PLINK <-> BLUPF90 conversion (requires plinkR) ======
  observeEvent(input$geno_format_help, {
    # Extra safety: check plinkR availability at runtime
    if (!requireNamespace("plinkR", quietly = TRUE)) {
      showNotification("Optional package 'plinkR' is not installed. 请先安装 plinkR 才能使用基因型格式转换助手。", 
                       type = "error", duration = 8)
      return(NULL)
    }
    
    showModal(
      modalDialog(
        title = div(
          if (tolower(lang()) == "zh") "Genotype Format 转换助手" else "Genotype Format Helper",
          span(class = "geno-helper-arrow", "→"),
          span(style = "font-size: 0.9rem; font-weight: 500;", "PLINK → BLUPF90")
        ),
        size = "l",
        easyClose = TRUE,
        class = "geno-helper-modal",
        footer = tagList(
          actionButton("geno_convert_close", 
                      if (tolower(lang()) == "zh") "关闭" else "Close", 
                      class = "btn-secondary"),
          actionButton("geno_convert_run", 
                      if (tolower(lang()) == "zh") "🚀 运行转换" else "🚀 Run Conversion", 
                      class = "btn-primary",
                      icon = NULL)
        ),
        tagList(
          div(class = "geno-helper-info-box",
            p(if (tolower(lang()) == "zh") {
              "需要帮助？这里可以将 PLINK (.ped/.map) 转换为 BLUPF90 (.txt/.map/.bim)。转换后的文件将直接保存到您选择的输出目录。"
            } else {
              "Need help? Use this tool to convert PLINK (.ped/.map) to BLUPF90 (.txt/.map/.bim). Converted files will be saved directly to your selected output directory."
            })
          ),
          
          div(style = "margin-bottom: 20px;",
            h5(style = "font-weight: 700; color: #2c3e50; margin-bottom: 12px;",
               if (tolower(lang()) == "zh") "📋 转换方向" else "📋 Conversion Direction"),
            p(style = "margin: 0; color: #555;",
              if (tolower(lang()) == "zh") {
                "PLINK (.ped/.map) → BLUPF90 (.txt/.map/.bim)"
              } else {
                "PLINK (.ped/.map) → BLUPF90 (.txt/.map/.bim)"
              })
          ),
          
          div(class = "geno-helper-direction-card",
              h4(if (tolower(lang()) == "zh") "PLINK → BLUPF90" else "PLINK → BLUPF90"),
              p(if (tolower(lang()) == "zh") {
                "请上传同一前缀的 .ped 和 .map 文件（例如 mydata.ped / mydata.map）。转换完成后，文件将保存到您选择的输出目录。"
              } else {
                "Upload matching .ped and .map files (e.g. mydata.ped / mydata.map). After conversion, files will be saved to your selected output directory."
              }),
              
              div(class = "geno-helper-file-input-wrapper",
                fileInput("geno_help_plink_ped", 
                         label = tags$strong("📄 PLINK .ped 文件"), 
                         accept = c(".ped"),
                         buttonLabel = if (tolower(lang()) == "zh") "选择文件" else "Choose File",
                         placeholder = if (tolower(lang()) == "zh") "未选择文件" else "No file selected")
              ),
              div(class = "geno-helper-file-input-wrapper",
                fileInput("geno_help_plink_map", 
                         label = tags$strong("📄 PLINK .map 文件"), 
                         accept = c(".map"),
                         buttonLabel = if (tolower(lang()) == "zh") "选择文件" else "Choose File",
                         placeholder = if (tolower(lang()) == "zh") "未选择文件" else "No file selected")
              ),
              
              div(class = "geno-helper-file-input-wrapper",
                p(style = "font-size: 0.85rem; color: #666; margin-top: 8px; margin-bottom: 0;",
                  if (tolower(lang()) == "zh") {
                    "💡 提示：转换完成后请使用下方按钮下载 BLUPF90 文件。"
                  } else {
                    "💡 Tip: Use the buttons below to download BLUPF90 files after conversion."
                  }),
                uiOutput("blup_download_ui")
              )
            )
        )
      )
    )
  })
  
  output$blup_download_ui <- renderUI({
    if (!is.null(conversion_files$blup_txt) &&
        !is.null(conversion_files$blup_map) &&
        file.exists(conversion_files$blup_txt) &&
        file.exists(conversion_files$blup_map)) {
      div(
        style = "margin-top: 10px; display: flex; gap: 8px; flex-wrap: wrap;",
        downloadButton(
          "download_blup_txt",
          if (tolower(lang()) == "zh") "下载 BLUPF90 .txt" else "Download BLUPF90 .txt",
          class = "btn btn-success btn-sm"
        ),
        downloadButton(
          "download_blup_map",
          if (tolower(lang()) == "zh") "下载 BLUPF90 .map" else "Download BLUPF90 .map",
          class = "btn btn-success btn-sm"
        )
      )
    }
  })
  
  output$download_blup_txt <- downloadHandler(
    filename = function() {
      basename(conversion_files$blup_txt %||% "converted.txt")
    },
    content = function(file) {
      req(conversion_files$blup_txt)
      file.copy(conversion_files$blup_txt, file, overwrite = TRUE)
    }
  )
  
  output$download_blup_map <- downloadHandler(
    filename = function() {
      basename(conversion_files$blup_map %||% "converted.map")
    },
    content = function(file) {
      req(conversion_files$blup_map)
      file.copy(conversion_files$blup_map, file, overwrite = TRUE)
    }
  )
  
  # Handle close button click
  observeEvent(input$geno_convert_close, {
    cleanup_conversion_files()
    removeModal()
  }, ignoreInit = TRUE)
  
  observeEvent(input$geno_convert_run, {
    if (!requireNamespace("plinkR", quietly = TRUE)) {
      showNotification("Optional package 'plinkR' is not installed. 请先安装 plinkR。", 
                       type = "error", duration = 8)
      return(NULL)
    }
    
    cleanup_conversion_files()
    
    # 仅支持 PLINK → BLUPF90，通过下载按钮获取结果
    
    req(input$geno_help_plink_ped, input$geno_help_plink_map)
      
      withProgress(message = if (tolower(lang()) == "zh") "正在转换 PLINK → BLUPF90..." else "Converting PLINK → BLUPF90...",
                   detail = if (tolower(lang()) == "zh") "准备文件..." else "Preparing files...",
                   value = 0, {
        
        ped_name <- input$geno_help_plink_ped$name[1]
        ped_datapath <- input$geno_help_plink_ped$datapath[1]
        map_datapath <- input$geno_help_plink_map$datapath[1]
        
        # 使用临时目录进行转换
        tmp_dir <- tempfile(pattern = "easyblup_plink_", tmpdir = tempdir())
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        
        base_prefix <- tools::file_path_sans_ext(basename(ped_name))
        target_prefix <- file.path(tmp_dir, base_prefix)
        
        ped_target <- paste0(target_prefix, ".ped")
        map_target <- paste0(target_prefix, ".map")
        file.copy(ped_datapath, ped_target, overwrite = TRUE)
        file.copy(map_datapath, map_target, overwrite = TRUE)
        
        setProgress(value = 0.1, detail = if (tolower(lang()) == "zh") "正在运行 PLINK 转换..." else "Running PLINK conversion...")
        
        res <- tryCatch({
          plinkR::plink_to_blupf90(prefix = target_prefix, out_prefix = target_prefix, verbose = TRUE)
        }, error = function(e) e)
        
        if (inherits(res, "error")) {
          setProgress(value = 1)
          showNotification(
            paste("PLINK → BLUPF90 转换失败:", res$message),
            type = "error", duration = 10
          )
          unlink(tmp_dir, recursive = TRUE, force = TRUE)
        } else {
          setProgress(value = 0.7, detail = if (tolower(lang()) == "zh") "正在保存文件到输出目录..." else "Saving files to output directory...")
          
        out_txt <- paste0(target_prefix, ".txt")
        out_map <- paste0(target_prefix, ".map")
          
          setProgress(value = 1, detail = if (tolower(lang()) == "zh") "完成！" else "Complete!")
          
          download_dir <- tempfile(pattern = "easyblup_blup_download_", tmpdir = tempdir())
          dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
          if (file.exists(out_txt)) {
            dl_txt <- file.path(download_dir, paste0(base_prefix, ".txt"))
            file.copy(out_txt, dl_txt, overwrite = TRUE)
            conversion_files$blup_txt <- dl_txt
          }
          if (file.exists(out_map)) {
            dl_map <- file.path(download_dir, paste0(base_prefix, ".map"))
            file.copy(out_map, dl_map, overwrite = TRUE)
            conversion_files$blup_map <- dl_map
          }
          
        showNotification(
          if (tolower(lang()) == "zh") {
            "PLINK → BLUPF90 转换完成！请使用下方按钮下载 .txt 和 .map 文件。"
          } else {
            "PLINK → BLUPF90 conversion finished! Use the buttons below to download .txt and .map files."
          },
          type = "message", duration = 15
        )
          
          unlink(tmp_dir, recursive = TRUE, force = TRUE)
        }
      })
  })

  # Mount AI server if available
  if (exists("aiAssistantServer")) {
    # reactive data snapshot for AI
    blup_ai_data <- reactive({
      list(
        traits = values$traits,
        fixed = values$fixed,
        random = values$random,
        animal = values$animal,
        opt = list(
          method = input$opt_method,
          mat = isTRUE(input$opt_mat),
          pe = isTRUE(input$opt_pe),
          mpe = isTRUE(input$opt_mpe)
        ),
        current_param = values$current_param
      )
    })
    # callback to apply AI suggestion to editor
    blup_apply <- function(suggestion) {
      # Defensive apply: ensure suggestion has usable text, log size and a preview for debugging
      if (!is.list(suggestion) || is.null(suggestion$param_text)) {
        showNotification("Apply aborted: suggestion missing param_text", type = "error", duration = 4)
        return()
      }
      txt <- as.character(suggestion$param_text)
      if (!nzchar(txt)) {
        showNotification("Apply aborted: suggestion param_text is empty", type = "error", duration = 4)
        return()
      }
  # Mark that an AI suggestion was applied so auto-regeneration doesn't immediately overwrite it
  values$ai_applied <- TRUE
  # Update reactive value and push to front-end textarea
  values$current_param <- txt
      session$sendCustomMessage("update_textarea", list(content = values$current_param))
      # Notify with a short preview to confirm what was applied
      preview <- substr(gsub("\n", " ", trimws(txt)), 1, 140)
      showNotification(paste0("Applied AI suggestion to parameter editor — preview: ", preview), type = "message", duration = 5)
    }
    # collect AI settings from UI (provider fixed to OpenAI)
    ai_settings <- reactive({
      default_model <- "gpt-4o-mini"
      list(
        provider = "openai",
        base_url = trimws(input$ai_api_base %||% ""),
        api_key = input$ai_api_key %||% "",
        model = trimws(input$ai_model %||% default_model),
        temperature = suppressWarnings(as.numeric(input$ai_temperature %||% 0.2)),
        max_tokens = suppressWarnings(as.integer(input$ai_max_tokens %||% 2048)),
        system_prompt = input$ai_system_prompt %||% "",
        deployment = "",
        api_version = "",
        organization = ""
      )
    })

    observeEvent(input$ai_key_file, {
      req(input$ai_key_file)
      tryCatch({
        key_lines <- readLines(input$ai_key_file$datapath, warn = FALSE, encoding = "UTF-8")
        key_text <- trimws(paste(key_lines, collapse = "\n"))
        updateTextInput(session, "ai_api_key", value = key_text)
        showNotification("API key imported from file", type = "message")
      }, error = function(e) {
        showNotification(paste("无法读取 API key 文件：", conditionMessage(e)), type = "error")
      })
    })

    # When the user saves AI settings, return to the chat view (hide settings panel)
    observeEvent(input$aiSaveSettings, {
      # small delay to allow UI fields to settle client-side
      session$sendCustomMessage("ai_show_chat", list())
      showNotification("AI settings saved. Returning to chat.", type = "message", duration = 2)
    })

    rules <- load_app_rules("easyblupf90")
    # MCP tools: allow the assistant to directly control Shiny inputs (whitelisted)
    easyblup_tools <- reactive({
      if (exists("easyblup_openai_tools", mode = "function")) {
        get("easyblup_openai_tools", mode = "function")()
      } else {
        NULL
      }
    })
    easyblup_tool_call <- function(tool_name, args) {
      if (!exists("easyblup_mcp_call_tool", mode = "function")) {
        return(list(ok = FALSE, error = "MCP tools not loaded"))
      }
      get("easyblup_mcp_call_tool", mode = "function")(
        name = tool_name,
        arguments = args,
        session = session,
        input = input,
        values = values
      )
    }

    get("aiAssistantServer")("ai_blup", blup_ai_data, blup_apply, app_context = reactive(list(
      app_name = "easyblupf90",
      locale = tryCatch({ if (exists("language_code", mode = "function")) get("language_code")(lang()) else "zh" }, error = function(e) "zh"),
      rules = rules
    )), ai_settings = ai_settings, tool_specs = easyblup_tools, tool_call = easyblup_tool_call)
  }
}

# App entrypoints
run_easyblup_app <- function() {
  list(ui = ui, server = server)
}

if (!identical(Sys.getenv("EASYBREEDER_SOURCE_ONLY", ""), "1")) {
  shinyApp(ui = ui, server = server)
}
