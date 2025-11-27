# ================================================
# easyblup - Interactive BLUPF90 Parameter Generator  
# Open BreedX (OBX) Clean Three-Panel Layout
# Version: 0.4.0 (Fixed Language Settings)
# Created: 2025-10-22
# Last Modified: 2025-10-31
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
          fileInput("geno_file", uiOutput("geno_label_ui", inline = TRUE),
                   accept = c(".ped", ".map"),
                   multiple = TRUE,
                   buttonLabel = "Browse...",
                   placeholder = "Optional"),
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
      checkboxInput("opt_tuned_g2", get_label("tuned_g2", l), value = TRUE),
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
      checkboxInput("opt_origID", get_label("origID_store_solutions", l), value = FALSE)
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
              "Defaults: OpenAI: https://api.openai.com; DeepSeek: https://api.deepseek.com; Gemini endpoints may include /v1beta.")
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
    get_label("genotype", lang())
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
    checkboxInput("opt_store_accuracy_orig", paste0("store_accuracy with original ID (Animal effect: ", an, ")"), value = FALSE)
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
  # animal 后接 mat, pe 始终在 animal/mat 之后, mpe 永远最后, mpe 存在时必须有 mat
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
    
    # mpe 永远在最后，且存在 mpe 时必须有 mat
    if (opt_mpe) {
      if (is.null(idx$m)) stop("MPE requires MAT effect to be present.")
      last_idx <- if (!is.null(idx$p)) idx$p else idx$m
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
      # Check pedigree file
      has_ped <- !is.null(input$ped_file) && nrow(input$ped_file) > 0
    
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
      
      # Add pedigree file information
      pedigree_filename <- if (has_ped) basename(input$ped_file$name) else "pedigree.txt"
        param_text <- paste0(param_text, "FILE\n", pedigree_filename, "\nFILE_POS\n1 2 3 # Progeny Sire Dam\n")
        
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
    # (CO)VARIANCES: output n x n matrix (like RESIDUAL_VARIANCE)
    n_traits <- length(values$traits)
    if (n_traits > 0) {
      cov_lines <- c()
      for (i in 1:n_traits) {
        row_vals <- rep("1", n_traits)
        if (n_traits > 1) {
          for (j in 1:n_traits) {
            if (i != j) row_vals[j] <- "0.01"
          }
        }
        cov_lines <- c(cov_lines, paste(row_vals, collapse = " "))
      }
      param_text <- paste0(param_text, "(CO)VARIANCES\n", paste(cov_lines, collapse = "\n"), "\n\n")
    } else {
      param_text <- paste0(param_text, "(CO)VARIANCES\n1\n\n")
    }

    if (input$opt_pe) {
      if (n_traits > 0) {
        pe_lines <- c()
        for (i in 1:n_traits) {
          row_vals <- rep("0.001", n_traits)
          row_vals[i] <- "1"
          pe_lines <- c(pe_lines, paste(row_vals, collapse = " "))
        }
        param_text <- paste0(param_text, "(CO)VARIANCES_PE\n", paste(pe_lines, collapse = "\n"), "\n")
      } else {
        param_text <- paste0(param_text, "(CO)VARIANCES_PE\n0.001\n")
      }
    }
    if (input$opt_mpe) {
      if (n_traits > 0) {
        mpe_lines <- c()
        for (i in 1:n_traits) {
          row_vals <- rep("0.001", n_traits)
          row_vals[i] <- "1"
          mpe_lines <- c(mpe_lines, paste(row_vals, collapse = " "))
        }
        param_text <- paste0(param_text, "(CO)VARIANCES_MPE\n", paste(mpe_lines, collapse = "\n"), "\n")
      } else {
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
  # Only include use_yams/tunedG2 if genotype file is present
  has_geno <- !is.null(input$geno_file) && nrow(input$geno_file) > 0
  if (has_geno && input$opt_use_yams) options <- c(options, "OPTION use_yams")
  if (has_geno && input$opt_tuned_g2) options <- c(options, "OPTION tunedG2")
    if (!is.null(input$opt_maxrounds_val) && !is.na(input$opt_maxrounds_val)) options <- c(options, paste("OPTION maxrounds", input$opt_maxrounds_val))
    if (!is.null(input$opt_solv_method) && nzchar(input$opt_solv_method)) options <- c(options, paste("OPTION solv_method", tolower(input$opt_solv_method)))
    if (!is.null(input$opt_r_factor) && !is.na(input$opt_r_factor)) options <- c(options, paste("OPTION r_factor", input$opt_r_factor))
    if (!is.null(input$opt_blksize) && !is.na(input$opt_blksize)) options <- c(options, paste("OPTION blksize", input$opt_blksize))
    if (isTRUE(input$opt_residual_out)) options <- c(options, "OPTION residual")
    if (isTRUE(input$opt_stdresidual_out)) options <- c(options, "OPTION stdresidual")
    if (isTRUE(input$opt_prior_solutions)) options <- c(options, "OPTION prior_solutions")
    if (!is.null(input$opt_set_eig) && nzchar(input$opt_set_eig)) options <- c(options, paste("OPTION set_eig", input$opt_set_eig))
    if (input$opt_origID) options <- c(options, "OPTION origID")
    
    if (input$opt_store_accuracy || input$opt_store_accuracy_orig) {
      an <- animal_effect_number()
      line <- paste0("OPTION store_accuracy ", an)
      if (input$opt_store_accuracy_orig) line <- paste(line, "orig")
      options <- c(options, line)
    }
    if (!is.null(input$opt_acctype) && nzchar(input$opt_acctype)) options <- c(options, paste("OPTION acctype", input$opt_acctype))
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
    if (!is.null(input$opt_TauOmega) && nzchar(input$opt_TauOmega)) options <- c(options, paste("OPTION TauOmega", input$opt_TauOmega))
    if (!is.null(input$opt_AlphaBeta) && nzchar(input$opt_AlphaBeta)) options <- c(options, paste("OPTION AlphaBeta", input$opt_AlphaBeta))
    
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
        idx <- infer_effect_indices(n_fixed, n_user_random, isTRUE(input$opt_mat), isTRUE(input$opt_pe), isTRUE(input$opt_mpe))
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
    get("aiAssistantServer")("ai_blup", blup_ai_data, blup_apply, app_context = reactive(list(
      app_name = "easyblupf90",
      locale = tryCatch({ if (exists("language_code", mode = "function")) get("language_code")(lang()) else "zh" }, error = function(e) "zh"),
      rules = rules
    )), ai_settings = ai_settings)
  }
}

# Run the app
shinyApp(ui = ui, server = server)
