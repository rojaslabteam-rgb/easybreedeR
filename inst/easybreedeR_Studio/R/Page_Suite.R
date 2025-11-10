Home_UI <- function() {
  tabPanel(
    uiOutput("tab_home"),
    fluidPage(
      uiOutput("hero_content")
    )
  )
}

suite_ui <- function() {
  navbarPage(
    title = div(
      style = "position: relative; display:flex; align-items:center; justify-content:center; width:100%;",
      div(style="flex:1;"), # 左侧占位
      span("easybreedeR Studio ", style = "color:#CEB888; font-weight:bold;")
    ),
    theme = suite_theme,
    id = "main_nav",
    header = tagList(
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
        tags$style(HTML('
  .navbar { 
    border-bottom: 3px solid #CFB991; 
    box-shadow:none !important; 
  }

  /* Left menu, right-aligned title/menu container */
  .navbar .container-fluid {
    display: flex !important;
    align-items: center !important;
    justify-content: space-between !important;
    flex-direction: row !important;
    width: 100%;
    padding-right: 4px; /* reduce to bring menu closer to right edge */
    gap: 64px; /* increase space between brand and menu */
  }

  .navbar-brand {
    order: 1;
    margin-right: auto !important;
  }

  .navbar-nav {
    order: 3;
    margin-left: auto !important;
    display: flex !important;
    align-items: center !important;
    gap: 16px;
    background: transparent !important;
  }

  .navbar .nav-link { 
    font-weight: 700; 
    color: #000 !important; 
    padding: 10px 8px; 
    text-decoration: none !important; 
  }

  .navbar .nav-link.active, .navbar .nav-item.show .nav-link { 
    border-bottom: 3px solid #CEB888; 
    background: transparent !important; 
    box-shadow: none !important; 
  }

  /* Button styles aligned with PedivieweR */
  .btn-primary, .btn.btn-primary { background-color:#CEB888 !important; border-color:#CEB888 !important; color:#000000 !important; font-weight:600; }
  .btn-primary:hover, .btn.btn-primary:hover { background-color:#B89D5D !important; border-color:#B89D5D !important; }
  .btn-secondary { background-color:#95A5A6 !important; border-color:#95A5A6 !important; color:#fff !important; }
  .btn-secondary:hover { background-color:#7F8C8D !important; border-color:#7F8C8D !important; }

  /* Hero section inspired by Purdue homepage */
  .hero {
    background: linear-gradient(180deg, #ffffff 0%, #f7f3ea 100%);
    padding: 32px 24px 24px 24px;
    border-bottom: 3px solid #CFB991;
  }
  .hero .kicker {
    letter-spacing: .12em;
    font-size: 13px;
    text-transform: uppercase;
    color: #6c6c6c;
    font-weight: 700;
  }
  .hero h1 {
    font-family: "Crimson Text", serif;
    font-weight: 900;
    line-height: 1.05;
    margin: 8px 0 12px 0;
    color: #000;
    font-size: 44px;
  }
  .hero .subhead {
    color: #4a4a4a;
    font-size: 16px;
    max-width: 960px;
  }
  .quick-links { margin-top: 18px; display:flex; gap:12px; flex-wrap:wrap; }

  /* Home language button placement */
  .home-lang-container {
    display: flex;
    align-items: center;
    justify-content: flex-end;
  }
  .lang-btn {
    margin-left: 12px;
  }

  /* Right-aligned settings dropdown next to title */
  .navbar-right-group {
    position: absolute;
    right: 16px;
    top: 8px;
  }

  .dropdown-menu {
    min-width: 140px;
    font-size: 15px;
  }
  .dropdown-item:hover {
    background-color: #CEB888 !important;
    color: black !important;
  }
'))
      ),
      # Top-right gear aligned with title
      tags$div(
        class = "navbar-right-group",
        tags$div(
          class = "dropdown",
          tags$button(
            type = "button",
            id = "settingsDropdownTop",
            style = "font-size:20px; line-height:2; background:transparent; border:none; color:#555; padding:0; cursor:pointer; margin-right:8px;",
            "Language / 中文 / Português ⚙️"
          ),
          tags$ul(
            id = "dropdownMenuLangTop",
            class = "dropdown-menu dropdown-menu-end",
            style = "display:none; position:absolute; right:0; top:28px; background:white; border:1px solid #ccc; padding:6px 0; border-radius:6px;",
            tags$li(tags$a(class = "dropdown-item", href = '#', `data-lang` = "en", "English (English)")),
            tags$li(tags$a(class = "dropdown-item", href = '#', `data-lang` = "zh", "中文 (Chinese)")),
            tags$li(tags$a(class = "dropdown-item", href = '#', `data-lang` = "pt", "Português (Portuguese)"))
          ),
          tags$script(HTML("
            document.addEventListener('DOMContentLoaded', function() {
              const btnTop = document.getElementById('settingsDropdownTop');
              const menuTop = document.getElementById('dropdownMenuLangTop');
              btnTop.addEventListener('click', function(e) {
                e.stopPropagation();
                menuTop.style.display = (menuTop.style.display === 'none' || menuTop.style.display === '') ? 'block' : 'none';
              });
              document.addEventListener('click', function(e) {
                if (!btnTop.contains(e.target)) menuTop.style.display = 'none';
              });
              menuTop.addEventListener('click', function(e) {
                if (e.target && e.target.matches('a.dropdown-item')) {
                  e.preventDefault();
                  var code = e.target.getAttribute('data-lang');
                  if (code) {
                    Shiny.setInputValue('lang_select_top', code, {priority: 'event'});
                  }
                  menuTop.style.display = 'none';
                }
              });
            });
          "))
        )
      )
    ),
    Home_UI(),
    # Use stable 'value' for each tab so programmatic navigation doesn't depend on
    # the translated title string. Titles are rendered via uiOutput so they
    # react to language changes while the tab 'value' stays constant.
    tabPanel(uiOutput("tab_datapreviewer"), uiOutput("frame_datapreviewer"), value = "datapreviewR"),
    tabPanel(uiOutput("tab_pediviewer"), uiOutput("frame_pediviewer"), value = "pediviewer"),
    tabPanel(uiOutput("tab_easyblup"), uiOutput("frame_easyblup"), value = "easyblup"),
    tabPanel(uiOutput("tab_rnotebook"), uiOutput("frame_rnotebook"), value = "rnotebook"),
    # (Removed bottom-right gear; using top-right gear aligned with title)
  )
}
