Home_UI <- function() {
  tabPanel(
    uiOutput("tab_home"),
    fluidPage(
      uiOutput("hero_content"),
      div(class = "container",
        div(class = "main-content-wrapper",
          uiOutput("home_page_content")
        ),
        # Right sidebar for README
        div(id = "readmeSidebar", class = "readme-sidebar",
          div(class = "readme-sidebar-header",
            h3("README"),
            tags$button(
              id = "toggleReadmeBtn",
              class = "readme-toggle-btn",
              tags$span(class = "material-symbols-outlined", "close")
            )
          ),
          div(class = "readme-sidebar-content",
            uiOutput("readmeContent")
          )
        ),
        # Toggle button to show README
        tags$button(
          id = "showReadmeBtn",
          class = "show-readme-btn",
          tags$span(class = "material-symbols-outlined", "description"),
          "README"
        )
      )
    )
  )
}

suite_ui <- function() {
  navbarPage(
    title = div(
      style = "position: relative; display:flex; align-items:center; justify-content:center; width:100%;",
      div(style="flex:1;"), # 左侧占位
      span("easybreedeR Studio ", style = "color: var(--accent-deep, #8E6F3E); font-weight:bold;")
    ),
    theme = suite_theme,
    id = "main_nav",
    header = tagList(
      shinyjs::useShinyjs(),
      tags$head(
        tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200"),
        if (exists("eb_chrome_css_tag", mode = "function")) eb_chrome_css_tag(),
        tags$script(HTML("
          document.addEventListener('click', function (e) {
            var card = e.target.closest('.app-card--launch');
            if (!card || e.target.closest('button, a')) return;
            var btn = card.querySelector('button');
            if (btn) btn.click();
          });
        ")),
        tags$style(HTML('
  .navbar { 
    border-bottom: 1px solid var(--line, rgba(0,0,0,.10)); 
    box-shadow:none !important;
    background: color-mix(in srgb, var(--card, #fff) 78%, transparent) !important;
    backdrop-filter: blur(18px) saturate(160%);
  }

  /* Left menu, right-aligned title/menu container */
  .navbar .container-fluid {
    display: flex !important;
    align-items: center !important;
    justify-content: space-between !important;
    flex-direction: row !important;
    flex-wrap: wrap !important;
    width: 100%;
    max-width: 100%;
    padding-right: 4px;
    gap: 12px 24px;
    box-sizing: border-box;
  }

  .navbar-brand {
    order: 1;
    margin-right: auto !important;
    min-width: 0;
  }

  .navbar-nav {
    order: 3;
    margin-left: auto !important;
    display: flex !important;
    align-items: center !important;
    flex-wrap: wrap !important;
    gap: 4px 12px;
    background: transparent !important;
    max-width: 100%;
  }

  .navbar .nav-link { 
    font-weight: 650; 
    color: var(--ink, #202124) !important; 
    padding: 10px 8px; 
    text-decoration: none !important; 
    white-space: nowrap;
  }

  .navbar .nav-link.active, .navbar .nav-item.show .nav-link { 
    border-bottom: 2px solid var(--accent, #CFB991); 
    background: transparent !important; 
    box-shadow: none !important; 
  }

  /* Buttons come from easybreedeR-chrome.css */

  /* Hero — Xmosome surface */
  .hero {
    background: var(--surface-solid, #f4f5f7);
    padding: 32px 24px 24px 24px;
    border-bottom: 1px solid var(--line, rgba(0,0,0,.10));
  }
  .hero .kicker {
    letter-spacing: .12em;
    font-size: 11px;
    text-transform: uppercase;
    color: var(--accent, #CFB991);
    font-weight: 650;
  }
  .hero h1 {
    font-family: var(--font, -apple-system, BlinkMacSystemFont, "SF Pro Text", sans-serif);
    font-weight: 650;
    line-height: 1.15;
    margin: 8px 0 12px 0;
    color: var(--ink, #202124);
    font-size: 32px;
    letter-spacing: -0.02em;
  }
  .hero .subhead {
    color: #4a4a4a;
    font-size: 16px;
    max-width: 960px;
  }
  .quick-links { margin-top: 18px; display:flex; gap:12px; flex-wrap:wrap; }

  /* Container spacing from hero */
  .container {
    margin-top: 40px !important;
    margin-bottom: 48px;
    max-width: 100%;
    padding-left: clamp(12px, 3vw, 24px);
    padding-right: clamp(12px, 3vw, 24px);
    box-sizing: border-box;
  }

  /* Main content wrapper with sidebar */
  .main-content-wrapper {
    display: flex;
    gap: 24px;
    position: relative;
    width: 100%;
    min-width: 0;
    max-width: 100%;
    box-sizing: border-box;
  }

  /* Shiny output wrapper */
  #home_page_content {
    width: 100% !important;
    min-width: 0 !important;
    max-width: 100% !important;
    box-sizing: border-box;
  }

  .cards-container {
    flex: 1 0 0%;
    width: 100%;
    min-width: 100%;
    max-width: 100%;
  }

  /* Section title */
  .section-title {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 16px;
  }
  /* chrome .section-title zeroes margins; restore spacing between home sections */
  .cards-container .row + .section-title { margin-top: 40px !important; }
  .cards-container > .section-title { margin-bottom: 16px !important; }
  .section-title h2 {
    font-size: 22px;
    font-weight: 600;
    display: flex;
    align-items: center;
    gap: 8px;
    margin: 0;
    color: #000;
  }
  .section-title .material-symbols-outlined {
    color: #CFB991;
    font-size: 24px;
  }

  /* App cards — responsive; titles wrap instead of clipping */
  .app-card {
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 12px;
    padding: clamp(16px, 2.5vw, 24px);
    margin-bottom: 0;
    box-shadow: 0 1px 2px rgba(20, 32, 55, 0.06);
    transition: box-shadow 0.2s ease, transform 0.2s ease, border-color 0.2s ease;
    min-height: 0;
    height: 100%;
    width: 100%;
    min-width: 0;
    max-width: 100%;
    box-sizing: border-box;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    overflow: hidden;
  }

  /* Module cards: auto-fit grid across aspect ratios */
  .cards-grid {
    display: grid;
    width: 100%;
    max-width: 100%;
    box-sizing: border-box;
    gap: 16px;
  }
  .cards-grid--modules {
    grid-template-columns: repeat(auto-fit, minmax(min(100%, 240px), 1fr));
  }
  .cards-grid--deps {
    grid-template-columns: repeat(auto-fit, minmax(min(100%, 280px), 1fr));
  }

  /* Legacy Bootstrap rows (dependency fallback / older markup) */
  .cards-container .row {
    margin-left: 0 !important;
    margin-right: 0 !important;
    width: 100% !important;
    max-width: 100% !important;
    flex-wrap: wrap !important;
    display: flex !important;
    gap: 0;
  }
  .cards-container .row > [class*="col-"] {
    padding-left: 12px;
    padding-right: 12px;
    flex: 1 1 240px !important;
    width: auto !important;
    max-width: 100% !important;
    min-width: 0 !important;
    box-sizing: border-box;
    overflow: hidden;
    display: flex;
    flex-direction: column;
    margin-bottom: 16px;
  }
  .cards-container .row > [class*="col-"] > .app-card {
    flex: 1 1 auto;
    display: flex;
    flex-direction: column;
    align-self: stretch;
  }
  .cards-container .row + .row {
    margin-top: 8px;
  }
  .app-card:hover {
    box-shadow: 0 6px 16px rgba(20, 32, 55, 0.10);
    transform: translateY(-2px);
  }
  .app-card--launch { cursor: pointer; }
  .app-card--launch:hover { border-color: var(--accent, #CFB991); }
  .app-card-header {
    display: flex;
    align-items: flex-start;
    gap: 16px;
    margin-bottom: 20px;
    min-width: 0;
  }
  .app-card-icon {
    width: 56px;
    height: 56px;
    display: flex;
    align-items: center;
    justify-content: center;
    border-radius: 12px;
    flex-shrink: 0;
  }
  .app-card-icon.blue { background-color: #E3F2FD; }
  .app-card-icon.green { background-color: #E8F5E9; }
  .app-card-icon.red { background-color: #FFEBEE; }
  .app-card-icon.yellow { background-color: #FFFDE7; }
  .app-card-icon.orange { background-color: #FFF3E0; }
  .app-card-icon.purple { background-color: #F3E5F5; }
  .app-card-icon.teal { background-color: #E0F2F1; }
  .app-card-icon.gold { background-color: rgba(207, 185, 145, 0.28); }
  .material-symbols-outlined {
    font-family: "Material Symbols Outlined";
    font-weight: normal;
    font-style: normal;
    font-size: 32px;
    line-height: 1;
    letter-spacing: normal;
    text-transform: none;
    display: inline-block;
    white-space: nowrap;
    word-wrap: normal;
    direction: ltr;
    -webkit-font-feature-settings: "liga";
    -webkit-font-smoothing: antialiased;
  }
  .material-symbols-outlined.blue { color: #1976D2; }
  .material-symbols-outlined.green { color: #388E3C; }
  .material-symbols-outlined.red { color: #D32F2F; }
  .material-symbols-outlined.yellow { color: #F57F17; }
  .material-symbols-outlined.orange { color: #F57C00; }
  .material-symbols-outlined.purple { color: #7B1FA2; }
  .material-symbols-outlined.teal { color: #00796B; }
  .material-symbols-outlined.gold { color: #8E6F3E; }
  .app-card-title-wrapper {
    flex: 1;
    min-width: 0;
    display: flex;
    flex-direction: column;
  }
  .app-card h3 {
    color: #000;
    font-size: clamp(16px, 2.2vw, 22px);
    font-weight: 700;
    margin-top: 0;
    margin-bottom: 8px;
    line-height: 1.25;
    overflow-wrap: anywhere;
    word-break: break-word;
  }
  .app-card-header p {
    color: #4a4a4a;
    font-size: 14px;
    line-height: 1.5;
    margin: 0;
    min-height: 0;
    max-height: none;
    overflow: visible;
    display: block;
    -webkit-line-clamp: unset;
    line-clamp: unset;
  }
  .app-card > p {
    color: #4a4a4a;
    font-size: 16px;
    line-height: 1.6;
    margin-bottom: 24px;
    min-height: 0;
    max-height: none;
    overflow: visible;
  }
  .app-card .btn {
    margin-top: auto;
    width: 100%;
    min-height: 36px !important;
    padding: 8px 16px !important;
    font-size: 13px !important;
    border-radius: 8px !important;
    text-decoration: none;
    text-align: center;
  }

  /* Dependency cards */
  .dependency-card {
    margin-top: 0;
    overflow: hidden;
  }
  .dependency-card .app-card-header p {
    min-height: 0;
    max-height: none;
  }

  @media (max-width: 900px) {
    .hero { padding: 24px 16px 20px; }
    .hero h1 { font-size: 24px; }
    .navbar .nav-link { padding: 8px 6px; font-size: 13px; }
    .app-card-icon { width: 48px; height: 48px; }
    .material-symbols-outlined { font-size: 28px; }
  }
  @media (max-width: 600px) {
    .cards-grid--modules,
    .cards-grid--deps {
      grid-template-columns: 1fr;
    }
    .navbar-nav { margin-left: 0 !important; width: 100%; }
  }
  
  /* Package lists — collapsed by default; they are reference, not a task */
  .deps-disclosure {
    margin-top: 40px;
    border-top: 1px solid var(--line, rgba(0,0,0,.10));
    padding-top: 16px;
  }
  .deps-disclosure > summary {
    cursor: pointer;
    font-size: 14px;
    font-weight: 600;
    color: var(--muted, #6e7178);
  }
  .deps-disclosure > summary:hover { color: var(--ink, #202124); }
  .deps-group { margin-top: 16px; }
  .deps-group h4 {
    margin: 0 0 8px;
    font-size: 12px;
    font-weight: 650;
    color: var(--muted, #6e7178);
  }
  .deps-chips { display: flex; flex-wrap: wrap; gap: 6px; }
  .deps-chip {
    padding: 3px 10px;
    border: 1px solid var(--line, rgba(0,0,0,.10));
    border-radius: 999px;
    background: var(--card, #fff);
    color: var(--ink, #202124);
    font-size: 12.5px;
    text-decoration: none;
  }
  a.deps-chip:hover {
    border-color: var(--accent, #CFB991);
    color: var(--accent-deep, #8E6F3E);
    text-decoration: none;
  }

  /* Required Dependencies styling */
  .dependency-item {
    padding: 12px 0;
    margin-bottom: 8px;
  }
  
  .dependency-link {
    display: flex;
    align-items: center;
    padding: 10px 16px;
    background: #f8f9fa;
    border: 1px solid #e0e0e0;
    border-radius: 8px;
    text-decoration: none;
    color: #333;
    transition: all 0.2s ease;
    font-size: 15px;
    font-weight: 500;
  }
  
  .dependency-link:hover {
    background: #CFB991;
    color: #000;
    border-color: #CFB991;
    text-decoration: none;
    transform: translateY(-1px);
    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
  }
  
  .dependency-name {
    color: inherit;
  }
  
  .dependency-item-no-link .dependency-name {
    display: inline-block;
    padding: 10px 16px;
    background: #f8f9fa;
    border: 1px solid #e0e0e0;
    border-radius: 8px;
    color: #999;
    font-size: 15px;
    font-weight: 500;
    width: 100%;
  }

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
    display: flex;
    align-items: center;
    gap: 12px;
  }

  .dropdown-menu {
    min-width: 120px;
    font-size: 14px;
  }
  .dropdown-item:hover {
    background-color: #CFB991 !important;
    color: black !important;
  }

  /* Language dropdown button */
  .lang-dropdown-btn {
    font-size: 14px;
    line-height: 1.5;
    background: transparent;
    border: none;
    color: #555;
    padding: 6px 12px;
    cursor: pointer;
    border-radius: 6px;
    transition: all 0.2s;
  }
  .lang-dropdown-btn:hover {
    background-color: #f0f0f0;
    color: #000;
  }

  /* GitHub star button */
  .github-star-btn {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 6px 16px;
    background-color: #1e293b;
    color: white;
    border-radius: 9999px;
    text-decoration: none;
    font-size: 12px;
    font-weight: 500;
    transition: all 0.2s;
  }
  .github-star-btn:hover {
    background-color: #334155;
    color: white;
    text-decoration: none;
  }
  .github-star-btn svg {
    width: 14px;
    height: 14px;
    fill: currentColor;
  }
  .github-star-count {
    padding-left: 8px;
    margin-left: 8px;
    border-left: 1px solid rgba(255, 255, 255, 0.3);
  }

  /* README Sidebar */
  .readme-sidebar {
    position: fixed;
    right: -600px;
    top: 0;
    width: 600px;
    height: 100vh;
    background: white;
    box-shadow: -2px 0 8px rgba(0,0,0,0.1);
    transition: right 0.3s ease;
    z-index: 1000;
    display: flex;
    flex-direction: column;
  }

  .readme-sidebar.open {
    right: 0;
  }

  .readme-sidebar-header {
    padding: 14px 16px;
    border-bottom: 1px solid var(--line, rgba(0,0,0,.10));
    display: flex;
    justify-content: space-between;
    align-items: center;
    background: var(--accent-soft, rgba(207, 185, 145,.10));
  }

  .readme-sidebar-header h3 {
    margin: 0;
    color: var(--ink, #202124);
    font-size: 14px;
    font-weight: 650;
  }

  .readme-toggle-btn {
    background: transparent;
    border: none;
    cursor: pointer;
    padding: 4px;
    color: var(--muted, #6e7178);
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .readme-toggle-btn:hover {
    opacity: 0.7;
  }

  .readme-sidebar-content {
    flex: 1;
    overflow-y: auto;
    padding: 24px;
    font-size: 14px;
    line-height: 1.6;
  }

  .readme-sidebar-content h1 {
    font-size: 24px;
    margin-top: 0;
    margin-bottom: 16px;
    color: #000;
    border-bottom: 2px solid #CFB991;
    padding-bottom: 8px;
  }

  .readme-sidebar-content h2 {
    font-size: 20px;
    margin-top: 24px;
    margin-bottom: 12px;
    color: #000;
  }

  .readme-sidebar-content h3 {
    font-size: 18px;
    margin-top: 20px;
    margin-bottom: 10px;
    color: #333;
  }

  .readme-sidebar-content code {
    background: #f5f5f5;
    padding: 2px 6px;
    border-radius: 3px;
    font-family: \"Courier New\", monospace;
    font-size: 13px;
  }

  .readme-sidebar-content pre {
    background: #f5f5f5;
    padding: 12px;
    border-radius: 6px;
    overflow-x: auto;
    border-left: 3px solid #CFB991;
  }

  .readme-sidebar-content pre code {
    background: transparent;
    padding: 0;
  }

  .readme-sidebar-content ul, .readme-sidebar-content ol {
    margin-left: 20px;
    margin-bottom: 12px;
  }

  .readme-sidebar-content li {
    margin-bottom: 6px;
  }

  .readme-sidebar-content a {
    color: #CFB991;
    text-decoration: none;
  }

  .readme-sidebar-content a:hover {
    text-decoration: underline;
  }

  .readme-sidebar-content p {
    margin-bottom: 12px;
  }

  .readme-sidebar-content blockquote {
    border-left: 3px solid #CFB991;
    padding-left: 16px;
    margin-left: 0;
    color: #666;
    font-style: italic;
  }

  .readme-sidebar-content img {
    max-width: 100%;
    height: auto;
    border-radius: 6px;
  }

  .readme-sidebar-content table {
    width: 100%;
    border-collapse: collapse;
    margin-bottom: 16px;
  }

  .readme-sidebar-content table th,
  .readme-sidebar-content table td {
    padding: 8px;
    border: 1px solid #e0e0e0;
    text-align: left;
  }

  .readme-sidebar-content table th {
    background-color: #f5f5f5;
    font-weight: 600;
  }

  /* Show README button */
  .show-readme-btn {
    position: fixed;
    right: 24px;
    bottom: 24px;
    padding: 12px 20px;
    background: #CFB991;
    color: #000;
    border: none;
    border-radius: 24px;
    cursor: pointer;
    font-weight: 600;
    font-size: 14px;
    display: flex;
    align-items: center;
    gap: 8px;
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    transition: all 0.3s ease;
    z-index: 999;
  }

  .show-readme-btn:hover {
    background: #8E6F3E;
    transform: translateY(-2px);
    box-shadow: 0 6px 16px rgba(0,0,0,0.2);
  }

  .show-readme-btn.hidden {
    display: none;
  }
')),
        if (exists("easybreedeR_a4_layout_css", mode = "function")) {
          tags$style(HTML(easybreedeR_a4_layout_css()))
        },
        tags$style(HTML("
          .suite-iframe-wrap { height: calc(100vh - 52px) !important; }
          @media print {
            .navbar-right-group, .github-star-btn, #settingsDropdownTop,
            #dropdownMenuLangTop, .show-readme-btn, .readme-sidebar { display: none !important; }
            .navbar { border-bottom: 2px solid #b6c9e0 !important; }
            .suite-iframe-wrap, .suite-iframe-wrap iframe {
              height: 175mm !important; max-height: 175mm !important;
              border: 1px solid #E6D9B8 !important; border-radius: 3px !important;
            }
          }
        "))
      ),
      # Top-right GitHub star and language selector
      tags$div(
        class = "navbar-right-group",
        tags$a(
          href = "https://github.com/rojaslabteam-rgb/easybreedeR",
          target = "_blank",
          class = "github-star-btn",
          id = "githubStarBtn",
          tags$svg(
            viewBox = "0 0 24 24",
            tags$path(d = "M12 .297c-6.63 0-12 5.373-12 12 0 5.303 3.438 9.8 8.205 11.385.6.113.82-.258.82-.577 0-.285-.01-1.04-.015-2.04-3.338.724-4.042-1.61-4.042-1.61C4.422 18.07 3.633 17.7 3.633 17.7c-1.087-.744.084-.729.084-.729 1.205.084 1.838 1.236 1.838 1.236 1.07 1.835 2.809 1.305 3.495.998.108-.776.417-1.305.76-1.605-2.665-.3-5.466-1.332-5.466-5.93 0-1.31.465-2.38 1.235-3.22-.135-.303-.54-1.523.105-3.176 0 0 1.005-.322 3.3 1.23.96-.267 1.98-.399 3-.405 1.02.006 2.04.138 3 .405 2.28-1.552 3.285-1.23 3.285-1.23.645 1.653.24 2.873.12 3.176.765.84 1.23 1.91 1.23 3.22 0 4.61-2.805 5.625-5.475 5.92.42.36.81 1.096.81 2.22 0 1.606-.015 2.896-.015 3.286 0 .315.21.69.825.57C20.565 22.092 24 17.592 24 12.297c0-6.627-5.373-12-12-12")
          ),
          "Star on GitHub",
          tags$span(class = "github-star-count", id = "githubStarCount", "0")
        ),
        tags$div(
          class = "dropdown",
          tags$button(
            type = "button",
            id = "settingsDropdownTop",
            class = "lang-dropdown-btn",
            "EN / 中文 / PT"
          ),
            tags$ul(
            id = "dropdownMenuLangTop",
            class = "dropdown-menu dropdown-menu-end",
            style = "display:none; position:absolute; right:0; top:32px; background:white; border:1px solid #ccc; padding:6px 0; border-radius:6px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);",
            tags$li(tags$a(class = "dropdown-item", href = '#', `data-lang` = "en", "English")),
            tags$li(tags$a(class = "dropdown-item", href = '#', `data-lang` = "zh", "中文")),
            tags$li(tags$a(class = "dropdown-item", href = '#', `data-lang` = "pt", "Português"))
          ),
          tags$script(HTML("
            document.addEventListener('DOMContentLoaded', function() {
              // Language dropdown
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
              
              // Fetch GitHub star count
              fetch('https://api.github.com/repos/rojaslabteam-rgb/easybreedeR')
                .then(response => response.json())
                .then(data => {
                  const starCount = data.stargazers_count || 0;
                  const countElement = document.getElementById('githubStarCount');
                  if (countElement) {
                    // Format number (e.g., 1200 -> 1.2k)
                    const formattedCount = starCount >= 1000 
                      ? (starCount / 1000).toFixed(1) + 'k'
                      : starCount.toString();
                    countElement.textContent = formattedCount;
                  }
                })
                .catch(error => {
                  console.error('Error fetching GitHub star count:', error);
                  // Keep default 0 if fetch fails
                });
              
              // README Sidebar toggle
              const showReadmeBtn = document.getElementById('showReadmeBtn');
              const toggleReadmeBtn = document.getElementById('toggleReadmeBtn');
              const readmeSidebar = document.getElementById('readmeSidebar');
              
              if (showReadmeBtn && readmeSidebar) {
                showReadmeBtn.addEventListener('click', function() {
                  readmeSidebar.classList.add('open');
                  if (showReadmeBtn) showReadmeBtn.classList.add('hidden');
                });
              }
              
              if (toggleReadmeBtn && readmeSidebar) {
                toggleReadmeBtn.addEventListener('click', function() {
                  readmeSidebar.classList.remove('open');
                  if (showReadmeBtn) showReadmeBtn.classList.remove('hidden');
                });
              }
            });
          "))
        )
      )
    ),
    Home_UI(),
    # Use stable 'value' for each tab so programmatic navigation doesn't depend on
    # the translated title string. Titles are rendered via uiOutput so they
    # react to language changes while the tab 'value' stays constant.
    tabPanel(uiOutput("tab_dataviewer"), uiOutput("frame_dataviewer"), value = "dataviewR"),
    tabPanel(uiOutput("tab_pediviewer"), uiOutput("frame_pediviewer"), value = "pediviewer"),
    tabPanel(uiOutput("tab_genoviewer"), uiOutput("frame_genoviewer"), value = "genoviewer"),
    tabPanel(uiOutput("tab_easyblup"), uiOutput("frame_easyblup"), value = "easyblup"),
    tabPanel(uiOutput("tab_xmosome"), uiOutput("frame_xmosome"), value = "xmosome"),
    # (Removed bottom-right gear; using top-right gear aligned with title)
  )
}
