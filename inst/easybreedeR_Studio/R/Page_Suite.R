Home_UI <- function() {
  tabPanel(
    uiOutput("tab_home"),
    fluidPage(
      uiOutput("hero_content"),
      div(class = "container",
        div(class = "main-content-wrapper",
          div(class = "cards-container",
            div(class = "section-title",
              h2(
                tags$span(class = "material-symbols-outlined", "apps"),
                "Interactive Modules"
              )
            ),
            fluidRow(
              column(3,
                div(class = "app-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon blue",
                      tags$span(class = "material-symbols-outlined blue", "bar_chart")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("dataprevieweR"),
                      p("Interactive data exploration and visualization")
                    )
                  ),
                  actionButton("open_datapreviewR", "Launch", class = "btn-primary")
                )
              ),
              column(3,
                div(class = "app-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon green",
                      tags$span(class = "material-symbols-outlined green", "account_tree")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("PedivieweR"),
                      p("Pedigree viewer, pedigree quality control, inbreeding analysis, interactive visualization")
                    )
                  ),
                  actionButton("open_pediviewer", "Launch", class = "btn-primary")
                )
              ),
              column(3,
                div(class = "app-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon orange",
                      tags$span(class = "material-symbols-outlined orange", "scatter_plot")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("genovieweR"),
                      p("Genotype visualization and analysis")
                    )
                  ),
                  actionButton("open_genoviewer", "Launch", class = "btn-primary")
                )
              ),
              column(3,
                div(class = "app-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon red",
                      tags$span(class = "material-symbols-outlined red", "show_chart")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("easyblup"),
                      p("BLUPF90 parameter cards generator,genotype format conversion")
                    )
                  ),
                  actionButton("open_easyblup", "Launch", class = "btn-primary")
                )
              )
            ),
            fluidRow(
              column(3,
                div(class = "app-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon yellow",
                      tags$span(class = "material-symbols-outlined yellow", "bolt")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("R Canvas Workflow"),
                      p("R script pipeline management, visual R script pipeline management")
                    )
                  ),
                  actionButton("open_rnotebook", "Launch", class = "btn-primary")
                )
              )
            ),
            div(class = "section-title", style = "margin-top: 48px;",
              h2(
                tags$span(class = "material-symbols-outlined", "extension"),
                "Optional Dependencies"
              )
            ),
            fluidRow(
              column(6,
                div(class = "app-card dependency-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon purple",
                      tags$span(class = "material-symbols-outlined purple", "code")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("plinkR"),
                      p("R wrapper for PLINK/PLINK2 with automatic output capture, genotype reading, and BLUPF90 format conversion")
                    )
                  ),
                  tags$a(
                    href = "https://github.com/Thymine2001/plinkR",
                    target = "_blank",
                    class = "btn btn-primary",
                    style = "width: 100%; padding: 14px 24px; font-size: 16px; font-weight: 600; border-radius: 6px; text-decoration: none; display: block; text-align: center;",
                    "View on GitHub"
                  )
                )
              ),
              column(6,
                div(class = "app-card dependency-card",
                  div(class = "app-card-header",
                    div(class = "app-card-icon teal",
                      tags$span(class = "material-symbols-outlined teal", "settings")
                    ),
                    div(class = "app-card-title-wrapper",
                      h3("linkbreedeR"),
                      p("R package for running genetic analysis tools (BLUPF90+, PLINK, RENUMF90) directly from R")
                    )
                  ),
                  tags$a(
                    href = "https://github.com/Thymine2001/linkbreedeR",
                    target = "_blank",
                    class = "btn btn-primary",
                    style = "width: 100%; padding: 14px 24px; font-size: 16px; font-weight: 600; border-radius: 6px; text-decoration: none; display: block; text-align: center;",
                    "View on GitHub"
                  )
                )
              )
            ),
            div(class = "section-title", style = "margin-top: 48px;",
              h2(
                tags$span(class = "material-symbols-outlined", "library_books"),
                "Required Dependencies"
              )
            ),
            fluidRow(
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/rstudio/shiny",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "shiny")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/rstudio/bslib",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "bslib")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/rstudio/DT",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "DT")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/jeroen/jsonlite",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "jsonlite")
                  )
                )
              )
            ),
            fluidRow(
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/ropensci/plotly",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "plotly")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/thomasp85/shinyFiles",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "shinyFiles")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/r-lib/fs",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "fs")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/datastorm-open/visNetwork",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "visNetwork")
                  )
                )
              )
            ),
            fluidRow(
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/igraph/igraph",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "igraph")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/tidyverse/readxl",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "readxl")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/rstudio/reticulate",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "reticulate")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/irudnyts/openai",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "openai")
                  )
                )
              )
            ),
            fluidRow(
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/jeroen/curl",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "curl")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/r-lib/testthat",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "testthat")
                  )
                )
              ),
              column(3,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://github.com/Rpedigree/pedigreeTools",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "pedigreeTools")
                  )
                )
              )
            ),
            div(class = "section-title", style = "margin-top: 48px;",
              h2(
                tags$span(class = "material-symbols-outlined", "computer"),
                "Related Software"
              )
            ),
            fluidRow(
              column(6,
                div(class = "dependency-item",
                  tags$a(
                    href = "https://www.cog-genomics.org/plink/",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "PLINK")
                  )
                )
              ),
              column(6,
                div(class = "dependency-item",
                  tags$a(
                    href = "http://nce.ads.uga.edu/wiki/doku.php?id=readme.blupf90",
                    target = "_blank",
                    class = "dependency-link",
                    tags$span(class = "dependency-name", "BLUPF90")
                  )
                )
              )
            )
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
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Material+Symbols+Outlined:opsz,wght,FILL,GRAD@20..48,100..700,0..1,-50..200"),
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

  /* Container spacing from hero */
  .container {
    margin-top: 64px !important;
    margin-bottom: 48px;
    max-width: 100%;
    padding-left: 24px;
    padding-right: 24px;
  }

  /* Main content wrapper with sidebar */
  .main-content-wrapper {
    display: flex;
    gap: 24px;
    position: relative;
  }

  .cards-container {
    flex: 1;
    width: 100%;
  }

  /* Section title */
  .section-title {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 24px;
  }
  .section-title h2 {
    font-size: 28px;
    font-weight: 600;
    display: flex;
    align-items: center;
    gap: 8px;
    margin: 0;
    color: #000;
  }
  .section-title .material-symbols-outlined {
    color: #CEB888;
    font-size: 24px;
  }

  /* App cards styling - larger and more prominent */
  .app-card {
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 12px;
    padding: 32px 24px;
    margin-bottom: 0;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
    transition: all 0.3s ease;
    min-height: 320px;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    height: 100%;
  }
  
  /* Grid alignment for 2x4 layout */
  .cards-container .row {
    margin-left: 0;
    margin-right: 0;
  }
  
  /* Ensure columns align perfectly in grid - consistent padding for all columns */
  .cards-container .row > [class*="col-3"] {
    padding-left: 12px;
    padding-right: 12px;
  }
  
  /* First column in each row: no left padding */
  .cards-container .row > [class*="col-3"]:first-child {
    padding-left: 0;
  }
  
  /* Last column in each row: no right padding */
  .cards-container .row > [class*="col-3"]:last-child {
    padding-right: 0;
  }
  
  /* Ensure cards fill column height and align */
  .cards-container .row > [class*="col-3"] {
    display: flex;
    flex-direction: column;
  }
  
  .cards-container .row > [class*="col-3"] > .app-card {
    flex: 1;
    display: flex;
    flex-direction: column;
  }
  
  /* Row spacing */
  .cards-container .row + .row {
    margin-top: 24px;
  }
  .app-card:hover {
    box-shadow: 0 8px 16px rgba(0,0,0,0.15);
    transform: translateY(-2px);
  }
  .app-card-header {
    display: flex;
    align-items: flex-start;
    gap: 16px;
    margin-bottom: 20px;
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
  .app-card-title-wrapper {
    flex: 1;
    display: flex;
    flex-direction: column;
  }
  .app-card h3 {
    color: #000;
    font-size: 22px;
    font-weight: 700;
    margin-top: 0;
    margin-bottom: 8px;
    line-height: 1.2;
  }
  .app-card-header p {
    color: #4a4a4a;
    font-size: 14px;
    line-height: 1.5;
    margin: 0;
  }
  .app-card p {
    color: #4a4a4a;
    font-size: 16px;
    line-height: 1.6;
    margin-bottom: 24px;
    flex-grow: 1;
  }
  .app-card .btn-primary {
    margin-top: auto;
    width: 100%;
    padding: 14px 24px;
    font-size: 16px;
    font-weight: 600;
    border-radius: 6px;
  }
  
  /* Dependency cards styling */
  .dependency-card {
    margin-top: 0;
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
    background: #CEB888;
    color: #000;
    border-color: #CEB888;
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
    background-color: #CEB888 !important;
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
    padding: 20px;
    border-bottom: 2px solid #CFB991;
    display: flex;
    justify-content: space-between;
    align-items: center;
    background: linear-gradient(135deg, #CEB888 0%, #B89D5D 100%);
  }

  .readme-sidebar-header h3 {
    margin: 0;
    color: #000;
    font-size: 20px;
    font-weight: 700;
  }

  .readme-toggle-btn {
    background: transparent;
    border: none;
    cursor: pointer;
    padding: 4px;
    color: #000;
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
    border-bottom: 2px solid #CEB888;
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
    border-left: 3px solid #CEB888;
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
    color: #CEB888;
    text-decoration: none;
  }

  .readme-sidebar-content a:hover {
    text-decoration: underline;
  }

  .readme-sidebar-content p {
    margin-bottom: 12px;
  }

  .readme-sidebar-content blockquote {
    border-left: 3px solid #CEB888;
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
    background: #CEB888;
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
    background: #B89D5D;
    transform: translateY(-2px);
    box-shadow: 0 6px 16px rgba(0,0,0,0.2);
  }

  .show-readme-btn.hidden {
    display: none;
  }
'))
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
    tabPanel(uiOutput("tab_datapreviewer"), uiOutput("frame_datapreviewer"), value = "datapreviewR"),
    tabPanel(uiOutput("tab_pediviewer"), uiOutput("frame_pediviewer"), value = "pediviewer"),
    tabPanel(uiOutput("tab_genoviewer"), uiOutput("frame_genoviewer"), value = "genoviewer"),
    tabPanel(uiOutput("tab_easyblup"), uiOutput("frame_easyblup"), value = "easyblup"),
    tabPanel(uiOutput("tab_rnotebook"), uiOutput("frame_rnotebook"), value = "rnotebook"),
    # (Removed bottom-right gear; using top-right gear aligned with title)
  )
}
