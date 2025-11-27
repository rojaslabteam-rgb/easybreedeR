# Open BreedX (OBX) Clean Three-Panel Layout
# Version: 0.4.0 (Fixed Language Settings)
# Created: 2025-10-22
# Last Modified: 2025-10-31

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(jsonlite)
  library(uuid)
  
  # Optional deps (do NOT install at runtime to avoid blocking on load)
  HAS_SHINYFILES <- requireNamespace("shinyFiles", quietly = TRUE)
  HAS_FS <- requireNamespace("fs", quietly = TRUE)
  if (HAS_SHINYFILES) library(shinyFiles)
  if (HAS_FS) library(fs)
})

# Source shared language helpers
try(source(normalizePath(file.path("..", "Language.R"), winslash = "/", mustWork = FALSE), local = TRUE), silent = TRUE)

# Capture shared get_label before defining local alias
shared_get_label <- if (exists("get_label", mode = "function")) get_label else NULL

# App-local label resolver with app prefix rcw_
get_label_local <- function(key, lang = "EN") {
  lang_lower <- tolower(lang)
  gl <- shared_get_label
  if (is.null(gl) || !is.function(gl)) return(key)
  effective_key <- if (startsWith(key, "rcw_")) key else paste0("rcw_", key)
  result <- try(gl(effective_key, lang_lower, app = "rcw"), silent = TRUE)
  if (inherits(result, "try-error")) {
    result <- try(gl(key, lang_lower, app = "rcw"), silent = TRUE)
    if (inherits(result, "try-error")) return(key)
  }
  result
}

# Alias for compatibility
get_label <- get_label_local

# ============================
# Global Variables
# ============================
# Determine workspace path relative to app.R location
# Priority: inst/RCW/root (the actual location) > other locations
.rcw_workspace <- NULL

# Check multiple possible locations (in order of preference)
# IMPORTANT: Prioritize inst/RCW/root which is where files actually are
candidate_paths <- c(
  file.path(getwd(), "inst", "RCW", "root"),          # From project root: inst/RCW/root (PRIORITY)
  file.path(getwd(), "easybreedeR-main", "inst", "RCW", "root"),  # If in parent directory
  file.path(getwd(), "root"),                          # Same directory as current working dir (fallback)
  normalizePath(file.path("inst", "RCW", "root"), mustWork = FALSE),  # Direct inst/RCW/root
  normalizePath(file.path("root"), mustWork = FALSE)   # Direct relative path
)

# Find first existing root directory that has R files or is the correct structure
for (candidate in candidate_paths) {
  if (dir.exists(candidate)) {
    # Prefer inst/RCW/root structure
    if (grepl("inst[/\\\\]RCW[/\\\\]root", candidate, ignore.case = TRUE)) {
      .rcw_workspace <- normalizePath(candidate, winslash = "/")
      break
    }
    # Otherwise check if it has files
    r_files <- list.files(candidate, pattern = "\\.R$", recursive = TRUE, full.names = FALSE)
    if (length(r_files) > 0) {
      .rcw_workspace <- normalizePath(candidate, winslash = "/")
      break
    }
  }
}

# If no existing root found, create one in the correct location
if (is.null(.rcw_workspace)) {
  # Always prefer inst/RCW/root if we're in a package structure
  if (dir.exists("inst") && dir.exists("inst/RCW")) {
    .rcw_workspace <- normalizePath(file.path("inst", "RCW", "root"), winslash = "/", mustWork = FALSE)
  } else if (dir.exists("easybreedeR-main") && dir.exists("easybreedeR-main/inst/RCW")) {
    .rcw_workspace <- normalizePath(file.path("easybreedeR-main", "inst", "RCW", "root"), winslash = "/", mustWork = FALSE)
  } else {
    # Otherwise use current directory
    .rcw_workspace <- normalizePath(file.path(getwd(), "root"), winslash = "/", mustWork = FALSE)
  }
  
  # Create the directory if it doesn't exist
  if (!dir.exists(.rcw_workspace)) {
    dir.create(.rcw_workspace, recursive = TRUE, showWarnings = FALSE)
  }
  .rcw_workspace <- normalizePath(.rcw_workspace, winslash = "/")
}

.rcw_external_folders <- character(0)  # Store external folder paths

# ============================
# Helper Functions
# ============================
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Scan workspace for folders and R files
scan_workspace <- function(workspace_paths = c(.rcw_workspace, .rcw_external_folders)) {
  structure <- list(
    folders = list(),
    files = list()
  )
  
  # Filter out NULL or empty paths
  workspace_paths <- workspace_paths[!is.null(workspace_paths) & nzchar(workspace_paths)]
  
  for (workspace_path in workspace_paths) {
    if (is.null(workspace_path) || !nzchar(workspace_path) || !dir.exists(workspace_path)) {
      next
    }
    
    # Get all R files recursively
    all_files <- list.files(workspace_path, pattern = "\\.R$", 
                            recursive = TRUE, full.names = TRUE)
    
    for (file_path in all_files) {
      # Normalize paths for cross-platform compatibility
      workspace_norm <- gsub("\\\\", "/", normalizePath(workspace_path, winslash = "/", mustWork = FALSE))
      file_path_norm <- gsub("\\\\", "/", normalizePath(file_path, winslash = "/", mustWork = FALSE))
      
      # Extract relative path
      rel_path <- gsub(paste0("^", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", workspace_norm), "/?"), "", file_path_norm)
      folder <- dirname(rel_path)
      if (folder == "." || folder == "") folder <- "root"
      else folder <- paste0("root", "/", folder)
      
      file_info <- list(
        name = basename(file_path),
        path = file_path,
        rel_path = rel_path,
        folder = folder,
        workspace = workspace_path,
        size = file.size(file_path),
        modified = file.mtime(file_path)
      )
      
      structure$files[[length(structure$files) + 1]] <- file_info
      
      # Track folders
      if (!folder %in% structure$folders) {
        structure$folders[[length(structure$folders) + 1]] <- folder
      }
    }

    # Also include empty folders so newly created ones appear immediately
    all_dirs <- list.dirs(workspace_path, recursive = TRUE, full.names = TRUE)
    workspace_norm <- gsub("\\\\", "/", normalizePath(workspace_path, winslash = "/", mustWork = FALSE))
    for (dir_path in c(workspace_path, all_dirs)) {
      dir_path_norm <- gsub("\\\\", "/", normalizePath(dir_path, winslash = "/", mustWork = FALSE))
      rel_dir <- gsub(paste0("^", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", workspace_norm), "/?"), "", dir_path_norm)
      folder_label <- if (identical(rel_dir, "") || rel_dir == ".") "root" else paste0("root", "/", rel_dir)
      if (!folder_label %in% structure$folders) {
        structure$folders[[length(structure$folders) + 1]] <- folder_label
      }
    }
  }
  
  return(structure)
}

# Read R file content
read_r_file <- function(file_path) {
  if (!file.exists(file_path)) return("")
  paste(readLines(file_path, warn = FALSE), collapse = "\n")
}

# Read first line of R file
read_r_file_first_line <- function(file_path) {
  if (!file.exists(file_path)) return("")
  lines <- readLines(file_path, n = 1, warn = FALSE)
  if (length(lines) == 0) return("")
  # Remove leading/trailing whitespace
  trimws(lines[1])
}

# Write R file content
write_r_file <- function(file_path, content) {
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, file_path)
}

# Execute R code and capture output
execute_r_code <- function(code, env = new.env()) {
  output <- list(
    success = TRUE,
    result = NULL,
    output = "",
    error = NULL
  )
  
  # Capture output
  output_text <- capture.output({
    result <- tryCatch({
      eval(parse(text = code), envir = env)
    }, error = function(e) {
      output$success <<- FALSE
      output$error <<- e$message
      NULL
    })
    })
  output$output <- paste(output_text, collapse = "\n")
  output$result <- result
  
  return(output)
}

# Build hierarchical structure following canvas connection order
build_node_hierarchy <- function(nodes, edges) {
  if (length(nodes) == 0) return(list())
  
  all_node_ids <- names(nodes)
  
  # Build adjacency list maintaining edge order
  adj_list <- setNames(lapply(all_node_ids, function(id) character(0)), all_node_ids)
  in_degree <- setNames(rep(0, length(all_node_ids)), all_node_ids)
  
  # Preserve the order of edges as they appear in the canvas
  for (edge in edges) {
    adj_list[[edge$source]] <- c(adj_list[[edge$source]], edge$target)
    in_degree[[edge$target]] <- in_degree[[edge$target]] + 1
  }
  
  # Find root nodes (no incoming edges)
  root_nodes <- character(0)
  for (node_id in all_node_ids) {
    if (in_degree[[node_id]] == 0) {
      root_nodes <- c(root_nodes, node_id)
    }
  }
  
  # If no roots (circular graph), use all nodes
  if (length(root_nodes) == 0) {
    root_nodes <- all_node_ids
  }
  
  # Depth-first traversal following connection order
  sorted_nodes <- list()
  visited <- character(0)
  
  traverse <- function(node_id, level) {
    if (node_id %in% visited) return()
    visited <<- c(visited, node_id)
    
    sorted_nodes <<- c(sorted_nodes, list(list(
      id = node_id,
      node = nodes[[node_id]],
      level = level
    )))
    
    # Visit children in the order they were connected
    if (length(adj_list[[node_id]]) > 0) {
      for (child_id in adj_list[[node_id]]) {
        traverse(child_id, level + 1)
      }
    }
  }
  
  # Traverse from each root in order
  for (root_id in root_nodes) {
    traverse(root_id, 1)
  }
  
  # Add any unvisited nodes (isolated nodes)
  for (node_id in all_node_ids) {
    if (!(node_id %in% visited)) {
      sorted_nodes <- c(sorted_nodes, list(list(
        id = node_id,
        node = nodes[[node_id]],
        level = 1
      )))
    }
  }
  
  return(sorted_nodes)
}

# Generate R Markdown from sorted hierarchy
generate_rmd_from_hierarchy <- function(sorted_nodes, workspace_files) {
  rmd_lines <- c(
    "---",
    "title: \"RCW Pipeline Workflow\"",
    paste0("date: \"", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\""),
    "output: html_document",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
    "```",
    "",
    "# RCW Workflow Export",
    "",
    "This document was automatically generated from the RCW canvas workflow.",
    "Nodes are ordered according to the pipeline execution flow.",
    ""
  )
  if (length(sorted_nodes) == 0) {
    return(paste(c(rmd_lines, "", "_No nodes in workflow_", ""), collapse = "\n"))
  }
  
  # Add each node in topological order
  for (node_info in sorted_nodes) {
    level <- node_info$level
    node <- node_info$node
    
    # Create heading (level + 1 to start from ##)
    heading <- paste0(paste(rep("#", level + 1), collapse = ""), " ", node$fileName)
    rmd_lines <- c(rmd_lines, "", heading, "")
    
    # Find file content
    matching_file <- Find(function(f) f$rel_path == node$filePath, workspace_files)
    if (!is.null(matching_file)) {
      code_content <- tryCatch({
        read_r_file(matching_file$path)
      }, error = function(e) {
        paste("# Error reading file:", e$message)
      })
      
      # Add code chunk
      chunk_label <- gsub("[^a-zA-Z0-9_-]", "_", tools::file_path_sans_ext(node$fileName))
      rmd_lines <- c(rmd_lines, paste0("```{r ", chunk_label, "}"), code_content, "```", "")
    } else {
      rmd_lines <- c(rmd_lines, "_File not found in workspace_", "")
    }
  }
  
  return(paste(rmd_lines, collapse = "\n"))
}

# ============================
# UI Definition
# ============================
ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#333333",
    primary = "#CEB888",
    base_font = font_google("Crimson Text")
  ),
  
    tags$head(
    # jQuery UI for drag/drop
    tags$link(rel = "stylesheet", href = "https://code.jquery.com/ui/1.13.2/themes/base/jquery-ui.min.css"),
    tags$script(src = "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js"),
    # jsPlumb CDN
    tags$script(src = "https://cdn.jsdelivr.net/npm/jsplumb@2.15.6/dist/js/jsplumb.min.js"),
    # html2canvas for PNG export (fallback)
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/1.4.1/html2canvas.min.js"),
    # dom-to-image for better SVG support
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/dom-to-image/0.11.0/dom-to-image.min.js"),
    
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
    }
    .title-bar p { margin: 5px 0 0 0; font-size: 1rem; color: #000; opacity: .9; }
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
    .left-panel { border-right: 2px solid #CFB991; }
    .right-panel { border-left: 2px solid #CFB991; }
    .left-panel.hidden, .right-panel.hidden { width: 0; padding: 0; overflow: hidden; }

    .center-panel {
      flex: 1; overflow: hidden;
      padding: 0; background: #FFFFFF;
      position: relative;
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

    .panel-section {
      background: linear-gradient(135deg,#FFFFFF 0%,#F8F9FA 100%);
      border: 2px solid #CFB991; border-radius: 8px;
      padding: 15px; margin-bottom: 20px;
      box-shadow: 0 2px 8px rgba(206,184,136,.15);
    }
    .section-title {
      font-size: 1.1rem; font-weight: 700; color: #2c3e50;
      margin: 0 0 12px 0; padding-bottom: 8px; border-bottom: 2px solid #CEB888;
    }

    /* Folder/File Tree */
    .file-tree {
      margin: 0; padding: 0; list-style: none;
    }
    .file-tree-item {
      padding: 8px 10px; margin: 4px 0;
      background: #fff; border: 1px solid #e0e0e0;
      border-radius: 4px; cursor: pointer;
      transition: all 0.2s;
      user-select: none;
    }
    .file-tree-item:hover {
      background: #FFF9F0; border-color: #CEB888;
    }
    .file-tree-item.folder {
      background: #f8f9fa; font-weight: 600;
      border-left: 4px solid #CEB888;
      display: flex; align-items: center;
      justify-content: space-between;
    }
    .folder-header {
      display: flex; align-items: center; flex: 1;
      min-width: 0;
    }
    .folder-name {
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      flex: 1;
    }
    .folder-toggle {
      font-size: 0.8rem; color: #666;
      margin-right: 6px; transition: transform 0.2s;
      display: inline-block; width: 16px;
      flex-shrink: 0;
    }
    .folder-toggle.collapsed {
      transform: rotate(-90deg);
    }
    .folder-files {
      margin-left: 20px; overflow: hidden;
      transition: max-height 0.3s ease-out, opacity 0.3s ease-out;
    }
    .folder-files.collapsed {
      max-height: 0 !important; opacity: 0;
    }
    .file-tree-item.file {
      padding-left: 24px;
      cursor: grab;
      display: flex;
      align-items: center;
    }
    .file-tree-item.file:active {
      cursor: grabbing;
    }
    .file-tree-item.file.selected {
      background: #FFF5E6;
      border-color: #B89D5D;
    }
    .file-name {
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
      flex: 1;
    }
    .file-icon {
      margin-right: 8px;
      flex-shrink: 0;
    }
    
    /* File Tree Context Menu */
    .file-tree-context-menu {
      position: fixed;
      background: #FFFFFF;
      border: 2px solid #CEB888;
      border-radius: 8px;
      box-shadow: 0 4px 16px rgba(0,0,0,0.2);
      padding: 8px 0;
      z-index: 9999;
      min-width: 160px;
      display: none;
    }
    .file-tree-context-menu-item {
      padding: 10px 16px;
      cursor: pointer;
      color: #2c3e50;
      font-size: 0.9rem;
      border-bottom: 1px solid #F0F0F0;
    }
    .file-tree-context-menu-item:last-child {
      border-bottom: none;
    }
    .file-tree-context-menu-item:hover {
      background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%);
      color: #B89D5D;
    }
    .file-tree-context-menu-item.danger:hover {
      background: #ffebee;
      color: #c62828;
    }

    /* Canvas */
    #canvas {
      width: 100%; height: 100%;
      background: #FAFAFA;
      position: relative;
      overflow: hidden;
    }

    /* Canvas nodes */
    .canvas-node {
      position: absolute;
      width: 200px; min-height: 100px;
      background: linear-gradient(135deg, #FFFFFF 0%, #F8F9FA 100%);
      border: 3px solid #CEB888; border-radius: 10px;
      padding: 12px; cursor: move;
      box-shadow: 0 4px 12px rgba(0,0,0,0.15);
      z-index: 100;
    }
    .canvas-node:hover {
      border-color: #B89D5D;
      box-shadow: 0 6px 16px rgba(0,0,0,0.25);
    }
    .canvas-node.selected {
      border-color: #B89D5D;
      background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%);
      box-shadow: 0 6px 20px rgba(184,157,93,0.5);
    }
    
    /* Connection selection style */
    .connection-selected {
      stroke-width: 4 !important;
      stroke: #CEB888 !important;
      filter: drop-shadow(0 0 3px rgba(206,184,136,0.6));
    }
    .node-title { 
      font-weight: 700; color: #2c3e50; 
      font-size: 0.95rem; margin-bottom: 4px;
      white-space: nowrap; overflow: hidden;
      text-overflow: ellipsis;
    }
    .node-info { 
      font-size: 0.75rem; color: #666;
      white-space: nowrap; overflow: hidden;
      text-overflow: ellipsis;
    }
    
    /* jsPlumb endpoints */
    .jtk-endpoint { z-index: 200; }
    .jtk-connector { z-index: 50; }

    /* Context Menu */
    .context-menu {
      position: fixed;
      background: #FFFFFF;
      border: 2px solid #CEB888;
      border-radius: 8px;
      box-shadow: 0 4px 16px rgba(0,0,0,0.2);
      padding: 8px 0;
      z-index: 9999;
      min-width: 180px;
      display: none;
    }
    .context-menu-item {
      padding: 10px 16px;
      cursor: pointer;
      color: #2c3e50;
      font-size: 0.9rem;
      border-bottom: 1px solid #F0F0F0;
      position: relative;
    }
    .context-menu-item:last-child {
      border-bottom: none;
    }
    .context-menu-item:hover {
      background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%);
      color: #B89D5D;
    }
    .context-menu-item.has-submenu::after {
      content: '▶';
      float: right;
      font-size: 0.7rem;
      color: #999;
    }
    .context-submenu {
      position: absolute;
      left: 100%;
      top: 0;
      background: #FFFFFF;
      border: 2px solid #CEB888;
      border-radius: 8px;
      box-shadow: 0 4px 16px rgba(0,0,0,0.2);
      padding: 8px 0;
      min-width: 160px;
      display: none;
      margin-left: 4px;
      z-index: 10000;
    }
    .context-menu-item:hover > .context-submenu {
      display: block;
    }
    .context-submenu-item {
      padding: 8px 16px;
      cursor: pointer;
      color: #2c3e50;
      font-size: 0.85rem;
      border-bottom: 1px solid #F0F0F0;
    }
    .context-submenu-item:last-child {
      border-bottom: none;
    }
    .context-submenu-item:hover {
      background: linear-gradient(135deg, #FFF9F0 0%, #FFF5E6 100%);
      color: #B89D5D;
    }

    /* Buttons */
    .btn, .btn-sm {
      transition: all 0.2s ease;
      cursor: pointer;
      border-radius: 6px;
      font-weight: 600;
      border: 1px solid transparent;
      box-shadow: none;
    }
    .btn:hover, .btn-sm:hover {
      transform: translateY(-1px);
      box-shadow: 0 6px 14px rgba(0,0,0,0.15);
      opacity: 1;
    }
    .btn:active, .btn-sm:active {
      transform: translateY(0);
      box-shadow: 0 3px 8px rgba(0,0,0,0.12);
    }
    .btn:focus, .btn-sm:focus { outline: none; box-shadow: 0 0 0 4px rgba(206,184,136,0.35); }

    /* Primary (match easyblup) */
    .btn-primary, .btn.btn-primary {
      background-color: #CEB888 !important;
      border-color: #CEB888 !important;
      color: #000 !important;
      font-weight: 600;
    }
    .btn-primary:hover, .btn.btn-primary:hover {
      background-color: #B89D5D !important;
      border-color: #B89D5D !important;
      color: #000 !important;
    }

    /* Secondary (match easyblup grey) */
    .btn-secondary, .btn.btn-secondary {
      background-color:#95A5A6 !important;
      border-color:#95A5A6 !important;
      color:#fff !important;
    }
    .btn-secondary:hover, .btn.btn-secondary:hover {
      background-color:#7F8C8D !important;
      border-color:#7F8C8D !important;
    }
    .btn-row { display: flex; gap: 8px; margin-bottom: 12px; flex-wrap: wrap; }
    
    /* Code Editor Modal */
    .code-editor-area {
      width: 100%; min-height: 400px;
      font-family: 'Courier New', monospace;
      font-size: 13px; padding: 12px;
      border: 2px solid #CFB991; border-radius: 4px;
      background: #f8f9fa;
    }
    
    /* Preview area with scrollbar */
    #rmd_preview {
      max-height: 500px;
      overflow-y: auto;
      overflow-x: auto;
      white-space: pre;
      font-family: 'Courier New', monospace;
      font-size: 12px;
      background: #f8f9fa;
      border: 1px solid #dee2e6;
      border-radius: 4px;
      padding: 12px;
    }
    #rmd_preview::-webkit-scrollbar {
      width: 8px;
      height: 8px;
    }
    #rmd_preview::-webkit-scrollbar-track {
      background: #f1f1f1;
      border-radius: 4px;
    }
    #rmd_preview::-webkit-scrollbar-thumb {
      background: #CEB888;
      border-radius: 4px;
    }
    #rmd_preview::-webkit-scrollbar-thumb:hover {
      background: #B89D5D;
    }
  ")),
      # Additional node-shape styles
      tags$style(HTML("\
        /* Node shape modifiers applied to .canvas-node */\
        .canvas-node.node-shape-rounded { border-radius: 12px !important; }\
        .canvas-node.node-shape-rectangle { border-radius: 3px !important; }\
        .canvas-node.node-shape-circle { border-radius: 50% !important; width: 120px !important; height: 120px !important; display: flex !important; align-items: center !important; justify-content: center !important; padding: 12px !important; }\
        .canvas-node.node-shape-circle .node-title { white-space: normal; text-align: center; }\
        .canvas-node.node-shape-circle .node-info { display: none !important; }\
        /* Square and Diamond shapes for per-node appearance */\
        .canvas-node.node-shape-square { border-radius: 6px !important; width: 160px !important; height: 64px !important; display: flex !important; align-items: center !important; justify-content: center !important; padding: 8px !important; }\
  .canvas-node.node-shape-diamond { width: 110px !important; height: 110px !important; /* use clip-path to form a diamond without rotating the element so anchors align */ clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%); display: flex !important; align-items: center !important; justify-content: center !important; position: relative !important; }\
  /* Draw a matching diamond outline using a pseudo-element so the border follows the clip-path */\
  .canvas-node.node-shape-diamond::before { content: ''; position: absolute; left: 0; top: 0; right: 0; bottom: 0; box-sizing: border-box; pointer-events: none; clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%); border: 3px solid #CEB888; border-radius: 0; z-index: 0; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }\
  /* Ensure inner content sits above the outline */\
  .canvas-node.node-shape-diamond > * { position: relative; z-index: 1; }\
  .canvas-node.node-shape-diamond .node-title, .canvas-node.node-shape-diamond .node-info { transform: none; text-align: center; }\
  /* Hover/selected states for diamond outline */\
  .canvas-node.node-shape-diamond:hover::before { border-color: #B89D5D; box-shadow: 0 6px 16px rgba(0,0,0,0.25); }\
  .canvas-node.selected.node-shape-diamond::before { border-color: #B89D5D; box-shadow: 0 6px 20px rgba(184,157,93,0.5); }\
      ")),
    
    # jsPlumb initialization (similar to OBX Canvas)
    tags$script(HTML("
    var jsPlumbInstance;
    var canvasNodes = {};

    // Minimal client-side canvas settings (per-node appearance is handled per-node)
    var canvasSettings = { allow_vertical: true };

    // Receive limited messages from server
    if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
      // Allow server to request an immediate sync of connections
      Shiny.addCustomMessageHandler('request_graph_sync', function(msg) {
        try { syncConnections(); } catch (e) { console.warn('graph sync failed', e); }
      });
      // Server can instruct client to apply appearance to a specific node
      Shiny.addCustomMessageHandler('apply_node_appearance', function(msg) {
        try { applyNodeAppearance(msg.id, msg.appearance); } catch (e) { console.warn('apply appearance failed', e); }
      });
    }
    
    $(document).ready(function() {
      jsPlumbInstance = jsPlumb.getInstance({
        Container: 'canvas',
        Connector: ['Bezier', { curviness: 50 }],
        // smaller endpoints to match request
        Endpoint: ['Dot', { radius: 5 }],
        EndpointStyle: { fill: '#CEB888', stroke: '#B89D5D', strokeWidth: 1 },
        PaintStyle: { stroke: '#B89D5D', strokeWidth: 2 },
        HoverPaintStyle: { stroke: '#CEB888', strokeWidth: 3 },
        ConnectionOverlays: [
          ['Arrow', { location: 1, width: 16, length: 16 }]
        ]
      });

      // Validate drops so only directional connections (source->target) are allowed
      jsPlumbInstance.bind('beforeDrop', function(info) {
        try {
          // Prevent self-connection
          if (info.sourceId === info.targetId) return false;

          var sEP = info.sourceEndpoint;
          var tEP = info.targetEndpoint;

          // If endpoints are present, ensure source endpoint is a source and target endpoint is a target
          if (sEP && tEP) {
            if (sEP.isSource === true && tEP.isTarget === true) return true;
            return false;
          }

          // Fallback: allow only if sourceId != targetId
          return info.sourceId !== info.targetId;
        } catch (e) {
          console.warn('beforeDrop validation failed', e);
          return false;
        }
      });

      jsPlumbInstance.bind('connection', function(info) {
        syncConnections();
        try { attachConnectionContextMenu(info.connection); } catch (e) { console.warn('attach menu failed', e); }
      });
      jsPlumbInstance.bind('connectionDetached', function(info) {
        syncConnections();
        // Clear selection if deleted connection was selected
        if (window.selectedConnection === info.connection) {
          window.selectedConnection = null;
        }
      });

      $('#canvas').droppable({
        accept: '.file-tree-item.file',
        drop: function(event, ui) {
          var filePath = ui.draggable.data('filepath');
          var fileName = ui.draggable.data('filename');
          var offset = $(this).offset();
          var x = ui.offset.left - offset.left;
          var y = ui.offset.top - offset.top;
          
          var nodeId = crypto.randomUUID();
          createCanvasNode(nodeId, filePath, fileName, x, y, '', null);
          
          Shiny.setInputValue('node_created', {
            id: nodeId, 
            filePath: filePath, 
            fileName: fileName,
            x: x, 
            y: y
          }, {priority: 'event'});
        }
      });
      
      // Click on canvas to deselect all
      $('#canvas').on('click', function(e) {
        // Only deselect if clicking directly on canvas (not on nodes or connections)
        if ($(e.target).is('#canvas') || $(e.target).closest('.canvas-node').length === 0) {
          // Check if clicked on a connection SVG element
          var clickedOnConnection = false;
          try {
            var svgs = $('#canvas svg');
            svgs.each(function() {
              var svgRect = this.getBoundingClientRect();
              if (e.clientX >= svgRect.left && e.clientX <= svgRect.right &&
                  e.clientY >= svgRect.top && e.clientY <= svgRect.bottom) {
                clickedOnConnection = true;
                return false; // break
              }
            });
          } catch(e) {}
          
          if (!clickedOnConnection) {
            $('.canvas-node').removeClass('selected');
            if (window.selectedConnection) {
              try {
                var connEl = window.selectedConnection.canvas || (window.selectedConnection.getConnector && window.selectedConnection.getConnector().canvas);
                if (connEl) $(connEl).removeClass('connection-selected');
              } catch(e) {}
              window.selectedConnection = null;
            }
            Shiny.setInputValue('node_selected', {id: null, filePath: null}, {priority: 'event'});
          }
        }
      });
      
      // Keyboard event handler for Delete/Backspace
      $(document).on('keydown', function(e) {
        // Only handle if not typing in an input field
        var target = $(e.target);
        if (target.is('input') || target.is('textarea') || target.attr('contenteditable') === 'true') {
          return;
        }
        
        // Handle Delete or Backspace key
        if (e.key === 'Delete' || e.key === 'Backspace') {
          e.preventDefault();
          
          // Delete selected node
          var selectedNode = $('.canvas-node.selected');
          if (selectedNode.length > 0) {
            var nodeId = selectedNode.attr('id');
            var nodeData = canvasNodes[nodeId];
            if (nodeData) {
              Shiny.setInputValue('node_delete', {
                id: nodeId,
                filePath: nodeData.filePath,
                fileName: nodeData.fileName
              }, {priority: 'event'});
            }
          }
          
          // Delete selected connection
          if (window.selectedConnection) {
            try {
              jsPlumbInstance.deleteConnection(window.selectedConnection);
              syncConnections();
              window.selectedConnection = null;
            } catch(e) {
              console.warn('Error deleting connection:', e);
            }
          }
        }
      });

      initFileDraggable();
      initFolderToggle();

      // Attach context menus to any pre-existing connections (after restore, etc.)
      setTimeout(function(){
        try {
          jsPlumbInstance.getConnections().forEach(function(c){ attachConnectionContextMenu(c); });
        } catch(e) { console.warn('initial attach failed', e); }
      }, 300);
    });

    function initFileDraggable() {
      setTimeout(function() {
        $('.file-tree-item.file').draggable({
          helper: 'clone',
          revert: 'invalid',
          zIndex: 1000,
          appendTo: 'body',
          cursorAt: { top: 20, left: 40 }
        });
        // Multi-select support with Cmd/Ctrl
        $('.file-tree-item.file').off('click.select').on('click.select', function(e) {
          if (e.metaKey || e.ctrlKey) {
            $(this).toggleClass('selected');
          } else {
            $('.file-tree-item.file.selected').removeClass('selected');
            $(this).addClass('selected');
          }
          var selected = $('.file-tree-item.file.selected').map(function(){ return $(this).data('filepath'); }).get();
          Shiny.setInputValue('file_tree_selection', selected);
        });
      }, 300);
    }

    function initFolderToggle() {
      setTimeout(function() {
        $('.file-tree-item.folder').off('click').on('click', function(e) {
          e.stopPropagation();
          var folderId = $(this).data('folder-id');
          var filesContainer = $('#folder-files-' + folderId);
          var toggle = $(this).find('.folder-toggle');
          
          if (filesContainer.hasClass('collapsed')) {
            filesContainer.removeClass('collapsed');
            filesContainer.css('max-height', filesContainer[0].scrollHeight + 'px');
            toggle.removeClass('collapsed');
          } else {
            filesContainer.addClass('collapsed');
            filesContainer.css('max-height', '0');
            toggle.addClass('collapsed');
          }
        });
        
        // Initialize max-height for all folder containers
        $('.folder-files').each(function() {
          if (!$(this).hasClass('collapsed')) {
            $(this).css('max-height', this.scrollHeight + 'px');
          }
        });
      }, 300);
    }
    
    function initFileTreeContextMenu() {
      setTimeout(function() {
        // File right-click
        $('.file-tree-item.file').off('contextmenu').on('contextmenu', function(e) {
          e.preventDefault();
          var filePath = $(this).data('filepath');
          var fileName = $(this).data('filename');
          showFileTreeContextMenu(e.pageX, e.pageY, 'file', filePath, fileName);
          return false;
        });
        
        // Folder right-click
        $('.file-tree-item.folder').off('contextmenu').on('contextmenu', function(e) {
          e.preventDefault();
          var folderId = $(this).data('folder-id');
          // Use the exact folder label from the title attribute to avoid extra characters
          var folderName = $(this).find('.folder-name').attr('title') || $(this).find('.folder-name').text().trim();
          showFileTreeContextMenu(e.pageX, e.pageY, 'folder', folderId, folderName);
          return false;
        });
      }, 300);
    }
    
    function showFileTreeContextMenu(x, y, type, path, name) {
      var menu = $('#fileTreeContextMenu');
      if (menu.length === 0) {
        menu = $('<div id=\"fileTreeContextMenu\" class=\"file-tree-context-menu\"></div>');
        $('body').append(menu);
      }
      
      var html = '';
      if (type === 'file') {
        html = '<div class=\"file-tree-context-menu-item\" data-action=\"edit-file\" data-path=\"' + path + '\" data-name=\"' + name + '\">' +
               '  Edit File' +
               '</div>' +
               '<div class=\"file-tree-context-menu-item\" data-action=\"rename-file\" data-path=\"' + path + '\" data-name=\"' + name + '\">' +
               '  Rename File' +
               '</div>' +
               '<div class=\"file-tree-context-menu-item danger\" data-action=\"delete-file\" data-path=\"' + path + '\">' +
               '  Delete File' +
               '</div>';
        var selected = $('.file-tree-item.file.selected').length;
        if (selected > 1) {
          html += '<div class=\"file-tree-context-menu-item danger\" data-action=\"delete-files-selected\">' +
                  '  Delete Selected (' + selected + ')' +
                  '</div>';
        }
      } else if (type === 'folder') {
        html = '<div class=\"file-tree-context-menu-item\" data-action=\"rename-folder\" data-name=\"' + name + '\">' +
               '  Rename Folder' +
               '</div>' +
               '<div class=\"file-tree-context-menu-item danger\" data-action=\"delete-folder\" data-name=\"' + name + '\">' +
               '  Delete Folder' +
               '</div>';
      }
      
      menu.html(html);
      menu.css({ left: x + 'px', top: y + 'px', display: 'block' });
      
      menu.find('.file-tree-context-menu-item').off('click').on('click', function() {
        var action = $(this).data('action');
        var itemPath = $(this).data('path');
        var itemName = $(this).data('name');
        
        if (action === 'edit-file') {
          Shiny.setInputValue('edit_file_tree', { path: itemPath, name: itemName }, {priority: 'event'});
        } else if (action === 'rename-file') {
          Shiny.setInputValue('rename_file', { path: itemPath, name: itemName }, {priority: 'event'});
        } else if (action === 'delete-file') {
          Shiny.setInputValue('delete_file', { path: itemPath }, {priority: 'event'});
        } else if (action === 'delete-files-selected') {
          var selected = $('.file-tree-item.file.selected').map(function(){ return $(this).data('filepath'); }).get();
          Shiny.setInputValue('delete_files', { paths: selected }, {priority: 'event'});
        } else if (action === 'rename-folder') {
          Shiny.setInputValue('rename_folder', { name: itemName }, {priority: 'event'});
        } else if (action === 'delete-folder') {
          Shiny.setInputValue('delete_folder', { name: itemName }, {priority: 'event'});
        }
        
        hideFileTreeContextMenu();
      });
    }
    
    function hideFileTreeContextMenu() {
      $('#fileTreeContextMenu').hide();
    }
    
    $(document).on('click', function(e) {
      if (!$(e.target).closest('.file-tree-context-menu').length) {
        hideFileTreeContextMenu();
      }
    });

    $(document).on('shiny:value', function(event) {
      if (event.name === 'file_tree_ui') {
        initFileDraggable();
        initFolderToggle();
        initFileTreeContextMenu();
      }
    });

    function applyNodeAppearance(nodeId, appearance) {
      if (!nodeId || !appearance) return;
      var el = $('#' + nodeId);
      if (!el.length) return;
      // remove old shape classes
      el.removeClass('node-shape-rounded node-shape-rectangle node-shape-circle node-shape-square node-shape-diamond');
      var shape = appearance.shape || 'square';
      if (shape === 'diamond') {
        el.addClass('node-shape-diamond');
      } else if (shape === 'circle') {
        el.addClass('node-shape-circle');
      } else {
        el.addClass('node-shape-square');
      }

      if (appearance.color) {
        el.css('background', appearance.color);
      }

      // Apply title color if specified
      if (appearance.titleColor) {
        el.find('.node-title').css('color', appearance.titleColor);
      }

      // We use clip-path for diamond, so do not rotate inner text; ensure neutral transform
      el.find('.node-title, .node-info').css('transform', 'none');

      // Rebuild endpoints to match the new shape (diamond uses corners)
      try { setNodeEndpoints(nodeId, shape); } catch (e) { console.warn('set endpoints failed', e); }

      if (canvasNodes[nodeId]) {
        canvasNodes[nodeId].appearance = appearance;
      }
    }

    // Helper: (re)build endpoints for a node according to its shape
    function setNodeEndpoints(id, shape) {
      // Preserve connection descriptors for this node so we can restore them after endpoints are rebuilt.
      var savedConns = [];
      try {
        var connsSrc = jsPlumbInstance.getConnections({ source: id }) || [];
        var connsTgt = jsPlumbInstance.getConnections({ target: id }) || [];
        var all = connsSrc.concat(connsTgt);
        all.forEach(function(c) {
          try {
            var srcAnchor = (c.endpoints && c.endpoints[0] && c.endpoints[0].anchor) || null;
            var tgtAnchor = (c.endpoints && c.endpoints[1] && c.endpoints[1].anchor) || null;
            // try to capture connector info if available
            var connectorType = null;
            try { connectorType = (c.getConnector && c.getConnector().type) || (c.connector && c.connector.type) || null; } catch (e) { connectorType = null; }
            savedConns.push({ source: c.sourceId, target: c.targetId, sourceAnchor: srcAnchor, targetAnchor: tgtAnchor, connectorType: connectorType, hasArrow: !!(c.getOverlay && c.getOverlay('arrow')) });
          } catch (ee) { /* ignore per-connection */ }
        });
        // remove duplicates (same pair may appear twice)
        var uniq = {};
        savedConns = savedConns.filter(function(d) {
          var k = d.source + '->' + d.target;
          if (uniq[k]) return false; uniq[k] = true; return true;
        });
      } catch (e) { savedConns = []; }

      try { jsPlumbInstance.removeAllEndpoints(id); } catch (e) {}

      var epOpts = { endpoint: ['Dot', { radius: 5 }], paintStyle: { fill: '#CEB888', stroke: '#B89D5D', strokeWidth: 1 }, hoverPaintStyle: { fill: '#CEB888' }, maxConnections: -1 };
      // For diamond we want endpoints at the visual corners. Use proportional anchors near corners.
      if (shape === 'diamond') {
        var anchors = [ [0.5,0,0, -1], [1,0.5,1,0], [0.5,1,0,1], [0,0.5,-1,0] ];
        anchors.forEach(function(a){ try { jsPlumbInstance.addEndpoint(id, Object.assign({ anchor: a, isSource: true, isTarget: true }, epOpts)); } catch(e){} });
      } else if (shape === 'circle') {
        var anchors = ['Top','Bottom','Left','Right'];
        anchors.forEach(function(a){ try { jsPlumbInstance.addEndpoint(id, Object.assign({ anchor: a, isSource: true, isTarget: true }, epOpts)); } catch(e){} });
      } else {
        var anchors = ['Top','Bottom','Left','Right'];
        anchors.forEach(function(a){ try { jsPlumbInstance.addEndpoint(id, Object.assign({ anchor: a, isSource: true, isTarget: true }, epOpts)); } catch(e){} });
      }

      // Reconnect saved connections after a short delay to allow endpoints to exist
      if (savedConns.length > 0) {
        setTimeout(function() {
          savedConns.forEach(function(d) {
            try {
              // Avoid creating duplicate connections if still present
              var exists = jsPlumbInstance.getConnections({ source: d.source, target: d.target });
              if (exists && exists.length > 0) return;
              var overlays = d.hasArrow ? [['Arrow', { id: 'arrow', location: 1, width: 16, length: 16 }]] : [];
              var connectOpts = { source: d.source, target: d.target, overlays: overlays };
              // restore anchors if we captured them
              try {
                if (d.sourceAnchor || d.targetAnchor) {
                  connectOpts.anchors = [d.sourceAnchor || null, d.targetAnchor || null];
                }
              } catch (e) {}
              // restore connector type when available (best-effort)
              try {
                if (d.connectorType) {
                  // common connector types: 'Bezier','Straight','Flowchart'
                  if (d.connectorType.toLowerCase && d.connectorType.toLowerCase().indexOf('bezier') !== -1) connectOpts.connector = ['Bezier', { curviness: 50 }];
                  else if (d.connectorType.toLowerCase && d.connectorType.toLowerCase().indexOf('flowchart') !== -1) connectOpts.connector = ['Flowchart', { cornerRadius: 6 }];
                  else if (d.connectorType.toLowerCase && d.connectorType.toLowerCase().indexOf('straight') !== -1) connectOpts.connector = 'Straight';
                }
              } catch (e) {}

              var conn = jsPlumbInstance.connect(connectOpts);
              try { attachConnectionContextMenu(conn); } catch (e) {}
            } catch (e) { /* ignore reconnect errors */ }
          });
          try { jsPlumbInstance.repaintEverything(); } catch (e) {}
        }, 40);
      }
    }

    function createCanvasNode(id, filePath, fileName, x, y, firstLine, appearance) {
      var node = $('<div>')
        .attr('id', id)
        .addClass('canvas-node node-shape-square')
        .css({ left: x + 'px', top: y + 'px', background: '#ffffff' })
        .html(
          '<div class=\"node-title\">' + fileName + '</div>'
        );

      $('#canvas').append(node);
      var defaultAppearance = appearance || { shape: 'square', color: '#ffffff', titleColor: '#2c3e50' };
      canvasNodes[id] = { filePath: filePath, fileName: fileName, x: x, y: y, firstLine: firstLine || '', appearance: defaultAppearance };
      
      // Apply appearance (including title color)
      applyNodeAppearance(id, defaultAppearance);

      jsPlumbInstance.draggable(id, {
        containment: 'parent',
        stop: function(params) {
          Shiny.setInputValue('node_moved', {
            id: id,
            x: params.pos[0],
            y: params.pos[1]
          }, {priority: 'event'});
        }
      });

  // Build endpoints according to default appearance
  try { setNodeEndpoints(id, defaultAppearance.shape); } catch(e){}

      // Persist default appearance to server
      try {
        Shiny.setInputValue('node_set_appearance', { id: id, color: defaultAppearance.color, shape: defaultAppearance.shape }, {priority: 'event'});
      } catch (e) {}

      $('#' + id).on('click', function(e) {
        if (e.button !== 0) return;
        e.stopPropagation(); // Prevent canvas click from deselecting
        $('.canvas-node').removeClass('selected');
        $(this).addClass('selected');
        // Clear connection selection when node is selected
        if (window.selectedConnection) {
          try {
            var connEl = window.selectedConnection.canvas || (window.selectedConnection.getConnector && window.selectedConnection.getConnector().canvas);
            if (connEl) $(connEl).removeClass('connection-selected');
          } catch(e) {}
          window.selectedConnection = null;
        }
        Shiny.setInputValue('node_selected', {id: id, filePath: filePath}, {priority: 'event'});
      });
      
      $('#' + id).on('contextmenu', function(e) {
        e.preventDefault();
        $('.canvas-node').removeClass('selected');
        $(this).addClass('selected');
        showContextMenu(e.pageX, e.pageY, id, filePath, fileName);
        return false;
      });
    }

    var currentContextNode = null;
    
    function showContextMenu(x, y, nodeId, filePath, fileName) {
      currentContextNode = { id: nodeId, filePath: filePath, fileName: fileName };
      
      var menu = $('#contextMenu');
      if (menu.length === 0) {
        menu = $('<div id=\"contextMenu\" class=\"context-menu\"></div>');
        $('body').append(menu);
      }
      
      menu.html(
        '<div class=\"context-menu-item\" data-action=\"edit\">' +
        '  Edit Code' +
        '</div>' +
        '<div class=\"context-menu-item\" data-action=\"run\">' +
        '  Run This Script' +
        '</div>' +
        '<div class=\"context-menu-item\" data-action=\"delete\">' +
        '  Remove from Canvas' +
        '</div>' +
        '<div style=\"border-top: 1px solid #e0e0e0; margin: 4px 0;\"></div>' +
        '<div class=\"context-menu-item has-submenu\">' +
        '  Shape' +
        '  <div class=\"context-submenu\">' +
        '    <div class=\"context-submenu-item\" data-action=\"shape-square\">Square</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"shape-diamond\">Diamond</div>' +
        '  </div>' +
        '</div>' +
        '<div class=\"context-menu-item has-submenu\">' +
        '  Text Color' +
        '  <div class=\"context-submenu\">' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-red\">Red</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-green\">Green</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-blue\">Blue</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-yellow\">Yellow</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-black\">Black</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-orange\">Orange</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"title-color-purple\">Purple</div>' +
        '  </div>' +
        '</div>' +
        '<div class=\"context-menu-item has-submenu\">' +
        '  Background Color' +
        '  <div class=\"context-submenu\">' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-red\">Red</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-green\">Green</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-blue\">Blue</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-yellow\">Yellow</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-white\">White</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-orange\">Orange</div>' +
        '    <div class=\"context-submenu-item\" data-action=\"bg-color-purple\">Purple</div>' +
        '  </div>' +
        '</div>'
      );
      
      menu.css({ left: x + 'px', top: y + 'px', display: 'block' });
      
      // Handle main menu items (non-submenu)
      menu.find('.context-menu-item').off('click').on('click', function(e) {
        var action = $(this).data('action');
        if (action && !$(this).hasClass('has-submenu')) {
          handleContextMenuAction(action);
          hideContextMenu();
        }
        e.stopPropagation();
      });
      
      // Handle submenu items
      menu.find('.context-submenu-item').off('click').on('click', function(e) {
        var action = $(this).data('action');
        if (action) {
          handleContextMenuAction(action);
          hideContextMenu();
        }
        e.stopPropagation();
      });
    }
    
    function hideContextMenu() {
      $('#contextMenu').hide();
    }
    
    function handleContextMenuAction(action) {
      if (!currentContextNode) return;
      
      if (action === 'edit') {
        Shiny.setInputValue('node_edit', currentContextNode, {priority: 'event'});
      } else if (action === 'run') {
        Shiny.setInputValue('node_run', currentContextNode, {priority: 'event'});
      } else if (action === 'delete') {
        Shiny.setInputValue('node_delete', currentContextNode, {priority: 'event'});
      } else if (action === 'shape-square' || action === 'shape-diamond') {
        var ap = {};
        if (action === 'shape-square') ap.shape = 'square';
        if (action === 'shape-diamond') ap.shape = 'diamond';
        // Preserve existing appearance
        if (canvasNodes[currentContextNode.id] && canvasNodes[currentContextNode.id].appearance) {
          ap = Object.assign({}, canvasNodes[currentContextNode.id].appearance, ap);
        }
        try { applyNodeAppearance(currentContextNode.id, ap); } catch(e) {}
        try { Shiny.setInputValue('node_set_appearance', { id: currentContextNode.id, shape: ap.shape }, {priority: 'event'}); } catch(e) {}
      } else if (action.indexOf('bg-color-') === 0) {
        // Handle background color changes
        var bgColorMap = {
          'bg-color-red': '#ffdddd',
          'bg-color-green': '#ddffdd',
          'bg-color-blue': '#dde7ff',
          'bg-color-yellow': '#fff6cc',
          'bg-color-white': '#ffffff',
          'bg-color-orange': '#ffe4cc',
          'bg-color-purple': '#e6d9f5'
        };
        var bgColor = bgColorMap[action] || '#ffffff';
        
        // Get existing appearance
        var ap = {};
        if (canvasNodes[currentContextNode.id] && canvasNodes[currentContextNode.id].appearance) {
          ap = Object.assign({}, canvasNodes[currentContextNode.id].appearance);
        }
        ap.color = bgColor;
        
        // Apply background color
        try { applyNodeAppearance(currentContextNode.id, ap); } catch(e) {}
        
        // Send to server
        try { 
          Shiny.setInputValue('node_set_appearance', { 
            id: currentContextNode.id, 
            color: bgColor 
          }, {priority: 'event'}); 
        } catch(e) {}
      } else if (action.indexOf('title-color-') === 0) {
        // Handle title color changes
        var titleColorMap = {
          'title-color-red': '#dc3545',
          'title-color-green': '#28a745',
          'title-color-blue': '#007bff',
          'title-color-yellow': '#ffc107',
          'title-color-black': '#2c3e50',
          'title-color-orange': '#fd7e14',
          'title-color-purple': '#6f42c1'
        };
        var titleColor = titleColorMap[action] || '#2c3e50';
        
        // Get existing appearance
        var ap = {};
        if (canvasNodes[currentContextNode.id] && canvasNodes[currentContextNode.id].appearance) {
          ap = Object.assign({}, canvasNodes[currentContextNode.id].appearance);
        }
        ap.titleColor = titleColor;
        
        // Apply color to node title
        try { applyNodeAppearance(currentContextNode.id, ap); } catch(e) {}
        
        // Send to server
        try { 
          Shiny.setInputValue('node_set_appearance', { 
            id: currentContextNode.id, 
            titleColor: titleColor 
          }, {priority: 'event'}); 
        } catch(e) {}
      }
    }
    
    $(document).on('click', function(e) {
      if (!$(e.target).closest('.context-menu').length) {
        hideContextMenu();
      }
    });

    function syncConnections() {
      var conns = jsPlumbInstance.getConnections();
      var edges = conns.map(function(c) {
        return { source: c.sourceId, target: c.targetId };
      });
      Shiny.setInputValue('graph_connections', edges, {priority: 'event'});
    }

    function clearCanvas() {
      jsPlumbInstance.deleteEveryConnection();
      jsPlumbInstance.deleteEveryEndpoint();
      $('#canvas .canvas-node').remove();
      canvasNodes = {};
      Shiny.setInputValue('graph_connections', [], {priority: 'event'});
    }

    // ==================== Connection Context Menu ====================
    function showConnectionContextMenu(x, y, conn) {
      var menu = $('#connContextMenu');
      if (menu.length === 0) {
        menu = $('<div id=\"connContextMenu\" class=\"context-menu\"></div>');
        $('body').append(menu);
      }
      var html = '' +
        '<div class=\"context-menu-item\" data-action=\"style-bezier\">Bezier</div>' +
        '<div class=\"context-menu-item\" data-action=\"style-straight\">Straight</div>' +
        '<div class=\"context-menu-item\" data-action=\"style-flowchart\">Flowchart</div>' +
        '<div class=\"context-menu-item danger\" data-action=\"delete\">Delete Connection</div>';
      menu.html(html);
      menu.css({ left: x + 'px', top: y + 'px', display: 'block' });

      menu.find('.context-menu-item').off('click').on('click', function(){
        var action = $(this).data('action');
        try {
          if (action === 'delete') {
            jsPlumbInstance.deleteConnection(conn);
          } else if (action === 'style-bezier') {
            conn.setConnector(['Bezier', {curviness:50}]);
            // Recreate arrow overlay to avoid overlays being lost when connector is rebuilt
            try {
              if (conn.removeOverlay) {
                try { conn.removeOverlay('arrow'); } catch (e) { /* ignore */ }
              }
              // add a fresh overlay with an id so we can reliably remove/inspect it later
              try { conn.addOverlay(['Arrow', { id: 'arrow', location: 1, width: 16, length: 16 }]); } catch (e) { }
            } catch (e) { /* ignore overlay attach errors */ }
            jsPlumbInstance.repaintEverything();
            try { attachConnectionContextMenu(conn); } catch(e) {}
          } else if (action === 'style-straight') {
            conn.setConnector('Straight');
            try {
              if (conn.removeOverlay) {
                try { conn.removeOverlay('arrow'); } catch (e) { }
              }
              try { conn.addOverlay(['Arrow', { id: 'arrow', location: 1, width: 16, length: 16 }]); } catch (e) { }
            } catch (e) { }
            jsPlumbInstance.repaintEverything();
            try { attachConnectionContextMenu(conn); } catch(e) {}
          } else if (action === 'style-flowchart') {
            conn.setConnector(['Flowchart', {cornerRadius:6}]);
            try {
              if (conn.removeOverlay) {
                try { conn.removeOverlay('arrow'); } catch (e) { }
              }
              try { conn.addOverlay(['Arrow', { id: 'arrow', location: 1, width: 16, length: 16 }]); } catch (e) { }
            } catch (e) { }
            jsPlumbInstance.repaintEverything();
            try { attachConnectionContextMenu(conn); } catch(e) {}
          }
        } catch (e) { console.warn('conn action failed', e); }
        hideConnectionContextMenu();
      });
    }
    function hideConnectionContextMenu(){ $('#connContextMenu').hide(); }
    $(document).on('click', function(e){ if (!$(e.target).closest('#connContextMenu').length) hideConnectionContextMenu(); });

    // Track selected connection globally
    window.selectedConnection = null;
    
    function attachConnectionContextMenu(connection) {
      var el = connection.canvas || (connection.getConnector && connection.getConnector().canvas);
      if (!el) return;
      
      // Ensure we don't duplicate handlers
      $(el).off('contextmenu.conn').on('contextmenu.conn', function(e){
        e.preventDefault(); e.stopPropagation();
        showConnectionContextMenu(e.pageX, e.pageY, connection);
        return false;
      });
      
      // Add click handler to select connection
      $(el).off('click.conn').on('click.conn', function(e) {
        e.stopPropagation(); // Prevent canvas click from deselecting
        
        // Deselect all nodes
        $('.canvas-node').removeClass('selected');
        Shiny.setInputValue('node_selected', {id: null, filePath: null}, {priority: 'event'});
        
        // Deselect previous connection
        if (window.selectedConnection && window.selectedConnection !== connection) {
          try {
            var prevEl = window.selectedConnection.canvas || (window.selectedConnection.getConnector && window.selectedConnection.getConnector().canvas);
            if (prevEl) $(prevEl).removeClass('connection-selected');
          } catch(e) {}
        }
        
        // Select this connection
        window.selectedConnection = connection;
        $(el).addClass('connection-selected');
      });
    }

    Shiny.addCustomMessageHandler('clear_canvas_ui', function(msg) {
      clearCanvas();
    });

    Shiny.addCustomMessageHandler('export_svg_trigger', function(msg) {
      exportCanvasToSVG();
    });

    Shiny.addCustomMessageHandler('delete_node_ui', function(data) {
      var nodeId = data.id;
      try {
        var conns = jsPlumbInstance.getConnections();
        var connectionsToRemove = [];
        for (var i = 0; i < conns.length; i++) {
          if (conns[i].sourceId === nodeId || conns[i].targetId === nodeId) {
            connectionsToRemove.push(conns[i]);
          }
        }
        connectionsToRemove.forEach(function(conn) {
          jsPlumbInstance.deleteConnection(conn);
        });
        jsPlumbInstance.removeAllEndpoints(nodeId);
        jsPlumbInstance.remove(nodeId);
        $('#' + nodeId).remove();
        delete canvasNodes[nodeId];
        syncConnections();
      } catch (error) {
        console.error('Error during node deletion:', error);
      }
    });

    Shiny.addCustomMessageHandler('restore_canvas', function(data) {
      clearCanvas();
      data.nodes.forEach(function(n) {
        createCanvasNode(n.id, n.filePath, n.fileName, n.x, n.y, n.firstLine || '', n.appearance);
      });
      setTimeout(function() {
        data.edges.forEach(function(e) {
          jsPlumbInstance.connect({ source: e.source, target: e.target, overlays: [['Arrow', { id: 'arrow', location: 1, width: 16, length: 16 }]] });
        });
      }, 100);
    });
    
    // Handle node display update (for first line)
    Shiny.addCustomMessageHandler('update_node_display', function(data) {
      var nodeId = data.id;
      var nodeEl = $('#' + nodeId);
      if (nodeEl.length > 0) {
        var infoEl = nodeEl.find('.node-info');
        if (infoEl.length > 0) {
          var firstLine = data.firstLine || '';
          // Truncate if too long
          if (firstLine.length > 40) {
            firstLine = firstLine.substring(0, 40) + '...';
          }
          infoEl.text(firstLine);
        }
        // Update stored data
        if (canvasNodes[nodeId]) {
          canvasNodes[nodeId].firstLine = data.firstLine || '';
        }
      }
    });

    // Export canvas to SVG (more stable than PNG)
    function exportCanvasToSVG() {
      const canvas = document.getElementById('canvas');
      if (!canvas) {
        console.error('Canvas element not found');
        Shiny.setInputValue('svg_export_error', 'Canvas element not found', {priority: 'event'});
        return;
      }
      
      // Show loading notification
      Shiny.setInputValue('svg_export_started', true, {priority: 'event'});
      
      // Ensure jsPlumb has finished rendering all connections
      if (jsPlumbInstance) {
        try {
          jsPlumbInstance.repaintEverything();
        } catch (e) {
          console.warn('Repaint warning:', e);
        }
      }
      
      // Wait a bit to ensure all connections are rendered
      setTimeout(function() {
        try {
          // Ensure all connectors are up to date before export
          if (jsPlumbInstance) {
            jsPlumbInstance.repaintEverything();
          }
          
          // Get canvas dimensions
          const canvasRect = canvas.getBoundingClientRect();
          const canvasWidth = canvas.offsetWidth || canvas.clientWidth || 800;
          const canvasHeight = canvas.offsetHeight || canvas.clientHeight || 600;
          
          // Create a new SVG wrapper
          const wrapperSvg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
          wrapperSvg.setAttribute('width', canvasWidth);
          wrapperSvg.setAttribute('height', canvasHeight);
          wrapperSvg.setAttribute('xmlns', 'http://www.w3.org/2000/svg');
          wrapperSvg.setAttribute('xmlns:xlink', 'http://www.w3.org/1999/xlink');
          
          // Add background
          const bgRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
          bgRect.setAttribute('width', '100%');
          bgRect.setAttribute('height', '100%');
          bgRect.setAttribute('fill', '#FAFAFA');
          wrapperSvg.appendChild(bgRect);
          
          // Helper function to get relative position
          const relPos = function(el) {
            const r = el.getBoundingClientRect();
            return {
              x: r.left - canvasRect.left,
              y: r.top - canvasRect.top,
              w: r.width,
              h: r.height
            };
          };
          
          // Add all nodes as SVG elements
          const nodes = canvas.querySelectorAll('.canvas-node');
          console.log('Found', nodes.length, 'nodes to export');
          
          nodes.forEach(function(node) {
            const p = relPos(node);
            const computedStyle = window.getComputedStyle(node);
            
            // Create a group for the node
            const nodeGroup = document.createElementNS('http://www.w3.org/2000/svg', 'g');
            
            // Draw node rectangle
            const nodeRect = document.createElementNS('http://www.w3.org/2000/svg', 'rect');
            nodeRect.setAttribute('x', p.x);
            nodeRect.setAttribute('y', p.y);
            nodeRect.setAttribute('width', p.w);
            nodeRect.setAttribute('height', p.h);
            nodeRect.setAttribute('rx', '10');
            nodeRect.setAttribute('ry', '10');
            nodeRect.setAttribute('fill', computedStyle.backgroundColor || '#ffffff');
            nodeRect.setAttribute('stroke', computedStyle.borderColor || '#CEB888');
            nodeRect.setAttribute('stroke-width', computedStyle.borderWidth || '3');
            nodeGroup.appendChild(nodeRect);
            
            // Add text
            const titleEl = node.querySelector('.node-title');
            const infoEl = node.querySelector('.node-info');
            
            if (titleEl) {
              // Get text color from node appearance or computed style
              var titleColor = '#2c3e50'; // default
              var nodeId = node.id;
              if (canvasNodes[nodeId] && canvasNodes[nodeId].appearance && canvasNodes[nodeId].appearance.titleColor) {
                titleColor = canvasNodes[nodeId].appearance.titleColor;
              } else {
                // Fallback to computed style
                var titleStyle = window.getComputedStyle(titleEl);
                titleColor = titleStyle.color || '#2c3e50';
              }
              
              const titleText = document.createElementNS('http://www.w3.org/2000/svg', 'text');
              titleText.setAttribute('x', p.x + 12);
              titleText.setAttribute('y', p.y + 25);
              titleText.setAttribute('font-family', 'Arial, sans-serif');
              titleText.setAttribute('font-size', '15');
              titleText.setAttribute('font-weight', 'bold');
              titleText.setAttribute('fill', titleColor);
              titleText.textContent = titleEl.textContent || titleEl.innerText;
              nodeGroup.appendChild(titleText);
            }
            
            if (infoEl) {
              const infoText = document.createElementNS('http://www.w3.org/2000/svg', 'text');
              infoText.setAttribute('x', p.x + 12);
              infoText.setAttribute('y', p.y + 45);
              infoText.setAttribute('font-family', 'Arial, sans-serif');
              infoText.setAttribute('font-size', '11');
              infoText.setAttribute('fill', '#666');
              let text = infoEl.textContent || infoEl.innerText;
              // Truncate at 40 chars as per requirements
              if (text.length > 40) text = text.substring(0, 40) + '...';
              infoText.textContent = text;
              nodeGroup.appendChild(infoText);
            }
            
            wrapperSvg.appendChild(nodeGroup);
          });
          
          // Add all jsPlumb connections (SVG elements) with correct coordinate offsets
          const svgs = canvas.querySelectorAll('svg');
          console.log('Found', svgs.length, 'SVG elements (connections)');
          
          svgs.forEach(function(svg) {
            const rect = svg.getBoundingClientRect();
            
            // Calculate offset of this SVG relative to the main canvas
            const dx = rect.left - canvasRect.left;
            const dy = rect.top - canvasRect.top;
            
            // Create a group element to shift the coordinates
            const g = document.createElementNS('http://www.w3.org/2000/svg', 'g');
            g.setAttribute('transform', 'translate(' + dx + ', ' + dy + ')');
            
            // Clone and append each child path/element
            const children = svg.children;
            for (let i = 0; i < children.length; i++) {
              const cloned = children[i].cloneNode(true);
              
              // Force stroke style if missing
              if (cloned.tagName === 'path' || cloned.tagName === 'line' || cloned.tagName === 'polyline') {
                if (!cloned.getAttribute('stroke')) {
                  cloned.setAttribute('stroke', '#B89D5D');
                }
                if (!cloned.getAttribute('stroke-width')) {
                  cloned.setAttribute('stroke-width', '2');
                }
              }
              
              g.appendChild(cloned);
            }
            
            // Append the group to the wrapper SVG
            wrapperSvg.appendChild(g);
          });
          
          // Serialize SVG to string
          const serializer = new XMLSerializer();
          const source = serializer.serializeToString(wrapperSvg);
          
          // Create blob and download
          const blob = new Blob([source], { type: 'image/svg+xml;charset=utf-8' });
          const url = URL.createObjectURL(blob);
          const link = document.createElement('a');
          const timestamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, -5);
          link.href = url;
          link.download = 'rcw_workflow_' + timestamp + '.svg';
          link.click();
          URL.revokeObjectURL(url);
          
          // Notify server that export completed
          Shiny.setInputValue('svg_export_completed', true, {priority: 'event'});
          
        } catch (error) {
          console.error('Error in SVG export:', error);
          Shiny.setInputValue('svg_export_error', error.toString(), {priority: 'event'});
        }
      }, 300);
    }

    // Export SVG will be triggered via Shiny input system
    // The server will send a custom message to trigger export

    // Toggle panels
    $(function () {
      var leftOpen = true, rightOpen = true;
      function toggleLeft()  { $('#leftPanel').toggleClass('hidden'); leftOpen  = !leftOpen; }
      function toggleRight() { $('#rightPanel').toggleClass('hidden'); rightOpen = !rightOpen; }
      $('#toggleLeftPanel').on('click', toggleLeft);
      $('#toggleRightPanel').on('click', toggleRight);
    });
  "))
  ),
  
  # Title bar
  div(class = "title-bar",
    # Left: title/subtitle
    div(style = "display: flex; align-items: center; justify-content: space-between;",
      div(style = "flex: 1;",
        uiOutput("app_title_ui"),
        uiOutput("app_subtitle_ui")
      ),
      # Right: canvas settings button (removed - per-node controls now via right-click)
      div(style = "margin-left: 12px;",
        HTML('')
      )
    )
  ),

  # Toggle buttons
  div(id = "toggleLeftBtn", class = "toggle-btn-left",
      actionButton("toggleLeftPanel", HTML("&#10094;"), class = "btn btn-sm btn-secondary")),
  div(id = "toggleRightBtn", class = "toggle-btn-right",
      actionButton("toggleRightPanel", HTML("&#10095;"), class = "btn btn-sm btn-secondary")),

  # Three-panel container
  div(class = "three-panel-container",
    # Left: File Tree
    div(id = "leftPanel", class = "left-panel",
      div(class = "panel-section",
        h4(textOutput("left_title_notebooks"), class = "section-title"),
        div(style = "background: #e7f3ff; padding: 10px; border-radius: 6px; margin-bottom: 12px;",
          p(style = "margin: 0; font-size: 0.85rem; color: #1565c0;",
            textOutput("left_tip_drag"))
        ),
        div(class = "btn-row",
          actionButton("new_folder", label = NULL, 
                      class = "btn btn-sm btn-primary",
                      style = "flex: 1;"),
          actionButton("new_file", label = NULL, 
                      class = "btn btn-sm btn-primary",
                      style = "flex: 1;")
        ),
        div(class = "btn-row",
          actionButton("add_external_folder", label = NULL, 
                      class = "btn btn-sm btn-secondary", 
                      style = "flex: 1;")
        ),
        hr(style = "margin: 12px 0;"),
        uiOutput("file_tree_ui")
      ),
      div(class = "btn-row",
        actionButton("run_pipeline", label = NULL, 
                    class = "btn btn-primary",
                    style = "flex: 1; font-size: 1rem; padding: 10px;"),
        actionButton("clear_canvas", label = NULL, 
                    class = "btn btn-secondary",
                    style = "padding: 10px;")
      )
    ),
    
    # Center: Canvas
    div(class = "center-panel",
      div(id = "canvas")
    ),
    
    # Right: Output & Download
    div(id = "rightPanel", class = "right-panel",
      navset_card_tab(
        id = "rightTabs",
        nav_panel(uiOutput("tab_title_pipeline_output"),
          div(style = "margin-top: 12px;",
            h5(textOutput("right_exec_results"), style = "color: #2c3e50;"),
            verbatimTextOutput("pipeline_output"),
            hr(),
            h5(textOutput("right_run_log"), style = "color: #2c3e50;"),
            verbatimTextOutput("run_log")
          )
        ),
        nav_panel(uiOutput("tab_title_export_rmd"),
          div(style = "margin-top: 12px;",
            h5(textOutput("right_export_rmd_title"), style = "color: #2c3e50;"),
            p(textOutput("right_export_rmd_desc"), 
              style = "color: #666; font-size: 0.9em;"),
            div(class = "btn-row",
              downloadButton("export_rmd", label = NULL, 
                           class = "btn btn-primary",
                           style = "flex: 1; padding: 10px;")
            ),
            hr(),
            h5(textOutput("right_export_svg_title"), style = "color: #2c3e50; margin-top: 20px;"),
            p(textOutput("right_export_svg_desc"), 
              style = "color: #666; font-size: 0.9em;"),
            div(class = "btn-row",
              actionButton("export_svg_btn", label = NULL, 
                          class = "btn btn-primary",
                          style = "flex: 1; padding: 10px;")
            ),
            hr(),
            h5(textOutput("right_preview"), style = "color: #2c3e50;"),
            verbatimTextOutput("rmd_preview")
          )
        )
      )
    )
  )
)

# ============================
# Server Logic
# ============================
server <- function(input, output, session) {
  # Ensure workspace path is correct (re-check in server context)
  # This is important because getwd() may differ when Shiny app runs
  # Priority: Find inst/RCW/root which contains the actual files
  possible_roots <- c(
    file.path(getwd(), "inst", "RCW", "root"),                    # PRIORITY: inst/RCW/root
    file.path(getwd(), "easybreedeR-main", "inst", "RCW", "root"), # If in parent directory
    file.path(getwd(), "root"),                                    # Fallback
    normalizePath(file.path("inst", "RCW", "root"), mustWork = FALSE),
    normalizePath(file.path("root"), mustWork = FALSE),
    # Also try absolute path from common locations
    file.path(dirname(getwd()), "inst", "RCW", "root"),
    file.path(dirname(dirname(getwd())), "inst", "RCW", "root")
  )
  
  # Find existing root directory - prioritize inst/RCW/root structure
  found_root <- NULL
  for (root_candidate in possible_roots) {
    if (dir.exists(root_candidate)) {
      # Always prefer inst/RCW/root structure
      if (grepl("inst[/\\\\]RCW[/\\\\]root", root_candidate, ignore.case = TRUE)) {
        found_root <- normalizePath(root_candidate, winslash = "/")
        break
      }
      # Otherwise check if it has R files
      r_files <- list.files(root_candidate, pattern = "\\.R$", recursive = TRUE, full.names = FALSE)
      if (length(r_files) > 0) {
        found_root <- normalizePath(root_candidate, winslash = "/")
        break
      }
    }
  }
  
  # If found a valid root, update workspace
  if (!is.null(found_root)) {
    .rcw_workspace <<- found_root
  } else {
    # If still not found, try to find or create inst/RCW/root
    if (dir.exists("inst") && dir.exists("inst/RCW")) {
      root_path <- file.path("inst", "RCW", "root")
      if (!dir.exists(root_path)) {
        dir.create(root_path, recursive = TRUE, showWarnings = FALSE)
      }
      .rcw_workspace <<- normalizePath(root_path, winslash = "/")
    } else if (dir.exists("easybreedeR-main") && dir.exists("easybreedeR-main/inst/RCW")) {
      root_path <- file.path("easybreedeR-main", "inst", "RCW", "root")
      if (!dir.exists(root_path)) {
        dir.create(root_path, recursive = TRUE, showWarnings = FALSE)
      }
      .rcw_workspace <<- normalizePath(root_path, winslash = "/")
    }
  }
  
  # Debug: Print workspace path to console (helpful for troubleshooting)
  cat("=== RCW Workspace Debug ===\n")
  cat("Current working directory:", getwd(), "\n")
  cat("Workspace path:", .rcw_workspace, "\n")
  cat("Workspace exists:", dir.exists(.rcw_workspace), "\n")
  if (dir.exists(.rcw_workspace)) {
    r_files <- list.files(.rcw_workspace, pattern = "\\.R$", recursive = TRUE, full.names = FALSE)
    cat("R files found:", length(r_files), "\n")
    if (length(r_files) > 0) {
      cat("Files:", paste(r_files, collapse = ", "), "\n")
    }
  }
  cat("===========================\n")
  
  # Language state
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
          get("map_suite_lang_for_app", mode = "function")(resolved, app = "rcw")
        } else {
          resolved
        }
      }, silent = TRUE)
      if (!inherits(mapped, "try-error")) lang(mapped)
    }
  })

  # Title bar
  output$app_title_ui <- renderUI({ h1(get_label("app_name", lang())) })
  output$app_subtitle_ui <- renderUI({ p(get_label("app_subtitle", lang())) })

  # Global canvas settings UI removed. Per-node appearance is available via right-click context menu.

  # Left panel labels/buttons
  output$left_title_notebooks <- renderText({ get_label("left_notebooks", lang()) })
  output$left_tip_drag <- renderText({ get_label("left_tip_drag", lang()) })
  observe({
    updateActionButton(session, "new_folder", label = get_label("btn_new_folder", lang()))
    updateActionButton(session, "new_file", label = get_label("btn_new_file", lang()))
    updateActionButton(session, "add_external_folder", label = get_label("btn_add_folder", lang()))
    updateActionButton(session, "run_pipeline", label = get_label("btn_run_pipeline", lang()))
    updateActionButton(session, "clear_canvas", label = get_label("btn_clear", lang()))
    updateActionButton(session, "toggleLeftPanel", label = HTML("&#10094;"))
    updateActionButton(session, "toggleRightPanel", label = HTML("&#10095;"))
  })

  # Right tabs and labels
  output$tab_title_pipeline_output <- renderUI({ span(get_label("tab_pipeline_output", lang())) })
  output$tab_title_export_rmd <- renderUI({ span(get_label("tab_export_rmd", lang())) })
  output$right_exec_results <- renderText({ get_label("right_exec_results", lang()) })
  output$right_run_log <- renderText({ get_label("right_run_log", lang()) })
  output$right_export_rmd_title <- renderText({ get_label("right_export_rmd_title", lang()) })
  output$right_export_rmd_desc <- renderText({ get_label("right_export_rmd_desc", lang()) })
  output$right_export_svg_title <- renderText({ get_label("right_export_svg_title", lang()) })
  output$right_export_svg_desc <- renderText({ get_label("right_export_svg_desc", lang()) })
  output$right_preview <- renderText({ get_label("right_preview", lang()) })
  observe({
    # Fallback if updateDownloadButton is not available in current shiny version
    if (exists("updateDownloadButton", where = asNamespace("shiny"), mode = "function")) {
      get("updateDownloadButton", envir = asNamespace("shiny"))(session, "export_rmd", label = get_label("btn_download_rmd", lang()))
    }
    # Update SVG export button label
    updateActionButton(session, "export_svg_btn", label = get_label("btn_export_svg", lang()))
  })
  
  # Reactive state - scan workspace after path is confirmed
  rv <- reactiveValues(
    workspace = scan_workspace(),  # Will be refreshed immediately below
    nodes = list(),
    edges = list(),
    rmd_preview = "",
    selected_node_id = NULL,
    logs = character(0),
    pipeline_result = NULL,
    file_to_delete = NULL,
    folder_to_delete = NULL,
    files_to_delete = NULL,
    selected_dir = NULL,
    editing_tree_file = NULL,
    file_to_rename = NULL,
    folder_to_rename = NULL
  )
  
  # Force refresh workspace scan with correct path
  rv$workspace <- scan_workspace()

  # Helper: regenerate Rmd preview from current nodes/edges
  regenerate_rmd <- function() {
    if (length(rv$nodes) == 0) {
      rv$rmd_preview <- "No nodes on canvas. Add some R files to the canvas to generate R Markdown."
      return(invisible(NULL))
    }
    # Build hierarchy and generate rmd; protect against errors
    hierarchy <- tryCatch({ build_node_hierarchy(rv$nodes, rv$edges) }, error = function(e) NULL)
    if (is.null(hierarchy) || length(hierarchy) == 0) {
      rv$rmd_preview <- "No nodes in workflow or failed to compute order."
      return(invisible(NULL))
    }
    rv$rmd_preview <- tryCatch({ generate_rmd_from_hierarchy(hierarchy, rv$workspace$files) }, error = function(e) paste0("Error generating Rmd: ", e$message))
    invisible(NULL)
  }

  # Compute a robust topological execution order from current graph (Kahn + position tie-break)
  compute_execution_order <- function() {
    ids <- names(rv$nodes)
    if (length(ids) == 0) return(list())

    # Build adjacency and indegree
    adj <- setNames(lapply(ids, function(i) character(0)), ids)
    indeg <- setNames(rep(0L, length(ids)), ids)
    if (length(rv$edges) > 0) {
      for (e in rv$edges) {
        s <- as.character(e$source); t <- as.character(e$target)
        if (nzchar(s) && nzchar(t) && s %in% ids && t %in% ids) {
          adj[[s]] <- c(adj[[s]], t)
          indeg[[t]] <- indeg[[t]] + 1L
        }
      }
    }

    # Position tie-breaker (top-to-bottom, then left-to-right)
    pos_key <- function(id) {
      node <- rv$nodes[[id]]
      y <- suppressWarnings(as.numeric(node$y)); if (is.na(y)) y <- 0
      x <- suppressWarnings(as.numeric(node$x)); if (is.na(x)) x <- 0
      sprintf("%09d_%09d", as.integer(y), as.integer(x))
    }
    order_by_pos <- function(vec) {
      if (length(vec) <= 1) return(vec)
      keys <- vapply(vec, pos_key, character(1))
      vec[order(keys, vec)]
    }

    # Initialize queue with roots ordered by position
    q <- order_by_pos(ids[indeg[ids] == 0L])
    out <- character(0)
    while (length(q) > 0) {
      n <- q[1]; q <- q[-1]
      out <- c(out, n)
      for (m in adj[[n]]) {
        indeg[[m]] <- indeg[[m]] - 1L
        if (indeg[[m]] == 0L) q <- order_by_pos(c(q, m))
      }
    }
    if (length(out) < length(ids)) {
      out <- c(out, setdiff(ids, out))
    }
    lapply(out, function(id) list(id = id, node = rv$nodes[[id]], level = 1))
  }

  # Note: global canvas settings removed; per-node appearance is stored on nodes
  
  # Setup shinyDirChoose for folder selection (only if deps available)
  if (requireNamespace("fs", quietly = TRUE) && requireNamespace("shinyFiles", quietly = TRUE)) {
    volumes <- c(Home = fs::path_home(), getVolumes()())
    shinyDirChoose(input, "dir_choose", roots = volumes, session = session)
  }
  
  # Scan workspace on start and refresh
  observe({
    invalidateLater(20000)  # Refresh every 20 seconds
    rv$workspace <- scan_workspace()
  })
  
  # Initialize selected folder display
  output$selected_folder_display <- renderUI({
    p(style = "margin: 0; color: #666; font-size: 0.9rem;",
      "No folder selected yet...")
  })
  
  # Render file tree
  output$file_tree_ui <- renderUI({
    ws <- rv$workspace
    
    # Prefer folders discovered (includes empty ones); fallback to folders from files
    folders <- unique(unlist(ws$folders))
    if (length(folders) == 0) {
      folders <- unique(sapply(ws$files, function(f) f$folder))
    }
    
    if (length(folders) == 0) {
      return(div(
        style = "padding: 20px; text-align: center; color: #999;",
        p("No folders yet"),
        p(style = "font-size: 0.85rem;", "Use 'New Folder' to create one")
      ))
    }
    
    folders <- sort(folders)
    
    # Helpers to compute hierarchy
    get_parent <- function(label) {
      parts <- strsplit(label, "/", fixed = TRUE)[[1]]
      if (length(parts) <= 1) return(NA_character_)
      paste(parts[-length(parts)], collapse = "/")
    }
    get_children <- function(label) {
      prefix <- if (is.na(label)) "" else paste0(label, "/")
      # direct children: start with prefix, and remaining has no '/'
      Filter(function(x) {
        if (!startsWith(x, prefix)) return(FALSE)
        # Safely obtain the rest of the path after the prefix without regex
        rest <- if (nchar(prefix) == 0) x else substring(x, nchar(prefix) + 1)
        !grepl("/", rest, fixed = TRUE)
      }, folders[folders != label])
    }
    
    rendered <- new.env(parent = emptyenv())
    render_node <- function(label, level = 0) {
      if (exists(label, envir = rendered, inherits = FALSE)) return(NULL)
      assign(label, TRUE, envir = rendered)
      folder_safe_id <- gsub("[^A-Za-z0-9]", "-", label)
      folder_id <- paste0("folder-", folder_safe_id)
      folder_files <- Filter(function(f) f$folder == label, ws$files)
      children <- get_children(label)
      indent <- paste0(level * 16, "px")
      
      tagList(
        div(class = "file-tree-item folder",
          `data-folder-id` = folder_id,
          style = paste0("margin-left:", indent, ";"),
          div(class = "folder-header",
            span(class = "folder-toggle", "▼"),
            span(class = "file-icon", ""),
            span(class = "folder-name", title = label, label)
          )
        ),
        div(id = paste0("folder-files-", folder_id), class = "folder-files",
          # files in this folder
          lapply(folder_files, function(f) {
            div(class = "file-tree-item file",
              style = paste0("margin-left:", indent, ";"),
              `data-filepath` = f$rel_path,
              `data-filename` = f$name,
              span(class = "file-icon", ""),
              span(class = "file-name", title = f$name, f$name)
            )
          }),
          # then recursively render child folders
          lapply(children, function(ch) render_node(ch, level + 1))
        )
      )
    }
    
    tops <- Filter(function(x) is.na(get_parent(x)), folders)
    tagList(lapply(tops, function(t) render_node(t, 0)))
  })
  
  # New folder
  observeEvent(input$new_folder, {
    showModal(modalDialog(
      title = "Create New Folder",
      textInput("folder_name", "Folder Name:", placeholder = "e.g., data_processing"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_folder", "Create", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_new_folder, {
    req(input$folder_name)
    folder_path <- file.path(.rcw_workspace, input$folder_name)
    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    removeModal()
    showNotification(paste0("Created folder: ", input$folder_name), type = "message")
    rv$workspace <- scan_workspace()
  })
  
  # New file
  observeEvent(input$new_file, {
    ws <- rv$workspace
    base_label <- basename(.rcw_workspace)
    raw <- unique(unlist(ws$folders))
    rels <- character(0)
    if (length(raw) > 0) {
      for (lbl in raw) {
        if (identical(lbl, base_label)) rels <- c(rels, "")
        if (startsWith(lbl, paste0(base_label, "/"))) {
          rels <- c(rels, sub(paste0("^", base_label, "/"), "", lbl))
        }
      }
    }
    rels <- sort(unique(rels[rels != ""]))
    showModal(modalDialog(
      title = "Create New R File",
      textInput("file_name", "File Name:", placeholder = "e.g., analysis.R"),
      selectInput("file_folder", "Folder:", choices = c("Root", rels)),
      textAreaInput("file_content", "Initial Content:",
                    value = "# New R Script\n\n# Your code here\n",
                    rows = 10),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_new_file", "Create", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_new_file, {
    req(input$file_name)
    file_name <- input$file_name
    if (!grepl("\\.R$", file_name, ignore.case = TRUE)) {
      file_name <- paste0(file_name, ".R")
    }

    folder <- input$file_folder
    if (folder == "Root") {
      folder_path <- .rcw_workspace
    } else {
      folder <- sub("^/+", "", folder)
      folder_path <- file.path(.rcw_workspace, folder)
    }

    dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
    file_path <- file.path(folder_path, file_name)
    write_r_file(file_path, input$file_content %||% "# New R Script\n")
    removeModal()
    showNotification(paste0("Created file: ", file_name, " in ", folder), type = "message")
    rv$workspace <- scan_workspace()
  })
  
  # Add external folder
  observeEvent(input$add_external_folder, {
    if (!requireNamespace("shinyFiles", quietly = TRUE)) {
      showNotification("shinyFiles package not installed; external folder feature is disabled.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Add External Folder",
      size = "m",
      div(style = "margin-bottom: 15px;",
        shinyDirButton("dir_choose", "Browse for Folder", 
                      "Please select a folder containing .R files",
                      class = "btn btn-primary",
                      style = "width: 100%; padding: 12px; font-size: 1rem;")
      ),
      div(id = "selected_folder_display", style = "margin-top: 15px; padding: 12px; background: #f8f9fa; border-radius: 4px; border-left: 4px solid #CEB888; min-height: 50px;",
        p(style = "margin: 0; color: #666; font-size: 0.9rem;",
          "No folder selected yet...")
      ),
      div(style = "margin-top: 10px; padding: 10px; background: #e7f3ff; border-radius: 4px; border-left: 4px solid #2196F3;",
        p(style = "margin: 0; font-size: 0.85rem; color: #1565c0;",
          "Tip: Browse and select a folder containing .R files. All .R files in the folder and subfolders will be available for use.")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_external_folder", "Add Folder", class = "btn-primary")
      )
    ))
  })
  
  # Watch for directory selection
  observe({
    if (!requireNamespace("shinyFiles", quietly = TRUE)) return()
    req(input$dir_choose)
    
    if (is.integer(input$dir_choose)) {
      return()
    }
    
    dir_path <- parseDirPath(volumes, input$dir_choose)
    
    if (length(dir_path) > 0 && dir.exists(dir_path)) {
      rv$selected_dir <- dir_path
      
      # Update display
      output$selected_folder_display <- renderUI({
        div(style = "padding: 12px; background: #e8f5e9; border-radius: 4px; border-left: 4px solid #4CAF50;",
          p(style = "margin: 0; font-weight: 600; color: #2e7d32;",
            "Selected folder:"),
          p(style = "margin: 5px 0 0 0; color: #1b5e20; font-family: monospace; font-size: 0.85rem;",
            dir_path)
        )
      })
    }
  })
  
  observeEvent(input$confirm_add_external_folder, {
    req(rv$selected_dir)
    if (!requireNamespace("shinyFiles", quietly = TRUE)) {
      showNotification("shinyFiles package not installed; external folder feature is disabled.", type = "warning")
      return()
    }
    folder_path <- rv$selected_dir
    
    if (!dir.exists(folder_path)) {
      showNotification("Folder does not exist!", type = "error")
      return()
    }
    
    # Check if already added
    if (folder_path %in% .rcw_external_folders) {
      showNotification("Folder already added!", type = "warning")
      removeModal()
      rv$selected_dir <- NULL
      return()
    }
    
    # Add to external folders
    .rcw_external_folders <<- c(.rcw_external_folders, folder_path)
    
    removeModal()
    showNotification(paste0("Added folder: ", basename(folder_path)), type = "message")
    rv$selected_dir <- NULL
    rv$workspace <- scan_workspace()
  })
  
  # Delete file
  observeEvent(input$delete_file, {
    req(input$delete_file)
    file_path <- input$delete_file$path
    
    # Find full path
    matching_file <- Find(function(f) f$rel_path == file_path, rv$workspace$files)
    if (is.null(matching_file)) {
      showNotification("File not found!", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Confirm Delete",
      p(sprintf("Are you sure you want to delete '%s'?", matching_file$name)),
      p(style = "color: #999; font-size: 0.85rem;", matching_file$path),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_file", "Delete", class = "btn-danger",
                    style = "background: #dc3545; border-color: #dc3545;")
      )
    ))
    
    rv$file_to_delete <- matching_file$path
  })
  
  observeEvent(input$confirm_delete_file, {
    req(rv$file_to_delete)
    
    if (file.exists(rv$file_to_delete)) {
      unlink(rv$file_to_delete)
      showNotification("File deleted", type = "message")
    }
    
    removeModal()
    rv$file_to_delete <- NULL
    rv$workspace <- scan_workspace()
  })
  
  # Edit file from tree
  observeEvent(input$edit_file_tree, {
    req(input$edit_file_tree)
    file_rel_path <- input$edit_file_tree$path
    
    # Find the actual file
    matching_file <- Find(function(f) f$rel_path == file_rel_path, rv$workspace$files)
    if (is.null(matching_file)) {
      showNotification("File not found!", type = "error")
      return()
    }
    
    file_path <- matching_file$path
    content <- read_r_file(file_path)
    
    showModal(modalDialog(
      title = paste0("Edit: ", matching_file$name),
      size = "l",
      tags$style(HTML("#edit_tree_file_content { 
        font-family: 'Courier New', monospace !important;
        font-size: 13px !important;
        padding: 12px !important;
        border: 2px solid #CFB991 !important;
        border-radius: 4px !important;
        background: #f8f9fa !important;
      }")),
      textAreaInput("edit_tree_file_content", NULL, value = content, 
                   rows = 20, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_tree_file_edit", "Save", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
    
    rv$editing_tree_file <- file_path
  })
  
  observeEvent(input$save_tree_file_edit, {
    req(rv$editing_tree_file, input$edit_tree_file_content)
    write_r_file(rv$editing_tree_file, input$edit_tree_file_content)
    removeModal()
    showNotification("Code saved successfully!", type = "message")
    rv$editing_tree_file <- NULL
    rv$workspace <- scan_workspace()
  })
  
  # Rename file
  observeEvent(input$rename_file, {
    req(input$rename_file)
    file_rel_path <- input$rename_file$path
    old_name <- input$rename_file$name
    
    # Find the actual file
    matching_file <- Find(function(f) f$rel_path == file_rel_path, rv$workspace$files)
    if (is.null(matching_file)) {
      showNotification("File not found!", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Rename File",
      textInput("new_file_name", "New File Name:", value = old_name),
      p(style = "color: #999; font-size: 0.85rem;", matching_file$path),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_rename_file", "Rename", class = "btn-primary")
      )
    ))
    
    rv$file_to_rename <- matching_file$path
  })
  
  observeEvent(input$confirm_rename_file, {
    req(rv$file_to_rename, input$new_file_name)
    
    old_path <- rv$file_to_rename
    new_name <- input$new_file_name
    
    # Ensure .R extension
    if (!grepl("\\.R$", new_name, ignore.case = TRUE)) {
      new_name <- paste0(new_name, ".R")
    }
    
    new_path <- file.path(dirname(old_path), new_name)
    
    if (file.exists(new_path) && new_path != old_path) {
      showNotification("A file with this name already exists!", type = "error")
      return()
    }
    
    file.rename(old_path, new_path)
    removeModal()
    showNotification(paste0("Renamed to: ", new_name), type = "message")
    rv$file_to_rename <- NULL
    rv$workspace <- scan_workspace()
  })
  
  # Rename folder
  observeEvent(input$rename_folder, {
    req(input$rename_folder)
    folder_name <- input$rename_folder$name
    
    # Find folder path
    folder_path <- NULL
    base_name <- "root"
    
    if (folder_name == base_name) {
      folder_path <- .rcw_workspace
    } else if (folder_name %in% sapply(.rcw_external_folders, basename)) {
      showNotification("Cannot rename external folders!", type = "error")
      return()
    } else {
      # It's a subfolder - need to extract the base folder name
      parts <- strsplit(folder_name, "/")[[1]]
      if (length(parts) == 1) {
        folder_path <- file.path(.rcw_workspace, folder_name)
      } else {
        # It's a nested folder like "R_Notebooks/subfolder"
        folder_path <- file.path(.rcw_workspace, parts[-1])
      }
    }
    
    if (is.null(folder_path) || !dir.exists(folder_path)) {
      showNotification("Folder not found!", type = "error")
      return()
    }
    
    showModal(modalDialog(
      title = "Rename Folder",
      textInput("new_folder_name", "New Folder Name:", value = basename(folder_path)),
      p(style = "color: #999; font-size: 0.85rem;", folder_path),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_rename_folder", "Rename", class = "btn-primary")
      )
    ))
    
    rv$folder_to_rename <- folder_path
  })
  
  observeEvent(input$confirm_rename_folder, {
    req(rv$folder_to_rename, input$new_folder_name)
    
    old_path <- rv$folder_to_rename
    new_name <- input$new_folder_name
    new_path <- file.path(dirname(old_path), new_name)
    
    if (dir.exists(new_path) && new_path != old_path) {
      showNotification("A folder with this name already exists!", type = "error")
      return()
    }
    
    file.rename(old_path, new_path)
    removeModal()
    showNotification(paste0("Renamed to: ", new_name), type = "message")
    rv$folder_to_rename <- NULL
    rv$workspace <- scan_workspace()
  })
  
  # Delete folder
  observeEvent(input$delete_folder, {
    req(input$delete_folder)
    folder_name <- input$delete_folder$name
    
    # Check if it's internal or external folder
    if (folder_name == "root") {
      showNotification("Cannot delete main workspace folder!", type = "error")
      return()
    }
    
    # Try to find matching folder path
    folder_path <- NULL
    # Determine which base this label belongs to (workspace or an external)
    parts <- strsplit(folder_name, "/")[[1]]
    base_label <- parts[1]
    external_labels <- sapply(.rcw_external_folders, basename)
    if (identical(base_label, "root")) {
      base_dir <- .rcw_workspace
      rel_parts <- parts[-1]
    } else if (base_label %in% external_labels) {
      base_dir <- .rcw_external_folders[which(external_labels == base_label)[1]]
      rel_parts <- parts[-1]
    } else {
      # Fallback: assume workspace base if not matched
      base_dir <- .rcw_workspace
      rel_parts <- if (length(parts) > 1) parts[-1] else parts
    }
    if (length(rel_parts) == 0) {
      folder_path <- base_dir
    } else {
      folder_path <- file.path(base_dir, do.call(file.path, as.list(rel_parts)))
    }
    
    if (is.null(folder_path) || !dir.exists(folder_path)) {
      showNotification("Folder not found!", type = "error")
      return()
    }
    
    # Count files
    file_count <- length(list.files(folder_path, pattern = "\\.R$", recursive = TRUE))
    
    showModal(modalDialog(
      title = "Confirm Delete Folder",
      p(sprintf("Are you sure you want to delete folder '%s'?", folder_name)),
      p(style = "color: #dc3545; font-weight: 600;", 
        sprintf("This will delete %d R file(s)!", file_count)),
      p(style = "color: #999; font-size: 0.85rem;", folder_path),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete_folder", "Delete Folder", class = "btn-danger",
                    style = "background: #dc3545; border-color: #dc3545;")
      )
    ))
    
    rv$folder_to_delete <- folder_path
  })
  
  observeEvent(input$confirm_delete_folder, {
    req(rv$folder_to_delete)
    
    # Check if it's external folder
    if (rv$folder_to_delete %in% .rcw_external_folders) {
      # Just remove from list, don't delete actual folder
      .rcw_external_folders <<- .rcw_external_folders[.rcw_external_folders != rv$folder_to_delete]
      showNotification("External folder removed from workspace", type = "message")
    } else if (dir.exists(rv$folder_to_delete)) {
      # Delete internal folder
      unlink(rv$folder_to_delete, recursive = TRUE)
      showNotification("Folder deleted", type = "message")
    }
    
    removeModal()
    rv$folder_to_delete <- NULL
    rv$workspace <- scan_workspace()
  })
  
  # Handle node created
  observeEvent(input$node_created, {
    req(input$node_created)
    nc <- input$node_created
    
    # Get first line of the R file
    first_line <- ""
    matching_file <- Find(function(f) f$rel_path == nc$filePath, rv$workspace$files)
    if (!is.null(matching_file)) {
      file_path <- matching_file$path
      first_line <- read_r_file_first_line(file_path)
    }
    
    rv$nodes[[nc$id]] <- list(
      id = nc$id,
      filePath = nc$filePath,
      fileName = nc$fileName,
      x = nc$x,
      y = nc$y,
      firstLine = first_line
    )
    # Default per-node appearance (keeps client/server in sync)
    rv$nodes[[nc$id]]$appearance <- list(shape = 'square', color = '#ffffff', titleColor = '#2c3e50')
    
    # Send first line to client to update node display
    session$sendCustomMessage("update_node_display", list(
      id = nc$id,
      fileName = nc$fileName,
      firstLine = first_line
    ))
    
    # regenerate Rmd since nodes changed
    try({ regenerate_rmd() }, silent = TRUE)
  })

  # Persist per-node appearance changes coming from the client
  observeEvent(input$node_set_appearance, {
    req(input$node_set_appearance)
    msg <- input$node_set_appearance
    # msg is expected to contain id and either color, shape, or titleColor
    id <- as.character(msg$id)
    if (!nzchar(id) || is.null(rv$nodes[[id]])) return()
    if (!is.list(rv$nodes[[id]])) rv$nodes[[id]] <- list(id = id)
    ap <- rv$nodes[[id]]$appearance %||% list()
    if (!is.null(msg$color)) ap$color <- as.character(msg$color)
    if (!is.null(msg$shape)) ap$shape <- as.character(msg$shape)
    if (!is.null(msg$titleColor)) ap$titleColor <- as.character(msg$titleColor)
    rv$nodes[[id]]$appearance <- ap
    # Optionally inform other clients (if multi-session) to apply appearance
    try({ session$sendCustomMessage('apply_node_appearance', list(id = id, appearance = ap)) }, silent = TRUE)
  })
  
  # Handle node moved
  observeEvent(input$node_moved, {
    req(input$node_moved)
    nm <- input$node_moved
    if (nm$id %in% names(rv$nodes)) {
      rv$nodes[[nm$id]]$x <- nm$x
      rv$nodes[[nm$id]]$y <- nm$y
    }
  })
  
  # Handle node selected
  observeEvent(input$node_selected, {
    req(input$node_selected)
    rv$selected_node_id <- input$node_selected$id
  })
  
  # Handle node edit
  observeEvent(input$node_edit, {
    req(input$node_edit)
    node <- input$node_edit
    
    # Find the actual file path from workspace
    matching_file <- Find(function(f) f$rel_path == node$filePath, rv$workspace$files)
    if (is.null(matching_file)) {
      showNotification("File not found!", type = "error")
      return()
    }
    
    file_path <- matching_file$path
    content <- read_r_file(file_path)
    
    showModal(modalDialog(
      title = paste0("Edit: ", node$fileName),
      size = "l",
      tags$style(HTML("#edit_code_content { 
        font-family: 'Courier New', monospace !important;
        font-size: 13px !important;
        padding: 12px !important;
        border: 2px solid #CFB991 !important;
        border-radius: 4px !important;
        background: #f8f9fa !important;
      }")),
      textAreaInput("edit_code_content", NULL, value = content, 
                   rows = 20, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_code_edit", "Save", class = "btn-primary")
      ),
      easyClose = FALSE
    ))
    
    # Store current editing file
    rv$editing_file <- file_path
  })
  
  observeEvent(input$save_code_edit, {
    req(rv$editing_file, input$edit_code_content)
    write_r_file(rv$editing_file, input$edit_code_content)
    
    # Update first line for all nodes using this file
    new_first_line <- read_r_file_first_line(rv$editing_file)
    file_rel_path <- gsub(paste0("^", gsub("([.*+?^${}()|\\[\\]\\\\])", "\\\\\\1", .rcw_workspace), "/?"), "", rv$editing_file)
    
    for (node_id in names(rv$nodes)) {
      if (rv$nodes[[node_id]]$filePath == file_rel_path) {
        rv$nodes[[node_id]]$firstLine <- new_first_line
        # Update client-side display
        session$sendCustomMessage("update_node_display", list(
          id = node_id,
          fileName = rv$nodes[[node_id]]$fileName,
          firstLine = new_first_line
        ))
      }
    }
    
    removeModal()
    showNotification("Code saved successfully!", type = "message")
    rv$editing_file <- NULL
  })
  
  # Handle node run
  observeEvent(input$node_run, {
    req(input$node_run)
    node <- input$node_run
    
    # Find the actual file path from workspace
    matching_file <- Find(function(f) f$rel_path == node$filePath, rv$workspace$files)
    if (is.null(matching_file)) {
      showNotification("File not found!", type = "error")
      return()
    }
    
    file_path <- matching_file$path
    code <- read_r_file(file_path)
    
    rv$logs <- c(rv$logs, sprintf("[%s] RUNNING: %s", Sys.time(), node$fileName))
    
    result <- execute_r_code(code)
    
    if (result$success) {
      rv$logs <- c(rv$logs, sprintf("[%s] DONE", Sys.time()))
      rv$pipeline_result <- result$output
    } else {
      rv$logs <- c(rv$logs, sprintf("[%s] ERROR: %s", Sys.time(), result$error))
      rv$pipeline_result <- paste0("ERROR:\n", result$error, "\n\nOUTPUT:\n", result$output)
    }
  })
  
  # Handle node delete
  observeEvent(input$node_delete, {
    req(input$node_delete)
    node_id <- input$node_delete$id
    
    rv$nodes[[node_id]] <- NULL
    
    # Remove edges
    if (length(rv$edges) > 0) {
      new_edges <- list()
      for (edge in rv$edges) {
        if (!is.null(edge) && edge$source != node_id && edge$target != node_id) {
          new_edges[[length(new_edges) + 1]] <- edge
        }
      }
      rv$edges <- new_edges
    }
    
    session$sendCustomMessage("delete_node_ui", list(id = node_id))
    showNotification("Node removed from canvas", type = "message")
    # regenerate Rmd since nodes/edges changed
    try({ regenerate_rmd() }, silent = TRUE)
  })
  
  # Handle graph connections
  observeEvent(input$graph_connections, {
    connections <- input$graph_connections
    if (is.null(connections) || length(connections) == 0) {
      rv$edges <- list()
      # regenerate Rmd since edges changed
      try({ regenerate_rmd() }, silent = TRUE)
    } else {
      # Check if connections is a list (not atomic vector)
      if (is.list(connections)) {
        rv$edges <- lapply(connections, function(e) {
          list(source = as.character(e$source), target = as.character(e$target))
        })
        # regenerate Rmd since edges changed
        try({ regenerate_rmd() }, silent = TRUE)
      } else {
        rv$edges <- list()
        # regenerate Rmd since edges changed
        try({ regenerate_rmd() }, silent = TRUE)
      }
    }
  })
  
  # Clear canvas
  observeEvent(input$clear_canvas, {
    rv$nodes <- list()
    rv$edges <- list()
    rv$selected_node_id <- NULL
    rv$logs <- character(0)
    rv$pipeline_result <- NULL
    session$sendCustomMessage("clear_canvas_ui", list())
    # regenerate Rmd (will set default message)
    try({ regenerate_rmd() }, silent = TRUE)
  })
  
  # Run pipeline
  observeEvent(input$run_pipeline, {
    rv$logs <- character(0)
    rv$pipeline_result <- ""
    
    if (length(rv$nodes) == 0) {
      showNotification("No nodes on canvas!", type = "warning")
      return()
    }
    
    rv$logs <- c(rv$logs, sprintf("[%s] Pipeline execution started", Sys.time()))
    rv$logs <- c(rv$logs, sprintf("[%s] Total scripts: %d", Sys.time(), length(rv$nodes)))
    
    # Execution: follow connection order (use build_node_hierarchy to respect arrows)
    executed_env <- new.env()  # Shared environment for all scripts
    all_output <- character(0)

    # Prefer explicit topological sort; fallback to DFS hierarchy then insertion order
    ordered <- tryCatch({ compute_execution_order() }, error = function(e) NULL)
    if (is.null(ordered) || length(ordered) == 0) {
      ordered <- tryCatch({ build_node_hierarchy(rv$nodes, rv$edges) }, error = function(e) NULL)
    }
    if (is.null(ordered) || length(ordered) == 0) {
      ordered <- lapply(names(rv$nodes), function(id) list(id = id, node = rv$nodes[[id]], level = 1))
    }

    if (length(ordered) == 0) {
      showNotification('No executable nodes found', type = 'warning')
    }

    for (node_info in ordered) {
      node <- node_info$node
      node_id <- node_info$id

      # Find the actual file path from workspace
      matching_file <- Find(function(f) f$rel_path == node$filePath, rv$workspace$files)
      if (is.null(matching_file)) {
        rv$logs <- c(rv$logs, sprintf("[%s] File not found: %s", Sys.time(), node$fileName))
        next
      }

      file_path <- matching_file$path

      rv$logs <- c(rv$logs, sprintf("[%s] Executing: %s", Sys.time(), node$fileName))

      code <- read_r_file(file_path)
      result <- execute_r_code(code, executed_env)

      if (result$success) {
        rv$logs <- c(rv$logs, sprintf("[%s] Completed: %s", Sys.time(), node$fileName))
        all_output <- c(all_output,
                       sprintf("=== %s ===\n%s\n", node$fileName, result$output))
      } else {
        rv$logs <- c(rv$logs, sprintf("[%s] Error in %s: %s",
                                      Sys.time(), node$fileName, result$error))
        all_output <- c(all_output,
                       sprintf("=== %s (ERROR) ===\n%s\n", node$fileName, result$error))
      }
    }

    rv$pipeline_result <- paste(all_output, collapse = "\n\n")
    rv$logs <- c(rv$logs, sprintf("[%s] Pipeline completed", Sys.time()))
    showNotification("Pipeline execution completed!", type = "message")
  })
  
  # Output displays
  output$pipeline_output <- renderText({
    if (is.null(rv$pipeline_result) || rv$pipeline_result == "") {
      return("No output yet. Run the pipeline to see results.")
    }
    rv$pipeline_result
  })
  
  output$run_log <- renderText({
    if (length(rv$logs) == 0) {
      return("No logs yet.")
    }
    paste(rv$logs, collapse = "\n")
  })
  
  # Generate Rmd preview - show all content with scrollbar
  output$rmd_preview <- renderText({
    # Build on-demand using the same order as execution to ensure consistency
    ord <- tryCatch({ compute_execution_order() }, error = function(e) list())
    if (length(ord) == 0) return("No nodes on canvas. Add some R files to the canvas to generate R Markdown.")
    rmd <- tryCatch({
      generate_rmd_from_hierarchy(ord, rv$workspace$files)
    }, error = function(e) paste0("Error generating Rmd: ", e$message))
    rmd
  })
  
  # Export to Rmd
  output$export_rmd <- downloadHandler(
    filename = function() {
      paste0("rcw_workflow_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rmd")
    },
    content = function(file) {
      # Ensure client-side connections are synced before export
      try({ session$sendCustomMessage('request_graph_sync', list()) }, silent = TRUE)
      timeout <- Sys.time() + 1
      while (Sys.time() < timeout && length(rv$edges) == 0) Sys.sleep(0.05)

      ord <- compute_execution_order()
      rmd_content <- generate_rmd_from_hierarchy(ord, rv$workspace$files)
      writeLines(rmd_content, con = file)
    }
  )
  
  # Handle SVG export button click
  observeEvent(input$export_svg_btn, {
    # Trigger client-side SVG export
    session$sendCustomMessage("export_svg_trigger", list())
  })
  
  # Handle SVG export events
  observeEvent(input$svg_export_started, {
    msg <- get_label("rcw_svg_export_generating", lang())
    showNotification(msg, type = "message", duration = 2)
  })
  
  observeEvent(input$svg_export_completed, {
    msg <- get_label("rcw_svg_export_success", lang())
    showNotification(msg, type = "message", duration = 3)
  })
  
  observeEvent(input$svg_export_error, {
    msg <- get_label("rcw_svg_export_error_msg", lang())
    showNotification(paste0(msg, ": ", input$svg_export_error), type = "error", duration = 5)
  })
}

shinyApp(ui, server)
