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
  library(htmltools)
  
  # Optional deps (do NOT install at runtime to avoid blocking on load)
  HAS_SHINYFILES <- requireNamespace("shinyFiles", quietly = TRUE)
  HAS_FS <- requireNamespace("fs", quietly = TRUE)
  HAS_BASE64ENC <- requireNamespace("base64enc", quietly = TRUE)
  HAS_YAML <- requireNamespace("yaml", quietly = TRUE)
  if (HAS_SHINYFILES) library(shinyFiles)
  if (HAS_FS) library(fs)
  if (HAS_BASE64ENC) library(base64enc)
  if (HAS_YAML) library(yaml)
})

# Silence lintr global variable warnings for optional deps
if (exists("globalVariables", where = asNamespace("utils"), mode = "function")) {
  utils::globalVariables(c("HAS_YAML", "HAS_BASE64ENC", "HAS_SHINYFILES"))
}

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
    folders = character(0),
    files = list()
  )
  
  # Filter out NULL or empty paths
  workspace_paths <- workspace_paths[!is.null(workspace_paths) & nzchar(workspace_paths)]
  
  for (workspace_path in workspace_paths) {
    if (is.null(workspace_path) || !nzchar(workspace_path) || !dir.exists(workspace_path)) {
      next
    }
    
    # Get all R and Rmd files recursively
    all_files <- list.files(workspace_path, pattern = "\\.(R|r|Rmd|rmd)$", 
                            recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    
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
      
      # Track folders as character vector to keep unique() safe
      if (!folder %in% structure$folders) {
        structure$folders <- c(structure$folders, folder)
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
        structure$folders <- c(structure$folders, folder_label)
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

# Read YAML front matter from an Rmd file
read_rmd_front_matter <- function(file_path) {
  if (!file.exists(file_path)) return(NULL)
  lines <- readLines(file_path, warn = FALSE)
  if (length(lines) < 2 || trimws(lines[1]) != "---") return(NULL)
  end_idx <- which(trimws(lines[-1]) %in% c("---", "..."))
  if (length(end_idx) == 0) return(NULL)
  end_idx <- end_idx[1] + 1
  yaml_text <- paste(lines[2:(end_idx - 1)], collapse = "\n")
  if (!exists("HAS_YAML") || !HAS_YAML) { # nolint
    return(list(raw = yaml_text, parsed = NULL, error = "yaml package not available"))
  }
  parsed <- tryCatch({
    yaml::yaml.load(yaml_text)
  }, error = function(e) {
    list(.error = e$message)
  })
  list(raw = yaml_text, parsed = parsed)
}

# Normalize port specification into a consistent list
normalize_ports <- function(ports_raw) {
  if (is.null(ports_raw)) return(list())
  if (is.list(ports_raw) && !is.data.frame(ports_raw)) {
    # Named list (name -> spec) or list of specs
    if (!is.null(names(ports_raw)) && any(nzchar(names(ports_raw)))) {
      ports <- lapply(names(ports_raw), function(nm) {
        spec <- ports_raw[[nm]]
        if (!is.list(spec)) spec <- list()
        spec$name <- spec$name %||% nm
        spec
      })
      return(ports)
    }
    return(ports_raw)
  }
  list()
}

# Build a ModuleSpec from Rmd YAML front matter
parse_rmd_interface <- function(file_path) {
  fm <- read_rmd_front_matter(file_path)
  if (is.null(fm) || is.null(fm$parsed) || is.list(fm$parsed) && !is.null(fm$parsed$.error)) {
    return(NULL)
  }
  y <- fm$parsed
  module <- y$module %||% list()
  module_name <- module$name %||% y$title %||% tools::file_path_sans_ext(basename(file_path))
  module_desc <- module$description %||% y$description %||% ""
  module_version <- module$version %||% y$version %||% ""

  inputs <- normalize_ports(y$inputs)
  outputs <- normalize_ports(y$outputs)

  normalize_port <- function(p) {
    p <- p %||% list()
    list(
      name = p$name %||% "",
      base_type = p$base_type %||% p$type %||% "any",
      semantic_type = p$semantic_type %||% p$semantic %||% "",
      required = if (is.null(p$required)) TRUE else isTRUE(p$required),
      default = p$default %||% NULL,
      description = p$description %||% "",
      cardinality = p$cardinality %||% "single",
      file_extensions = p$file_extensions %||% p$extensions %||% NULL,
      tags = p$tags %||% NULL
    )
  }

  inputs_norm <- lapply(inputs, normalize_port)
  outputs_norm <- lapply(outputs, normalize_port)

  list(
    id = paste0("module_", gsub("[^a-zA-Z0-9]+", "_", tools::file_path_sans_ext(basename(file_path)))),
    name = module_name,
    description = module_desc,
    version = module_version,
    inputs = inputs_norm,
    outputs = outputs_norm
  )
}

# Example Rmd interface specifications (YAML front matter only)
rmd_interface_examples <- paste(
  "---",
  "module:",
  "  name: \"Inbreeding Summary\"",
  "  description: \"Compute summary statistics from pedigree.\"",
  "inputs:",
  "  - name: pedigree",
  "    type: dataframe",
  "    semantic_type: pedigree_table",
  "    required: true",
  "outputs:",
  "  - name: summary_table",
  "    type: dataframe",
  "    semantic_type: qc_report",
  "---",
  "",
  "---",
  "module:",
  "  name: \"GWAS Runner\"",
  "  description: \"Run GWAS on genotype/phenotype inputs.\"",
  "inputs:",
  "  - name: geno",
  "    type: file",
  "    semantic_type: genotype_plink",
  "    file_extensions: [\".bed\", \".bim\", \".fam\"]",
  "  - name: pheno",
  "    type: dataframe",
  "    semantic_type: phenotype_dataframe",
  "outputs:",
  "  - name: summary",
  "    type: dataframe",
  "    semantic_type: gwas_summary",
  "  - name: manhattan",
  "    type: plot",
  "    semantic_type: manhattan_plot",
  "---",
  "",
  "---",
  "module:",
  "  name: \"Relationship Matrix\"",
  "inputs:",
  "  - name: pedigree",
  "    type: dataframe",
  "    semantic_type: pedigree_table",
  "  - name: method",
  "    type: string",
  "    default: \"A\"",
  "outputs:",
  "  - name: K",
  "    type: dataframe",
  "    semantic_type: relationship_matrix",
  "---",
  sep = "\n"
)

# Execute R code and capture output (with echo support like RStudio console)
execute_r_code <- function(code, env = parent.frame(), echo = TRUE) {
  output <- list(
    success = TRUE,
    result = NULL,
    output = "",
    error = NULL,
    plots = list()
  )
  
  # Create temporary file for source()
  temp_file <- tempfile(fileext = ".R")
  writeLines(code, temp_file)
  on.exit(unlink(temp_file), add = TRUE)
  
  # Capture plots
  plot_files <- list()
  plot_counter <- 0
  
  # Capture all output including echo
  output_lines <- character(0)
  
  tryCatch({
    # Use source with echo=TRUE to show code lines
    if (echo) {
      # Capture both code and output
      output_text <- capture.output({
        result <- source(temp_file, local = env, echo = TRUE, print.eval = TRUE)
      }, type = "output")
      # Ensure output_text is atomic character vector
      if (is.character(output_text)) {
        output_lines <- c(output_lines, as.character(output_text))
      } else {
        output_lines <- c(output_lines, as.character(output_text))
      }
    } else {
      output_text <- capture.output({
        result <- source(temp_file, local = env, echo = FALSE)
      }, type = "output")
      # Ensure output_text is atomic character vector
      if (is.character(output_text)) {
        output_lines <- c(output_lines, as.character(output_text))
      } else {
        output_lines <- c(output_lines, as.character(output_text))
      }
    }
    
    # Check for plots - save current plot if exists
    # Note: This approach captures the last plot from the graphics device
    # For better plot capture, users should explicitly save plots in their code
    if (length(dev.list()) > 0) {
      # Get the current device (excluding null device)
      active_devs <- dev.list()
      if (length(active_devs) > 0) {
        # Use the first active device
        current_dev <- active_devs[1]
        plot_counter <- plot_counter + 1
        plot_file <- tempfile(fileext = ".png")
        tryCatch({
          # Switch to the device and save it
          dev.set(current_dev)
          # Create a new PNG device and copy the plot
          png(filename = plot_file, width = 800, height = 600, res = 100)
          # Replay the plot if we can get it
          if (exists("recordPlot", envir = .GlobalEnv) || 
              requireNamespace("grDevices", quietly = TRUE)) {
            # Try to get the plot record
            plot_record <- tryCatch(recordPlot(), error = function(e) NULL)
            if (!is.null(plot_record)) {
              replayPlot(plot_record)
            }
          }
          dev.off()
          plot_files[[plot_counter]] <- plot_file
        }, error = function(e) {
          # If recording fails, just note that a plot was created
          # The plot might still be visible in the device
          tryCatch(dev.off(), error = function(e2) {})
        })
      }
    }
    
    # Ensure output_lines is atomic before pasting
    output_lines <- as.character(output_lines)
    output$output <- paste(output_lines, collapse = "\n")
    output$result <- result
    output$plots <- plot_files
    
  }, error = function(e) {
    output$success <<- FALSE
    output$error <<- e$message
    # Ensure all components are atomic before pasting
    error_line <- as.character(paste0("Error: ", e$message))
    output_lines <- as.character(output_lines)
    output$output <<- paste(c(output_lines, error_line), collapse = "\n")
  })
  
  return(output)
}

# Build hierarchical structure following canvas connection order
build_node_hierarchy <- function(nodes, edges) {
  if (length(nodes) == 0) return(list())
  
  # Ensure nodes is a list
  if (!is.list(nodes)) {
    return(list())
  }
  
  all_node_ids <- names(nodes)
  if (is.null(all_node_ids) || length(all_node_ids) == 0) {
    return(list())
  }
  
  # Build adjacency list maintaining edge order
  adj_list <- setNames(lapply(all_node_ids, function(id) character(0)), all_node_ids)
  in_degree <- setNames(rep(0, length(all_node_ids)), all_node_ids)
  
  # Preserve the order of edges as they appear in the canvas
  # Ensure edges is a list and handle NULL/empty cases
  if (!is.null(edges) && length(edges) > 0 && is.list(edges)) {
    for (i in seq_along(edges)) {
      edge <- edges[[i]]
      # Check if edge is a list (not atomic vector)
      if (is.list(edge) && !is.null(edge$source) && !is.null(edge$target)) {
        source_id <- edge$source
        target_id <- edge$target
        if (source_id %in% all_node_ids && target_id %in% all_node_ids) {
          adj_list[[source_id]] <- c(adj_list[[source_id]], target_id)
          in_degree[[target_id]] <- in_degree[[target_id]] + 1
        }
      }
    }
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
    
    # Ensure node is a list before accessing
    node <- nodes[[node_id]]
    if (!is.list(node)) {
      # If node is not a list, create a basic structure
      node <- list(filePath = "", fileName = node_id)
    }
    
    sorted_nodes <<- c(sorted_nodes, list(list(
      id = node_id,
      node = node,
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
      # Ensure node is a list before accessing
      node <- nodes[[node_id]]
      if (!is.list(node)) {
        # If node is not a list, create a basic structure
        node <- list(filePath = "", fileName = node_id)
      }
      sorted_nodes <- c(sorted_nodes, list(list(
        id = node_id,
        node = node,
        level = 1
      )))
    }
  }
  
  return(sorted_nodes)
}

# Build a lightweight interface manifest from canvas nodes and edges
build_interface_manifest <- function(nodes, edges) {
  if (length(nodes) == 0) return(list(nodes = list(), edges = list()))
  node_list <- lapply(names(nodes), function(id) {
    n <- nodes[[id]]
    spec <- n$moduleSpec %||% list()
    list(
      id = id,
      fileName = n$fileName %||% "",
      filePath = n$filePath %||% "",
      module = list(
        name = spec$name %||% n$fileName %||% id,
        description = spec$description %||% "",
        version = spec$version %||% ""
      ),
      inputs = spec$inputs %||% list(),
      outputs = spec$outputs %||% list()
    )
  })
  edge_list <- list()
  if (!is.null(edges) && length(edges) > 0) {
    for (edge in edges) {
      if (is.list(edge)) {
        edge_list[[length(edge_list) + 1]] <- list(
          source = edge$source %||% "",
          target = edge$target %||% "",
          sourcePort = edge$sourcePort %||% "",
          targetPort = edge$targetPort %||% "",
          sourceType = edge$sourceType %||% "",
          targetType = edge$targetType %||% ""
        )
      }
    }
  }
  list(nodes = node_list, edges = edge_list)
}

# Generate R Markdown from sorted hierarchy with connection order markers
generate_rmd_from_hierarchy <- function(sorted_nodes, workspace_files, edges = NULL, include_metadata = TRUE, node_specs = list()) {
  rmd_lines <- c()
  
  if (include_metadata) {
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
  }
  
  if (length(sorted_nodes) == 0) {
    return(paste(c(rmd_lines, "", "_No nodes in workflow_", ""), collapse = "\n"))
  }
  
  # Build connection map for metadata
  node_connections <- list()
  for (i in seq_along(sorted_nodes)) {
    node_info <- sorted_nodes[[i]]
    node_id <- node_info$id
    node_connections[[node_id]] <- list(
      index = i,
      level = node_info$level,
      fileName = node_info$node$fileName
    )
  }
  
    # Add RCW metadata comment at the beginning (after YAML if included)
  if (include_metadata) {
    manifest <- build_interface_manifest(node_specs, edges)
    manifest_json <- tryCatch({
      jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE)
    }, error = function(e) {
      "{}"
    })
    rmd_lines <- c(rmd_lines, 
      "",
      "<!-- RCW_METADATA_START",
      paste0("RCW_VERSION: 1.0"),
      paste0("NODE_COUNT: ", length(sorted_nodes)),
      "INTERFACE_MANIFEST_JSON:",
      manifest_json,
      "NODES:",
      sep = "\n"
    )
    
    for (node_info in sorted_nodes) {
      node_id <- node_info$id
      node <- node_info$node
      rmd_lines <- c(rmd_lines, 
        paste0("  - id: ", node_id),
        paste0("    fileName: ", node$fileName),
        paste0("    filePath: ", node$filePath),
        paste0("    level: ", node_info$level),
        paste0("    index: ", which(sapply(sorted_nodes, function(x) x$id == node_id)))
      )
    }
    
    # Add connections/edges information
    if (!is.null(edges) && length(edges) > 0 && is.list(edges)) {
      rmd_lines <- c(rmd_lines, "CONNECTIONS:")
      valid_edges <- 0
      for (edge in edges) {
        if (is.list(edge) && !is.null(edge$source) && !is.null(edge$target)) {
          rmd_lines <- c(rmd_lines, 
            paste0("  - source: ", edge$source),
            paste0("    target: ", edge$target)
          )
          valid_edges <- valid_edges + 1
        }
      }
      # Add connection count for debugging
      rmd_lines <- c(rmd_lines, paste0("CONNECTION_COUNT: ", valid_edges))
    } else {
      rmd_lines <- c(rmd_lines, "CONNECTIONS:", "CONNECTION_COUNT: 0")
    }
    
    rmd_lines <- c(rmd_lines, "RCW_METADATA_END -->", "")
  }
  
  # Add each node in topological order with connection markers
  for (i in seq_along(sorted_nodes)) {
    node_info <- sorted_nodes[[i]]
    level <- node_info$level
    node <- node_info$node
    node_id <- node_info$id
    
    # Add connection order marker as comment
    rmd_lines <- c(rmd_lines, 
      paste0("<!-- RCW_NODE_START id:", node_id, " level:", level, " index:", i, " -->"),
      ""
    )
    
    # Create heading (level + 1 to start from ##)
    heading <- paste0(paste(rep("#", level + 1), collapse = ""), " ", node$fileName)
    rmd_lines <- c(rmd_lines, heading, "")
    
    # Find file content - match by absolute path or filename
    matching_file <- NULL
    for (f in workspace_files) {
      # Try to match by absolute path first
      if (normalizePath(f$path, winslash = "/", mustWork = FALSE) == 
          normalizePath(node$filePath, winslash = "/", mustWork = FALSE)) {
        matching_file <- f
        break
      }
      # Fallback: match by filename
      if (basename(f$path) == node$fileName) {
        matching_file <- f
        break
      }
    }
    
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
    
    # Add connection order marker end
    rmd_lines <- c(rmd_lines, 
      "",
      paste0("<!-- RCW_NODE_END id:", node_id, " -->"),
      ""
    )
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
    html, body { 
      height: 100%; margin: 0; padding: 0; 
      background: #1e1e1e; color: #d4d4d4;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
    }
    
    /* Top Navigation Bar */
    .top-header {
      height: 48px; background: #2d2d30;
      border-bottom: 1px solid #3e3e42;
      display: flex; align-items: center;
      padding: 0 16px; flex-shrink: 0;
      z-index: 1000;
    }
    .header-left {
      display: flex; align-items: center; gap: 12px;
      flex: 1;
    }
    .header-logo {
      width: 32px; height: 32px;
      background: linear-gradient(135deg, #CEB888 0%, #B89D5D 100%);
      border-radius: 4px; display: flex;
      align-items: center; justify-content: center;
      color: #000; font-weight: 700; font-size: 18px;
    }
    .header-title {
      font-size: 14px; font-weight: 600; color: #cccccc;
    }
    .header-right {
      display: flex; align-items: center; gap: 12px;
    }
    .webr-status {
      display: flex; align-items: center; gap: 6px;
      padding: 4px 12px; border-radius: 4px;
      font-size: 12px; font-weight: 500;
    }
    .webr-status.loading { background: #3e3e42; color: #ffa500; }
    .webr-status.ready { background: #1e4d1e; color: #4caf50; }
    .webr-status.error { background: #4d1e1e; color: #f44336; }
    .webr-status-dot {
      width: 8px; height: 8px; border-radius: 50%;
      background: currentColor; animation: pulse 2s infinite;
    }
    .webr-status.ready .webr-status-dot { animation: none; }
    
    /* Main Container */
    .main-container {
      display: flex; height: calc(100vh - 48px);
      overflow: hidden; position: relative;
    }
    
    /* Left Sidebar - Explorer */
    .left-sidebar {
      width: 256px; background: #252526;
      border-right: 1px solid #3e3e42;
      display: flex; flex-direction: column;
      position: relative;
    }
    .left-sidebar.hidden {
      width: 0; border-right: none; overflow: hidden;
    }
    .sidebar-toggle-btn {
      position: fixed; top: 50%; transform: translateY(-50%);
      width: 20px; height: 40px; background: #3e3e42;
      border: 1px solid #3e3e42; border-radius: 0 4px 4px 0;
      cursor: pointer; z-index: 1000; display: flex;
      align-items: center; justify-content: center;
      color: #cccccc; font-size: 12px;
      transition: all 0.2s;
    }
    .sidebar-toggle-btn:hover {
      background: #4e4e52; color: #ffffff;
    }
    #toggleLeftSidebarBtn {
      left: 0; border-radius: 0 4px 4px 0;
    }
    #toggleRightSidebarBtn {
      right: 0; border-radius: 4px 0 0 4px;
    }
    .file-tree-item.dragging {
      opacity: 0.5;
    }
    .folder-drop-hover {
      background: rgba(206, 184, 136, 0.2) !important;
      border: 2px dashed #CEB888 !important;
    }
    #toggleLeftSidebarBtn:not(.visible),
    #toggleRightSidebarBtn:not(.visible) {
      display: none;
    }
    .sidebar-header {
      height: 35px; padding: 0 12px;
      display: flex; align-items: center;
      justify-content: space-between;
      border-bottom: 1px solid #3e3e42;
      font-size: 11px; font-weight: 600;
      text-transform: uppercase; color: #cccccc;
      letter-spacing: 0.5px;
    }
    .sidebar-content {
      flex: 1; overflow-y: auto; overflow-x: hidden;
      padding: 8px;
    }
    .sidebar-footer {
      border-top: 1px solid #3e3e42;
      padding: 8px;
    }
    
    /* Center Canvas */
    .center-canvas {
      flex: 1; overflow: hidden;
      background: #1e1e1e; position: relative;
    }
    
    /* Right Sidebar */
    .right-sidebar {
      width: 320px; background: #252526;
      border-left: 1px solid #3e3e42;
      display: flex; flex-direction: column;
      flex-shrink: 0; transition: width 0.3s ease;
    }
    .right-sidebar.hidden { 
      width: 0; border-left: none; overflow: hidden;
    }
    .right-sidebar {
      position: relative;
    }
    .right-sidebar-tabs {
      display: flex; border-bottom: 1px solid #3e3e42;
      background: #2d2d30;
    }
    .right-sidebar-tab {
      flex: 1; padding: 10px 16px;
      background: transparent; border: none;
      color: #cccccc; font-size: 12px;
      font-weight: 500; cursor: pointer;
      border-bottom: 2px solid transparent;
      transition: all 0.2s;
    }
    .right-sidebar-tab:hover {
      background: #2d2d30; color: #ffffff;
    }
    .right-sidebar-tab.active {
      color: #CEB888; border-bottom-color: #CEB888;
      background: #252526;
    }
    .right-sidebar-content {
      flex: 1; overflow-y: auto; padding: 16px;
    }
    .right-sidebar-content .tab-pane {
      display: none;
    }
    .right-sidebar-content .tab-pane.active {
      display: block;
    }

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

    /* Folder/File Tree - Dark Theme */
    .file-tree {
      margin: 0; padding: 0; list-style: none;
    }
    .file-tree-item {
      padding: 4px 8px; margin: 2px 0;
      background: transparent; border: none;
      border-radius: 3px; cursor: pointer;
      transition: all 0.15s;
      user-select: none;
      color: #cccccc; font-size: 13px;
      display: flex; align-items: center;
    }
    .file-tree-item:hover {
      background: #2a2d2e;
    }
    .file-tree-item.folder {
      font-weight: 500; color: #cccccc;
      display: flex; align-items: center;
      justify-content: space-between;
    }
    .file-tree-item.file {
      padding-left: 24px; color: #cccccc;
    }
    .file-tree-item.file:hover {
      background: #37373d;
    }
    .file-tree-item.file.selected {
      background: #094771; color: #ffffff;
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

    /* Canvas - Dark Theme */
    #canvas {
      width: 100%; height: 100%;
      background: #1e1e1e;
      background-image: 
        linear-gradient(rgba(255,255,255,0.02) 1px, transparent 1px),
        linear-gradient(90deg, rgba(255,255,255,0.02) 1px, transparent 1px);
      background-size: 20px 20px;
      position: relative;
      overflow: hidden;
    }

    /* Canvas nodes - Dark Theme */
    .canvas-node {
      position: absolute;
      width: 200px; min-height: 100px;
      background: #2d2d30;
      border: 2px solid #3e3e42; border-radius: 6px;
      padding: 12px; cursor: move;
      box-shadow: 0 2px 8px rgba(0,0,0,0.3);
      z-index: 100;
      color: #d4d4d4;
    }
    .canvas-node:hover {
      border-color: #CEB888;
      box-shadow: 0 4px 12px rgba(206,184,136,0.3);
    }
    .canvas-node.selected {
      border-color: #CEB888;
      background: #37373d;
      box-shadow: 0 4px 16px rgba(206,184,136,0.4);
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
    
    /* Code Editor Modal */
    .modal { z-index: 10000; }
    .modal-content {
      border: 2px solid #CEB888;
      border-radius: 8px;
      box-shadow: 0 4px 20px rgba(0,0,0,0.3);
    }
    .modal-header {
      background: linear-gradient(135deg, #CEB888 0%, #B89D5D 100%);
      border-bottom: 2px solid #CFB991;
      color: #000;
    }
    .modal-title { font-weight: 700; }
    .code-editor-area {
      width: 100%; min-height: 400px;
      font-family: 'Courier New', monospace;
      font-size: 13px; padding: 12px;
      border: 2px solid #CFB991; border-radius: 4px;
      background: #f8f9fa;
      resize: vertical;
    }
    
    /* Log Panel - In Right Sidebar Tab */
    .log-content {
      font-family: 'Courier New', monospace; font-size: 12px;
      white-space: pre-wrap; word-wrap: break-word;
      line-height: 1.6;
    }
    .log-content .log-line { margin: 2px 0; }
    .log-content .log-error { color: #f48771; font-weight: 600; }
    .log-content .log-success { color: #89d185; }
    .log-content .log-code { color: #569cd6; }
    .log-content .log-output { color: #d4d4d4; }
    
    
    /* Node Status Icons */
    .node-status {
      position: absolute; top: 5px; right: 5px;
      width: 20px; height: 20px; border-radius: 50%;
      display: none; z-index: 101;
    }
    .node-status.running {
      display: block; background: #ffa500; animation: pulse 1s infinite;
    }
    .node-status.success {
      display: block; background: #4caf50;
    }
    .node-status.error {
      display: block; background: #f44336;
    }
    @keyframes pulse {
      0%, 100% { opacity: 1; }
      50% { opacity: 0.5; }
    }
    
    /* Action Buttons */
    .action-buttons {
      position: fixed; top: 10px; right: 20px; z-index: 1100;
      display: flex; gap: 8px;
    }
    
    /* Node Output Area */
    .node-output {
      margin-top: 8px; padding: 0;
      background: #f8f9fa; border-radius: 4px;
      font-size: 11px;
    }
    .node-output-header {
      padding: 6px 8px;
      background: #e9ecef;
      border-radius: 4px 4px 0 0;
      cursor: pointer;
      display: flex;
      align-items: center;
      justify-content: space-between;
      font-size: 10px;
      font-weight: 600;
      color: #495057;
      user-select: none;
    }
    .node-output-header:hover {
      background: #dee2e6;
    }
    .node-output-toggle {
      font-size: 10px;
      color: #6c757d;
    }
    .node-output-content {
      padding: 8px;
      max-height: 150px;
      overflow-y: auto;
      display: none;
    }
    .node-output-content.expanded {
      display: block;
    }
    .node-plot {
      margin-top: 8px; max-width: 100%;
      border: 1px solid #ddd; border-radius: 4px;
    }
    .node-ports {
      margin-top: 8px;
      display: grid;
      grid-template-columns: 1fr 1fr;
      gap: 6px;
      font-size: 11px;
    }
    .node-ports-section {
      background: #1e1e1e;
      border: 1px solid #3e3e42;
      border-radius: 4px;
      padding: 6px;
    }
    .node-ports-title {
      font-size: 10px;
      color: #999;
      text-transform: uppercase;
      margin-bottom: 4px;
    }
    .node-port {
      display: flex;
      align-items: center;
      gap: 4px;
      padding: 2px 0;
      white-space: nowrap;
      overflow: hidden;
      text-overflow: ellipsis;
    }
    .node-port .port-dot {
      width: 8px;
      height: 8px;
      border-radius: 50%;
      flex-shrink: 0;
      background: #CEB888;
    }
    .node-port .port-label {
      flex: 1;
      color: #d4d4d4;
    }
    .node-port .port-type {
      color: #89d185;
      font-size: 10px;
    }
    .node-port .port-semantic {
      color: #569cd6;
      font-size: 10px;
    }
  ")),
      # Additional node-shape styles
      tags$style(HTML("\
        /* Node shape modifiers applied to .canvas-node */\
        .canvas-node.node-shape-rectangle { border-radius: 3px !important; width: 200px !important; min-height: 100px !important; }\
        .canvas-node.node-shape-rounded { border-radius: 12px !important; width: 200px !important; min-height: 100px !important; }\
        .canvas-node.node-shape-circle { border-radius: 50% !important; width: 120px !important; height: 120px !important; display: flex !important; align-items: center !important; justify-content: center !important; padding: 12px !important; }\
        .canvas-node.node-shape-circle .node-title { white-space: normal; text-align: center; }\
        .canvas-node.node-shape-circle .node-info { display: none !important; }\
        .canvas-node.node-shape-diamond { width: 110px !important; height: 110px !important; clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%); display: flex !important; align-items: center !important; justify-content: center !important; position: relative !important; }\
        .canvas-node.node-shape-diamond::before { content: ''; position: absolute; left: 0; top: 0; right: 0; bottom: 0; box-sizing: border-box; pointer-events: none; clip-path: polygon(50% 0%, 100% 50%, 50% 100%, 0% 50%); border: 3px solid #CEB888; border-radius: 0; z-index: 0; box-shadow: 0 4px 12px rgba(0,0,0,0.15); }\
        .canvas-node.node-shape-diamond > * { position: relative; z-index: 1; }\
        .canvas-node.node-shape-diamond .node-title, .canvas-node.node-shape-diamond .node-info { transform: none; text-align: center; }\
        .canvas-node.node-shape-diamond:hover::before { border-color: #B89D5D; box-shadow: 0 6px 16px rgba(0,0,0,0.25); }\
        .canvas-node.selected.node-shape-diamond::before { border-color: #B89D5D; box-shadow: 0 6px 20px rgba(184,157,93,0.5); }\
        .canvas-node.node-shape-parallelogram { transform: skew(-20deg); width: 200px !important; min-height: 100px !important; position: relative !important; }\
        .canvas-node.node-shape-parallelogram > * { transform: skew(20deg); }\
        .canvas-node.node-shape-cylinder { border-radius: 50px 50px 0 0 !important; width: 200px !important; min-height: 100px !important; position: relative !important; }\
        .canvas-node.node-shape-cylinder::after { content: ''; position: absolute; bottom: -10px; left: 0; right: 0; height: 20px; background: inherit; border-radius: 0 0 50px 50px; border: 3px solid #CEB888; border-top: none; }\
        \
        /* Shape selector buttons */\
        .shape-selector { display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; margin-top: 12px; }\
        .shape-btn { width: 100%; height: 60px; border: 2px solid #3e3e42; border-radius: 6px; background: #1e1e1e; cursor: pointer; display: flex; align-items: center; justify-content: center; transition: all 0.2s; position: relative; }\
        .shape-btn:hover { border-color: #CEB888; background: #2d2d30; }\
        .shape-btn.selected { border-color: #CEB888; background: #CEB888; box-shadow: 0 0 0 2px rgba(206,184,136,0.3); }\
        .shape-preview { width: 40px; height: 30px; border: 2px solid #d4d4d4; background: #2d2d30; }\
        .shape-preview.rectangle { border-radius: 2px; }\
        .shape-preview.rounded { border-radius: 8px; }\
        .shape-preview.circle { border-radius: 50%; width: 30px; height: 30px; }\
        .shape-preview.diamond { transform: rotate(45deg); border-radius: 2px; width: 25px; height: 25px; }\
        .shape-preview.parallelogram { transform: skew(-20deg); border-radius: 2px; }\
        .shape-preview.cylinder { border-radius: 15px 15px 0 0; position: relative; }\
        .shape-preview.cylinder::after { content: ''; position: absolute; bottom: -3px; left: -2px; right: -2px; height: 6px; background: inherit; border: 2px solid #333; border-top: none; border-radius: 0 0 15px 15px; }\
        \
        /* Color picker */\
        .color-picker-section { margin-top: 16px; }\
        .color-grid { display: grid; grid-template-columns: repeat(8, 1fr); gap: 6px; margin-top: 12px; }\
        .color-swatch { width: 100%; aspect-ratio: 1; border: 2px solid #3e3e42; border-radius: 4px; cursor: pointer; transition: all 0.2s; position: relative; }\
        .color-swatch:hover { transform: scale(1.1); border-color: #d4d4d4; z-index: 10; }\
        .color-swatch.selected { border-color: #CEB888; border-width: 3px; box-shadow: 0 0 0 2px rgba(206,184,136,0.3); }\
        .color-swatch::after { content: '✓'; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); color: #fff; font-weight: bold; font-size: 14px; text-shadow: 0 0 2px rgba(0,0,0,0.5); display: none; }\
        .color-swatch.selected::after { display: block; }\
        .text-color-grid { display: grid; grid-template-columns: repeat(8, 1fr); gap: 6px; margin-top: 12px; }\
        .text-color-swatch { width: 100%; aspect-ratio: 1; border: 2px solid #3e3e42; border-radius: 4px; cursor: pointer; transition: all 0.2s; position: relative; }\
        .text-color-swatch:hover { transform: scale(1.1); border-color: #d4d4d4; z-index: 10; }\
        .text-color-swatch.selected { border-color: #CEB888; border-width: 3px; box-shadow: 0 0 0 2px rgba(206,184,136,0.3); }\
        .text-color-swatch::after { content: '✓'; position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); color: #fff; font-weight: bold; font-size: 14px; text-shadow: 0 0 2px rgba(0,0,0,0.5); display: none; }\
        .text-color-swatch.selected::after { display: block; }\
        .connector-style-selector { display: flex; gap: 8px; margin-top: 12px; }\
        .connector-style-btn { width: 80px; height: 50px; border: 2px solid #3e3e42; border-radius: 4px; background: #1e1e1e; display: flex; align-items: center; justify-content: center; cursor: pointer; transition: all 0.2s; position: relative; }\
        .connector-style-btn:hover { border-color: #CEB888; background: #2d2d30; }\
        .connector-style-btn.selected { border-color: #CEB888; border-width: 3px; box-shadow: 0 0 0 2px rgba(206,184,136,0.3); }\
        .sidebar-action-btn { transition: all 0.2s; }\
        .sidebar-action-btn:hover { background: #3e3e42 !important; border-radius: 3px; }\
        .sidebar-toggle-icon { transition: all 0.2s; }\
        .sidebar-toggle-icon:hover { color: #CEB888 !important; }\
        .color-input-wrapper { margin-top: 12px; display: flex; gap: 8px; align-items: center; }\
        .color-input-wrapper input[type='color'] { width: 60px; height: 40px; border: 2px solid #ddd; border-radius: 4px; cursor: pointer; }\
        .color-input-wrapper input[type='text'] { flex: 1; padding: 8px; border: 2px solid #ddd; border-radius: 4px; }\
        \
      ")),
    
    # jsPlumb initialization (similar to OBX Canvas)
    tags$script(HTML("
    var jsPlumbInstance;
    var canvasNodes = {};

    // Minimal client-side canvas settings (per-node appearance is handled per-node)
    var canvasSettings = { allow_vertical: true };

    function sanitizePortId(portName) {
      return String(portName || '').replace(/[^a-zA-Z0-9_-]/g, '_');
    }

    function isTypeCompatible(sourceType, targetType) {
      if (!sourceType || !targetType) return false;
      if (sourceType === 'any' || targetType === 'any') return true;
      return sourceType === targetType;
    }

    function renderPorts(nodeId, moduleSpec) {
      var nodeEl = $('#' + nodeId);
      if (!nodeEl.length) return;
      var ports = moduleSpec || { inputs: [], outputs: [] };
      var inputs = ports.inputs || [];
      var outputs = ports.outputs || [];

      nodeEl.find('.node-ports').remove();
      var portsHtml = $('<div class=\"node-ports\"></div>');
      var inSection = $('<div class=\"node-ports-section\"></div>');
      var outSection = $('<div class=\"node-ports-section\"></div>');
      inSection.append('<div class=\"node-ports-title\">Inputs</div>');
      outSection.append('<div class=\"node-ports-title\">Outputs</div>');

      inputs.forEach(function(p) {
        var baseType = (p.base_type || p.type || 'any').toLowerCase();
        var semanticType = p.semantic_type || '';
        var portId = nodeId + '__in__' + sanitizePortId(p.name || 'input');
        var label = p.name || 'input';
        var row = $('<div class=\"node-port input\"></div>')
          .attr('id', portId)
          .attr('data-port-name', label)
          .attr('data-base-type', baseType)
          .attr('data-semantic-type', semanticType)
          .append('<span class=\"port-dot\"></span>')
          .append('<span class=\"port-label\">' + label + '</span>')
          .append('<span class=\"port-type\">' + baseType + '</span>');
        if (semanticType) {
          row.append('<span class=\"port-semantic\">' + semanticType + '</span>');
        }
        inSection.append(row);
      });

      outputs.forEach(function(p) {
        var baseType = (p.base_type || p.type || 'any').toLowerCase();
        var semanticType = p.semantic_type || '';
        var portId = nodeId + '__out__' + sanitizePortId(p.name || 'output');
        var label = p.name || 'output';
        var row = $('<div class=\"node-port output\"></div>')
          .attr('id', portId)
          .attr('data-port-name', label)
          .attr('data-base-type', baseType)
          .attr('data-semantic-type', semanticType)
          .append('<span class=\"port-dot\"></span>')
          .append('<span class=\"port-label\">' + label + '</span>')
          .append('<span class=\"port-type\">' + baseType + '</span>');
        if (semanticType) {
          row.append('<span class=\"port-semantic\">' + semanticType + '</span>');
        }
        outSection.append(row);
      });

      portsHtml.append(inSection).append(outSection);
      nodeEl.append(portsHtml);
    }

    function setPortEndpoints(nodeId) {
      var nodeEl = $('#' + nodeId);
      if (!nodeEl.length) return;
      var inPorts = nodeEl.find('.node-port.input');
      var outPorts = nodeEl.find('.node-port.output');
      inPorts.each(function() {
        var portId = $(this).attr('id');
        var baseType = $(this).data('base-type');
        var portName = $(this).data('port-name');
        try { jsPlumbInstance.removeAllEndpoints(portId); } catch(e) {}
        jsPlumbInstance.addEndpoint(portId, Object.assign({
          anchor: 'Left',
          isSource: false,
          isTarget: true,
          parameters: { baseType: baseType, portName: portName, direction: 'in', nodeId: nodeId }
        }, { endpoint: ['Dot', { radius: 5 }], paintStyle: { fill: '#CEB888', stroke: '#B89D5D', strokeWidth: 1 } }));
      });
      outPorts.each(function() {
        var portId = $(this).attr('id');
        var baseType = $(this).data('base-type');
        var portName = $(this).data('port-name');
        try { jsPlumbInstance.removeAllEndpoints(portId); } catch(e) {}
        jsPlumbInstance.addEndpoint(portId, Object.assign({
          anchor: 'Right',
          isSource: true,
          isTarget: false,
          parameters: { baseType: baseType, portName: portName, direction: 'out', nodeId: nodeId }
        }, { endpoint: ['Dot', { radius: 5 }], paintStyle: { fill: '#CEB888', stroke: '#B89D5D', strokeWidth: 1 } }));
      });
    }

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
      // Server can instruct client to apply ports to a specific node
      Shiny.addCustomMessageHandler('apply_node_ports', function(msg) {
        try {
          try { jsPlumbInstance.removeAllEndpoints(msg.id); } catch (e) {}
          renderPorts(msg.id, msg.moduleSpec || {});
          if (msg.moduleSpec && msg.moduleSpec.name) {
            $('#' + msg.id + ' .node-title').text(msg.moduleSpec.name);
          }
          if (canvasNodes[msg.id]) {
            canvasNodes[msg.id].ports = msg.moduleSpec || {};
            canvasNodes[msg.id].hasPorts = true;
          }
          setTimeout(function(){ try { setPortEndpoints(msg.id); } catch (e) {} }, 50);
        } catch (e) { console.warn('apply ports failed', e); }
      });
      // Toggle panel visibility
      Shiny.addCustomMessageHandler('toggle_panel', function(msg) {
        try {
          var side = msg.side;
          if (side === 'left') {
            $('#leftPanel').toggleClass('hidden');
          } else if (side === 'right') {
            $('#rightPanel').toggleClass('hidden');
          }
        } catch (e) { console.warn('toggle panel failed', e); }
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
            if (!(sEP.isSource === true && tEP.isTarget === true)) return false;
            var sType = sEP.getParameter('baseType');
            var tType = tEP.getParameter('baseType');
            if (!isTypeCompatible(sType, tType)) return false;
            return true;
          }

          // Fallback: allow only if sourceId != targetId
          return info.sourceId !== info.targetId;
        } catch (e) {
          console.warn('beforeDrop validation failed', e);
          return false;
        }
      });

      jsPlumbInstance.bind('connection', function(info) {
        // Apply current connector type to new connection (with arrow)
        if (window.currentConnectorType) {
          try {
            var connectorConfig = null;
            if (window.currentConnectorType === 'bezier') {
              connectorConfig = ['Bezier', { curviness: 50 }];
            } else if (window.currentConnectorType === 'straight') {
              connectorConfig = 'Straight';
            } else if (window.currentConnectorType === 'flowchart') {
              connectorConfig = ['Flowchart', { cornerRadius: 6 }];
            }
            
            if (connectorConfig) {
              info.connection.setConnector(connectorConfig);
              // Ensure arrow overlay is added
              try {
                info.connection.addOverlay(['Arrow', { location: 1, width: 16, length: 16 }]);
              } catch(e) {}
            }
          } catch(e) {
            console.warn('Failed to set connector:', e);
          }
        }
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
      initFileTreeContextMenu();

      // Attach context menus to any pre-existing connections (after restore, etc.)
      setTimeout(function(){
        try {
          jsPlumbInstance.getConnections().forEach(function(c){ attachConnectionContextMenu(c); });
        } catch(e) { console.warn('initial attach failed', e); }
      }, 300);
    });
    
    // Sync connections to server
    function syncConnections() {
      try {
        var connections = jsPlumbInstance.getConnections();
        var edges = [];
        connections.forEach(function(conn) {
          var sEP = conn.endpoints && conn.endpoints[0];
          var tEP = conn.endpoints && conn.endpoints[1];
          var sourcePort = sEP && sEP.getParameter ? sEP.getParameter('portName') : null;
          var targetPort = tEP && tEP.getParameter ? tEP.getParameter('portName') : null;
          var sourceType = sEP && sEP.getParameter ? sEP.getParameter('baseType') : null;
          var targetType = tEP && tEP.getParameter ? tEP.getParameter('baseType') : null;
          var sourceNode = sEP && sEP.getParameter ? sEP.getParameter('nodeId') : conn.sourceId;
          var targetNode = tEP && tEP.getParameter ? tEP.getParameter('nodeId') : conn.targetId;
          edges.push({
            source: sourceNode,
            target: targetNode,
            sourcePort: sourcePort,
            targetPort: targetPort,
            sourceType: sourceType,
            targetType: targetType
          });
        });
        Shiny.setInputValue('canvas_connections', edges, {priority: 'event'});
      } catch(e) {
        console.warn('syncConnections failed', e);
      }
    }
    
    // Attach context menu to connection
    function attachConnectionContextMenu(connection) {
      try {
        var connEl = connection.canvas || (connection.getConnector && connection.getConnector().canvas);
        if (!connEl) return;
        
        $(connEl).off('contextmenu.connection').on('contextmenu.connection', function(e) {
          e.preventDefault();
          e.stopPropagation();
          
          // Select this connection
          if (window.selectedConnection) {
            try {
              var oldEl = window.selectedConnection.canvas || (window.selectedConnection.getConnector && window.selectedConnection.getConnector().canvas);
              if (oldEl) $(oldEl).removeClass('connection-selected');
            } catch(e) {}
          }
          window.selectedConnection = connection;
          $(connEl).addClass('connection-selected');
          
          // Show context menu
          var menu = $('#connectionContextMenu');
          if (menu.length === 0) {
            menu = $('<div id=\"connectionContextMenu\" class=\"context-menu\"></div>');
            $('body').append(menu);
          }
          menu.html('<div class=\"context-menu-item\" data-action=\"delete-connection\">Delete Connection</div>');
          menu.css({ left: e.pageX + 'px', top: e.pageY + 'px', display: 'block' });
          
          menu.find('.context-menu-item').off('click').on('click', function() {
            var action = $(this).data('action');
            if (action === 'delete-connection') {
              try {
                jsPlumbInstance.deleteConnection(connection);
                syncConnections();
                window.selectedConnection = null;
              } catch(e) {
                console.warn('Error deleting connection:', e);
              }
            }
            menu.hide();
          });
          
          return false;
        });
      } catch(e) {
        console.warn('attachConnectionContextMenu failed', e);
      }
    }

    function initFileDraggable() {
      setTimeout(function() {
        // Make files draggable
        $('.file-tree-item.file').draggable({
          helper: 'clone',
          revert: 'invalid',
          zIndex: 1000,
          appendTo: 'body',
          cursorAt: { top: 20, left: 40 },
          start: function(e, ui) {
            $(this).addClass('dragging');
          },
          stop: function(e, ui) {
            $(this).removeClass('dragging');
          }
        });
        
        // Make folders droppable for file movement
        $('.file-tree-item.folder').droppable({
          accept: '.file-tree-item.file',
          hoverClass: 'folder-drop-hover',
          tolerance: 'pointer',
          drop: function(event, ui) {
            var filePath = ui.draggable.data('filepath');
            var fileName = ui.draggable.data('filename');
            var targetFolder = $(this).data('folder-id') || 'root';
            
            // Move file to folder
            Shiny.setInputValue('move_file_to_folder', {
              filePath: filePath,
              fileName: fileName,
              targetFolder: targetFolder
            }, {priority: 'event'});
          }
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
      el.removeClass('node-shape-rounded node-shape-rectangle node-shape-circle node-shape-square node-shape-diamond node-shape-parallelogram node-shape-cylinder');
      var shape = appearance.shape || 'rounded';
      el.addClass('node-shape-' + shape);

      if (appearance.color) {
        el.css('background', appearance.color);
        // For cylinder, also update the pseudo-element
        if (shape === 'cylinder') {
          el.css('--cylinder-color', appearance.color);
        }
      }

      // Apply title color if specified
      if (appearance.titleColor) {
        el.find('.node-title').css('color', appearance.titleColor);
      }

      // Reset transforms for shapes that need it
      if (shape === 'parallelogram') {
        el.find('.node-title, .node-info').css('transform', 'skew(20deg)');
      } else {
        el.find('.node-title, .node-info').css('transform', 'none');
      }

      // Rebuild endpoints to match the new shape
      try {
        if (canvasNodes[nodeId] && canvasNodes[nodeId].hasPorts) {
          setPortEndpoints(nodeId);
        } else {
          setNodeEndpoints(nodeId, shape);
        }
      } catch (e) { console.warn('set endpoints failed', e); }

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
        // Rectangle, Rounded, Parallelogram, Cylinder all use standard anchors
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
        .addClass('canvas-node node-shape-rounded')
        .css({ left: x + 'px', top: y + 'px', background: '#ffffff' })
        .html(
          '<div class=\"node-status\" id=\"status-' + id + '\"></div>' +
          '<div class=\"node-title\">' + fileName + '</div>' +
          '<div class=\"node-output\" id=\"output-' + id + '\" style=\"display: none;\">' +
            '<div class=\"node-output-header\" onclick=\"toggleNodeOutput(\\'' + id + '\\')\">' +
              '<span>Output</span>' +
              '<span class=\"node-output-toggle\" id=\"output-toggle-' + id + '\">▼</span>' +
            '</div>' +
            '<div class=\"node-output-content\" id=\"output-content-' + id + '\"></div>' +
          '</div>'
        );

      $('#canvas').append(node);
      var defaultAppearance = appearance || { shape: 'rounded', color: '#2d2d30', titleColor: '#d4d4d4' };
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
        
        // Update style inputs
        var nodeData = canvasNodes[id];
        if (nodeData && nodeData.appearance) {
          var shape = nodeData.appearance.shape || 'rounded';
          // Update shape selector buttons
          $('.shape-btn').removeClass('selected');
          $('.shape-btn[data-shape=\"' + shape + '\"]').addClass('selected');
          // Update color swatches (shape fill)
          $('.color-swatch').removeClass('selected');
          var color = nodeData.appearance.color || '#2d2d30';
          $('.color-swatch[data-color=\"' + color + '\"]').addClass('selected');
          $('#node_color_input').val(color);
          $('#node_color_text').val(color);
          
          // Update text color swatches
          $('.text-color-swatch').removeClass('selected');
          var titleColor = nodeData.appearance.titleColor || '#d4d4d4';
          $('.text-color-swatch[data-color=\"' + titleColor + '\"]').addClass('selected');
          $('#node_title_color').val(titleColor);
          $('#node_title_color_text').val(titleColor);
        }
        
        // Update logs tab with node output
        if (nodeData && nodeData.lastOutput) {
          Shiny.setInputValue('get_node_logs', {nodeId: id}, {priority: 'event'});
        }
        
        // Switch to Style tab when node is selected (but keep execution logs visible)
        $('#rightSidebar').removeClass('hidden');
        $('.right-sidebar-tab[data-tab=\"style\"]').click();
        
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
        '</div>'
      );
      
      menu.css({ left: x + 'px', top: y + 'px', display: 'block' });
      
      // Handle main menu items (non-submenu)
      menu.find('.context-menu-item').off('click').on('click', function(e) {
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
    
    // Hide context menu when clicking outside
    $(document).on('click', function(e) {
      // Check if click is on a context menu item - let the menu handle it first
      var clickedOnMenuItem = $(e.target).closest('.context-menu-item').length > 0;
      
      // Hide node context menu if clicking outside it
      if (!$(e.target).closest('#contextMenu').length && !clickedOnMenuItem) {
        hideContextMenu();
      }
      
      // Hide connection context menu if clicking outside it
      if (!$(e.target).closest('#connectionContextMenu').length && !clickedOnMenuItem) {
        $('#connectionContextMenu').hide();
      }
    });
    
    // Also hide menus when right-clicking elsewhere (to show new menu)
    $(document).on('contextmenu', function(e) {
      // If right-clicking outside menus, hide existing menus
      // (New menu will be shown by the specific contextmenu handler)
      if (!$(e.target).closest('#contextMenu').length && 
          !$(e.target).closest('#connectionContextMenu').length) {
        hideContextMenu();
        $('#connectionContextMenu').hide();
      }
    });
    
    function handleContextMenuAction(action) {
      if (!currentContextNode) return;
      
      if (action === 'edit') {
        openCodeEditor(currentContextNode.id, currentContextNode.filePath, currentContextNode.fileName);
      } else if (action === 'run') {
        Shiny.setInputValue('node_run', currentContextNode, {priority: 'event'});
      } else if (action === 'delete') {
        Shiny.setInputValue('node_delete', currentContextNode, {priority: 'event'});
      }
    }

    // Shape selector click handler
    $(document).on('click', '.shape-btn', function() {
      var shape = $(this).data('shape');
      $('.shape-btn').removeClass('selected');
      $(this).addClass('selected');
      
      var selectedNode = $('.canvas-node.selected');
      if (selectedNode.length === 0) return;
      
      var nodeId = selectedNode.attr('id');
      var nodeData = canvasNodes[nodeId];
      if (!nodeData) return;
      
      var appearance = nodeData.appearance || { shape: 'rounded', color: '#2d2d30', titleColor: '#d4d4d4' };
      appearance.shape = shape;
      
      canvasNodes[nodeId].appearance = appearance;
      applyNodeAppearance(nodeId, appearance);
      
      Shiny.setInputValue('node_set_appearance', { 
        id: nodeId, 
        color: appearance.color, 
        shape: shape,
        titleColor: appearance.titleColor 
      }, {priority: 'event'});
    });
    
    // Color swatch click handler (for shape fill)
    $(document).on('click', '.color-swatch', function() {
      var color = $(this).data('color');
      $('.color-swatch').removeClass('selected');
      $(this).addClass('selected');
      $('#node_color_input').val(color);
      $('#node_color_text').val(color);
      
      var selectedNode = $('.canvas-node.selected');
      if (selectedNode.length === 0) return;
      
      var nodeId = selectedNode.attr('id');
      var nodeData = canvasNodes[nodeId];
      if (!nodeData) return;
      
      var appearance = nodeData.appearance || { shape: 'rounded', color: '#2d2d30', titleColor: '#d4d4d4' };
      appearance.color = color;
      
      canvasNodes[nodeId].appearance = appearance;
      applyNodeAppearance(nodeId, appearance);
      
      Shiny.setInputValue('node_set_appearance', { 
        id: nodeId, 
        color: color, 
        shape: appearance.shape,
        titleColor: appearance.titleColor 
      }, {priority: 'event'});
    });
    
    // Text color swatch click handler
    $(document).on('click', '.text-color-swatch', function() {
      var color = $(this).data('color');
      $('.text-color-swatch').removeClass('selected');
      $(this).addClass('selected');
      $('#node_title_color').val(color);
      $('#node_title_color_text').val(color);
      
      var selectedNode = $('.canvas-node.selected');
      if (selectedNode.length === 0) return;
      
      var nodeId = selectedNode.attr('id');
      var nodeData = canvasNodes[nodeId];
      if (!nodeData) return;
      
      var appearance = nodeData.appearance || { shape: 'rounded', color: '#2d2d30', titleColor: '#d4d4d4' };
      appearance.titleColor = color;
      
      canvasNodes[nodeId].appearance = appearance;
      applyNodeAppearance(nodeId, appearance);
      
      Shiny.setInputValue('node_set_appearance', { 
        id: nodeId, 
        color: appearance.color, 
        shape: appearance.shape,
        titleColor: color 
      }, {priority: 'event'});
    });
    
    // Color input change handler
    $(document).on('input change', '#node_color_input, #node_color_text, #node_title_color, #node_title_color_text', function() {
      var selectedNode = $('.canvas-node.selected');
      if (selectedNode.length === 0) return;
      
      var nodeId = selectedNode.attr('id');
      var colorInput = $('#node_color_input').val();
      var colorText = $('#node_color_text').val();
      var titleColor = $('#node_title_color').val();
      var titleColorText = $('#node_title_color_text').val();
      
      // Sync shape fill color inputs
      if ($(this).attr('id') === 'node_color_input') {
        $('#node_color_text').val(colorInput);
        colorText = colorInput;
      } else if ($(this).attr('id') === 'node_color_text') {
        // Validate hex color
        if (/^#[0-9A-F]{6}$/i.test(colorText)) {
          $('#node_color_input').val(colorText);
          colorInput = colorText;
        }
      }
      
      // Sync text color inputs
      if ($(this).attr('id') === 'node_title_color') {
        $('#node_title_color_text').val(titleColor);
        titleColorText = titleColor;
      } else if ($(this).attr('id') === 'node_title_color_text') {
        // Validate hex color
        if (/^#[0-9A-F]{6}$/i.test(titleColorText)) {
          $('#node_title_color').val(titleColorText);
          titleColor = titleColorText;
        }
      }
      
      var nodeData = canvasNodes[nodeId];
      if (!nodeData) return;
      
      var appearance = nodeData.appearance || { shape: 'rounded', color: '#2d2d30', titleColor: '#d4d4d4' };
      if ($(this).attr('id') === 'node_color_input' || $(this).attr('id') === 'node_color_text') {
        appearance.color = colorInput;
        // Update swatch selection if color matches
        $('.color-swatch').removeClass('selected');
        var matchingSwatch = $('.color-swatch[data-color=\"' + colorInput + '\"]');
        if (matchingSwatch.length) {
          matchingSwatch.addClass('selected');
        }
      } else if ($(this).attr('id') === 'node_title_color' || $(this).attr('id') === 'node_title_color_text') {
        appearance.titleColor = titleColor;
        // Update text color swatch selection if color matches
        $('.text-color-swatch').removeClass('selected');
        var matchingTextSwatch = $('.text-color-swatch[data-color=\"' + titleColor + '\"]');
        if (matchingTextSwatch.length) {
          matchingTextSwatch.addClass('selected');
        }
      }
      
      canvasNodes[nodeId].appearance = appearance;
      applyNodeAppearance(nodeId, appearance);
      
      Shiny.setInputValue('node_set_appearance', { 
        id: nodeId, 
        color: appearance.color, 
        shape: appearance.shape,
        titleColor: appearance.titleColor 
      }, {priority: 'event'});
    });
    
    // Sync color text input when color picker changes (shape fill)
    $(document).on('input', '#node_color_input', function() {
      $('#node_color_text').val($(this).val());
    });
    
    // Sync color picker when text input changes (with validation) - shape fill
    $(document).on('input', '#node_color_text', function() {
      var val = $(this).val();
      if (/^#[0-9A-F]{6}$/i.test(val)) {
        $('#node_color_input').val(val);
      }
    });
    
    // Sync text color text input when color picker changes
    $(document).on('input', '#node_title_color', function() {
      $('#node_title_color_text').val($(this).val());
    });
    
    // Sync text color picker when text input changes (with validation)
    $(document).on('input', '#node_title_color_text', function() {
      var val = $(this).val();
      if (/^#[0-9A-F]{6}$/i.test(val)) {
        $('#node_title_color').val(val);
      }
    });
    
    // Code Editor handlers
    var currentEditingNode = null;
    var currentEditingFilePath = null;
    
    // Open code editor
    window.openCodeEditor = function(nodeId, filePath, fileName) {
      currentEditingNode = nodeId;
      currentEditingFilePath = filePath;
      $('#codeEditorStatus').text('Loading...');
      
      Shiny.setInputValue('load_file_content', { path: filePath }, {priority: 'event'});
      $('#codeEditorModal').modal('show');
    };
    
    // Close modal (Cancel button and close button)
    $(document).on('click', '#cancelCodeBtn, .btn-close', function() {
      $('#codeEditorModal').modal('hide');
    });
    
    // Also support Bootstrap 5 data-bs-dismiss
    $(document).on('click', '[data-bs-dismiss=\"modal\"]', function() {
      $('#codeEditorModal').modal('hide');
    });
    
    // Save code
    $(document).on('click', '#saveCodeBtn', function() {
      var code = $('#codeEditor').val();
      if (currentEditingFilePath) {
        Shiny.setInputValue('save_file_content', {
          path: currentEditingFilePath,
          content: code,
          nodeId: currentEditingNode
        }, {priority: 'event'});
        $('#codeEditorModal').modal('hide');
      }
    });
    
    // Update node status
    window.updateNodeStatus = function(nodeId, status) {
      var statusEl = $('#status-' + nodeId);
      statusEl.removeClass('running success error');
      if (status) {
        statusEl.addClass(status);
      }
    };
    
    // Toggle node output visibility
    window.toggleNodeOutput = function(nodeId) {
      var contentEl = $('#output-content-' + nodeId);
      var toggleEl = $('#output-toggle-' + nodeId);
      
      if (contentEl.hasClass('expanded')) {
        contentEl.removeClass('expanded');
        toggleEl.text('▼');
      } else {
        contentEl.addClass('expanded');
        toggleEl.text('▲');
      }
    };
    
    // Update node output
    window.updateNodeOutput = function(nodeId, output, isError) {
      var outputEl = $('#output-' + nodeId);
      var contentEl = $('#output-content-' + nodeId);
      
      if (output) {
        // Update content
        contentEl.html('<pre class=\"' + (isError ? 'log-error' : 'log-success') + '\">' + 
                     output.replace(/</g, '&lt;').replace(/>/g, '&gt;') + '</pre>');
        
        // Show output container (header is always visible)
        outputEl.show();
        
        // Auto-expand on first output
        if (!contentEl.hasClass('expanded')) {
          contentEl.addClass('expanded');
          $('#output-toggle-' + nodeId).text('▲');
        }
      } else {
        outputEl.hide();
      }
    };
    
    // Toggle log panel
    $(document).on('click', '#toggleLog', function() {
      $('#logPanel').toggleClass('collapsed');
      $(this).html($('#logPanel').hasClass('collapsed') ? '&#9660;' : '&#9650;');
    });
    
    // Append to log (now goes to right sidebar LOGS tab)
    window.appendToLog = function(message, type) {
      var logContent = $('#nodeLogsContent');
      var className = 'log-line';
      if (type === 'error') className += ' log-error';
      else if (type === 'success') className += ' log-success';
      else if (type === 'code') className += ' log-code';
      
      // Remove placeholder text if exists
      logContent.find('p').remove();
      
      // Create log line element
      var logLine = $('<div>').addClass(className).text(message);
      logContent.append(logLine);
      
      // Scroll to bottom
      logContent.scrollTop(logContent[0].scrollHeight);
      
      // Also update bottom log panel if it exists (for backward compatibility)
      var bottomLogContent = $('#logContent');
      if (bottomLogContent.length > 0) {
        bottomLogContent.append('<div class=\"' + className + '\">' + 
                             message.replace(/</g, '&lt;').replace(/>/g, '&gt;') + '</div>');
        bottomLogContent.scrollTop(bottomLogContent[0].scrollHeight);
      }
    };
    
    // Clear log (now clears right sidebar LOGS tab)
    window.clearLog = function() {
      var logContent = $('#nodeLogsContent');
      logContent.empty();
      logContent.html('<p style=\"color: #888; font-style: italic;\">Execution logs will appear here</p>');
      
      // Also clear bottom log panel if it exists
      var bottomLogContent = $('#logContent');
      if (bottomLogContent.length > 0) {
        bottomLogContent.empty();
      }
    };
    
    // Load code into editor
    if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('loadCodeEditor', function(msg) {
        $('#codeEditor').val(msg.content || '');
        $('#codeEditorStatus').text('File loaded');
      });
      
      Shiny.addCustomMessageHandler('openCodeEditorFromTree', function(msg) {
        currentEditingNode = null;
        currentEditingFilePath = msg.filePath;
        $('#codeEditor').val(msg.content || '');
        $('#codeEditorStatus').text('File loaded: ' + (msg.fileName || ''));
        $('#codeEditorModal').modal('show');
      });
      
      Shiny.addCustomMessageHandler('promptRenameFile', function(msg) {
        var newName = prompt('Enter new name for ' + msg.oldName + ':', msg.oldName);
        if (newName && newName !== msg.oldName) {
          Shiny.setInputValue('confirm_rename_file', {
            filePath: msg.filePath,
            newName: newName
          }, {priority: 'event'});
        }
      });
      
      Shiny.addCustomMessageHandler('promptRenameFolder', function(msg) {
        var newName = prompt('Enter new name for ' + msg.oldName + ':', msg.oldName);
        if (newName && newName !== msg.oldName) {
          Shiny.setInputValue('confirm_rename_folder', {
            oldName: msg.oldName,
            newName: newName
          }, {priority: 'event'});
        }
      });
      
      Shiny.addCustomMessageHandler('triggerFileInputClick', function(msg) {
        // Try to find and click the file input
        var selector = msg.selector || 'input[type=\\'file\\']';
        var fileInput = $(selector).first();
        if (fileInput.length > 0) {
          fileInput.click();
          console.log('Triggered file input click via message');
        } else {
          console.warn('Could not find file input with selector:', selector);
          // Try all file inputs
          $('input[type=\\'file\\']').each(function() {
            var $input = $(this);
            var dataNamespace = $input.attr('data-namespace') || '';
            if (dataNamespace.indexOf('shiny') !== -1) {
              $input.click();
              console.log('Found and clicked shiny file input');
              return false; // break
            }
          });
        }
      });
      
      Shiny.addCustomMessageHandler('codeEditorStatus', function(msg) {
        $('#codeEditorStatus').text(msg.message || '');
        $('#codeEditorStatus').css('color', msg.success ? '#4caf50' : '#f44336');
      });
      
      Shiny.addCustomMessageHandler('openCodeEditorFromTree', function(msg) {
        currentEditingNode = null;
        currentEditingFilePath = msg.filePath;
        $('#codeEditor').val(msg.content || '');
        $('#codeEditorStatus').text('File loaded: ' + (msg.fileName || ''));
        $('#codeEditorModal').modal('show');
      });
      
      Shiny.addCustomMessageHandler('promptRenameFile', function(msg) {
        var newName = prompt('Enter new name for ' + msg.oldName + ':', msg.oldName);
        if (newName && newName !== msg.oldName) {
          Shiny.setInputValue('confirm_rename_file', {
            filePath: msg.filePath,
            newName: newName
          }, {priority: 'event'});
        }
      });
      
      Shiny.addCustomMessageHandler('promptRenameFolder', function(msg) {
        var newName = prompt('Enter new name for ' + msg.oldName + ':', msg.oldName);
        if (newName && newName !== msg.oldName) {
          Shiny.setInputValue('confirm_rename_folder', {
            oldName: msg.oldName,
            newName: newName
          }, {priority: 'event'});
        }
      });
      
      Shiny.addCustomMessageHandler('updateNodeStatus', function(msg) {
        updateNodeStatus(msg.id, msg.status);
      });
      
      Shiny.addCustomMessageHandler('updateNodeOutput', function(msg) {
        updateNodeOutput(msg.id, msg.output, msg.isError);
      });
      
      Shiny.addCustomMessageHandler('appendToLog', function(msg) {
        appendToLog(msg.message, msg.type);
      });
      
      Shiny.addCustomMessageHandler('clearLog', function(msg) {
        clearLog();
      });
      
      Shiny.addCustomMessageHandler('addNodePlot', function(msg) {
        var outputEl = $('#output-' + msg.id);
        var contentEl = $('#output-content-' + msg.id);
        var img = $('<img>').attr('src', 'data:image/png;base64,' + msg.plotData)
                           .addClass('node-plot');
        contentEl.append(img);
        outputEl.show();
        
        // Auto-expand on first plot
        if (!contentEl.hasClass('expanded')) {
          contentEl.addClass('expanded');
          $('#output-toggle-' + msg.id).text('▲');
        }
      });
      
      Shiny.addCustomMessageHandler('downloadFile', function(msg) {
        var blob = new Blob([msg.content], { type: msg.mimeType });
        var url = URL.createObjectURL(blob);
        var a = document.createElement('a');
        a.href = url;
        a.download = msg.filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
      });
      
      Shiny.addCustomMessageHandler('updateWebRStatus', function(msg) {
        updateWebRStatus(msg.status, msg.message);
      });
      
      Shiny.addCustomMessageHandler('updateNodeLogs', function(msg) {
        var logsContent = $('#nodeLogsContent');
        if (msg.logs) {
          // Check if there are already execution logs
          var existingLogs = logsContent.find('.log-line');
          var hasExecutionLogs = existingLogs.length > 0;
          
          // If there are execution logs, append node output as a separator section
          if (hasExecutionLogs) {
            var separator = $('<div>').addClass('log-line').css({
              'border-top': '1px solid #444',
              'margin-top': '10px',
              'padding-top': '10px',
              'color': '#888',
              'font-style': 'italic'
            }).text('--- Node Output ---');
            logsContent.append(separator);
          } else {
            // If no execution logs, replace placeholder
            logsContent.find('p').remove();
          }
          
          // Format as pre-formatted text with proper escaping
          var formattedLogs = msg.logs.replace(/</g, '&lt;').replace(/>/g, '&gt;');
          var nodeOutput = $('<pre>').addClass('log-content').css({
            'white-space': 'pre-wrap',
            'word-wrap': 'break-word',
            'font-family': 'monospace',
            'font-size': '12px',
            'padding': '10px',
            'background': '#1e1e1e',
            'color': '#d4d4d4',
            'border-radius': '4px',
            'margin-top': '5px'
          }).text(formattedLogs);
          logsContent.append(nodeOutput);
          
          // Scroll to bottom
          logsContent.scrollTop(logsContent[0].scrollHeight);
        } else if (logsContent.find('.log-line').length === 0) {
          // Only show placeholder if there are no logs at all
          logsContent.html('<p style=\"color: #888; font-style: italic;\">Execution logs will appear here</p>');
        }
      });
      
      Shiny.addCustomMessageHandler('showOpenFolder', function(msg) {
        if (msg.show) {
          $('#openFolderContainer').show();
        } else {
          $('#openFolderContainer').hide();
        }
      });
      
      // Remove node from canvas
      Shiny.addCustomMessageHandler('removeNode', function(msg) {
        try {
          var nodeId = msg.id;
          if (!nodeId) return;
          
          // Remove from jsPlumb
          try {
            var connections = jsPlumbInstance.getConnections({source: nodeId});
            connections.forEach(function(conn) {
              jsPlumbInstance.deleteConnection(conn);
            });
            connections = jsPlumbInstance.getConnections({target: nodeId});
            connections.forEach(function(conn) {
              jsPlumbInstance.deleteConnection(conn);
            });
            jsPlumbInstance.remove(nodeId);
          } catch (e) {
            console.warn('Error removing node from jsPlumb:', e);
          }
          
          // Remove from DOM
          $('#' + nodeId).remove();
          
          // Remove from canvasNodes
          if (window.canvasNodes && window.canvasNodes[nodeId]) {
            delete window.canvasNodes[nodeId];
          }
          
          // Sync connections after removal
          syncConnections();
        } catch (e) {
          console.error('Error in removeNode handler:', e);
        }
      });
    }
    
    // Run workflow button (in header)
    $(document).on('click', '#runWorkflowBtn', function() {
      // Switch to LOGS tab when running workflow
      $('.right-sidebar-tab[data-tab=\"logs\"]').click();
      $('#rightSidebar').removeClass('hidden');
      Shiny.setInputValue('run_workflow', 1, {priority: 'event'});
    });
    
    // Export RMarkdown button (in header)
    $(document).on('click', '#exportRmdBtn', function() {
      Shiny.setInputValue('export_rmd', 1, {priority: 'event'});
    });
    
    // Refresh RMD Preview button
    $(document).on('click', '#refreshRmdPreviewBtn', function() {
      Shiny.setInputValue('refresh_rmd_preview', 1, {priority: 'event'});
    });
    
    // Export RMD from preview button
    $(document).on('click', '#exportRmdFromPreviewBtn', function() {
      var previewContent = $('#rmdPreview').val();
      if (previewContent) {
        Shiny.setInputValue('export_rmd_from_preview', {content: previewContent}, {priority: 'event'});
      }
    });
    
    // Import RMD button
    $(document).on('click', '#importRmdBtn', function() {
      // Create file input
      var fileInput = $('<input>').attr({
        type: 'file',
        accept: '.Rmd,.rmd',
        style: 'display: none;'
      });
      
      fileInput.on('change', function(e) {
        var file = e.target.files[0];
        if (file) {
          var reader = new FileReader();
          reader.onload = function(e) {
            var content = e.target.result;
            Shiny.setInputValue('import_rmd', {content: content, fileName: file.name}, {priority: 'event'});
          };
          reader.readAsText(file);
        }
        fileInput.remove();
      });
      
      $('body').append(fileInput);
      fileInput.click();
    });
    
    // Update RMD preview handler
    if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler('updateRmdPreview', function(msg) {
        $('#rmdPreview').val(msg.content || '');
      });
      
      // Create node from RMD import
      Shiny.addCustomMessageHandler('createNodeFromRmd', function(msg) {
        var nodeId = msg.id;
        var filePath = msg.filePath;
        var fileName = msg.fileName;
        var level = msg.level || 1;
        
        // Calculate position based on level
        var x = 100 + (level - 1) * 250;
        var y = 100 + (Object.keys(canvasNodes).length * 150);
        
        // Create node on canvas
        createCanvasNode(nodeId, filePath, fileName, x, y, '', null);
      });
      
      // Create connection from RMD import
      Shiny.addCustomMessageHandler('createConnectionFromRmd', function(msg) {
        var sourceId = msg.source;
        var targetId = msg.target;
        
        try {
          // Wait a bit for nodes to be ready - increase delay to ensure nodes are fully initialized
          setTimeout(function() {
            var sourceEl = $('#' + sourceId);
            var targetEl = $('#' + targetId);
            
            if (sourceEl.length && targetEl.length) {
              // Check if connection already exists
              var existing = jsPlumbInstance.getConnections({
                source: sourceId,
                target: targetId
              });
              
              if (existing && existing.length > 0) {
                console.log('Connection already exists:', sourceId, '->', targetId);
                return;
              }
              
              // Get current connector style from default or selected style
              var connectorType = $('#connectorStyleBtn').data('type') || 'bezier';
              var connectorConfig = null;
              if (connectorType === 'bezier') {
                connectorConfig = ['Bezier', { curviness: 50 }];
              } else if (connectorType === 'straight') {
                connectorConfig = 'Straight';
              } else if (connectorType === 'flowchart') {
                connectorConfig = ['Flowchart', { stub: [10, 15], gap: 10, cornerRadius: 5, alwaysRespectStubs: true }];
              } else {
                connectorConfig = ['Bezier', { curviness: 50 }];
              }
              
              // Create connection using jsPlumb
              var connection = jsPlumbInstance.connect({
                source: sourceId,
                target: targetId,
                anchors: ['Bottom', 'Top'],
                endpoint: ['Dot', { radius: 5 }],
                paintStyle: { stroke: '#B89D5D', strokeWidth: 2 },
                connector: connectorConfig,
                overlays: [['Arrow', { location: 1, width: 16, length: 16 }]]
              });
              
              if (connection) {
                console.log('Connection created:', sourceId, '->', targetId);
                // Sync connections to server
                syncConnections();
              } else {
                console.warn('Failed to create connection:', sourceId, '->', targetId);
              }
            } else {
              console.warn('Nodes not found for connection:', sourceId, '->', targetId, 
                'sourceEl:', sourceEl.length, 'targetEl:', targetEl.length);
            }
          }, 300);
        } catch(e) {
          console.error('Error creating connection from RMD:', e, 'source:', sourceId, 'target:', targetId);
        }
      });
    }
    
    // Toggle left sidebar
    $(document).on('click', '#toggleLeftSidebar, #toggleLeftSidebarBtn', function() {
      var leftSidebar = $('#leftSidebar');
      leftSidebar.toggleClass('hidden');
      var isHidden = leftSidebar.hasClass('hidden');
      $('#toggleLeftSidebar').html(isHidden ? '▶' : '▼');
      updateToggleButtons();
    });
    
    // Toggle right sidebar
    $(document).on('click', '#toggleRightSidebar, #toggleRightSidebarBtn', function() {
      var rightSidebar = $('#rightSidebar');
      rightSidebar.toggleClass('hidden');
      updateToggleButtons();
    });
    
    function updateToggleButtons() {
      var leftHidden = $('#leftSidebar').hasClass('hidden');
      var rightHidden = $('#rightSidebar').hasClass('hidden');
      
      if (leftHidden) {
        $('#toggleLeftSidebarBtn').show().addClass('visible');
      } else {
        $('#toggleLeftSidebarBtn').hide().removeClass('visible');
      }
      
      if (rightHidden) {
        $('#toggleRightSidebarBtn').show().addClass('visible');
      } else {
        $('#toggleRightSidebarBtn').hide().removeClass('visible');
      }
    }
    
    // Initialize toggle button visibility
    $(document).ready(function() {
      updateToggleButtons();
      
      // Update toggle buttons when sidebars change
      var observer = new MutationObserver(function(mutations) {
        updateToggleButtons();
      });
      
      if ($('#leftSidebar').length) {
        observer.observe(document.getElementById('leftSidebar'), { attributes: true, attributeFilter: ['class'] });
      }
      if ($('#rightSidebar').length) {
        observer.observe(document.getElementById('rightSidebar'), { attributes: true, attributeFilter: ['class'] });
      }
    });
    
    // New file button
    $(document).on('click', '#newFileBtn', function() {
      var fileName = prompt('Enter file name (e.g., new_script.R):');
      if (fileName && fileName.trim()) {
        if (!fileName.endsWith('.R') && !fileName.endsWith('.r')) {
          fileName = fileName + '.R';
        }
        Shiny.setInputValue('create_new_file', {fileName: fileName}, {priority: 'event'});
      }
    });
    
    // New folder button
    $(document).on('click', '#newFolderBtn', function() {
      var folderName = prompt('Enter folder name:');
      if (folderName && folderName.trim()) {
        Shiny.setInputValue('create_new_folder', {folderName: folderName}, {priority: 'event'});
      }
    });
    
    // Refresh files button
    $(document).on('click', '#refreshFilesBtn', function() {
      Shiny.setInputValue('refresh_files', 1, {priority: 'event'});
    });
    
    // Open folder button (if shinyFiles is available)
    $(document).on('click', '#openFolderBtn, #openFolderBtnHeader', function() {
      // shinyFiles/shinyDirChoose creates hidden file inputs
      // We need to find and trigger them
      var found = false;
      
      // Try multiple selectors to find the input
      var selectors = [
        'input[type=\\'file\\'][data-namespace=\\'shinyFiles\\']',
        'input[type=\\'file\\'][data-namespace=\\'shinyDirectories\\']',
        'input[type=\\'file\\'][id*=\\'importFolder\\']',
        'input[type=\\'file\\'][id*=\\'shinyFiles\\']',
        'input[type=\\'file\\'][data-input=\\'importFolder\\']'
      ];
      
      for (var i = 0; i < selectors.length; i++) {
        var fileInput = $(selectors[i]);
        if (fileInput.length > 0) {
          fileInput.click();
          found = true;
          console.log('Found and clicked shinyFiles input:', selectors[i]);
          break;
        }
      }
      
      // Also try finding by traversing all file inputs
      if (!found) {
        $('input[type=\\'file\\']').each(function() {
          var $input = $(this);
          var id = $input.attr('id') || '';
          var dataInput = $input.attr('data-input') || '';
          var dataNamespace = $input.attr('data-namespace') || '';
          
          if (id.indexOf('importFolder') !== -1 || 
              id.indexOf('shinyFiles') !== -1 ||
              dataInput.indexOf('importFolder') !== -1 ||
              dataNamespace.indexOf('shinyFiles') !== -1 ||
              dataNamespace.indexOf('shinyDirectories') !== -1) {
            $input.click();
            found = true;
            console.log('Found and clicked shinyFiles input by traversal:', id, dataInput, dataNamespace);
            return false; // break
          }
        });
      }
      
      if (!found) {
        console.warn('Could not find shinyFiles input, trying server-side trigger');
        // Fallback: send message to server
        Shiny.setInputValue('trigger_import_folder', 1, {priority: 'event'});
      }
    });
    
    // Store current connector type
    window.currentConnectorType = 'bezier';
    
    // Connection line style selector click handler
    $(document).on('click', '.connector-style-btn', function() {
      var connectorType = $(this).data('connector');
      $('.connector-style-btn').removeClass('selected');
      $(this).addClass('selected');
      window.currentConnectorType = connectorType;
      
      // Update all connections' connector style (preserve arrow overlay)
      try {
        var connections = jsPlumbInstance.getConnections();
        connections.forEach(function(conn) {
          try {
            var connectorConfig = null;
            if (connectorType === 'bezier') {
              connectorConfig = ['Bezier', { curviness: 50 }];
            } else if (connectorType === 'straight') {
              connectorConfig = 'Straight';
            } else if (connectorType === 'flowchart') {
              connectorConfig = ['Flowchart', { cornerRadius: 6 }];
            }
            
            if (connectorConfig) {
              // Always add arrow overlay after setting connector
              conn.setConnector(connectorConfig);
              
              // Always add arrow overlay (setConnector may remove overlays)
              try {
                // Remove existing arrow if any
                try {
                  conn.removeOverlay('arrow');
                } catch(e) {}
                // Add arrow overlay
                conn.addOverlay(['Arrow', { id: 'arrow', location: 1, width: 16, length: 16 }]);
              } catch(e) {
                console.warn('Failed to add arrow overlay:', e);
              }
            }
          } catch(e) {
            console.warn('Failed to update connector:', e);
          }
        });
        
        // Update default connector for new connections (with arrow)
        if (connectorType === 'bezier') {
          jsPlumbInstance.Defaults.Connector = ['Bezier', { curviness: 50 }];
        } else if (connectorType === 'straight') {
          jsPlumbInstance.Defaults.Connector = 'Straight';
        } else if (connectorType === 'flowchart') {
          jsPlumbInstance.Defaults.Connector = ['Flowchart', { cornerRadius: 6 }];
        }
        // Ensure arrow overlay is always in defaults
        jsPlumbInstance.Defaults.ConnectionOverlays = [
          ['Arrow', { location: 1, width: 16, length: 16 }]
        ];
        
        jsPlumbInstance.repaintEverything();
      } catch(e) {
        console.warn('Failed to update connector style:', e);
      }
    });
    
    // Right sidebar tab switching
    $(document).on('click', '.right-sidebar-tab', function() {
      var tabName = $(this).data('tab');
      $('.right-sidebar-tab').removeClass('active');
      $(this).addClass('active');
      $('.tab-pane').removeClass('active');
      $('#' + tabName + 'Tab').addClass('active');
      
      // Auto-refresh RMD preview when switching to RMD tab
      if (tabName === 'rmd') {
        setTimeout(function() {
          Shiny.setInputValue('refresh_rmd_preview', 1, {priority: 'event'});
        }, 100);
      }
    });
    
    // Close folder button
    $(document).on('click', '#closeFolderBtn', function() {
      // Clear external folders
      Shiny.setInputValue('close_folder', 1, {priority: 'event'});
    });
    
    // Update WebR status
    window.updateWebRStatus = function(status, message) {
      var statusEl = $('#webrStatus');
      statusEl.removeClass('loading ready error');
      statusEl.addClass(status);
      statusEl.find('span:last').text(message || status);
    };
    
    // Initialize WebR status as ready (since we're using server-side R)
    $(document).ready(function() {
      setTimeout(function() {
        updateWebRStatus('ready', 'R Ready');
      }, 1000);
    });
    "))
  ),
    
    # Top Header
    tags$div(class = "top-header",
      tags$div(class = "header-left",
        tags$div(class = "header-logo", "R"),
        tags$div(class = "header-title", "RCW Editor")
      ),
      tags$div(class = "header-right",
        tags$div(id = "webrStatus", class = "webr-status loading",
          tags$span(class = "webr-status-dot"),
          tags$span("Loading...")
        ),
        tags$button(class = "btn btn-primary btn-sm", id = "runWorkflowBtn", 
          style = "background: #007acc; border: none; color: white; padding: 6px 16px;",
          "Run"),
        tags$button(class = "btn btn-secondary btn-sm", id = "exportRmdBtn",
          style = "background: #3e3e42; border: none; color: white; padding: 6px 16px;",
          HTML("&#11123; Export"))
      )
    ),
    
    # Main Container
    tags$div(class = "main-container",
      # Left Sidebar - Explorer
      tags$div(class = "left-sidebar", id = "leftSidebar",
        tags$div(class = "sidebar-header",
          tags$div(style = "display: flex; align-items: center; gap: 8px; flex: 1;",
            tags$button(id = "toggleLeftSidebar", class = "sidebar-toggle-icon", 
              style = "background: none; border: none; color: #cccccc; cursor: pointer; padding: 2px 4px; font-size: 10px;",
              HTML("&#9660;")),
            tags$span("EXPLORER")
          ),
          tags$div(style = "display: flex; align-items: center; gap: 4px;",
            tags$button(id = "newFileBtn", class = "sidebar-action-btn",
              style = "background: none; border: none; color: #cccccc; cursor: pointer; padding: 4px; font-size: 12px;",
              title = "New File", HTML("&#128196;&#43;")),
            tags$button(id = "newFolderBtn", class = "sidebar-action-btn",
              style = "background: none; border: none; color: #cccccc; cursor: pointer; padding: 4px; font-size: 12px;",
              title = "New Folder", HTML("&#128193;&#43;")),
            tags$button(id = "refreshFilesBtn", class = "sidebar-action-btn",
              style = "background: none; border: none; color: #cccccc; cursor: pointer; padding: 4px; font-size: 12px;",
              title = "Refresh", HTML("&#8635;")),
            if (exists("HAS_SHINYFILES") && HAS_SHINYFILES) {
              tags$button(id = "openFolderBtnHeader", class = "sidebar-action-btn",
                style = "background: none; border: none; color: #cccccc; cursor: pointer; padding: 4px; font-size: 12px;",
                title = "Open Folder", HTML("&#128193;"))
            },
            tags$button(id = "closeFolderBtn", class = "sidebar-action-btn",
              style = "background: none; border: none; color: #cccccc; cursor: pointer; padding: 4px;",
              title = "Close Folder", HTML("&times;"))
          )
        ),
        tags$div(class = "sidebar-content",
          uiOutput("file_tree_ui"),
          # Open Folder button (shown when no files)
          tags$div(id = "openFolderContainer", style = "padding: 20px; text-align: center; display: none;",
            tags$p(style = "color: #888; margin-bottom: 12px; font-size: 12px;", "No files found"),
            if (exists("HAS_SHINYFILES") && HAS_SHINYFILES) {
              tags$button(class = "btn btn-primary", id = "openFolderBtn",
                style = "background: #CEB888; border: none; color: #000; padding: 8px 16px; font-weight: 600; cursor: pointer;",
                "Open Folder")
            } else {
              tags$p(style = "color: #666; font-size: 11px;", "Install shinyFiles package to import folders")
            }
          )
        ),
      ),
      
      # Center Canvas
      tags$div(class = "center-canvas",
        tags$div(id = "canvas")
      ),
      
      # Sidebar Toggle Buttons (always visible)
      tags$button(id = "toggleLeftSidebarBtn", class = "sidebar-toggle-btn", HTML("▶")),
      tags$button(id = "toggleRightSidebarBtn", class = "sidebar-toggle-btn", HTML("◀")),
      
      # Right Sidebar
      tags$div(class = "right-sidebar", id = "rightSidebar",
        tags$div(class = "right-sidebar-tabs",
          tags$button(class = "right-sidebar-tab active", `data-tab` = "style", "STYLE"),
          tags$button(class = "right-sidebar-tab", `data-tab` = "logs", "LOGS"),
          tags$button(class = "right-sidebar-tab", `data-tab` = "rmd", "RMD")
        ),
        tags$div(class = "right-sidebar-content",
          # Style Tab
          tags$div(id = "styleTab", class = "tab-pane active",
          
          # Shape Selector
          tags$div(style = "margin-bottom: 20px;",
            tags$h4(style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 8px; color: #d4d4d4;", "Shape"),
            tags$div(class = "shape-selector",
              tags$div(class = "shape-btn", `data-shape` = "rectangle", title = "Rectangle",
                tags$div(class = "shape-preview rectangle")
              ),
              tags$div(class = "shape-btn selected", `data-shape` = "rounded", title = "Rounded",
                tags$div(class = "shape-preview rounded")
              ),
              tags$div(class = "shape-btn", `data-shape` = "circle", title = "Circle",
                tags$div(class = "shape-preview circle")
              ),
              tags$div(class = "shape-btn", `data-shape` = "diamond", title = "Diamond",
                tags$div(class = "shape-preview diamond")
              ),
              tags$div(class = "shape-btn", `data-shape` = "parallelogram", title = "Parallelogram",
                tags$div(class = "shape-preview parallelogram")
              ),
              tags$div(class = "shape-btn", `data-shape` = "cylinder", title = "Cylinder",
                tags$div(class = "shape-preview cylinder")
              )
            )
          ),
          
          # Shape Fill Color (图形颜色)
          tags$div(class = "color-picker-section", style = "margin-bottom: 20px;",
            tags$h4(style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 8px; color: #d4d4d4;", "Shape Fill"),
            tags$div(class = "color-grid",
              # Common colors
              tags$div(class = "color-swatch selected", `data-color` = "#2d2d30", style = "background: #2d2d30;", title = "Dark Gray"),
              tags$div(class = "color-swatch", `data-color` = "#f0f0f0", style = "background: #f0f0f0;", title = "Light Gray"),
              tags$div(class = "color-swatch", `data-color` = "#e0e0e0", style = "background: #e0e0e0;", title = "Gray"),
              tags$div(class = "color-swatch", `data-color` = "#d0d0d0", style = "background: #d0d0d0;", title = "Dark Gray"),
              tags$div(class = "color-swatch", `data-color` = "#ffebee", style = "background: #ffebee;", title = "Light Red"),
              tags$div(class = "color-swatch", `data-color` = "#fff3e0", style = "background: #fff3e0;", title = "Light Orange"),
              tags$div(class = "color-swatch", `data-color` = "#fff9c4", style = "background: #fff9c4;", title = "Light Yellow"),
              tags$div(class = "color-swatch", `data-color` = "#f1f8e9", style = "background: #f1f8e9;", title = "Light Green"),
              tags$div(class = "color-swatch", `data-color` = "#e3f2fd", style = "background: #e3f2fd;", title = "Light Blue"),
              tags$div(class = "color-swatch", `data-color` = "#f3e5f5", style = "background: #f3e5f5;", title = "Light Purple"),
              tags$div(class = "color-swatch", `data-color` = "#fce4ec", style = "background: #fce4ec;", title = "Light Pink"),
              tags$div(class = "color-swatch", `data-color` = "#e0f2f1", style = "background: #e0f2f1;", title = "Light Teal"),
              tags$div(class = "color-swatch", `data-color` = "#fff5e6", style = "background: #fff5e6;", title = "Cream"),
              tags$div(class = "color-swatch", `data-color` = "#f5f5f5", style = "background: #f5f5f5;", title = "Off White"),
              tags$div(class = "color-swatch", `data-color` = "#CEB888", style = "background: #CEB888;", title = "Primary"),
              tags$div(class = "color-swatch", `data-color` = "#B89D5D", style = "background: #B89D5D;", title = "Primary Dark")
            ),
            tags$div(class = "color-input-wrapper",
              tags$input(type = "color", id = "node_color_input", value = "#2d2d30", style = "width: 60px; height: 40px; border: 2px solid #3e3e42; border-radius: 4px; cursor: pointer; background: #1e1e1e;"),
              tags$input(type = "text", id = "node_color_text", value = "#2d2d30", placeholder = "#2d2d30", style = "flex: 1; padding: 8px; border: 2px solid #3e3e42; border-radius: 4px; background: #1e1e1e; color: #d4d4d4;")
            )
          ),
          
          # Connection Line Style (连接线形状)
          tags$div(style = "margin-top: 20px; margin-bottom: 20px;",
            tags$h4(style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 8px; color: #d4d4d4;", "Connection Line Style"),
            tags$div(class = "connector-style-selector", style = "display: flex; gap: 8px; flex-wrap: wrap;",
              tags$div(class = "connector-style-btn selected", `data-connector` = "bezier", title = "Curved Line (Bezier)",
                style = "width: 80px; height: 50px; border: 2px solid #3e3e42; border-radius: 4px; background: #1e1e1e; display: flex; align-items: center; justify-content: center; cursor: pointer; position: relative;",
                tags$div(style = "width: 60px; height: 2px; background: #CEB888; border-radius: 50px; transform: rotate(-5deg); position: absolute; top: 20px; left: 10px;")),
              tags$div(class = "connector-style-btn", `data-connector` = "straight", title = "Straight Line",
                style = "width: 80px; height: 50px; border: 2px solid #3e3e42; border-radius: 4px; background: #1e1e1e; display: flex; align-items: center; justify-content: center; cursor: pointer; position: relative;",
                tags$div(style = "width: 60px; height: 2px; background: #CEB888; position: absolute; top: 24px; left: 10px;")),
              tags$div(class = "connector-style-btn", `data-connector` = "flowchart", title = "Step Line (Flowchart)",
                style = "width: 80px; height: 50px; border: 2px solid #3e3e42; border-radius: 4px; background: #1e1e1e; display: flex; align-items: center; justify-content: center; cursor: pointer; position: relative;",
                tags$svg(style = "width: 60px; height: 30px; position: absolute; top: 10px; left: 10px;",
                  tags$path(d = "M 0 15 L 20 15 L 20 5 L 50 5 L 50 15 L 60 15", 
                    stroke = "#CEB888", "stroke-width" = "2", fill = "none")))
            )
          ),
          
          # Text Color (文字颜色)
          tags$div(class = "color-picker-section", style = "margin-top: 20px;",
            tags$h4(style = "font-size: 0.9rem; font-weight: 600; margin-bottom: 8px; color: #d4d4d4;", "Text Color"),
            tags$div(class = "color-grid text-color-grid",
              # Common text colors
              tags$div(class = "text-color-swatch selected", `data-color` = "#d4d4d4", style = "background: #d4d4d4;", title = "Light Gray"),
              tags$div(class = "text-color-swatch", `data-color` = "#ffffff", style = "background: #ffffff;", title = "White"),
              tags$div(class = "text-color-swatch", `data-color` = "#000000", style = "background: #000000;", title = "Black"),
              tags$div(class = "text-color-swatch", `data-color` = "#2d2d30", style = "background: #2d2d30;", title = "Dark Gray"),
              tags$div(class = "text-color-swatch", `data-color` = "#f44336", style = "background: #f44336;", title = "Red"),
              tags$div(class = "text-color-swatch", `data-color` = "#ff9800", style = "background: #ff9800;", title = "Orange"),
              tags$div(class = "text-color-swatch", `data-color` = "#ffeb3b", style = "background: #ffeb3b;", title = "Yellow"),
              tags$div(class = "text-color-swatch", `data-color` = "#4caf50", style = "background: #4caf50;", title = "Green"),
              tags$div(class = "text-color-swatch", `data-color` = "#2196f3", style = "background: #2196f3;", title = "Blue"),
              tags$div(class = "text-color-swatch", `data-color` = "#9c27b0", style = "background: #9c27b0;", title = "Purple"),
              tags$div(class = "text-color-swatch", `data-color` = "#e91e63", style = "background: #e91e63;", title = "Pink"),
              tags$div(class = "text-color-swatch", `data-color` = "#00bcd4", style = "background: #00bcd4;", title = "Cyan"),
              tags$div(class = "text-color-swatch", `data-color` = "#ff5722", style = "background: #ff5722;", title = "Deep Orange"),
              tags$div(class = "text-color-swatch", `data-color` = "#795548", style = "background: #795548;", title = "Brown"),
              tags$div(class = "text-color-swatch", `data-color` = "#CEB888", style = "background: #CEB888;", title = "Primary"),
              tags$div(class = "text-color-swatch", `data-color` = "#B89D5D", style = "background: #B89D5D;", title = "Primary Dark")
            ),
            tags$div(class = "color-input-wrapper",
              tags$input(type = "color", id = "node_title_color", value = "#d4d4d4", style = "width: 60px; height: 40px; border: 2px solid #3e3e42; border-radius: 4px; cursor: pointer; background: #1e1e1e;"),
              tags$input(type = "text", id = "node_title_color_text", value = "#d4d4d4", placeholder = "#d4d4d4", style = "flex: 1; padding: 8px; border: 2px solid #3e3e42; border-radius: 4px; background: #1e1e1e; color: #d4d4d4;")
            )
          )
          ),
          
          # Logs Tab
          tags$div(id = "logsTab", class = "tab-pane",
            tags$div(id = "nodeLogsContent", class = "log-content", style = "max-height: calc(100vh - 200px); overflow-y: auto; padding: 10px;",
              tags$p(style = "color: #888; font-style: italic;", "Execution logs will appear here")
            )
          ),
          
          # RMD Preview Tab
          tags$div(id = "rmdTab", class = "tab-pane",
            tags$div(style = "padding: 10px;",
              tags$details(
                tags$summary("Rmd Interface Spec (YAML)"),
                tags$pre(rmd_interface_examples, style = "margin-top: 8px; white-space: pre;")
              ),
              tags$div(style = "margin-bottom: 10px; display: flex; gap: 8px;",
                tags$button(class = "btn btn-primary btn-sm", id = "refreshRmdPreviewBtn",
                  style = "background: #CEB888; border: none; color: #000; padding: 6px 12px; font-weight: 600; cursor: pointer;",
                  "Refresh Preview"),
                tags$button(class = "btn btn-secondary btn-sm", id = "exportRmdFromPreviewBtn",
                  style = "background: #3e3e42; border: none; color: white; padding: 6px 12px; cursor: pointer;",
                  "Export RMD"),
                tags$button(class = "btn btn-secondary btn-sm", id = "importRmdBtn",
                  style = "background: #3e3e42; border: none; color: white; padding: 6px 12px; cursor: pointer;",
                  "Import RMD")
              ),
              tags$textarea(id = "rmdPreview", 
                style = "width: 100%; height: calc(100vh - 250px); font-family: 'Courier New', monospace; font-size: 12px; padding: 10px; background: #1e1e1e; color: #d4d4d4; border: 1px solid #3e3e42; border-radius: 4px; resize: vertical;",
                readonly = "readonly",
                placeholder = "RMD preview will appear here. Click 'Refresh Preview' to generate.")
            )
          )
        )
      )
    ),
    
    # Code Editor Modal
    tags$div(id = "codeEditorModal", class = "modal fade", tabindex = "-1", role = "dialog",
      tags$div(class = "modal-dialog modal-lg", role = "document",
        tags$div(class = "modal-content",
          tags$div(class = "modal-header",
            tags$h5(class = "modal-title", "Edit R Code"),
            tags$button(type = "button", class = "btn-close", `data-bs-dismiss` = "modal", `aria-label` = "Close")
          ),
          tags$div(class = "modal-body",
            tags$textarea(id = "codeEditor", class = "code-editor-area", rows = "20", style = "width: 100%; font-family: 'Courier New', monospace; font-size: 13px;"),
            tags$div(id = "codeEditorStatus", style = "margin-top: 10px; font-size: 12px; color: #666;")
          ),
          tags$div(class = "modal-footer",
            tags$button(type = "button", class = "btn btn-secondary", id = "cancelCodeBtn", "Cancel"),
            tags$button(type = "button", class = "btn btn-primary", id = "saveCodeBtn", "Save")
          )
        )
      )
    ),
    
    # Log Display Area (bottom panel) - Hidden, logs now shown in right sidebar LOGS tab
    tags$div(id = "logPanel", class = "log-panel", style = "display: none;",
      tags$div(class = "log-header",
        tags$span("Execution Log"),
        tags$button(id = "toggleLog", class = "btn btn-sm", HTML("&#9650;"))
      ),
      tags$div(id = "logContent", class = "log-content")
    ),
    
  )

# Server function
server <- function(input, output, session) {
  # Initialize canvas state
  canvas_state <- reactiveValues(
    nodes = list(),
    edges = list()
  )
  validation_cache <- reactiveVal(list())
  
  # Shared R environment for all nodes
  shared_env <- new.env()
  
  # Initialize shared environment with common packages
  tryCatch({
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      attach(list(), name = "ggplot2_env", pos = 2)
      library(ggplot2)
    }
  }, error = function(e) {})

  validate_required_inputs <- function(nodes, edges) {
    if (length(nodes) == 0) return(list())
    missing_by_node <- list()
    for (node_id in names(nodes)) {
      node <- nodes[[node_id]]
      spec <- node$moduleSpec %||% NULL
      if (is.null(spec) || is.null(spec$inputs)) next
      req_inputs <- Filter(function(p) isTRUE(p$required), spec$inputs)
      if (length(req_inputs) == 0) next
      missing <- character(0)
      for (p in req_inputs) {
        pname <- p$name %||% ""
        if (!nzchar(pname)) next
        has_edge <- FALSE
        if (!is.null(edges) && length(edges) > 0) {
          for (edge in edges) {
            if (is.list(edge) &&
                identical(edge$target, node_id) &&
                !is.null(edge$targetPort) &&
                identical(edge$targetPort, pname)) {
              has_edge <- TRUE
              break
            }
          }
        }
        if (!has_edge) missing <- c(missing, pname)
      }
      if (length(missing) > 0) {
        missing_by_node[[node_id]] <- missing
      }
    }
    missing_by_node
  }
  
  # Render file tree
  output$file_tree_ui <- renderUI({
    structure <- scan_workspace()
    
    if (length(structure$folders) == 0 && length(structure$files) == 0) {
      # Show Open Folder button
      session$sendCustomMessage("showOpenFolder", list(show = TRUE))
      return(tags$div())
    }
    
    # Hide Open Folder button if files exist
    session$sendCustomMessage("showOpenFolder", list(show = FALSE))
    
    # Group files by folder
    files_by_folder <- list()
    for (file_info in structure$files) {
      folder <- file_info$folder
      if (!folder %in% names(files_by_folder)) {
        files_by_folder[[folder]] <- list()
      }
      files_by_folder[[folder]] <- c(files_by_folder[[folder]], list(file_info))
    }
    
    # Build HTML
    html_list <- list()
    
    # Sort folders
    sorted_folders <- sort(unique(structure$folders))
    
    for (folder in sorted_folders) {
      folder_id <- gsub("[^a-zA-Z0-9]", "_", folder)
      folder_name <- if (folder == "root") "root" else gsub("^root/", "", folder)
      
      # Folder header
      folder_files <- files_by_folder[[folder]] %||% list()
      
      html_list <- c(html_list, list(
        tags$div(
          class = "file-tree-item folder",
          `data-folder-id` = folder_id,
          tags$div(class = "folder-header",
            tags$span(class = "folder-toggle", "▶"),
            tags$span(class = "folder-name", title = folder, folder_name)
          )
        ),
        tags$div(
          id = paste0("folder-files-", folder_id),
          class = "folder-files",
          lapply(folder_files, function(file_info) {
            tags$div(
              class = "file-tree-item file",
              `data-filepath` = file_info$path,
              `data-filename` = file_info$name,
              tags$span(class = "file-icon", "📄"),
              tags$span(class = "file-name", title = file_info$name, file_info$name)
            )
          })
        )
      ))
    }
    
    tags$ul(class = "file-tree", html_list)
  })
  
  # Toggle panel visibility
  observeEvent(input$toggle_left, {
    session$sendCustomMessage("toggle_panel", list(side = "left"))
  })
  
  observeEvent(input$toggle_right, {
    session$sendCustomMessage("toggle_panel", list(side = "right"))
  })
  
  # Create new file
  observeEvent(input$create_new_file, {
    if (!is.null(input$create_new_file) && !is.null(input$create_new_file$fileName)) {
      tryCatch({
        file_name <- input$create_new_file$fileName
        # Determine workspace root
        workspace_root <- .rcw_workspace
        if (length(.rcw_external_folders) > 0) {
          workspace_root <- .rcw_external_folders[1]
        }
        
        # Create file path
        file_path <- file.path(workspace_root, file_name)
        
        # Create empty R file
        writeLines("", file_path)
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Created file: ", file_name), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error creating file: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Create new folder
  observeEvent(input$create_new_folder, {
    if (!is.null(input$create_new_folder) && !is.null(input$create_new_folder$folderName)) {
      tryCatch({
        folder_name <- input$create_new_folder$folderName
        # Determine workspace root
        workspace_root <- .rcw_workspace
        if (length(.rcw_external_folders) > 0) {
          workspace_root <- .rcw_external_folders[1]
        }
        
        # Create folder path
        folder_path <- file.path(workspace_root, folder_name)
        
        # Create directory
        dir.create(folder_path, recursive = TRUE, showWarnings = FALSE)
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Created folder: ", folder_name), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error creating folder: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Refresh files
  observeEvent(input$refresh_files, {
    file_tree_trigger(isolate(file_tree_trigger()) + 1)
    session$sendCustomMessage("appendToLog", list(
      message = "✓ Files refreshed", 
      type = "success"
    ))
  })
  
  # Move file to folder
  observeEvent(input$move_file_to_folder, {
    if (!is.null(input$move_file_to_folder)) {
      tryCatch({
        file_path <- input$move_file_to_folder$filePath
        file_name <- input$move_file_to_folder$fileName
        target_folder <- input$move_file_to_folder$targetFolder
        
        if (is.null(file_path) || !file.exists(file_path)) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ File not found: ", file_name), 
            type = "error"
          ))
          return()
        }
        
        # Determine workspace root
        workspace_root <- .rcw_workspace
        if (length(.rcw_external_folders) > 0) {
          workspace_root <- .rcw_external_folders[1]
        }
        
        # Calculate target path
        if (target_folder == "root" || target_folder == "") {
          target_path <- file.path(workspace_root, file_name)
        } else {
          # Remove "root/" prefix if present
          folder_path <- gsub("^root/", "", target_folder)
          target_path <- file.path(workspace_root, folder_path, file_name)
        }
        
        # Create target directory if needed
        dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
        
        # Move file (copy then delete original)
        if (file_path != target_path) {
          file.copy(file_path, target_path, overwrite = TRUE)
          file.remove(file_path)
          
          # Trigger file tree refresh
          file_tree_trigger(isolate(file_tree_trigger()) + 1)
          
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✓ Moved file: ", file_name, " to ", target_folder), 
            type = "success"
          ))
        }
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error moving file: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Handle node creation
  observeEvent(input$node_created, {
    if (!is.null(input$node_created)) {
      node_data <- input$node_created
      node_id <- node_data$id
      
      # Ensure filePath is absolute and exists
      file_path <- node_data$filePath
      if (!is.null(file_path) && nzchar(file_path)) {
        # Convert to absolute path if relative
        if (!file.exists(file_path)) {
          # Try to find file in workspace
          structure <- scan_workspace()
          for (file_info in structure$files) {
            if (basename(file_info$path) == basename(file_path)) {
              file_path <- file_info$path
              break
            }
          }
        }
        node_data$filePath <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
      }
      
      canvas_state$nodes[[node_id]] <- node_data
      
      # If Rmd, parse interface and attach ports
      if (!is.null(node_data$filePath) && grepl("\\.Rmd$", node_data$filePath, ignore.case = TRUE)) {
        module_spec <- parse_rmd_interface(node_data$filePath)
        if (!is.null(module_spec)) {
          canvas_state$nodes[[node_id]]$moduleSpec <- module_spec
          session$sendCustomMessage("apply_node_ports", list(
            id = node_id,
            moduleSpec = module_spec
          ))
        }
      }
      session$sendCustomMessage("appendToLog", list(
        message = paste0("Node created: ", node_data$fileName), 
        type = "success"
      ))
    }
  })
  
  # Handle node movement
  observeEvent(input$node_moved, {
    if (!is.null(input$node_moved)) {
      node_data <- input$node_moved
      node_id <- node_data$id
      if (node_id %in% names(canvas_state$nodes)) {
        canvas_state$nodes[[node_id]]$x <- node_data$x
        canvas_state$nodes[[node_id]]$y <- node_data$y
      }
    }
  })
  
  # Handle node appearance updates
  observeEvent(input$node_set_appearance, {
    if (!is.null(input$node_set_appearance)) {
      node_data <- input$node_set_appearance
      node_id <- node_data$id
      if (node_id %in% names(canvas_state$nodes)) {
        canvas_state$nodes[[node_id]]$appearance <- list(
          shape = node_data$shape,
          color = node_data$color,
          titleColor = node_data$titleColor
        )
      }
    }
  })
  
  # Handle connections
  observeEvent(input$canvas_connections, {
    if (!is.null(input$canvas_connections)) {
      # Ensure edges is always a list
      if (is.list(input$canvas_connections)) {
        canvas_state$edges <- input$canvas_connections
      } else {
        # If it's not a list, convert to empty list
        canvas_state$edges <- list()
      }
    } else {
      canvas_state$edges <- list()
    }

    # Validate required inputs whenever connections change
    missing <- validate_required_inputs(canvas_state$nodes, canvas_state$edges)
    last_missing <- validation_cache()
    if (!identical(missing, last_missing)) {
      validation_cache(missing)
      if (length(missing) > 0) {
        for (node_id in names(missing)) {
          node_name <- canvas_state$nodes[[node_id]]$fileName %||% node_id
          session$sendCustomMessage("appendToLog", list(
            message = paste0("⚠ Missing required inputs for ", node_name, ": ", paste(missing[[node_id]], collapse = ", ")),
            type = "warning"
          ))
        }
      }
    }
  })
  
  # Get node logs
  observeEvent(input$get_node_logs, {
    if (!is.null(input$get_node_logs)) {
      node_id <- input$get_node_logs$nodeId
      if (node_id %in% names(canvas_state$nodes)) {
        node <- canvas_state$nodes[[node_id]]
        logs <- node$lastOutput %||% "No logs available for this node."
        session$sendCustomMessage("updateNodeLogs", list(logs = logs))
      }
    }
  })
  
  # Close folder
  observeEvent(input$close_folder, {
    .rcw_external_folders <<- character(0)
    # Trigger file tree refresh
    output$file_tree_ui <- renderUI({
      structure <- scan_workspace()
      if (length(structure$folders) == 0 && length(structure$files) == 0) {
        session$sendCustomMessage("showOpenFolder", list(show = TRUE))
        return(tags$div())
      }
      session$sendCustomMessage("showOpenFolder", list(show = FALSE))
      
      # Group files by folder
      files_by_folder <- list()
      for (file_info in structure$files) {
        folder <- file_info$folder
        if (!folder %in% names(files_by_folder)) {
          files_by_folder[[folder]] <- list()
        }
        files_by_folder[[folder]] <- c(files_by_folder[[folder]], list(file_info))
      }
      
      # Build HTML
      html_list <- list()
      sorted_folders <- sort(unique(structure$folders))
      
      for (folder in sorted_folders) {
        folder_id <- gsub("[^a-zA-Z0-9]", "_", folder)
        folder_name <- if (folder == "root") "root" else gsub("^root/", "", folder)
        folder_files <- files_by_folder[[folder]] %||% list()
        
        html_list <- c(html_list, list(
          tags$div(
            class = "file-tree-item folder",
            `data-folder-id` = folder_id,
            tags$div(class = "folder-header",
              tags$span(class = "folder-toggle", "▶"),
              tags$span(class = "folder-name", title = folder, folder_name)
            )
          ),
          tags$div(
            id = paste0("folder-files-", folder_id),
            class = "folder-files",
            lapply(folder_files, function(file_info) {
              tags$div(
                class = "file-tree-item file",
                `data-filepath` = file_info$path,
                `data-filename` = file_info$name,
                tags$span(class = "file-icon", "📄"),
                tags$span(class = "file-name", title = file_info$name, file_info$name)
              )
            })
          )
        ))
      }
      
      tags$ul(class = "file-tree", html_list)
    })
  })
  
  # Edit file from file tree
  observeEvent(input$edit_file_tree, {
    if (!is.null(input$edit_file_tree)) {
      tryCatch({
        file_path <- input$edit_file_tree$path
        file_name <- input$edit_file_tree$name
        
        if (is.null(file_path) || !file.exists(file_path)) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ File not found: ", file_name), 
            type = "error"
          ))
          return()
        }
        
        # Read file content
        file_content <- paste(readLines(file_path, warn = FALSE), collapse = "\n")
        
        # Send to client to open editor
        session$sendCustomMessage("openCodeEditorFromTree", list(
          filePath = file_path,
          fileName = file_name,
          content = file_content
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error opening file: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Delete file from file tree
  observeEvent(input$delete_file, {
    if (!is.null(input$delete_file)) {
      tryCatch({
        file_path <- input$delete_file$path
        
        if (is.null(file_path) || !file.exists(file_path)) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ File not found"), 
            type = "error"
          ))
          return()
        }
        
        # Delete file
        file.remove(file_path)
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Deleted file: ", basename(file_path)), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error deleting file: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Delete multiple files
  observeEvent(input$delete_files, {
    if (!is.null(input$delete_files) && !is.null(input$delete_files$paths)) {
      tryCatch({
        file_paths <- input$delete_files$paths
        deleted_count <- 0
        
        for (file_path in file_paths) {
          if (file.exists(file_path)) {
            file.remove(file_path)
            deleted_count <- deleted_count + 1
          }
        }
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Deleted ", deleted_count, " file(s)"), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error deleting files: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Delete folder
  observeEvent(input$delete_folder, {
    if (!is.null(input$delete_folder)) {
      tryCatch({
        folder_name <- input$delete_folder$name
        
        # Determine workspace root
        workspace_root <- .rcw_workspace
        if (length(.rcw_external_folders) > 0) {
          workspace_root <- .rcw_external_folders[1]
        }
        
        # Calculate folder path
        if (folder_name == "root" || folder_name == "" || is.null(folder_name)) {
          session$sendCustomMessage("appendToLog", list(
            message = "✗ Cannot delete root folder", 
            type = "error"
          ))
          return()
        }
        
        # Remove "root/" prefix if present
        folder_name_clean <- gsub("^root/", "", folder_name)
        folder_path <- file.path(workspace_root, folder_name_clean)
        
        
        if (!dir.exists(folder_path)) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ Folder not found: ", folder_path, " (name: ", folder_name, ")"), 
            type = "error"
          ))
          return()
        }
        
        # Delete folder recursively
        unlink(folder_path, recursive = TRUE)
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Deleted folder: ", folder_name), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error deleting folder: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Rename file
  observeEvent(input$rename_file, {
    if (!is.null(input$rename_file)) {
      tryCatch({
        file_path <- input$rename_file$path
        old_name <- input$rename_file$name
        
        if (is.null(file_path) || !file.exists(file_path)) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ File not found: ", old_name), 
            type = "error"
          ))
          return()
        }
        
        # Use JavaScript prompt instead of readline (which doesn't work in Shiny)
        session$sendCustomMessage("promptRenameFile", list(
          filePath = file_path,
          oldName = old_name
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error renaming file: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Handle rename file confirmation
  observeEvent(input$confirm_rename_file, {
    if (!is.null(input$confirm_rename_file)) {
      tryCatch({
        file_path <- input$confirm_rename_file$filePath
        new_name <- input$confirm_rename_file$newName
        
        if (is.null(file_path) || !file.exists(file_path)) {
          return()
        }
        
        # Ensure .R extension
        if (!grepl("\\.(r|R)$", new_name)) {
          new_name <- paste0(new_name, ".R")
        }
        
        # Rename file
        new_path <- file.path(dirname(file_path), new_name)
        file.rename(file_path, new_path)
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Renamed file: ", basename(file_path), " -> ", new_name), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error renaming file: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Rename folder
  observeEvent(input$rename_folder, {
    if (!is.null(input$rename_folder)) {
      tryCatch({
        old_name <- input$rename_folder$name
        
        if (old_name == "root" || old_name == "") {
          session$sendCustomMessage("appendToLog", list(
            message = "✗ Cannot rename root folder", 
            type = "error"
          ))
          return()
        }
        
        # Use JavaScript prompt
        session$sendCustomMessage("promptRenameFolder", list(
          oldName = old_name
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error renaming folder: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Handle rename folder confirmation
  observeEvent(input$confirm_rename_folder, {
    if (!is.null(input$confirm_rename_folder)) {
      tryCatch({
        old_name <- input$confirm_rename_folder$oldName
        new_name <- input$confirm_rename_folder$newName
        
        # Determine workspace root
        workspace_root <- .rcw_workspace
        if (length(.rcw_external_folders) > 0) {
          workspace_root <- .rcw_external_folders[1]
        }
        
        old_path <- file.path(workspace_root, old_name)
        
        if (!dir.exists(old_path)) {
          return()
        }
        
        # Rename folder
        new_path <- file.path(workspace_root, new_name)
        file.rename(old_path, new_path)
        
        # Trigger file tree refresh
        file_tree_trigger(isolate(file_tree_trigger()) + 1)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Renamed folder: ", old_name, " -> ", new_name), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error renaming folder: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Update node output and store logs
  observeEvent(input$node_run, {
    # This will be handled by existing node_run observer
    # But we'll also store the output for logs tab
  })
  
  # Load file content for editor
  observeEvent(input$load_file_content, {
    if (!is.null(input$load_file_content)) {
      file_path <- input$load_file_content$path
      content <- read_r_file(file_path)
      session$sendCustomMessage("loadCodeEditor", list(content = content))
    }
  })
  
  # Save file content
  observeEvent(input$save_file_content, {
    if (!is.null(input$save_file_content)) {
      file_path <- input$save_file_content$path
      content <- input$save_file_content$content
      write_r_file(file_path, content)
      session$sendCustomMessage("codeEditorStatus", list(message = "File saved successfully", success = TRUE))
    }
  })
  
  # Run single node
  observeEvent(input$node_run, {
    if (!is.null(input$node_run)) {
      tryCatch({
        node_data <- input$node_run
        node_id <- node_data$id
        file_path <- node_data$filePath
        
        # Validate inputs
        if (is.null(node_id) || is.null(file_path)) {
          session$sendCustomMessage("appendToLog", list(message = "✗ Invalid node data", type = "error"))
          return()
        }
        
        # Check if file exists
        if (!file.exists(file_path)) {
          session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
          session$sendCustomMessage("appendToLog", list(message = paste0("✗ File not found: ", file_path), type = "error"))
          return()
        }
        
        # Update status to running
        session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "running"))
        session$sendCustomMessage("appendToLog", list(message = paste0("Running: ", node_data$fileName), type = "code"))
        
        # Read and execute code
        code <- read_r_file(file_path)
        if (is.null(code) || !is.character(code)) {
          code <- ""
        }
        
        if (nzchar(code)) {
          # Ensure shared_env exists
          if (!exists("shared_env") || is.null(shared_env)) {
            shared_env <<- new.env()
          }
          
          result <- execute_r_code(code, env = shared_env, echo = TRUE)
          
          if (is.null(result)) {
            session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
            session$sendCustomMessage("appendToLog", list(message = paste0("✗ Execution returned NULL: ", node_data$fileName), type = "error"))
            return()
          }
          
          if (result$success) {
            session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "success"))
            output_text <- if (is.null(result$output)) "" else as.character(result$output)
            session$sendCustomMessage("updateNodeOutput", list(id = node_id, output = output_text, isError = FALSE))
            session$sendCustomMessage("appendToLog", list(message = paste0("✓ Success: ", node_data$fileName), type = "success"))
            # Store output for logs tab
            if (node_id %in% names(canvas_state$nodes)) {
              canvas_state$nodes[[node_id]]$lastOutput <- output_text
            }
            
            # Handle plots
            if (!is.null(result$plots) && length(result$plots) > 0 && exists("HAS_BASE64ENC") && HAS_BASE64ENC) { # nolint
              for (i in seq_along(result$plots)) {
                plot_file <- result$plots[[i]]
                if (!is.null(plot_file) && file.exists(plot_file)) {
                  tryCatch({
                    # Read file as raw bytes, then encode
                    file_size <- file.info(plot_file)$size
                    if (is.numeric(file_size) && file_size > 0) {
                      plot_bytes <- readBin(plot_file, "raw", n = as.integer(file_size))
                      if (length(plot_bytes) > 0) {
                        plot_data <- base64enc::base64encode(plot_bytes)
                        session$sendCustomMessage("addNodePlot", list(id = node_id, plotData = plot_data, index = i))
                      }
                    }
                  }, error = function(e) {
                    # Skip if encoding fails
                    session$sendCustomMessage("appendToLog", list(
                      message = paste0("Failed to encode plot: ", e$message), 
                      type = "error"
                    ))
                  })
                }
              }
            }
          } else {
            session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
            error_msg <- if (is.null(result$error)) "Unknown error" else paste0("Error: ", result$error)
            session$sendCustomMessage("updateNodeOutput", list(id = node_id, output = error_msg, isError = TRUE))
            session$sendCustomMessage("appendToLog", list(message = paste0("✗ Error in ", node_data$fileName, ": ", error_msg), type = "error"))
          }
        } else {
          session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
          session$sendCustomMessage("appendToLog", list(message = paste0("✗ Empty file: ", node_data$fileName), type = "error"))
        }
      }, error = function(e) {
        # Catch any unexpected errors
        node_id <- if (exists("node_id")) node_id else "unknown"
        error_msg <- paste0("Unexpected error: ", e$message)
        session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
        session$sendCustomMessage("appendToLog", list(message = error_msg, type = "error"))
      })
    }
  })
  
  # Handle node deletion
  observeEvent(input$node_delete, {
    if (!is.null(input$node_delete)) {
      tryCatch({
        node_data <- input$node_delete
        node_id <- node_data$id
        
        if (is.null(node_id)) {
          session$sendCustomMessage("appendToLog", list(message = "✗ Invalid node ID for deletion", type = "error"))
          return()
        }
        
        # Remove from canvas_state
        if (node_id %in% names(canvas_state$nodes)) {
          canvas_state$nodes[[node_id]] <- NULL
        }
        
        # Remove all edges connected to this node
        if (!is.null(canvas_state$edges) && length(canvas_state$edges) > 0 && is.list(canvas_state$edges)) {
          edges_to_remove <- c()
          for (i in seq_along(canvas_state$edges)) {
            edge <- canvas_state$edges[[i]]
            # Check if edge is a list (not atomic vector)
            if (is.list(edge) && !is.null(edge)) {
              if (!is.null(edge$source) && edge$source == node_id) {
                edges_to_remove <- c(edges_to_remove, i)
              } else if (!is.null(edge$target) && edge$target == node_id) {
                edges_to_remove <- c(edges_to_remove, i)
              }
            }
          }
          if (length(edges_to_remove) > 0) {
            canvas_state$edges <- canvas_state$edges[-edges_to_remove]
          }
        }
        
        # Send message to client to remove node from DOM and jsPlumb
        session$sendCustomMessage("removeNode", list(id = node_id))
        node_name <- if (!is.null(node_data$fileName)) node_data$fileName else node_id
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Removed node: ", node_name), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error deleting node: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # Run workflow (topological order)
  observeEvent(input$run_workflow, {
    tryCatch({
      if (is.null(canvas_state$nodes) || length(canvas_state$nodes) == 0) {
        session$sendCustomMessage("appendToLog", list(message = "No nodes to run", type = "error"))
        return()
      }
      
      # Ensure shared_env exists
      if (!exists("shared_env") || is.null(shared_env)) {
        shared_env <<- new.env()
      }
      
      # Build hierarchy
      sorted_nodes <- tryCatch({
        build_node_hierarchy(canvas_state$nodes, canvas_state$edges)
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error building node hierarchy: ", e$message), 
          type = "error"
        ))
        return(list())
      })
      
      if (length(sorted_nodes) == 0) {
        session$sendCustomMessage("appendToLog", list(message = "No valid nodes to execute", type = "error"))
        return()
      }
      
      # Clear log
      session$sendCustomMessage("clearLog", list())
      session$sendCustomMessage("appendToLog", list(message = "Starting workflow execution...", type = "code"))
      
      # Reset all node statuses
      for (node_id in names(canvas_state$nodes)) {
        session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = NULL))
      }
      
      # Execute nodes in order (following arrow connections)
      for (node_info in sorted_nodes) {
        tryCatch({
          node_id <- node_info$id
          node <- node_info$node
          file_path <- node$filePath
          
          # Validate inputs
          if (is.null(node_id) || is.null(node) || is.null(file_path)) {
            session$sendCustomMessage("appendToLog", list(
              message = "✗ Invalid node data in workflow", 
              type = "error"
            ))
            next
          }
          
          # Validate file path
          if (!nzchar(file_path) || !file.exists(file_path)) {
            session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
            session$sendCustomMessage("appendToLog", list(
              message = paste0("✗ File not found: ", if (!is.null(node$fileName)) node$fileName else file_path), 
              type = "error"
            ))
            session$sendCustomMessage("appendToLog", list(
              message = "Workflow stopped due to missing file", 
              type = "error"
            ))
            break
          }
          
          # Update status to running
          session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "running"))
          session$sendCustomMessage("appendToLog", list(
            message = paste0("→ Running: ", if (!is.null(node$fileName)) node$fileName else basename(file_path)), 
            type = "code"
          ))
          
          # Execute code in shared environment
          code <- read_r_file(file_path)
          if (is.null(code) || !is.character(code)) {
            code <- ""
          }
          
          if (nzchar(code)) {
            result <- tryCatch({
              execute_r_code(code, env = shared_env, echo = TRUE)
            }, error = function(e) {
              list(success = FALSE, error = e$message, output = paste0("Execution error: ", e$message))
            })
            
            if (is.null(result)) {
              session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
              session$sendCustomMessage("appendToLog", list(
                message = paste0("✗ Execution returned NULL: ", if (!is.null(node$fileName)) node$fileName else basename(file_path)), 
                type = "error"
              ))
              break
            }
            
            if (result$success) {
              session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "success"))
              output_text <- if (is.null(result$output)) "" else as.character(result$output)
              session$sendCustomMessage("updateNodeOutput", list(
                id = node_id, 
                output = output_text, 
                isError = FALSE
              ))
              # Store output for logs tab
              if (node_id %in% names(canvas_state$nodes)) {
                canvas_state$nodes[[node_id]]$lastOutput <- output_text
              }
              session$sendCustomMessage("appendToLog", list(
                message = paste0("✓ Success: ", if (!is.null(node$fileName)) node$fileName else basename(file_path)), 
                type = "success"
              ))
            } else {
              session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
              error_msg <- if (is.null(result$error)) "Unknown error" else paste0("Error: ", result$error)
              session$sendCustomMessage("updateNodeOutput", list(
                id = node_id, 
                output = error_msg, 
                isError = TRUE
              ))
              session$sendCustomMessage("appendToLog", list(
                message = paste0("✗ Error in ", if (!is.null(node$fileName)) node$fileName else basename(file_path), ": ", error_msg), 
                type = "error"
              ))
              # Stop execution on error (arrow connections ensure proper order)
              session$sendCustomMessage("appendToLog", list(
                message = "Workflow stopped due to error", 
                type = "error"
              ))
              break
            }
          } else {
            session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
            session$sendCustomMessage("appendToLog", list(
              message = paste0("✗ Empty file: ", if (!is.null(node$fileName)) node$fileName else basename(file_path)), 
              type = "error"
            ))
          }
        }, error = function(e) {
          # Catch errors in individual node execution
          node_id <- if (exists("node_id")) node_id else "unknown"
          session$sendCustomMessage("updateNodeStatus", list(id = node_id, status = "error"))
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ Unexpected error executing node: ", e$message), 
            type = "error"
          ))
        })
      }
      
      session$sendCustomMessage("appendToLog", list(message = "Workflow execution completed", type = "success"))
    }, error = function(e) {
      # Catch any top-level errors
      session$sendCustomMessage("appendToLog", list(
        message = paste0("✗ Fatal error in workflow execution: ", e$message), 
        type = "error"
      ))
    })
  })
  
  # Refresh RMD Preview
  observeEvent(input$refresh_rmd_preview, {
    tryCatch({
      # Check if nodes exist and are valid
      if (is.null(canvas_state$nodes) || length(canvas_state$nodes) == 0) {
        session$sendCustomMessage("updateRmdPreview", list(content = "# No nodes in workflow\n\nAdd nodes to the canvas to generate RMD preview."))
        return()
      }
      
      # Ensure nodes is a list
      if (!is.list(canvas_state$nodes)) {
        session$sendCustomMessage("updateRmdPreview", list(content = "# Invalid node structure\n\nPlease refresh the page and try again."))
        return()
      }
      
      # Build node hierarchy
      sorted_nodes <- tryCatch({
        build_node_hierarchy(canvas_state$nodes, canvas_state$edges)
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error building node hierarchy: ", e$message), 
          type = "error"
        ))
        return(list())
      })
      
      if (length(sorted_nodes) == 0) {
        session$sendCustomMessage("updateRmdPreview", list(content = "# No valid nodes in workflow\n\nEnsure nodes are properly connected."))
        return()
      }
      
      # Get workspace structure
      structure <- tryCatch({
        scan_workspace()
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error scanning workspace: ", e$message), 
          type = "error"
        ))
        return(list(files = list(), folders = character(0)))
      })
      
      # Generate RMD content
      rmd_content <- tryCatch({
        generate_rmd_from_hierarchy(sorted_nodes, structure$files, canvas_state$edges, include_metadata = TRUE, node_specs = canvas_state$nodes)
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error generating RMD: ", e$message), 
          type = "error"
        ))
        return("# Error generating RMD preview\n\nPlease check the logs for details.")
      })
      
      # Update preview
      session$sendCustomMessage("updateRmdPreview", list(content = rmd_content))
      session$sendCustomMessage("appendToLog", list(message = "✓ RMD preview refreshed", type = "success"))
    }, error = function(e) {
      session$sendCustomMessage("updateRmdPreview", list(content = paste0("# Error refreshing RMD preview\n\n", e$message)))
      session$sendCustomMessage("appendToLog", list(
        message = paste0("✗ Error refreshing RMD preview: ", e$message), 
        type = "error"
      ))
    })
  })
  
  # Export RMD from preview
  observeEvent(input$export_rmd_from_preview, {
    if (!is.null(input$export_rmd_from_preview) && !is.null(input$export_rmd_from_preview$content)) {
      rmd_content <- input$export_rmd_from_preview$content
      session$sendCustomMessage("downloadFile", list(
        filename = "workflow.Rmd",
        content = rmd_content,
        mimeType = "text/plain"
      ))
      session$sendCustomMessage("appendToLog", list(message = "✓ RMD file exported", type = "success"))
    }
  })
  
  # Export to RMarkdown (from header button)
  observeEvent(input$export_rmd, {
    if (length(canvas_state$nodes) == 0) {
      session$sendCustomMessage("appendToLog", list(message = "No nodes to export", type = "error"))
      return()
    }
    
    sorted_nodes <- build_node_hierarchy(canvas_state$nodes, canvas_state$edges)
    structure <- scan_workspace()
    rmd_content <- generate_rmd_from_hierarchy(sorted_nodes, structure$files, canvas_state$edges, include_metadata = TRUE, node_specs = canvas_state$nodes)
    
    # Create download
    session$sendCustomMessage("downloadFile", list(
      filename = "workflow.Rmd",
      content = rmd_content,
      mimeType = "text/plain"
    ))
    session$sendCustomMessage("appendToLog", list(message = "✓ RMD file exported", type = "success"))
  })
  
  # Parse RMD file and restore canvas
  parse_rmd_file <- function(rmd_content) {
    # Extract metadata section - use more flexible regex
    # Try multiple patterns to handle different line ending formats
    patterns <- c(
      "<!-- RCW_METADATA_START[\\s\\S]*?RCW_METADATA_END -->",
      "<!--\\s*RCW_METADATA_START[\\s\\S]*?RCW_METADATA_END\\s*-->",
      "RCW_METADATA_START[\\s\\S]*?RCW_METADATA_END"
    )
    
    metadata_match <- NULL
    for (pattern in patterns) {
      metadata_match <- regmatches(rmd_content, regexpr(pattern, rmd_content, perl = TRUE))
      if (length(metadata_match) > 0 && nchar(metadata_match[1]) > 0) {
        break
      }
    }
    
    if (length(metadata_match) == 0 || nchar(metadata_match[1]) == 0) {
      # Try to find if file contains any RCW markers
      has_rcw_markers <- grepl("RCW_METADATA|RCW_NODE", rmd_content)
      if (!has_rcw_markers) {
        return(list(success = FALSE, error = "No RCW metadata found in RMD file. This file was not exported from RCW or metadata was removed."))
      } else {
        return(list(success = FALSE, error = "RCW markers found but metadata section could not be parsed. File may be corrupted."))
      }
    }
    
    metadata_text <- metadata_match[1]
    
    # Parse nodes and connections from metadata
    nodes <- list()
    connections <- list()
    node_lines <- strsplit(metadata_text, "\n")[[1]]
    current_node <- NULL
    parsing_connections <- FALSE
    parsing_manifest <- FALSE
    current_connection <- NULL
    
    for (line in node_lines) {
      # Remove HTML comment markers if present
      line <- gsub("^<!--\\s*", "", line)
      line <- gsub("\\s*-->$", "", line)
      line <- trimws(line)
      
      # Skip empty lines and metadata headers
      if (line == "" || grepl("^RCW_", line) || grepl("^RCW_VERSION|^NODE_COUNT", line)) {
        next
      }
      
      # Skip interface manifest JSON block
      if (grepl("^INTERFACE_MANIFEST_JSON:", line)) {
        parsing_manifest <- TRUE
        next
      }
      if (parsing_manifest) {
        # Stop skipping when we hit NODES:
        if (grepl("^NODES:", line)) {
          parsing_manifest <- FALSE
        } else {
          next
        }
      }

      # Check if we're starting to parse connections
      if (grepl("^CONNECTIONS:", line)) {
        parsing_connections <- TRUE
        # Save the last node if any
        if (!is.null(current_node) && !is.null(current_node$id)) {
          nodes[[current_node$id]] <- current_node
          current_node <- NULL
        }
        next
      }
      
      # Skip CONNECTION_COUNT line (for debugging, but we can use it to validate)
      if (grepl("^CONNECTION_COUNT:", line)) {
        next
      }
      
      # Parse connections
      if (parsing_connections) {
        if (grepl("^\\s*-\\s*source:", line) || grepl("^source:", line)) {
          # Save previous connection if any
          if (!is.null(current_connection) && !is.null(current_connection$source) && !is.null(current_connection$target)) {
            connections[[length(connections) + 1]] <- current_connection
          }
          # Extract source
          source_value <- gsub("^\\s*-\\s*source:\\s*", "", line)
          source_value <- gsub("^source:\\s*", "", source_value)
          current_connection <- list(source = trimws(source_value))
        } else if (grepl("^\\s+target:", line) && !is.null(current_connection)) {
          # Extract target
          target_value <- gsub("^\\s+target:\\s*", "", line)
          target_value <- gsub("^target:\\s*", "", target_value)
          current_connection$target <- trimws(target_value)
        }
        next
      }
      
      # Parse nodes (only if not parsing connections)
      if (grepl("^NODES:", line)) {
        next
      }
      
      # Handle YAML list item format: "  - id: xxx" or just "id: xxx"
      if (grepl("^\\s*-\\s*id:", line) || grepl("^id:", line)) {
        if (!is.null(current_node) && !is.null(current_node$id)) {
          nodes[[current_node$id]] <- current_node
        }
        # Extract ID - handle both "  - id: xxx" and "id: xxx" formats
        id_value <- gsub("^\\s*-\\s*id:\\s*", "", line)
        id_value <- gsub("^id:\\s*", "", id_value)
        current_node <- list(id = trimws(id_value))
      } else if (grepl("fileName:", line) && !is.null(current_node)) {
        # Handle indented fields: "    fileName: xxx" or "fileName: xxx"
        # Remove leading whitespace and "fileName:" prefix
        value <- gsub("^\\s+fileName:\\s*", "", line)
        value <- gsub("^fileName:\\s*", "", value)
        value <- trimws(value)
        # Remove quotes if present
        value <- gsub("^[\"']|[\"']$", "", value)
        current_node$fileName <- value
      } else if (grepl("filePath:", line) && !is.null(current_node)) {
        # Handle indented fields: "    filePath: xxx" or "filePath: xxx"
        # Remove leading whitespace and "filePath:" prefix
        value <- gsub("^\\s+filePath:\\s*", "", line)
        value <- gsub("^filePath:\\s*", "", value)
        value <- trimws(value)
        # Remove quotes if present
        value <- gsub("^[\"']|[\"']$", "", value)
        current_node$filePath <- value
      } else if (grepl("level:", line) && !is.null(current_node)) {
        level_str <- trimws(gsub("^\\s+level:\\s*", "", line))
        level_str <- gsub("^level:\\s*", "", level_str)
        level_str <- trimws(level_str)
        current_node$level <- as.integer(level_str)
      } else if (grepl("index:", line) && !is.null(current_node)) {
        # Also capture index if present
        index_str <- trimws(gsub("^\\s+index:\\s*", "", line))
        index_str <- gsub("^index:\\s*", "", index_str)
        index_str <- trimws(index_str)
        current_node$index <- as.integer(index_str)
      }
    }
    
    # Don't forget the last node and connection
    if (!is.null(current_node) && !is.null(current_node$id)) {
      nodes[[current_node$id]] <- current_node
    }
    if (!is.null(current_connection) && !is.null(current_connection$source) && !is.null(current_connection$target)) {
      connections[[length(connections) + 1]] <- current_connection
    }
    
    # Extract code chunks and match with nodes - use more flexible regex
    node_chunk_patterns <- c(
      "<!-- RCW_NODE_START[\\s\\S]*?RCW_NODE_END[^>]*-->",
      "<!--\\s*RCW_NODE_START[\\s\\S]*?RCW_NODE_END\\s*-->",
      "RCW_NODE_START[\\s\\S]*?RCW_NODE_END"
    )
    
    node_chunks <- NULL
    for (pattern in node_chunk_patterns) {
      node_chunks <- regmatches(rmd_content, gregexpr(pattern, rmd_content, perl = TRUE))[[1]]
      if (length(node_chunks) > 0) {
        break
      }
    }
    
    if (length(node_chunks) > 0) {
      for (chunk in node_chunks) {
        # Extract node ID - try multiple patterns
        id_patterns <- c("id:\\s*([^\\s]+)", "id:([^\\s]+)", "id=\"([^\"]+)\"", "id='([^']+)'")
        node_id <- NULL
        for (id_pattern in id_patterns) {
          id_match <- regmatches(chunk, regexpr(id_pattern, chunk, perl = TRUE))
          if (length(id_match) > 0) {
            node_id <- gsub(id_pattern, "\\1", id_match[1], perl = TRUE)
            break
          }
        }
        
        if (!is.null(node_id) && node_id != "") {
          # Extract level
          level_patterns <- c("level:\\s*([0-9]+)", "level:([0-9]+)", "level=\"([0-9]+)\"", "level='([0-9]+)'")
          for (level_pattern in level_patterns) {
            level_match <- regmatches(chunk, regexpr(level_pattern, chunk, perl = TRUE))
            if (length(level_match) > 0) {
              level <- as.integer(gsub(level_pattern, "\\1", level_match[1], perl = TRUE))
              if (node_id %in% names(nodes)) {
                nodes[[node_id]]$level <- level
              }
              break
            }
          }
        }
      }
    }
    
    return(list(success = TRUE, nodes = nodes, connections = connections))
  }
  
  # Import RMD file
  observeEvent(input$import_rmd, {
    if (!is.null(input$import_rmd) && !is.null(input$import_rmd$content)) {
      tryCatch({
        rmd_content <- input$import_rmd$content
        
        # Log file info for debugging
        file_size <- nchar(rmd_content)
        has_rcw_markers <- grepl("RCW_METADATA|RCW_NODE", rmd_content)
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("→ Importing RMD file (", file_size, " chars, RCW markers: ", if (has_rcw_markers) "found" else "not found", ")"), 
          type = "code"
        ))
        
        parsed <- parse_rmd_file(rmd_content)
        
        if (!parsed$success) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ Error importing RMD: ", parsed$error), 
            type = "error"
          ))
          return()
        }
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ Parsed ", length(parsed$nodes), " nodes from RMD metadata"), 
          type = "success"
        ))
        
        # Log connection information
        if (!is.null(parsed$connections) && length(parsed$connections) > 0) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("→ Found ", length(parsed$connections), " connections in RMD metadata"), 
            type = "code"
          ))
        } else {
          session$sendCustomMessage("appendToLog", list(
            message = "→ No connections found in RMD metadata", 
            type = "warning"
          ))
        }
        
        # Clear existing canvas
        canvas_state$nodes <- list()
        canvas_state$edges <- list()
        
        # Restore nodes directly from RMD metadata, without checking if files exist
        node_order <- names(parsed$nodes)
        
        # Sort nodes by level and index to maintain order
        node_levels <- sapply(parsed$nodes, function(n) n$level %||% 1)
        node_order <- node_order[order(node_levels)]
        
        restored_nodes <- list()
        
        for (node_id in node_order) {
          node_info <- parsed$nodes[[node_id]]
          
          # Debug: log what we have
          session$sendCustomMessage("appendToLog", list(
            message = paste0("→ Processing node ", node_id, ": filePath=", 
              if (is.null(node_info$filePath)) "NULL" else node_info$filePath,
              ", fileName=", 
              if (is.null(node_info$fileName)) "NULL" else node_info$fileName), 
            type = "code"
          ))
          
          file_path <- node_info$filePath
          file_name <- node_info$fileName
          
          # Determine the file path to use - prioritize filePath from metadata, fallback to fileName
          final_file_path <- NULL
          final_file_name <- NULL
          
          # Use filePath from metadata if available
          if (!is.null(file_path) && is.character(file_path) && nchar(trimws(file_path)) > 0) {
            final_file_path <- trimws(file_path)
            # Extract fileName from path if fileName is not provided
            if (is.null(file_name) || !is.character(file_name) || nchar(trimws(file_name)) == 0) {
              final_file_name <- basename(final_file_path)
            } else {
              final_file_name <- trimws(file_name)
            }
          } else if (!is.null(file_name) && is.character(file_name) && nchar(trimws(file_name)) > 0) {
            # Use fileName and construct path from workspace root
            final_file_name <- trimws(file_name)
            workspace_root <- .rcw_workspace
            if (length(.rcw_external_folders) > 0) {
              workspace_root <- .rcw_external_folders[1]
            }
            final_file_path <- file.path(workspace_root, final_file_name)
          } else {
            # If both are missing, try to extract from node_id or use a default
            # Sometimes node_id might be the filename
            if (!is.null(node_id) && is.character(node_id) && nchar(trimws(node_id)) > 0) {
              # Check if node_id looks like a filename
              if (grepl("\\.(R|r)$", node_id)) {
                final_file_name <- node_id
                workspace_root <- .rcw_workspace
                if (length(.rcw_external_folders) > 0) {
                  workspace_root <- .rcw_external_folders[1]
                }
                final_file_path <- file.path(workspace_root, final_file_name)
              } else {
                # Use node_id as fileName with .R extension
                final_file_name <- paste0(node_id, ".R")
                workspace_root <- .rcw_workspace
                if (length(.rcw_external_folders) > 0) {
                  workspace_root <- .rcw_external_folders[1]
                }
                final_file_path <- file.path(workspace_root, final_file_name)
              }
            }
          }
          
          # Create node if we have at least a fileName
          if (!is.null(final_file_name) && nchar(final_file_name) > 0) {
            # Create node on canvas directly from RMD metadata
            session$sendCustomMessage("createNodeFromRmd", list(
              id = node_id,
              filePath = if (is.null(final_file_path)) "" else final_file_path,
              fileName = final_file_name,
              level = node_info$level %||% 1
            ))
            
            # Store for later connection restoration
            restored_nodes[[node_id]] <- list(
              filePath = if (is.null(final_file_path)) "" else final_file_path,
              fileName = final_file_name,
              level = node_info$level %||% 1
            )

            # Update server-side canvas state
            canvas_state$nodes[[node_id]] <- list(
              id = node_id,
              filePath = if (is.null(final_file_path)) "" else final_file_path,
              fileName = final_file_name
            )
            if (!is.null(final_file_path) && grepl("\\.Rmd$", final_file_path, ignore.case = TRUE)) {
              module_spec <- parse_rmd_interface(final_file_path)
              if (!is.null(module_spec)) {
                canvas_state$nodes[[node_id]]$moduleSpec <- module_spec
                session$sendCustomMessage("apply_node_ports", list(
                  id = node_id,
                  moduleSpec = module_spec
                ))
              }
            }
            
            # Log whether file exists or not (informational only)
            file_exists_msg <- ""
            if (!is.null(final_file_path) && file.exists(final_file_path)) {
              file_exists_msg <- " (file exists)"
            } else if (!is.null(final_file_path)) {
              file_exists_msg <- " (file not found, but node created)"
            }
            
            session$sendCustomMessage("appendToLog", list(
              message = paste0("✓ Restored node: ", final_file_name, file_exists_msg), 
              type = "success"
            ))
          } else {
            session$sendCustomMessage("appendToLog", list(
              message = paste0("⚠ Skipping node ", node_id, ": no fileName or filePath available"), 
              type = "error"
            ))
          }
        }
        
        # Wait a bit for nodes to be created, then restore connections
        Sys.sleep(1.0)
        
        # Restore connections from parsed metadata
        if (!is.null(parsed$connections) && length(parsed$connections) > 0) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("→ Found ", length(parsed$connections), " connections in metadata"), 
            type = "code"
          ))
          
          connection_count <- 0
          for (conn in parsed$connections) {
            if (is.list(conn) && !is.null(conn$source) && !is.null(conn$target)) {
              source_id <- trimws(conn$source)
              target_id <- trimws(conn$target)
              
              # Verify both nodes exist in restored_nodes
              if (source_id %in% names(restored_nodes) && target_id %in% names(restored_nodes)) {
                # Create connection with a small delay between each to avoid race conditions
                Sys.sleep(0.1)
                session$sendCustomMessage("createConnectionFromRmd", list(
                  source = source_id,
                  target = target_id
                ))
                connection_count <- connection_count + 1
              } else {
                session$sendCustomMessage("appendToLog", list(
                  message = paste0("⚠ Skipping connection: source or target node not found (", source_id, " -> ", target_id, ")"), 
                  type = "warning"
                ))
              }
            }
          }
          
          # Wait a bit more for all connections to be created
          Sys.sleep(0.5)
          
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✓ Restored ", connection_count, " connections"), 
            type = "success"
          ))
        } else {
          # Fallback: Build connections based on level hierarchy if no connections in metadata
          session$sendCustomMessage("appendToLog", list(
            message = "→ No connection metadata found, inferring from level hierarchy", 
            type = "code"
          ))
          
          for (i in seq_along(restored_nodes)) {
            current_id <- names(restored_nodes)[i]
            current_level <- restored_nodes[[current_id]]$level
            
            # Find parent (node with level = current_level - 1, closest before current)
            if (current_level > 1) {
              for (j in seq_len(i - 1)) {
                parent_id <- names(restored_nodes)[j]
                parent_level <- restored_nodes[[parent_id]]$level
                if (parent_level == current_level - 1) {
                  # Create connection
                  session$sendCustomMessage("createConnectionFromRmd", list(
                    source = parent_id,
                    target = current_id
                  ))
                  break
                }
              }
            }
          }
        }
        
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✓ RMD file imported: ", length(parsed$nodes), " nodes restored"), 
          type = "success"
        ))
      }, error = function(e) {
        session$sendCustomMessage("appendToLog", list(
          message = paste0("✗ Error importing RMD: ", e$message), 
          type = "error"
        ))
      })
    }
  })
  
  # File import (if shinyFiles is available)
            if (exists("HAS_SHINYFILES") && HAS_SHINYFILES) { # nolint
    volumes <- c(Home = path.expand("~"), getVolumes()())
    
    # Use shinyDirChoose for folder selection (not shinyFileChoose)
    if (exists("shinyDirChoose", mode = "function")) {
      shinyDirChoose(input, "importFolder", roots = volumes, session = session)
    } else {
      # Fallback to shinyFileChoose if shinyDirChoose not available
      shinyFileChoose(input, "importFolder", roots = volumes, session = session, filetypes = NULL)
    }
    
    # Trigger folder selection dialog
    observeEvent(input$trigger_import_folder, {
      if (!is.null(input$trigger_import_folder)) {
        # Send JavaScript message to trigger the file input click
        session$sendCustomMessage("triggerFileInputClick", list(
          selector = "input[type='file'][data-namespace='shinyFiles'], input[type='file'][data-namespace='shinyDirectories']"
        ))
      }
    })
    
    observeEvent(input$importFolder, {
      if (!is.null(input$importFolder) && !identical(input$importFolder, "")) {
        tryCatch({
          # Try parseDirPath first (for shinyDirChoose)
          if (exists("parseDirPath")) {
            folder_path <- tryCatch({
              parseDirPath(volumes, input$importFolder)
            }, error = function(e) {
              # Fallback to parseFilePaths
              selected_path <- parseFilePaths(volumes, input$importFolder)
              if (nrow(selected_path) > 0) {
                # Get directory from file path
                dirname(selected_path$datapath[1])
              } else {
                NULL
              }
            })
          } else {
            # Use parseFilePaths and get directory
            selected_path <- parseFilePaths(volumes, input$importFolder)
            if (nrow(selected_path) > 0) {
              folder_path <- dirname(selected_path$datapath[1])
            } else {
              folder_path <- NULL
            }
          }
          
          if (!is.null(folder_path) && dir.exists(folder_path)) {
            # Add to external folders if not already added
            folder_path_norm <- normalizePath(folder_path, winslash = "/")
            if (!folder_path_norm %in% .rcw_external_folders) {
              .rcw_external_folders <<- c(.rcw_external_folders, folder_path_norm)
            }
            
            # Trigger file tree refresh
            file_tree_trigger(isolate(file_tree_trigger()) + 1)
            
            session$sendCustomMessage("appendToLog", list(
              message = paste0("✓ Imported folder: ", folder_path), 
              type = "success"
            ))
          } else {
            session$sendCustomMessage("appendToLog", list(
              message = paste0("✗ Invalid folder path or not a directory: ", ifelse(is.null(folder_path), "NULL", folder_path)), 
              type = "error"
            ))
          }
        }, error = function(e) {
          session$sendCustomMessage("appendToLog", list(
            message = paste0("✗ Error importing folder: ", e$message), 
            type = "error"
          ))
        })
      }
    })
  }
  
  # Reactive value to trigger file tree refresh
  file_tree_trigger <- reactiveVal(0)
  
  # Auto-refresh file tree
  observe({
    file_tree_trigger()  # Depend on this reactive value
    structure <- scan_workspace()
    
    output$file_tree_ui <- renderUI({
      if (length(structure$folders) == 0 && length(structure$files) == 0) {
        session$sendCustomMessage("showOpenFolder", list(show = TRUE))
        return(tags$div())
      }
      
      session$sendCustomMessage("showOpenFolder", list(show = FALSE))
      
      # Group files by folder
      files_by_folder <- list()
      for (file_info in structure$files) {
        folder <- file_info$folder
        if (!folder %in% names(files_by_folder)) {
          files_by_folder[[folder]] <- list()
        }
        files_by_folder[[folder]] <- c(files_by_folder[[folder]], list(file_info))
      }
      
      # Build HTML
      html_list <- list()
      sorted_folders <- sort(unique(structure$folders))
      
      for (folder in sorted_folders) {
        folder_id <- gsub("[^a-zA-Z0-9]", "_", folder)
        folder_name <- if (folder == "root") "root" else gsub("^root/", "", folder)
        folder_files <- files_by_folder[[folder]] %||% list()
        
        html_list <- c(html_list, list(
          tags$div(
            class = "file-tree-item folder",
            `data-folder-id` = folder_id,
            tags$div(class = "folder-header",
              tags$span(class = "folder-toggle", "▶"),
              tags$span(class = "folder-name", title = folder, folder_name)
            )
          ),
          tags$div(
            id = paste0("folder-files-", folder_id),
            class = "folder-files",
            lapply(folder_files, function(file_info) {
              tags$div(
                class = "file-tree-item file",
                `data-filepath` = file_info$path,
                `data-filename` = file_info$name,
                tags$span(class = "file-icon", "📄"),
                tags$span(class = "file-name", title = file_info$name, file_info$name)
              )
            })
          )
        ))
      }
      
      tags$ul(class = "file-tree", html_list)
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)
