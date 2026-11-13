# Shared runtime limits and session storage (installed or source layout).
.runtime_candidates <- c(file.path("..", "Runtime.R"), "Runtime.R",
                         file.path("inst", "Runtime.R"),
                         system.file("Runtime.R", package = "easybreedeR"))
.runtime_file <- .runtime_candidates[file.exists(.runtime_candidates)][1L]
if (is.na(.runtime_file)) stop("Missing Runtime.R; include shared runtime helpers in the deployment.")
source(.runtime_file, local = TRUE)
eb_configure_uploads()

# ================================================
# Xmosome_blupf90kit — BLUPF90 parameter card builder
# Visual language aligned with Xmosome BundledPlugins
# (GenoKit / PedigreeKit): SF system font, blue accent,
# glass panels, compact 13px chrome.
# ================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

# Hosted Studio embeds this UI under ?view=child; register www/ so CSS/JS resolve.
.xmosome_www_dir <- local({
  cands <- c(
    file.path(getwd(), "www"),
    "www",
    file.path("inst", "Xmosome_blupf90kit", "www"),
    tryCatch(
      system.file("Xmosome_blupf90kit", "www", package = "easybreedeR", mustWork = FALSE),
      error = function(e) ""
    )
  )
  for (d in unique(cands[nzchar(cands)])) {
    if (dir.exists(d)) return(normalizePath(d, winslash = "/", mustWork = FALSE))
  }
  NULL
})
.xmosome_asset_prefix <- "xmosome-assets"
if (!is.null(.xmosome_www_dir)) {
  try(shiny::addResourcePath(.xmosome_asset_prefix, .xmosome_www_dir), silent = TRUE)
}
.xmosome_asset <- function(file) {
  paste0(.xmosome_asset_prefix, "/", sub("^/+", "", as.character(file)))
}

# Declare externally provided helpers to silence lintr visibility warnings
utils::globalVariables(c("resolve_suite_lang", "map_suite_lang_for_app"))

# Source shared language helpers (export into global env so get_label is visible)
try(source(normalizePath(file.path("..", "Language.R"), winslash = "/", mustWork = FALSE), local = FALSE), silent = TRUE)

`%||%` <- function(x, y) if (is.null(x) || identical(x, "")) y else x

# AI assistant (OpenAI-compatible chat + diff helpers)
if (file.exists("ai_backend.R")) {
  source("ai_backend.R", local = FALSE)
} else if (file.exists(file.path(getwd(), "ai_backend.R"))) {
  source(file.path(getwd(), "ai_backend.R"), local = FALSE)
}

# Capture shared get_label before defining local alias
shared_get_label <- if (exists("get_label", mode = "function")) get_label else NULL

# Use shared get_label, but wrap it to add app prefix and handle EN/ZH uppercase
get_label_local <- function(key, lang = "EN") {
  # Normalize lang to lowercase for lookup, but remember original case
  lang_lower <- "en"
  
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

# Strip emoji / decorative prefixes for Xmosome-clean chrome
strip_ui_emoji <- function(x) {
  x <- as.character(if (is.null(x)) "" else x)
  if (!nzchar(x)) return(x)
  # Known Language.R decorative prefixes used by easyblup labels
  x <- sub("^(🧬|📊|🎲|🐄|➕|🔄|📝)\\s*", "", x)
  # Broad fallback: drop leading non-letter / non-CJK / non-digit symbols
  x <- sub("^[^A-Za-z0-9\u4e00-\u9fff]+\\s*", "", x)
  trimws(x)
}

get_label_clean <- function(key, lang = "en") {
  strip_ui_emoji(get_label(key, lang))
}

# Selectize options: keep dropdown anchored to body (no clip), but match control width
kit_selectize_opts <- function(placeholder, multi = TRUE) {
  opts <- list(
    placeholder = placeholder,
    dropdownParent = "body",
    onDropdownOpen = I(paste0(
      "function($dropdown){",
      "var w=this.$control.outerWidth()||240;",
      "w=Math.max(160,Math.min(w,320));",
      "$dropdown.css({width:w+'px',minWidth:w+'px',maxWidth:w+'px'});",
      "}"
    ))
  )
  if (isTRUE(multi)) opts$plugins <- list("remove_button")
  opts
}

# BLUPF90 skill model_templates.md — T1–T17 catalog
KIT_TEMPLATES <- list(
  t1  = list(label = "T1 · Animal model",
             hint  = "Fixed effects + animal + residual.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t2  = list(label = "T2 · + diagonal random",
             hint  = "Add independent random effects, such as litter.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t3  = list(label = "T3 · Repeatability (+PE)",
             hint  = "Repeated records: add permanent environment (PE).",
             pe = TRUE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t4  = list(label = "T4 · Maternal (+mat/mpe)",
             hint  = "Add maternal genetic and permanent environment effects.",
             pe = FALSE, mat = TRUE, mpe = TRUE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t5  = list(label = "T5 · Multi-trait animal",
             hint  = "Select multiple traits; keep covariance order consistent.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t6  = list(label = "T6 · Unknown parent groups",
             hint  = "Unknown-parent groups: complete group definitions manually.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = TRUE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t7  = list(label = "T7 · Random regression",
             hint  = "Random regression: complete basis and covariance matrices manually.",
             pe = TRUE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = TRUE, hetres = FALSE, gibbs = FALSE),
  t8  = list(label = "T8 · ssGBLUP",
             hint  = "Combine pedigree and genotypes for ssGBLUP.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = TRUE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t9  = list(label = "T9 · GBLUP (geno only)",
             hint  = "GBLUP: all phenotyped animals need genotypes. Review matrix settings.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = TRUE, omit_ped = TRUE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t10 = list(label = "T10 · Threshold / Gibbs",
             hint  = "Categorical traits: set category counts under Gibbs.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = TRUE),
  t11 = list(label = "T11 · Gibbs linear model",
             hint  = "Linear Gibbs model; configure the sampling chain separately.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = TRUE),
  t12 = list(label = "T12 · Heterogeneous residual",
             hint  = "Variable residual variance; requires AI-REML, not EM.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = TRUE, gibbs = FALSE),
  t13 = list(label = "T13 · Sire / sire–MGS",
             hint  = "Sire model: complete renf90.par manually.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t14 = list(label = "T14 · Competition / social",
             hint  = "Social effects: complete renf90.par manually.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t15 = list(label = "T15 · Dominance / user_file",
             hint  = "Custom relationships: configure user_file in renf90.par.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t16 = list(label = "T16 · Terminal cross (2 pedigrees)",
             hint  = "Two pedigrees: complete renf90.par manually.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE),
  t17 = list(label = "T17 · Fixed-only / COMBINE",
             hint  = "Fixed effects only; clear Animal ID if unused.",
             pe = FALSE, mat = FALSE, mpe = FALSE, geno = FALSE, omit_ped = FALSE,
             upg = FALSE, rr = FALSE, hetres = FALSE, gibbs = FALSE)
)

kit_template_choices <- function() {
  setNames(names(KIT_TEMPLATES), vapply(KIT_TEMPLATES, `[[`, "", "label"))
}

kit_template_info <- function(id) {
  id <- as.character(id %||% "t1")
  # legacy aliases from earlier UI
  id <- switch(id,
    animal = "t1", pe = "t3", maternal = "t4", ssgblup = "t8",
    id
  )
  KIT_TEMPLATES[[id]] %||% KIT_TEMPLATES[["t1"]]
}

# plinkR is optional. Prefer plinkR when available, but also support direct
# PLINK CLI usage so users do not need the plinkR R package installed.
get_plinkr_checked_backend <- function() {
  if (!requireNamespace("plinkR", quietly = TRUE)) {
    return(NULL)
  }
  chk <- tryCatch({
    ns <- asNamespace("plinkR")
    if (exists("check_plink", mode = "function", envir = ns, inherits = FALSE)) {
      plinkR::check_plink(verbose = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)

  if (is.list(chk) && "found" %in% names(chk) && !isTRUE(chk$found)) {
    return(NULL)
  }

  exe <- if (is.list(chk) && "exe" %in% names(chk)) chk$exe else Sys.which("plink")
  exe <- if (is.character(exe) && length(exe) >= 1) exe[1] else ""
  if (!nzchar(exe)) exe <- Sys.which("plink")[1]
  if (!is.character(exe) || length(exe) == 0 || !nzchar(exe) || !file.exists(exe)) {
    return(NULL)
  }

  list(exe = exe, via = "plinkR")
}

get_plink_cli_backend <- function() {
  exe <- Sys.which("plink")
  exe <- if (is.character(exe) && length(exe) >= 1) exe[1] else ""
  if (!nzchar(exe) || !file.exists(exe)) {
    return(NULL)
  }
  list(exe = exe, via = "cli")
}

get_plink_blup_backend <- function() {
  get_plinkr_checked_backend() %||% get_plink_cli_backend()
}

has_plinkr_blup_backend <- function() {
  !is.null(get_plink_blup_backend())
}

# Built-in pure-R fallback backend (no compiled code required) for PED/MAP path.
has_r_blup_backend <- function() {
  TRUE
}

# BED/BIM/FAM R backend follows the snpStats path (Suggests).
has_r_blup_backend_bfile <- function() {
  # Deployment hint: keep gdata in rsconnect dependency snapshot (Suggests).
  invisible(requireNamespace("gdata", quietly = TRUE))
  requireNamespace("snpStats", quietly = TRUE)
}

# PLINK v1.9 (--recode A) and PLINK2 (--export A) can differ in which allele
# copy count is emitted in .raw/.export A output. We mirror the currently
# detected plinkR backend so the Rcpp fallback matches the preferred backend.
get_plinkr_blup_counted_allele <- function() {
  # Manual override for debugging/compatibility:
  # Sys.setenv(EASYBLUP_COUNTED_ALLELE = "A1") or "A2"
  forced <- toupper(trimws(Sys.getenv("EASYBLUP_COUNTED_ALLELE", "")))
  if (forced %in% c("A1", "A2")) {
    return(forced)
  }

  if (!requireNamespace("plinkR", quietly = TRUE)) {
    exe2 <- Sys.which("plink2")
    if (is.character(exe2) && length(exe2) >= 1 && nzchar(exe2[1])) {
      return("A2")
    }
    exe1 <- Sys.which("plink")
    if (is.character(exe1) && length(exe1) >= 1 && nzchar(exe1[1])) {
      return("A1")
    }
    return("A1")
  }
  chk <- tryCatch({
    ns <- asNamespace("plinkR")
    if (exists("check_plink", mode = "function", envir = ns, inherits = FALSE)) {
      plinkR::check_plink(verbose = FALSE)
    } else {
      NULL
    }
  }, error = function(e) NULL)
  exe <- if (is.list(chk) && "exe" %in% names(chk)) chk$exe else NULL
  exe_base <- if (is.character(exe) && length(exe) >= 1 && nzchar(exe[1])) basename(exe[1]) else ""
  if (nzchar(exe_base) && grepl("^plink2", tolower(exe_base))) {
    "A2"
  } else {
    "A1"
  }
}

# Determine PLINK prefix from uploaded files.
derive_plink_prefix <- function(file_names) {
  if (is.null(file_names) || length(file_names) == 0) return(NULL)
  norm_names <- basename(file_names)
  bed_idx <- grepl("\\.bed$", norm_names, ignore.case = TRUE)
  bim_idx <- grepl("\\.bim$", norm_names, ignore.case = TRUE)
  fam_idx <- grepl("\\.fam$", norm_names, ignore.case = TRUE)
  ped_idx <- grepl("\\.ped$", norm_names, ignore.case = TRUE)
  map_idx <- grepl("\\.map$", norm_names, ignore.case = TRUE)
  candidate <- if (any(bed_idx)) {
    norm_names[bed_idx][1]
  } else if (any(bim_idx)) {
    norm_names[bim_idx][1]
  } else if (any(fam_idx)) {
    norm_names[fam_idx][1]
  } else if (any(ped_idx)) {
    norm_names[ped_idx][1]
  } else if (any(map_idx)) {
    norm_names[map_idx][1]
  } else {
    norm_names[1]
  }
  tools::file_path_sans_ext(candidate)
}

# Optional compiled backend for PLINK(.ped/.map) -> BLUPF90 conversion.
# This mirrors PLINK-style additive coding (0/1/2 minor-allele copies; 5 for missing).
use_rcpp_blup_convert <- FALSE
eb_ped_to_blup_codes_cpp_fn <- NULL
rcpp_target_env <- environment()

resolve_src_root <- function() {
  wd <- getwd()
  roots <- unique(vapply(
    c(wd, file.path(wd, ".."), file.path(wd, "..", "..")),
    function(p) normalizePath(p, winslash = "/", mustWork = FALSE),
    character(1)
  ))
  for (root in roots) {
    if (file.exists(file.path(root, "src", "plink_blup_convert.cpp"))) {
      return(root)
    }
  }
  NULL
}

load_rcpp_blup_backend <- function() {
  fn <- tryCatch({
    if (requireNamespace("easybreedeR", quietly = TRUE)) {
      ns <- asNamespace("easybreedeR")
      if (exists("eb_ped_to_blup_codes_cpp", mode = "function", envir = ns, inherits = FALSE)) {
        return(get("eb_ped_to_blup_codes_cpp", envir = ns, inherits = FALSE))
      }
    }
    NULL
  }, error = function(e) NULL)
  if (is.function(fn)) {
    return(fn)
  }

  # Fallback for source deployments (e.g., shinyapps.io).
  if (!requireNamespace("Rcpp", quietly = TRUE)) {
    return(NULL)
  }
  src_root <- resolve_src_root()
  cpp_file <- if (!is.null(src_root)) file.path(src_root, "src", "plink_blup_convert.cpp") else ""
  if (!nzchar(cpp_file) || !file.exists(cpp_file)) {
    return(NULL)
  }
  ok <- isTRUE(tryCatch({
    Rcpp::sourceCpp(cpp_file, env = rcpp_target_env, rebuild = FALSE, showOutput = FALSE, verbose = FALSE)
    TRUE
  }, error = function(e) FALSE))
  if (!ok) {
    return(NULL)
  }
  if (exists("eb_ped_to_blup_codes_cpp", mode = "function", envir = rcpp_target_env, inherits = FALSE)) {
    return(get("eb_ped_to_blup_codes_cpp", envir = rcpp_target_env, inherits = FALSE))
  }
  NULL
}

eb_ped_to_blup_codes_cpp_fn <- load_rcpp_blup_backend()
use_rcpp_blup_convert <- is.function(eb_ped_to_blup_codes_cpp_fn)
if (use_rcpp_blup_convert) {
  eb_log("Rcpp backend available - PLINK-compatible fallback conversion enabled", level = "debug")
} else {
  eb_log("Rcpp backend unavailable; fallback converter is disabled", level = "warn")
}

convert_plink_to_blupf90_rcpp <- function(ped_path, map_path, out_prefix) {
  if (!is.function(eb_ped_to_blup_codes_cpp_fn)) {
    stop("Rcpp converter backend is not available. Reinstall easybreedeR with compiled code enabled.", call. = FALSE)
  }

  read_tab <- function(path) {
    if (requireNamespace("data.table", quietly = TRUE)) {
      # Preserve literal allele strings such as "NA" to match PLINK .ped parsing.
      data.table::fread(
        path,
        header = FALSE,
        data.table = FALSE,
        showProgress = FALSE,
        colClasses = "character",
        na.strings = NULL
      )
    } else {
      # Preserve literal allele strings such as "NA" to match PLINK .ped parsing.
      read.table(
        path,
        header = FALSE,
        stringsAsFactors = FALSE,
        colClasses = "character",
        fill = TRUE,
        comment.char = "",
        na.strings = character()
      )
    }
  }

  map_df <- read_tab(map_path)
  if (ncol(map_df) < 4 || nrow(map_df) == 0) {
    stop("Invalid .map file: need at least 4 columns and >=1 marker row.", call. = FALSE)
  }
  map_df <- map_df[, 1:4, drop = FALSE]
  colnames(map_df) <- c("CHR", "SNP", "CM", "BP")
  marker_n <- nrow(map_df)

  chr_numeric <- suppressWarnings(as.numeric(map_df$CHR))
  valid_chr <- chr_numeric[is.finite(chr_numeric) & chr_numeric > 0]
  max_chr <- if (length(valid_chr) > 0) max(valid_chr) else 40
  zero_chr_idx <- is.finite(chr_numeric) & chr_numeric == 0
  chr_for_map <- as.character(map_df$CHR)
  if (any(zero_chr_idx)) {
    chr_for_map[zero_chr_idx] <- as.character(max_chr)
  }

  ped_df <- read_tab(ped_path)
  if (ncol(ped_df) < 7 || nrow(ped_df) == 0) {
    stop("Invalid .ped file: need >=7 columns and >=1 sample row.", call. = FALSE)
  }

  expected_cols <- 6 + marker_n * 2
  if (ncol(ped_df) < expected_cols) {
    stop(sprintf("PED/MAP mismatch: ped has %d cols, expected at least %d for %d markers.", ncol(ped_df), expected_cols, marker_n), call. = FALSE)
  }

  ped_df <- ped_df[, seq_len(expected_cols), drop = FALSE]
  sample_id <- as.character(ped_df[[2]])
  sample_id[!nzchar(sample_id) | is.na(sample_id)] <- as.character(ped_df[[1]][!nzchar(sample_id) | is.na(sample_id)])

  allele1_idx <- seq(7, expected_cols - 1, by = 2)
  allele2_idx <- seq(8, expected_cols, by = 2)
  allele1 <- as.matrix(ped_df[, allele1_idx, drop = FALSE])
  allele2 <- as.matrix(ped_df[, allele2_idx, drop = FALSE])

  counted_allele <- get_plinkr_blup_counted_allele()
  cpp_formals <- tryCatch(names(formals(eb_ped_to_blup_codes_cpp_fn)), error = function(e) character())
  if ("counted_allele" %in% cpp_formals) {
    conv <- eb_ped_to_blup_codes_cpp_fn(allele1, allele2, counted_allele = counted_allele)
  } else {
    conv <- eb_ped_to_blup_codes_cpp_fn(allele1, allele2)
  }
  dosage <- conv$dosage
  a1 <- as.character(conv$a1)
  a2 <- as.character(conv$a2)

  # Backward-compatibility fallback for older 2-argument compiled backends.
  # Newer Rcpp backends accept counted_allele directly.
  if (!("counted_allele" %in% cpp_formals) && identical(counted_allele, "A2")) {
    non_missing <- dosage != 5L
    dosage[non_missing] <- 2L - dosage[non_missing]
  }

  out_txt <- paste0(out_prefix, ".txt")
  out_map <- paste0(out_prefix, ".map")
  out_bim <- paste0(out_prefix, ".bim")

  # Match plinkR::plink_to_blupf90(..., format="continuous"): one ID column plus a
  # contiguous 0/1/2/5 genotype string (no separators between markers).
  con <- file(out_txt, open = "wt")
  on.exit(close(con), add = TRUE)
  write_chunk <- function(idx) {
    lines <- vapply(idx, function(i) {
      paste0(sample_id[i], " ", paste0(as.integer(dosage[i, ]), collapse = ""))
    }, character(1))
    writeLines(lines, con = con, sep = "\n")
  }
  n_samples <- nrow(dosage)
  if (n_samples > 0) {
    chunk_size <- 1000L
    starts <- seq.int(1L, n_samples, by = chunk_size)
    for (s in starts) {
      e <- min(s + chunk_size - 1L, n_samples)
      write_chunk(seq.int(s, e))
    }
  }
  map_out_df <- data.frame(
    CHR = chr_for_map,
    SNP = map_df$SNP,
    CM = map_df$CM,
    BP = map_df$BP,
    stringsAsFactors = FALSE
  )
  write.table(map_out_df, file = out_map, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  bim_df <- data.frame(
    CHR = chr_for_map,
    SNP = map_df$SNP,
    CM = map_df$CM,
    BP = map_df$BP,
    A1 = a1,
    A2 = a2,
    stringsAsFactors = FALSE
  )
  write.table(bim_df, file = out_bim, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  list(
    txt = out_txt,
    map = out_map,
    bim = out_bim,
    max_chr = max_chr,
    counted_allele = counted_allele
  )
}

eb_ped_to_blup_codes_r <- function(allele1, allele2, counted_allele = "A1") {
  if (!is.matrix(allele1) || !is.matrix(allele2)) {
    stop("allele1 and allele2 must be matrices", call. = FALSE)
  }
  n_samples <- nrow(allele1)
  n_markers <- ncol(allele1)
  if (nrow(allele2) != n_samples || ncol(allele2) != n_markers) {
    stop("allele1 and allele2 must have the same dimensions", call. = FALSE)
  }

  counted_allele <- toupper(trimws(as.character(counted_allele %||% "A1")))
  if (!counted_allele %in% c("A1", "A2")) {
    stop("counted_allele must be 'A1' or 'A2'", call. = FALSE)
  }
  count_a2 <- identical(counted_allele, "A2")

  dot_sentinel <- "__PLINK_PED_DOT_ALLELE__"

  normalize_vec <- function(x) {
    s <- as.character(x)
    s[is.na(s)] <- ""
    s <- toupper(trimws(s))
    s[s == "."] <- dot_sentinel
    s
  }
  is_missing_ped_default <- function(x) {
    !nzchar(x) | x == "0"
  }
  to_output_allele <- function(x) {
    y <- as.character(x)
    y[y == dot_sentinel] <- "0"
    y
  }

  dosage <- matrix(5L, nrow = n_samples, ncol = n_markers)
  a1_out <- character(n_markers)
  a2_out <- character(n_markers)

  for (j in seq_len(n_markers)) {
    xj <- normalize_vec(allele1[, j])
    yj <- normalize_vec(allele2[, j])

    allele_count <- integer(0)
    first_seen <- integer(0)
    seen_rank <- 0L

    for (i in seq_len(n_samples)) {
      x <- xj[i]
      y <- yj[i]
      if (!is_missing_ped_default(x)) {
        if (is.na(first_seen[x])) {
          first_seen[x] <- seen_rank
          seen_rank <- seen_rank + 1L
        }
        allele_count[x] <- (if (is.na(allele_count[x])) 0L else allele_count[x]) + 1L
      }
      if (!is_missing_ped_default(y)) {
        if (is.na(first_seen[y])) {
          first_seen[y] <- seen_rank
          seen_rank <- seen_rank + 1L
        }
        allele_count[y] <- (if (is.na(allele_count[y])) 0L else allele_count[y]) + 1L
      }
    }

    if (length(allele_count) == 0) {
      a1 <- "0"
      a2 <- "0"
    } else if (length(allele_count) == 1) {
      a1 <- "0"
      a2 <- names(allele_count)[1]
    } else {
      allele_names <- names(allele_count)
      ord <- order(-unname(allele_count[allele_names]), unname(first_seen[allele_names]))
      keep1 <- allele_names[ord[1]]  # top after stable sort on raw counts
      keep2 <- allele_names[ord[2]]  # second after stable sort on raw counts

      keep1_eff <- 0L
      keep2_eff <- 0L
      for (i in seq_len(n_samples)) {
        x <- xj[i]
        y <- yj[i]
        if (is_missing_ped_default(x) || is_missing_ped_default(y)) next
        if (!(x %in% c(keep1, keep2)) || !(y %in% c(keep1, keep2))) next
        keep1_eff <- keep1_eff + as.integer(x == keep1) + as.integer(y == keep1)
        keep2_eff <- keep2_eff + as.integer(x == keep2) + as.integer(y == keep2)
      }
      if (keep1_eff == 0L && keep2_eff == 0L) {
        keep1_eff <- as.integer(allele_count[[keep1]])
        keep2_eff <- as.integer(allele_count[[keep2]])
      }

      if (keep1_eff == keep2_eff) {
        # PLINK-compatible tie fallback after multiallelic pruning: keep preselected order.
        a1 <- keep2
        a2 <- keep1
      } else if (keep1_eff < keep2_eff) {
        a1 <- keep1
        a2 <- keep2
      } else {
        a1 <- keep2
        a2 <- keep1
      }
    }

    a1_out[j] <- to_output_allele(a1)
    a2_out[j] <- to_output_allele(a2)

    for (i in seq_len(n_samples)) {
      x <- xj[i]
      y <- yj[i]
      if (is_missing_ped_default(x) || is_missing_ped_default(y)) {
        dosage[i, j] <- 5L
        next
      }
      if (identical(a1, "0")) {
        dosage[i, j] <- if (identical(x, a2) && identical(y, a2)) 0L else 5L
        next
      }
      if (!(x %in% c(a1, a2)) || !(y %in% c(a1, a2))) {
        dosage[i, j] <- 5L
        next
      }
      d <- as.integer(x == a1) + as.integer(y == a1)
      if (count_a2) d <- 2L - d
      dosage[i, j] <- d
    }

    if (count_a2 && identical(a1, "0")) {
      idx <- dosage[, j] != 5L
      dosage[idx, j] <- 2L - dosage[idx, j]
    }
  }

  list(
    dosage = dosage,
    a1 = a1_out,
    a2 = a2_out,
    counted_allele = counted_allele
  )
}

convert_plink_to_blupf90_r <- function(ped_path, map_path, out_prefix) {
  read_tab <- function(path) {
    if (requireNamespace("data.table", quietly = TRUE)) {
      data.table::fread(
        path,
        header = FALSE,
        data.table = FALSE,
        showProgress = FALSE,
        colClasses = "character",
        na.strings = NULL
      )
    } else {
      read.table(
        path,
        header = FALSE,
        stringsAsFactors = FALSE,
        colClasses = "character",
        fill = TRUE,
        comment.char = "",
        na.strings = character()
      )
    }
  }

  map_df <- read_tab(map_path)
  if (ncol(map_df) < 4 || nrow(map_df) == 0) {
    stop("Invalid .map file: need at least 4 columns and >=1 marker row.", call. = FALSE)
  }
  map_df <- map_df[, 1:4, drop = FALSE]
  colnames(map_df) <- c("CHR", "SNP", "CM", "BP")
  marker_n <- nrow(map_df)

  chr_numeric <- suppressWarnings(as.numeric(map_df$CHR))
  valid_chr <- chr_numeric[is.finite(chr_numeric) & chr_numeric > 0]
  max_chr <- if (length(valid_chr) > 0) max(valid_chr) else 40
  zero_chr_idx <- is.finite(chr_numeric) & chr_numeric == 0
  chr_for_map <- as.character(map_df$CHR)
  if (any(zero_chr_idx)) {
    chr_for_map[zero_chr_idx] <- as.character(max_chr)
  }

  ped_df <- read_tab(ped_path)
  if (ncol(ped_df) < 7 || nrow(ped_df) == 0) {
    stop("Invalid .ped file: need >=7 columns and >=1 sample row.", call. = FALSE)
  }

  expected_cols <- 6 + marker_n * 2
  if (ncol(ped_df) < expected_cols) {
    stop(sprintf("PED/MAP mismatch: ped has %d cols, expected at least %d for %d markers.", ncol(ped_df), expected_cols, marker_n), call. = FALSE)
  }

  ped_df <- ped_df[, seq_len(expected_cols), drop = FALSE]
  sample_id <- as.character(ped_df[[2]])
  sample_id[!nzchar(sample_id) | is.na(sample_id)] <- as.character(ped_df[[1]][!nzchar(sample_id) | is.na(sample_id)])

  allele1_idx <- seq(7, expected_cols - 1, by = 2)
  allele2_idx <- seq(8, expected_cols, by = 2)
  allele1 <- as.matrix(ped_df[, allele1_idx, drop = FALSE])
  allele2 <- as.matrix(ped_df[, allele2_idx, drop = FALSE])

  counted_allele <- get_plinkr_blup_counted_allele()
  conv <- eb_ped_to_blup_codes_r(allele1, allele2, counted_allele = counted_allele)
  dosage <- conv$dosage
  a1 <- as.character(conv$a1)
  a2 <- as.character(conv$a2)

  out_txt <- paste0(out_prefix, ".txt")
  out_map <- paste0(out_prefix, ".map")
  out_bim <- paste0(out_prefix, ".bim")

  con <- file(out_txt, open = "wt")
  on.exit(close(con), add = TRUE)
  n_samples <- nrow(dosage)
  if (n_samples > 0) {
    chunk_size <- 1000L
    starts <- seq.int(1L, n_samples, by = chunk_size)
    for (s in starts) {
      e <- min(s + chunk_size - 1L, n_samples)
      lines <- vapply(seq.int(s, e), function(i) {
        paste0(sample_id[i], " ", paste0(as.integer(dosage[i, ]), collapse = ""))
      }, character(1))
      writeLines(lines, con = con, sep = "\n")
    }
  }

  map_out_df <- data.frame(
    CHR = chr_for_map,
    SNP = map_df$SNP,
    CM = map_df$CM,
    BP = map_df$BP,
    stringsAsFactors = FALSE
  )
  write.table(map_out_df, file = out_map, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  bim_df <- data.frame(
    CHR = chr_for_map,
    SNP = map_df$SNP,
    CM = map_df$CM,
    BP = map_df$BP,
    A1 = a1,
    A2 = a2,
    stringsAsFactors = FALSE
  )
  write.table(bim_df, file = out_bim, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  list(
    txt = out_txt,
    map = out_map,
    bim = out_bim,
    max_chr = max_chr,
    counted_allele = counted_allele
  )
}

convert_plink_to_blupf90_r_bfile <- function(bed_path, bim_path, fam_path, out_prefix) {
  if (!requireNamespace("snpStats", quietly = TRUE)) {
    stop("R BED/BIM/FAM backend requires the 'snpStats' package (Suggests).", call. = FALSE)
  }

  data <- snpStats::read.plink(
    bed_path,
    bim_path,
    fam_path,
    na.strings = c("0", "-9"),
    sep = ".",
    select.subjects = NULL,
    select.snps = NULL
  )

  if (is.null(data$genotypes) || nrow(data$genotypes) == 0 || ncol(data$genotypes) == 0) {
    stop("snpStats::read.plink returned an empty genotype matrix.", call. = FALSE)
  }
  if (is.null(data$map) || nrow(data$map) == 0 || ncol(data$map) < 4) {
    stop("snpStats::read.plink returned an invalid map table.", call. = FALSE)
  }

  # Match plinkR continuous output style: one sample ID column + compact genotype string.
  sample_ids <- rownames(data$genotypes)
  if (is.null(sample_ids)) {
    sample_ids <- as.character(seq_len(nrow(data$genotypes)))
  }
  sample_ids <- as.character(sample_ids)
  sample_ids[!nzchar(sample_ids) | is.na(sample_ids)] <- as.character(which(!nzchar(sample_ids) | is.na(sample_ids)))
  rownames(data$genotypes) <- sample_ids

  out_txt <- paste0(out_prefix, ".txt")
  out_map <- paste0(out_prefix, ".map")
  out_bim <- paste0(out_prefix, ".bim")

  dosage_num <- as(data$genotypes, "numeric")
  counted_allele <- get_plinkr_blup_counted_allele()
  if (identical(counted_allele, "A1")) {
    dosage_num <- 2 - dosage_num
  }
  dosage <- matrix(5L, nrow = nrow(dosage_num), ncol = ncol(dosage_num))
  non_missing <- !is.na(dosage_num)
  dosage[non_missing] <- as.integer(dosage_num[non_missing])

  con <- file(out_txt, open = "wt")
  on.exit(close(con), add = TRUE)
  if (nrow(dosage) > 0) {
    chunk_size <- 1000L
    starts <- seq.int(1L, nrow(dosage), by = chunk_size)
    for (s in starts) {
      e <- min(s + chunk_size - 1L, nrow(dosage))
      lines <- vapply(seq.int(s, e), function(i) {
        paste0(sample_ids[i], " ", paste0(as.character(dosage[i, ]), collapse = ""))
      }, character(1))
      writeLines(lines, con = con, sep = "\n")
    }
  }

  bim_df <- read.table(
    bim_path,
    header = FALSE,
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = character()
  )
  if (ncol(bim_df) < 4 || nrow(bim_df) == 0) {
    stop("Input .bim file is malformed.", call. = FALSE)
  }
  map_df <- bim_df[, 1:4, drop = FALSE]
  colnames(map_df) <- c("CHR", "SNP", "CM", "BP")

  chr_numeric <- suppressWarnings(as.numeric(map_df$CHR))
  valid_chr <- chr_numeric[is.finite(chr_numeric) & chr_numeric > 0]
  max_chr <- if (length(valid_chr) > 0) max(valid_chr) else 40
  zero_chr_idx <- is.finite(chr_numeric) & chr_numeric == 0
  chr_for_map <- as.character(map_df$CHR)
  if (any(zero_chr_idx)) {
    chr_for_map[zero_chr_idx] <- as.character(max_chr)
  }

  map_out_df <- data.frame(
    CHR = chr_for_map,
    SNP = map_df$SNP,
    CM = map_df$CM,
    BP = map_df$BP,
    stringsAsFactors = FALSE
  )
  write.table(map_out_df, file = out_map, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  # Preserve BED/BIM/FAM allele metadata exactly to match plinkR baseline path.
  same_bim_path <- identical(
    normalizePath(bim_path, winslash = "/", mustWork = FALSE),
    normalizePath(out_bim, winslash = "/", mustWork = FALSE)
  )
  if (!same_bim_path) {
    file.copy(bim_path, out_bim, overwrite = TRUE)
  }

  list(
    txt = out_txt,
    map = out_map,
    bim = if (file.exists(out_bim)) out_bim else NULL,
    max_chr = max_chr,
    counted_allele = counted_allele,
    via = "R (snpStats)"
  )
}

convert_plink_to_blupf90_plink_cli_bfile <- function(bed_path, bim_path, fam_path, out_prefix, plink_exe = NULL) {
  plink_exe <- plink_exe %||% {
    cli <- get_plink_cli_backend()
    if (!is.null(cli)) cli$exe else ""
  }
  if (!is.character(plink_exe) || length(plink_exe) == 0 || !nzchar(plink_exe) || !file.exists(plink_exe)) {
    stop("PLINK executable not found for direct CLI conversion.", call. = FALSE)
  }

  prefix <- out_prefix
  required_files <- c(paste0(prefix, ".bed"), paste0(prefix, ".bim"), paste0(prefix, ".fam"))
  if (!all(file.exists(required_files))) {
    stop("PLINK CLI conversion expects <prefix>.bed/.bim/.fam to exist.", call. = FALSE)
  }

  run_plink <- function(args) {
    out <- tryCatch(
      suppressWarnings(eb_system2(plink_exe, args = args, stdout = TRUE, stderr = TRUE)),
      error = function(e) e
    )
    status <- attr(out, "status")
    if (inherits(out, "error") || (!is.null(status) && status != 0)) {
      msg <- if (inherits(out, "error")) out$message else paste(out, collapse = "\n")
      stop(paste0("PLINK CLI failed: ", msg), call. = FALSE)
    }
    invisible(TRUE)
  }

  bim_meta <- read.table(
    paste0(prefix, ".bim"),
    header = FALSE,
    stringsAsFactors = FALSE,
    colClasses = "character",
    na.strings = character()
  )
  if (ncol(bim_meta) < 4 || nrow(bim_meta) == 0) {
    stop("Input .bim file is malformed.", call. = FALSE)
  }
  chr_numeric <- suppressWarnings(as.numeric(bim_meta[[1]]))
  valid_chr <- chr_numeric[is.finite(chr_numeric) & chr_numeric > 0]
  max_chr <- if (length(valid_chr) > 0) max(valid_chr) else 40

  # Match plinkR path: include chromosome/sex/nonfounder flags for broad compatibility.
  run_plink(c(
    "--bfile", prefix,
    "--allow-extra-chr",
    "--chr-set", as.character(max_chr),
    "--allow-no-sex",
    "--nonfounders",
    "--recode", "A",
    "--out", prefix
  ))

  out_raw <- paste0(prefix, ".raw")
  out_bim <- paste0(prefix, ".bim")
  if (!file.exists(out_raw) || !file.exists(out_bim)) {
    stop("PLINK CLI conversion did not produce expected .raw/.bim outputs.", call. = FALSE)
  }

  raw_df <- read.table(out_raw, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(raw_df) < 7 || nrow(raw_df) == 0) {
    stop("PLINK .raw output is empty or malformed.", call. = FALSE)
  }
  bim_df <- read.table(out_bim, header = FALSE, stringsAsFactors = FALSE, colClasses = "character", na.strings = character())
  if (ncol(bim_df) < 6) {
    stop("PLINK .bim output is malformed.", call. = FALSE)
  }

  sample_id <- as.character(raw_df$IID %||% raw_df[[2]])
  if ("FID" %in% names(raw_df)) {
    fid <- as.character(raw_df$FID)
    sample_id[!nzchar(sample_id) | is.na(sample_id)] <- fid[!nzchar(sample_id) | is.na(sample_id)]
  }

  dosage <- as.matrix(raw_df[, -(1:6), drop = FALSE])
  storage.mode(dosage) <- "integer"

  counted_allele <- if (grepl("^plink2", tolower(basename(plink_exe)))) "A2" else "A1"
  if (identical(counted_allele, "A2")) {
    non_missing <- !is.na(dosage)
    dosage[non_missing] <- 2L - dosage[non_missing]
  }

  out_txt <- paste0(prefix, ".txt")
  out_map <- paste0(prefix, ".map")

  con <- file(out_txt, open = "wt")
  on.exit(close(con), add = TRUE)
  if (nrow(dosage) > 0) {
    chunk_size <- 1000L
    starts <- seq.int(1L, nrow(dosage), by = chunk_size)
    for (s in starts) {
      e <- min(s + chunk_size - 1L, nrow(dosage))
      lines <- vapply(seq.int(s, e), function(i) {
        codes <- dosage[i, ]
        codes_chr <- ifelse(is.na(codes), "5", as.character(codes))
        paste0(sample_id[i], " ", paste0(codes_chr, collapse = ""))
      }, character(1))
      writeLines(lines, con = con, sep = "\n")
    }
  }

  map_out <- bim_df[, 1:4, drop = FALSE]
  chr_map_numeric <- suppressWarnings(as.numeric(map_out[[1]]))
  valid_chr_map <- chr_map_numeric[is.finite(chr_map_numeric) & chr_map_numeric > 0]
  max_chr_map <- if (length(valid_chr_map) > 0) max(valid_chr_map) else 40
  zero_chr_idx <- is.finite(chr_map_numeric) & chr_map_numeric == 0
  if (any(zero_chr_idx)) {
    map_out[zero_chr_idx, 1] <- as.character(max_chr_map)
  }
  write.table(map_out, file = out_map, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  list(
    txt = out_txt,
    map = out_map,
    bim = out_bim,
    counted_allele = counted_allele,
    plink_exe = plink_exe
  )
}

convert_plink_to_blupf90_plink_backend_bfile <- function(bed_path, bim_path, fam_path, out_prefix) {
  backend <- get_plink_blup_backend()
  if (is.null(backend)) {
    stop("No PLINK backend available.", call. = FALSE)
  }

  if (identical(backend$via, "plinkR") && requireNamespace("plinkR", quietly = TRUE)) {
    plinkr_out_prefix <- paste0(out_prefix, "__plinkr_out")
    plinkR::plink_to_blupf90(prefix = out_prefix, out_prefix = plinkr_out_prefix, verbose = TRUE)
    out_txt <- paste0(out_prefix, ".txt")
    out_map <- paste0(out_prefix, ".map")
    out_bim <- paste0(out_prefix, ".bim")
    file.copy(paste0(plinkr_out_prefix, ".txt"), out_txt, overwrite = TRUE)
    file.copy(paste0(plinkr_out_prefix, ".map"), out_map, overwrite = TRUE)
    if (file.exists(paste0(plinkr_out_prefix, ".bim"))) {
      file.copy(paste0(plinkr_out_prefix, ".bim"), out_bim, overwrite = TRUE)
    }
    for (ext in c(".txt", ".map", ".bim")) {
      f <- paste0(plinkr_out_prefix, ext)
      if (file.exists(f)) unlink(f, force = TRUE)
    }
    if (!file.exists(out_txt) || !file.exists(out_map)) {
      stop("PLINK (via plinkR) output files not found.", call. = FALSE)
    }
    return(list(txt = out_txt, map = out_map, bim = if (file.exists(out_bim)) out_bim else NULL, via = "plinkR", plink_exe = backend$exe))
  }

  cli_res <- convert_plink_to_blupf90_plink_cli_bfile(
    bed_path = bed_path,
    bim_path = bim_path,
    fam_path = fam_path,
    out_prefix = out_prefix,
    plink_exe = backend$exe
  )
  cli_res$via <- "cli"
  cli_res
}

convert_plink_to_blupf90_plink_cli <- function(ped_path, map_path, out_prefix, plink_exe = NULL) {
  plink_exe <- plink_exe %||% {
    cli <- get_plink_cli_backend()
    if (!is.null(cli)) cli$exe else ""
  }
  if (!is.character(plink_exe) || length(plink_exe) == 0 || !nzchar(plink_exe) || !file.exists(plink_exe)) {
    stop("PLINK executable not found for direct CLI conversion.", call. = FALSE)
  }

  prefix <- out_prefix
  if (!file.exists(paste0(prefix, ".ped")) || !file.exists(paste0(prefix, ".map"))) {
    stop("PLINK CLI conversion expects <prefix>.ped and <prefix>.map to exist.", call. = FALSE)
  }

  run_plink <- function(args) {
    out <- tryCatch(
      suppressWarnings(eb_system2(plink_exe, args = args, stdout = TRUE, stderr = TRUE)),
      error = function(e) e
    )
    status <- attr(out, "status")
    if (inherits(out, "error") || (!is.null(status) && status != 0)) {
      msg <- if (inherits(out, "error")) out$message else paste(out, collapse = "\n")
      stop(paste0("PLINK CLI failed: ", msg), call. = FALSE)
    }
    invisible(TRUE)
  }

  # Build BIM/FAM and recode additive counts.
  run_plink(c("--file", prefix, "--make-bed", "--out", prefix))
  run_plink(c("--file", prefix, "--recode", "A", "--out", prefix))

  out_raw <- paste0(prefix, ".raw")
  out_bim <- paste0(prefix, ".bim")
  if (!file.exists(out_raw) || !file.exists(out_bim)) {
    stop("PLINK CLI conversion did not produce expected .raw/.bim outputs.", call. = FALSE)
  }

  raw_df <- read.table(out_raw, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)
  if (ncol(raw_df) < 7 || nrow(raw_df) == 0) {
    stop("PLINK .raw output is empty or malformed.", call. = FALSE)
  }
  bim_df <- read.table(out_bim, header = FALSE, stringsAsFactors = FALSE, colClasses = "character", na.strings = character())
  if (ncol(bim_df) < 6) {
    stop("PLINK .bim output is malformed.", call. = FALSE)
  }

  sample_id <- as.character(raw_df$IID %||% raw_df[[2]])
  if ("FID" %in% names(raw_df)) {
    fid <- as.character(raw_df$FID)
    sample_id[!nzchar(sample_id) | is.na(sample_id)] <- fid[!nzchar(sample_id) | is.na(sample_id)]
  }

  dosage <- as.matrix(raw_df[, -(1:6), drop = FALSE])
  storage.mode(dosage) <- "integer"

  counted_allele <- get_plinkr_blup_counted_allele()
  if (identical(counted_allele, "A2")) {
    non_missing <- !is.na(dosage)
    dosage[non_missing] <- 2L - dosage[non_missing]
  }

  out_txt <- paste0(prefix, ".txt")
  out_map <- paste0(prefix, ".map")

  con <- file(out_txt, open = "wt")
  on.exit(close(con), add = TRUE)
  if (nrow(dosage) > 0) {
    chunk_size <- 1000L
    starts <- seq.int(1L, nrow(dosage), by = chunk_size)
    for (s in starts) {
      e <- min(s + chunk_size - 1L, nrow(dosage))
      lines <- vapply(seq.int(s, e), function(i) {
        codes <- dosage[i, ]
        codes_chr <- ifelse(is.na(codes), "5", as.character(codes))
        paste0(sample_id[i], " ", paste0(codes_chr, collapse = ""))
      }, character(1))
      writeLines(lines, con = con, sep = "\n")
    }
  }

  map_out <- bim_df[, 1:4, drop = FALSE]
  write.table(map_out, file = out_map, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = "\t")

  list(
    txt = out_txt,
    map = out_map,
    bim = out_bim,
    counted_allele = counted_allele,
    plink_exe = plink_exe
  )
}

convert_plink_to_blupf90_plink_backend <- function(ped_path, map_path, out_prefix) {
  backend <- get_plink_blup_backend()
  if (is.null(backend)) {
    stop("No PLINK backend available.", call. = FALSE)
  }

  if (identical(backend$via, "plinkR") && requireNamespace("plinkR", quietly = TRUE)) {
    plinkR::plink_to_blupf90(prefix = out_prefix, out_prefix = out_prefix, verbose = TRUE)
    out_txt <- paste0(out_prefix, ".txt")
    out_map <- paste0(out_prefix, ".map")
    out_bim <- paste0(out_prefix, ".bim")
    if (!file.exists(out_txt) || !file.exists(out_map)) {
      stop("PLINK (via plinkR) output files not found.", call. = FALSE)
    }
    return(list(txt = out_txt, map = out_map, bim = if (file.exists(out_bim)) out_bim else NULL, via = "plinkR", plink_exe = backend$exe))
  }

  cli_res <- convert_plink_to_blupf90_plink_cli(
    ped_path = ped_path,
    map_path = map_path,
    out_prefix = out_prefix,
    plink_exe = backend$exe
  )
  cli_res$via <- "cli"
  cli_res
}

# Hover / focus the text itself to see its explanation (dotted underline marks it)
with_tip <- function(label, ..., placement = "auto") {
  bslib::tooltip(
    tags$span(class = "tip-text", tabindex = "0", label),
    ..., placement = placement
  )
}

# Official BLUPF90 wiki (UGA NCE) — program readmes + FAQ pages
KIT_WIKI_BASE <- "https://nce.ads.uga.edu/wiki/doku.php?id="
kit_wiki <- function(page, section = NULL) {
  url <- paste0(KIT_WIKI_BASE, page)
  if (!is.null(section) && nzchar(section)) url <- paste0(url, "#", section)
  url
}

# Per-control docs: map help-id → official wiki URL (section anchors when stable)
KIT_OPTION_DOCS <- c(
  # 1 · Data & missing (renumf90 / blupf90+)
  "opt_remove_all_missing" = kit_wiki("readme.renumf90", "options"),
  "opt_missing_in_weights" = kit_wiki("readme.renumf90", "options"),
  "opt_no_basic_statistics" = kit_wiki("readme.renumf90", "options"),
  "opt_missing_value" = kit_wiki("readme.renumf90", "options"),
  "opt_origID" = kit_wiki("readme.blupf90plus", "storing_solutions_with_original_id"),
  # 2 · Solver
  "opt_method" = kit_wiki("readme.blupf90plus", "options_for_reml_in_blupf90_with_option_method_vce"),
  "opt_sol_se" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "opt_use_yams" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "opt_residual_out" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "opt_stdresidual_out" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "opt_prior_solutions" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "opt_conv_crit_val" = kit_wiki("readme.blupf90plus", "options_for_reml_in_blupf90_with_option_method_vce"),
  "opt_em_reml_rounds" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  "opt_em_reml_pure" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  "opt_em_reml_ai_conv" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  "opt_maxrounds_val" = kit_wiki("readme.blupf90plus", "options_for_reml_in_blupf90_with_option_method_vce"),
  "opt_auto_se_covar" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  "blksize" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "samples_se_covar_function" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  "out_se_covar_function" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  # 3 · Accuracy
  "store_accuracy" = kit_wiki("readme.blupf90plus", "storing_reliability_based_on_pev"),
  "opt_acctype" = kit_wiki("readme.blupf90plus", "storing_reliability_based_on_pev"),
  "opt_correct_acc_inb_direct0" = kit_wiki("readme.blupf90plus", "storing_reliability_based_on_pev"),
  # 4 · Pedigree / UPG
  "opt_omit_pedigree" = kit_wiki("how_to_run_pure_gblup"),
  "opt_ped_search_complete" = kit_wiki("readme.renumf90", "options"),
  "opt_use_ped_depth" = kit_wiki("readme.renumf90", "fields_in_the_parameter_file"),
  "opt_ped_depth" = kit_wiki("readme.renumf90", "fields_in_the_parameter_file"),
  "opt_upg_type" = kit_wiki("how_to_consider_upg_in_ssgblup"),
  "inbreeding_method" = kit_wiki("readme.renumf90", "options"),
  "opt_inbreeding_method" = kit_wiki("readme.renumf90", "options"),
  # 5 · Genomic
  "opt_save_clean_snps" = kit_wiki("readme.pregsf90", "quality_control_qc_for_g"),
  "opt_snp_p_value" = kit_wiki("readme.blupf90plus", "options_for_blupf90_with_option_method_blup_blup_default"),
  "opt_omit_ainv" = kit_wiki("readme.blupf90plus", "running_gblup_in_blupf90_omit_a-inverse"),
  "opt_map_file" = kit_wiki("readme.pregsf90", "input_files"),
  "opt_AlphaBeta" = kit_wiki("readme.pregsf90", "options_for_h"),
  "opt_TauOmega" = kit_wiki("readme.pregsf90", "options_for_h"),
  "opt_tunedG" = kit_wiki("readme.pregsf90", "options_for_h"),
  "opt_tunedG_a" = kit_wiki("readme.pregsf90", "options_for_h"),
  "opt_tunedG_b" = kit_wiki("readme.pregsf90", "options_for_h"),
  "opt_read_gim_a22i" = kit_wiki("readme.pregsf90", "save_and_read_intermediate_files"),
  # 6 · Heterogeneous residual
  "opt_hetres_pos" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  "opt_hetres_pol_preset" = kit_wiki("readme.blupf90plus", "options_for_em-reml"),
  # 7 · Random regression
  "opt_use_rr" = kit_wiki("readme.renumf90", "fields_in_the_parameter_file"),
  "opt_rr_type" = kit_wiki("readme.renumf90", "fields_in_the_parameter_file"),
  "opt_rr_order" = kit_wiki("readme.renumf90", "fields_in_the_parameter_file"),
  "opt_rr_position" = kit_wiki("readme.renumf90", "fields_in_the_parameter_file"),
  # 8 · Gibbs
  "opt_gibbs_enable" = kit_wiki("readme.gibbsf90plus"),
  "opt_gibbs_cat" = kit_wiki("readme.gibbsf90plus"),
  "opt_gibbs_thresholds" = kit_wiki("readme.gibbsf90plus"),
  "opt_gibbs_seed" = kit_wiki("readme.gibbsf90plus"),
  "opt_gibbs_solution_mean" = kit_wiki("faq.gibbs"),
  "opt_gibbs_fixed_var" = kit_wiki("faq.gibbs")
)

kit_doc_url <- function(id) {
  id <- as.character(id %||% "")
  if (!nzchar(id) || !(id %in% names(KIT_OPTION_DOCS))) return("")
  unname(KIT_OPTION_DOCS[[id]])
}

kit_doc_anchor <- function(url) {
  if (!nzchar(url %||% "")) return(NULL)
  tags$a(
    class = "opt-doc-link",
    href = url,
    target = "_blank",
    rel = "noopener noreferrer",
    title = "Open official BLUPF90 wiki page",
    "docs \u2197"
  )
}

# Help content audited against blupf90 skill references (renumf90, REML, genomic, Gibbs).
# Keep documentation separate from generation logic; advanced limitations are explicit.
KIT_FIELD_HELP <- list(
  "opt_remove_all_missing" = c(en = "Drop records with all selected traits missing; partly observed multi-trait records remain usable."),
  "opt_missing_in_weights" = c(en = "A zero weight makes the corresponding observation missing. A WEIGHT(S) column must be configured; this switch does not select one."),
  "opt_no_basic_statistics" = c(en = "Skip basic data statistics to save time on large files. Leave off during initial data checks."),
  "opt_missing_value" = c(en = "Integer code for missing observations, e.g. -999 when zero is a real value. Recode input missing values consistently. Covariate zero is a value; unknown parents stay 0."),
  "opt_origID" = c(en = "Request solutions.original with original IDs; ordinary solutions uses renumbered levels."),
  "opt_method" = c(en = "BLUP uses supplied variances as known values; VCE estimates them by REML. This does not launch the executable."),
  "opt_sol_se" = c(en = "sol se requests solution standard errors and can require sparse inversion and more memory."),
  "opt_use_yams" = c(en = "Use the YAMS sparse solver in supporting builds; speed and memory depend on the model and executable."),
  "opt_residual_out" = c(en = "Write fitted values and residuals to yhat_residual for model diagnostics."),
  "opt_stdresidual_out" = c(en = "Write fitted values and studentized residuals to yhat_student_residual."),
  "opt_prior_solutions" = c(en = "Start from existing solutions only when effect order, coding and model match the current run."),
  "opt_conv_crit_val" = c(en = "REML convergence tolerance; a smaller value is stricter. Verify variance stability and the final log, not just the stopping flag."),
  "opt_em_reml_rounds" = c(en = "EM-REML n performs n EM rounds before AI. pure uses only EM; AI conv runs EM to convergence then AI to convergence. Pure EM does not provide an AI matrix for SE."),
  "opt_em_reml_pure" = c(en = "Pure EM can be robust but slow; no AI-based variance-component SE. Overrides the initial EM round count."),
  "opt_em_reml_ai_conv" = c(en = "Run EM to convergence, then AI to convergence. This differs from a fixed number of initial EM rounds."),
  "opt_maxrounds_val" = c(en = "Maximum iterations. Reaching the limit is not evidence of convergence."),
  "opt_auto_se_covar" = c(en = "Generate functions of variance components for h2/correlations. Check effect indices and denominators, especially maternal, PE and other random effects."),
  "opt_acctype" = c(en = "1.0 requests reliability; 0.5 uses the BIF accuracy convention. Do not label the latter simply as the square root of reliability."),
  "opt_correct_acc_inb_direct0" = c(en = "Value 0 disables the direct inbreeding correction for reliability. It does not remove inbreeding from the relationship matrix."),
  "opt_omit_pedigree" = c(en = "GBLUP with a dummy pedigree requires all phenotyped animals to be genotyped. Omitting FILE is not by itself a complete genomic model."),
  "opt_ped_search_complete" = c(en = "Complete ancestor tracing; check the retained pedigree counts in the renumf90 log."),
  "opt_use_ped_depth" = c(en = "Emit PED_DEPTH explicitly. Without it the executable default applies."),
  "opt_ped_depth" = c(en = "0 loads the whole pedigree, including unrelated animals; positive values limit ancestral generations from recorded/genotyped animals."),
  "opt_save_clean_snps" = c(en = "Save QC-cleaned SNPs and removal reports. Inspect both animal and marker exclusions before reusing cleaned files."),
  "opt_snp_p_value" = c(en = "Store information needed for SNP p-values in postGSf90; this is not a GWAS result by itself and can use substantial memory."),
  "opt_omit_ainv" = c(en = "Omit A inverse. Use only in a fully specified GBLUP strategy; combining it blindly with a dummy-pedigree setup changes the intended equations."),
  "opt_map_file" = c(en = "Marker map path at execution time. Marker order must match genotype columns; inspect the format required by your build."),
  "opt_AlphaBeta" = c(en = "Two blending coefficients: G <- alpha G + beta A22. Example 0.95 0.05 is a starting choice, not an estimated result."),
  "opt_TauOmega" = c(en = "Scale the genomic correction tau G inverse - omega A22 inverse. Leave unset for standard ssGBLUP unless a justified strategy requires otherwise."),
  "opt_tunedG" = c(en = "Scale G vs A22. 0 none; 1 diag=1/off=0 (genotyped pop.); 2 match A22 means (default); 3 mean(G)=mean(A22); 4 Fst rescale; 9 a+bG with custom a b. Dummy-pedigree GBLUP usually uses 0."),
  "opt_tunedG_a" = c(en = "Constant a in G ← a + bG when tunedG = 9."),
  "opt_tunedG_b" = c(en = "Scale b in G ← a + bG when tunedG = 9."),
  "opt_read_gim_a22i" = c(en = "Reuse a precomputed genomic correction matrix. IDs, order, QC, pedigree and scaling must match; this option does not create the matrix."),
  "opt_hetres_pos" = c(en = "Columns for log residual variance: variance = exp(Xb). Supported by AI-REML, not EM-REML. Verify positions against the application data after renumbering."),
  "opt_hetres_pol_preset" = c(en = "Starting coefficients on the log-variance scale, not automatic polynomial expansion. Supply matching covariate/basis columns; presets alone do not create quadratic terms."),
  "opt_use_rr" = c(en = "Advanced draft only: verify keyword order, RR_POSITION and expanded genetic/PE covariance dimensions manually before running."),
  "opt_rr_type" = c(en = "legendre computes a polynomial basis; data uses supplied basis columns. The current single-column UI does not fully specify every RR model."),
  "opt_rr_order" = c(en = "For legendre, the order must be supplied with the column in RR_POSITION and must match covariance dimensions. The current generator records k as a comment only: manually complete the card."),
  "opt_rr_position" = c(en = "legendre requires order and raw covariate column; data requires the basis-column list. Verify the generated block before use."),
  "opt_gibbs_enable" = c(en = "Pass options to gibbsf90+ after renumf90. Configure chain length, burn-in and thinning separately; this checkbox does not launch Gibbs."),
  "opt_gibbs_cat" = c(en = "One category count per trait in TRAITS order: 0 continuous, 2 binary, k ordered categories. Example 0 2. Observed categories start at 1; 0 is missing for threshold traits."),
  "opt_gibbs_thresholds" = c(en = "Fixed thresholds for the categorical model. Binary uses threshold 0; do not treat this as one arbitrary zero per trait. Verify multi-category identification."),
  "opt_gibbs_seed" = c(en = "Random-number seed setting; verify the number of seeds accepted by the installed Gibbs version. Reproducibility also needs the same data and chain settings."),
  "opt_gibbs_solution_mean" = c(en = "Posterior means/SD of solutions while sampling variances. Interpret only after burn-in and convergence checks; use postgibbsf90 for posterior diagnostics."),
  "opt_gibbs_fixed_var" = c(en = "Fix variances at the card values and retain all solution samples. This does not estimate variance components and may generate large files."),
  "opt_upg_type" = c(en = "yob requires year boundaries, GEN_INT and birth-year position (5th FILE_POS item). group requires group-column positions; in_pedigrees uses negative parent-group codes. This UI does not supply all required fields: complete the card manually.")
)
kit_help_text <- function(id, lang = "en") {
  entry <- KIT_FIELD_HELP[[id]]
  if (is.null(entry)) return("")
  unname(entry[["en"]])
}
kit_help_label <- function(id, label, lang = "en") {
  help <- kit_help_text(id, lang)
  url <- kit_doc_url(id)
  tip_body <- if (nzchar(url) && nzchar(help)) {
    tagList(
      tags$div(help),
      tags$div(
        tags$a(
          href = url, target = "_blank", rel = "noopener noreferrer",
          "Official BLUPF90 docs \u2197"
        )
      )
    )
  } else if (nzchar(url)) {
    tags$a(href = url, target = "_blank", rel = "noopener noreferrer", "Official BLUPF90 docs \u2197")
  } else {
    help
  }
  tip <- if (nzchar(help) || nzchar(url)) with_tip(label, tip_body) else label
  tags$span(class = "opt-label-with-doc", tip, kit_doc_anchor(url))
}
KIT_FIELD_HELP[["store_accuracy"]] <- c(en = "Use the animal EFFECT number, not the data column. acc_bf90 stores reliability by default; orig adds original IDs. UPG reliability needs special treatment.")
KIT_FIELD_HELP[["blksize"]] <- c(en = "Block size for multi-trait BLUP preconditioning; set to the number of traits.")
KIT_FIELD_HELP[["samples_se_covar_function"]] <- c(en = "Number of samples for uncertainty of variance-component functions; not Gibbs chain length.")
KIT_FIELD_HELP[["out_se_covar_function"]] <- c(en = "Save samples of the requested variance-component functions.")
KIT_FIELD_HELP[["inbreeding_method"]] <- c(en = "Pedigree inbreeding algorithm 1–7; method 6 uses OpenMP, method 7 handles selfing generations.")
KIT_FIELD_HELP[["opt_inbreeding_method"]] <- KIT_FIELD_HELP[["inbreeding_method"]]
KIT_OPTION_HELP <- c(
  "remove_all_missing" = "opt_remove_all_missing",
  "missing_in_weights" = "opt_missing_in_weights",
  "no_basic_statistics" = "opt_no_basic_statistics",
  "missing" = "opt_missing_value",
  "origID" = "opt_origID",
  "method" = "opt_method",
  "sol" = "opt_sol_se",
  "use_yams" = "opt_use_yams",
  "residual" = "opt_residual_out",
  "stdresidual" = "opt_stdresidual_out",
  "prior_solutions" = "opt_prior_solutions",
  "conv_crit" = "opt_conv_crit_val",
  "EM-REML" = "opt_em_reml_ai_conv",
  "maxrounds" = "opt_maxrounds_val",
  "se_covar_function" = "opt_auto_se_covar",
  "acctype" = "opt_acctype",
  "correct_accuracy_by_inbreeding_direct" = "opt_correct_acc_inb_direct0",
  "ped_search" = "opt_ped_search_complete",
  "saveCleanSNPs" = "opt_save_clean_snps",
  "snp_p_value" = "opt_snp_p_value",
  "omit_ainv" = "opt_omit_ainv",
  "map_file" = "opt_map_file",
  "AlphaBeta" = "opt_AlphaBeta",
  "TauOmega" = "opt_TauOmega",
  "tunedG" = "opt_tunedG",
  "readGimA22i" = "opt_read_gim_a22i",
  "hetres_pos" = "opt_hetres_pos",
  "hetres_pol" = "opt_hetres_pol_preset",
  "cat" = "opt_gibbs_cat",
  "thresholds" = "opt_gibbs_thresholds",
  "seed" = "opt_gibbs_seed",
  "solution" = "opt_gibbs_solution_mean",
  "fixed_var" = "opt_gibbs_fixed_var"
)

KIT_OPTION_HELP <- c(KIT_OPTION_HELP, "store_accuracy" = "store_accuracy", "blksize" = "blksize", "samples_se_covar_function" = "samples_se_covar_function", "out_se_covar_function" = "out_se_covar_function", "inbreeding_method" = "inbreeding_method")

# Numbered header for each of the three workspace zones
zone_head <- function(n, title, help = NULL) {
  div(class = "zone-head",
    span(class = "zone-num", n),
    span(class = "zone-title",
      if (is.null(help)) title else with_tip(title, help, placement = "bottom"))
  )
}

# Read only raw-column mappings from RENUMF90 cards. Application-card positions
# refer to renf90.dat and must never be mapped against the uploaded raw header.
kit_card_mapping <- function(text, columns) {
  lines <- trimws(sub("#.*$", "", strsplit(text, "\n", fixed = TRUE)[[1]]))
  lines <- lines[nzchar(lines)]
  result <- list(traits = NULL, fixed = NULL, random = NULL, animal = NULL,
                 optional = NULL, notes = character())
  note <- function(x) result$notes <<- unique(c(result$notes, x))
  if (any(lines %in% c("NUMBER_OF_TRAITS", "EFFECTS:", "OBSERVATION(S)"))) {
    note("renf90.par uses recoded columns; raw-column selections were not changed.")
    return(result)
  }
  positions <- function(x) {
    tokens <- strsplit(x, "\\s+")[[1]]
    if (!length(tokens) || any(!grepl("^[1-9][0-9]*$", tokens))) return(NULL)
    n <- suppressWarnings(as.integer(tokens))
    if (anyNA(n) || any(n > length(columns)) || anyDuplicated(n)) return(NULL)
    columns[n]
  }
  ti <- which(lines == "TRAITS")
  if (length(ti) != 1 || ti >= length(lines)) {
    note("No unambiguous TRAITS block; selections were not changed."); return(result)
  }
  result$traits <- positions(lines[ti + 1])
  if (is.null(result$traits)) {
    note("TRAITS contains invalid or unavailable raw column numbers; selections were not changed.")
    return(result)
  }
  ef <- which(lines == "EFFECT")
  fixed <- random <- animal <- character()
  safe <- TRUE
  for (j in seq_along(ef)) {
    from <- ef[j] + 1L
    to <- if (j < length(ef)) ef[j + 1L] - 1L else length(lines)
    if (from > to) { safe <- FALSE; next }
    block <- lines[from:to]
    tokens <- strsplit(block[1], "\\s+")[[1]]
    kind <- which(tokens %in% c("cross", "cov"))
    if (length(kind) != 1 || kind < 2) { safe <- FALSE; next }
    nums <- tokens[seq_len(kind - 1)]
    # The UI can express one common class column across all traits, not
    # nested slopes, absent-trait effects, or distinct trait-specific columns.
    if (tokens[kind] != "cross" || any(block %in% c("NESTED", "RANDOM_REGRESSION")) ||
        !length(nums) || !all(nums == nums[1]) ||
        !(length(nums) %in% c(1L, length(result$traits)))) { safe <- FALSE; next }
    col <- positions(nums[1])
    if (is.null(col)) { safe <- FALSE; next }
    ri <- which(block == "RANDOM")
    if (!length(ri)) fixed <- c(fixed, col)
    else if (length(ri) != 1 || ri >= length(block)) safe <- FALSE
    else if (block[ri + 1] == "animal") animal <- c(animal, col)
    else if (block[ri + 1] == "diagonal") random <- c(random, col)
    else safe <- FALSE
  }
  roles <- c(result$traits, fixed, random, animal)
  if (length(animal) > 1 || anyDuplicated(roles)) safe <- FALSE
  if (safe) {
    result$fixed <- fixed; result$random <- random; result$animal <- animal
    oi <- which(lines == "OPTIONAL")
    if (!length(oi)) result$optional <- character()
    else if (length(oi) == 1 && oi < length(lines)) {
      opts <- strsplit(lines[oi + 1], "\\s+")[[1]]
      if (all(opts %in% c("pe", "mat", "mpe"))) result$optional <- opts
    }
  } else note("Traits synced; advanced or overlapping effects cannot be represented by the selectors and remain in the card.")
  result
}

# ====== UI Definition ======
# Stock BS5 theme → bslib precompiled CSS (no Sass recompile / contrast warnings).
# Brand colors live in www/app.css (--accent), not in bs_theme().
ui <- page_fillable(
  theme = bs_theme(version = 5),
  
  # Custom CSS — chrome lives in www/app.css; keep only modal helpers here
  tags$head(
    if (exists("easybreedeR_a4_layout_css", mode = "function")) {
      tags$style(HTML(easybreedeR_a4_layout_css()))
    },
    tags$style(HTML("
    /* Genotype Format Helper Modal — Xmosome chrome */
    .geno-helper-modal .modal-header {
      background: var(--accent-soft, rgba(207, 185, 145,0.10));
      border-bottom: 1px solid var(--line, rgba(0,0,0,0.10));
      padding: 14px 18px;
      border-radius: 14px 14px 0 0;
    }
    .geno-helper-modal .modal-title {
      color: var(--ink, #202124);
      font-weight: 650;
      font-size: 14px;
    }
    .geno-helper-modal .modal-body {
      padding: 16px 18px;
      background: var(--card, var(--card, #fff));
    }
    .geno-helper-info-box {
      background: var(--accent-soft, rgba(207, 185, 145,0.10));
      border: 1px solid color-mix(in srgb, #CFB991 22%, rgba(0,0,0,0.10));
      border-radius: 10px;
      padding: 12px 14px;
      margin-bottom: 14px;
    }
    .geno-helper-info-box p {
      margin: 0;
      color: var(--ink, #202124);
      font-size: 12.5px;
      line-height: 1.45;
    }
    .geno-helper-direction-card {
      background: var(--card, var(--card, #fff));
      border: 1px solid var(--line, rgba(0,0,0,0.10));
      border-radius: 12px;
      padding: 14px;
      margin-bottom: 14px;
    }
    .geno-helper-direction-card h4 {
      color: var(--ink, #202124);
      font-weight: 650;
      margin: 0 0 8px;
      font-size: 13px;
    }
    .geno-helper-direction-card p {
      color: var(--muted, #6e7178);
      font-size: 12px;
      margin-bottom: 12px;
      line-height: 1.4;
    }
    .geno-helper-file-input-wrapper {
      background: color-mix(in srgb, rgba(207, 185, 145,0.10) 45%, var(--card, #fff));
      border: 1px dashed color-mix(in srgb, #CFB991 28%, rgba(0,0,0,0.10));
      border-radius: 8px;
      padding: 10px 12px;
      margin-bottom: 10px;
    }
    .geno-helper-file-input-wrapper label {
      font-weight: 550;
      color: var(--muted, #6e7178);
      margin-bottom: 4px;
      display: block;
      font-size: 11.5px;
    }
    .geno-helper-file-input-wrapper .form-group { margin-bottom: 0; }
    .geno-helper-modal .modal-footer {
      background: var(--card, var(--card, #fff));
      border-top: 1px solid var(--line, rgba(0,0,0,0.10));
      padding: 12px 18px;
    }
    .geno-helper-modal .btn-primary {
      background-color: #CFB991 !important;
      border-color: #CFB991 !important;
      color: #fff !important;
    }
    .geno-helper-modal .btn-secondary {
      background: var(--card, var(--card, #fff)) !important;
      border-color: var(--line, rgba(0,0,0,0.10)) !important;
      color: var(--ink, #202124) !important;
    }
    #download_blup_map,
    #download_blup_txt {
      background-color: var(--accent-soft, rgba(207, 185, 145,0.10)) !important;
      border-color: color-mix(in srgb, #CFB991 25%, rgba(0,0,0,0.10)) !important;
      color: var(--ink, #202124) !important;
    }
    #geno_convert_output_dir,
    #geno_convert_output_dir_blup {
      height: 34px !important;
      box-sizing: border-box !important;
      line-height: 34px !important;
      padding: 0 10px !important;
    }
    .geno-helper-file-input-wrapper .form-group {
      display: flex !important;
      flex-direction: column !important;
    }
    .geno-helper-file-input-wrapper .form-group .input-group {
      order: 1 !important;
      width: 100% !important;
    }
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_bed_progress,
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_bim_progress,
    .geno-helper-file-input-wrapper .form-group #geno_help_plink_fam_progress {
      order: 2 !important;
      width: 100% !important;
      margin-top: 6px !important;
      margin-bottom: 0 !important;
      display: block !important;
    }
    .geno-helper-arrow {
      display: inline-block;
      margin: 0 8px;
      color: #CFB991;
      font-weight: 650;
      font-size: 1rem;
    }
    #geno_format-label,
    #geno_format_label_ui {
      display: inline-flex;
      align-items: center;
      gap: 6px;
    }
    #geno_format_help {
      display: inline-flex;
      align-items: center;
      padding: 0;
      text-decoration: none;
    }
  ")),
    tags$script(HTML("
      // Keep body-parented selectize dropdowns as wide as their control (not full page)
      (function () {
        function syncDropdownWidth(sel) {
          if (!sel || !sel.selectize) return;
          var s = sel.selectize;
          var $dd = s.$dropdown;
          var w = (s.$control && s.$control.outerWidth()) || 240;
          w = Math.max(160, Math.min(w, 320));
          if ($dd && $dd.length) {
            $dd.css({ width: w + 'px', minWidth: w + 'px', maxWidth: w + 'px' });
          }
        }
        function patchSelectize(sel) {
          if (!sel || !sel.selectize || sel._kitWidthPatched) return;
          sel._kitWidthPatched = true;
          sel.selectize.on('dropdown_open', function () { syncDropdownWidth(sel); });
        }
        function patchAll() {
          document.querySelectorAll('select.selectized').forEach(patchSelectize);
        }
        $(document).on('shiny:connected shiny:valuebound', patchAll);
        $(document).on('focus', '.selectize-input', function () {
          var sel = $(this).closest('.selectize-control').prev('select.selectized')[0]
                 || $(this).closest('.shiny-input-container').find('select.selectized')[0];
          patchSelectize(sel);
          syncDropdownWidth(sel);
        });
        setTimeout(patchAll, 0);
      })();

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
              alert('Folder selection is unavailable. Enter the output path manually.');
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
        var progressBarIds = ['geno_help_plink_bed_progress', 'geno_help_plink_bim_progress', 'geno_help_plink_fam_progress'];
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
      
      // Server -> editor (programmatic; does not fire an 'input' event)
      Shiny.addCustomMessageHandler('update_textarea', function(message) {
        var el = document.getElementById('param_editor');
        if (!el) return;
        el.value = message.content;
        Shiny.setInputValue('param_editor_text', el.value);
      });

      // Editor -> server (user typing), debounced so every keystroke is not a round-trip
      $(document).on('input', '#param_editor', (function () {
        var t = null;
        return function () {
          var el = this;
          clearTimeout(t);
          t = setTimeout(function () {
            Shiny.setInputValue('param_editor_text', el.value);
            Shiny.setInputValue('param_editor_user_edit', Date.now(), {priority: 'event'});
          }, 350);
        };
      })());

      // Alt+Tab inserts spaces; plain Tab and Shift+Tab preserve keyboard navigation
      $(document).on('keydown', '#param_editor', function (e) {
        if (e.key !== 'Tab' || !e.altKey) return;
        e.preventDefault();
        var el = this, s = el.selectionStart, en = el.selectionEnd;
        el.value = el.value.substring(0, s) + '  ' + el.value.substring(en);
        el.selectionStart = el.selectionEnd = s + 2;
        $(el).trigger('input');
      });

      window.copyParamCard = function () {
        var el = document.getElementById('param_editor');
        if (!el || !el.value) return;
        var btn = document.getElementById('copy_param');
        var done = function () {
          if (!btn) return;
          btn.textContent = 'Copied';
          setTimeout(function () { btn.textContent = 'Copy'; }, 1400);
        };
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(el.value).then(done);
        } else {
          el.select(); document.execCommand('copy'); done();
        }
      };
      
      // Toggle panel functionality
      $(function () {
        var leftOpen = true, rightOpen = true;
        function toggleLeft() { $('#leftPanel').toggleClass('hidden'); leftOpen = !leftOpen; $('#toggleLeftPanel').attr('aria-expanded', String(leftOpen)); }
        function toggleRight() { $('#rightPanel').toggleClass('hidden'); rightOpen = !rightOpen; $('#toggleRightPanel').attr('aria-expanded', String(rightOpen)); }
        $('#toggleLeftPanel').on('click', toggleLeft);
        $('#toggleRightPanel').on('click', toggleRight);
      });
    ")),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.css"),
    tags$script(defer = NA, src = "https://cdn.jsdelivr.net/npm/katex@0.16.11/dist/katex.min.js",
                onload = "window.renderModelTex && window.renderModelTex()"),
    tags$script(HTML("
      // Render .tex[data-tex] with KaTeX; plain-text fallback stays if KaTeX is unavailable (offline)
      // Selectize dropdowns near the bottom of the window open upward instead of running off-screen
      $(function () {
        if (!window.Selectize || Selectize.prototype.__flipPatched) return;
        var orig = Selectize.prototype.positionDropdown;
        Selectize.prototype.positionDropdown = function () {
          orig.apply(this, arguments);
          var $d = this.$dropdown, ctl = this.$control[0].getBoundingClientRect();
          var h = $d.outerHeight();
          var below = window.innerHeight - ctl.bottom, above = ctl.top;
          if (h + 8 > below && above > below) {
            var top = this.settings.dropdownParent === 'body'
              ? this.$control.offset().top - h - 2
              : -h - 2;
            $d.css({ top: top }).addClass('dropup');
          } else {
            $d.removeClass('dropup');
          }
          // Keep body-parented dropdowns inside the window horizontally
          if (this.settings.dropdownParent === 'body') {
            var rect = $d[0].getBoundingClientRect();
            var overflow = rect.right - (window.innerWidth - 8);
            if (overflow > 0) $d.css({ left: Math.max(8, parseFloat($d.css('left')) - overflow) });
          }
        };
        // Re-position after the option list changes height (typing / loading)
        var origRefresh = Selectize.prototype.refreshOptions;
        Selectize.prototype.refreshOptions = function () {
          var r = origRefresh.apply(this, arguments);
          if (this.isOpen) this.positionDropdown();
          return r;
        };
        Selectize.prototype.__flipPatched = true;
      });

      window.renderModelTex = function () {
        if (!window.katex) return;
        document.querySelectorAll('.tex[data-tex]:not([data-rendered])').forEach(function (el) {
          try {
            katex.render(el.getAttribute('data-tex'), el, { throwOnError: false, displayMode: false });
            el.setAttribute('data-rendered', '1');
          } catch (e) {}
        });
      };
      $(document).on('shiny:value', function (e) {
        if (e.name === 'model_summary_ui') setTimeout(window.renderModelTex, 0);
      });
    ")),
    if (exists("eb_chrome_css_tag", mode = "function")) eb_chrome_css_tag(),
    tags$link(rel = "stylesheet", type = "text/css", href = .xmosome_asset("app.css")),
    tags$link(rel = "stylesheet", type = "text/css", href = .xmosome_asset("ai-chat.css")),
    tags$script(src = .xmosome_asset("ai-chat.js")),
    tags$link(rel = "stylesheet", href = .xmosome_asset("workbench.css"))
  ),
  
  # Xmosome plugin chrome
  tags$header(class = "plugin-header",
    div(class = "plugin-mark",
      HTML('<svg viewBox="0 0 64 64" width="18" height="18" fill="none" xmlns="http://www.w3.org/2000/svg"><rect x="14" y="16" width="36" height="3.5" rx="1.5" fill="#CFB991"/><rect x="14" y="24" width="28" height="3.5" rx="1.5" fill="#CFB991" fill-opacity="0.55"/><rect x="14" y="32" width="32" height="3.5" rx="1.5" fill="#CFB991" fill-opacity="0.55"/><rect x="14" y="40" width="22" height="3.5" rx="1.5" fill="#CFB991" fill-opacity="0.55"/></svg>')
    ),
    div(class = "plugin-header-text",
      p(class = "eyebrow", "Xmosome plugin"),
      uiOutput("app_title_ui"),
      uiOutput("app_subtitle_ui")
    ),
    div(class = "panel-switches", role = "group", `aria-label` = "Workspace panels",
      actionButton("toggleLeftPanel", "Data", class = "btn panel-switch",
                   title = "Show or hide data and model", `aria-label` = "Show or hide data and model",
                   `aria-controls` = "leftPanel", `aria-expanded` = "true"),
      actionButton("toggleRightPanel", "Options", class = "btn panel-switch",
                   title = "Show or hide options", `aria-label` = "Show or hide options",
                   `aria-controls` = "rightPanel", `aria-expanded` = "true")
    )
  ),

  # Three-Panel Container (1 Data → 2 OPTIONS → 3 Parameter card)
  tags$main(class = "three-panel-container",
    
    # ====== Left Panel: Goal → Files → Model (renumf90 order) ======
    div(id = "leftPanel", class = "left-panel zone zone-1",
        zone_head(1, "Data & model",
          "Choose a model, upload files and assign columns."),

        div(class = "panel-section",
          h4(class = "section-title", with_tip("Goal & template",
            tags$div(tags$b("Goal:"), " BLUP = EBVs with known variances; VCE = estimate variances (AI-REML)."),
            tags$div(tags$b("Templates:"), " Full skill set T1–T17 (animal → genomic → Gibbs → specialty). Selecting a template presets OPTIONAL / genomic / UPG / RR flags.")
          )),
          div(class = "panel-body",
            selectInput(
              "analysis_goal", "Analysis goal",
              choices = c(
                "BLUP (EBV / BLUE)" = "BLUP",
                "VCE / REML" = "VCE"
              ),
              selected = "VCE",
              width = "100%",
              selectize = FALSE
            ),
            selectInput(
              "model_template", "Model template",
              choices = kit_template_choices(),
              selected = "t1",
              width = "100%",
              selectize = FALSE
            ),
            uiOutput("template_hint_ui"),
            tags$details(class = "kit-workflow-help",
              tags$summary("Quick guide"),
              tags$p("1. Upload and map columns. Match IDs across files; unknown parents use 0."),
              tags$p("2. Complete OPTIONS (data handling, estimation, advanced models)."),
              tags$p("3. Review and export the parameter card, then run renumf90 → blupf90+ / gibbsf90+."),
              tags$p("Check record counts and convergence. Convert spreadsheets to compatible text before running.")
            )
          )
        ),

        div(class = "panel-section",
          uiOutput("upload_title_ui"),
          div(class = "panel-body",
            fileInput("pheno_file", uiOutput("pheno_label_ui", inline = TRUE), 
                     accept = c(".csv", ".txt", ".dat", ".xlsx", ".xls"),
                     buttonLabel = "Browse",
                     placeholder = "No file selected"),
            fileInput("ped_file", uiOutput("ped_label_ui", inline = TRUE),
                     accept = c(".ped", ".txt"),
                     buttonLabel = "Browse",
                     placeholder = "Optional"),
            selectInput(
              inputId = "geno_format",
              label = uiOutput("geno_format_label_ui", inline = TRUE),
              choices = c(
                "PLINK (.bed/.bim/.fam)" = "plink",
                "BLUPF90 (.txt)" = "blupf90"
              ),
              selected = "plink",
              width = "100%",
              selectize = FALSE
            ),
            fileInput("geno_file", uiOutput("geno_label_ui", inline = TRUE),
                     accept = NULL,
                     multiple = TRUE,
                     buttonLabel = "Browse",
                     placeholder = "Optional"),
            uiOutput("clear_all_btn_ui")
          )
        ),
        
        div(class = "panel-section",
          uiOutput("model_builder_title_ui"),
          div(class = "panel-body field-stack",
            uiOutput("column_map_ui"),
            selectizeInput(
              inputId = "traits_select",
              label = uiOutput("traits_label_ui", inline = TRUE),
              choices = NULL,
              multiple = TRUE,
              width = "100%",
              options = kit_selectize_opts("Trait columns")
            ),
            selectizeInput(
              inputId = "fixed_select",
              label = uiOutput("fixed_label_ui", inline = TRUE),
              choices = NULL,
              multiple = TRUE,
              width = "100%",
              options = kit_selectize_opts("Fixed-effect columns")
            ),
            selectizeInput(
              inputId = "random_select",
              label = uiOutput("random_label_ui", inline = TRUE),
              choices = NULL,
              multiple = TRUE,
              width = "100%",
              options = kit_selectize_opts("Random-effect columns")
            ),
            selectizeInput(
              inputId = "animal_select",
              label = uiOutput("animal_label_ui", inline = TRUE),
              choices = NULL,
              multiple = FALSE,
              width = "100%",
              options = kit_selectize_opts("Animal ID column", multi = FALSE)
            ),
            div(class = "optional-row",
              checkboxInput("opt_pe", uiOutput("pe_label_ui", inline = TRUE), value = FALSE),
              checkboxInput("opt_mat", uiOutput("mat_label_ui", inline = TRUE), value = FALSE),
              checkboxInput("opt_mpe", uiOutput("mpe_label_ui", inline = TRUE), value = FALSE)
            )
          )
        )
      ),

    # ====== Center Panel: Parameter Card Workbench ======
    div(class = "center-panel zone zone-2",
      zone_head(3, "Parameter card",
          "Review and export renum.par. Editing turns on Manual lock."),
      div(class = "workspace-card",
        div(class = "workspace-head",
          div(class = "workspace-title-row",
            h2("Parameter card"),
            span(class = "eyebrow", "renumf90")
          ),
          uiOutput("model_summary_ui"),
          uiOutput("card_checks_ui")
        ),
        div(class = "param-toolbar",
          radioButtons(
            "param_edit_mode", NULL,
            choices = c("Auto update" = "auto", "Manual lock" = "manual"),
            selected = "auto", inline = TRUE
          ),
          uiOutput("reset_btn_ui"),
          actionButton("explain_param", "Explain", class = "btn btn-secondary btn-sm"),
          actionButton("validate_param", "Validate", class = "btn btn-secondary btn-sm"),
          tags$button(id = "copy_param", type = "button", class = "btn btn-secondary btn-sm",
                      onclick = "copyParamCard()", "Copy")
        ),
        div(class = "editor-wrap",
          div(class = "parameter-preview",
            div(class = "parameter-filebar",
              div(class = "par-filename",
                textInput("par_filename", tags$span(class = "visually-hidden", "Parameter filename"), value = "renum.par", width = "160px")
              ),
              uiOutput("download_btn_ui")
            ),
            tags$textarea(
              id = "param_editor",
              `aria-label` = "Parameter card editor", spellcheck = "false",
              placeholder = "Upload data and select traits to build your parameter card."
            )
          )
        )
      )
    ),

    # ====== Right Panel: progressive OPTIONS ======
    div(id = "rightPanel", class = "right-panel zone zone-3",
        zone_head(2, "OPTIONS",
          "Configure data handling, estimation and advanced models."),
        uiOutput("right_options_accordion_ui")
      )
  )
)

# ====== Server Logic ======
server <- function(input, output, session) {
  eb_track_session(session, "xmosome")
  
  # English UI copy; AI replies independently follow the user message language.
  lang <- reactiveVal("en")

  # Right-panel selects: attach the dropdown to <body> so accordion cards
  # (overflow: hidden) and the panel scroller never clip the option list.
  rp_select <- function(...) {
    selectizeInput(..., options = list(dropdownParent = "body"))
  }

  inbreeding_method_desc <- c(
    "1" = "Meuwissen and Luo (1992) — default, fast for most pedigrees.",
    "2" = "Modified Meuwissen & Luo by Sargolzaei & Iwaisaki (2004).",
    "3" = "Modified Colleau by Sargolzaei et al. (2005).",
    "4" = "Recursive tabular method.",
    "5" = "Method of Tier (1990).",
    "6" = "Hybrid parallel (OpenMP) Meuwissen & Luo — set OMP_NUM_THREADS.",
    "7" = "Recursive tabular with selfing generations (e.g. wheat)."
  )

  # ====== Right panel: accordion OPTIONS (skill-aligned categories) ======
  output$right_options_accordion_ui <- renderUI({
    l <- lang()
    vars <- tryCatch(colnames(isolate(data())), error = function(e) NULL)

    panels <- list(
      accordion_panel(
        title = with_tip("1 · Data & missing",
          "renumf90-native: remove_all_missing, missing code, missing_in_weights, no_basic_statistics. origID is copied to renf90.par."),
        value = "data",
        checkboxInput("opt_remove_all_missing", kit_help_label("opt_remove_all_missing", get_label("remove_all_missing", l), l), value = TRUE),
        checkboxInput("opt_missing_in_weights", kit_help_label("opt_missing_in_weights", get_label("missing_in_weights", l), l), value = FALSE),
        checkboxInput("opt_no_basic_statistics", kit_help_label("opt_no_basic_statistics", get_label("no_basic_statistics", l), l), value = FALSE),
        textInput("opt_missing_value", kit_help_label("opt_missing_value", get_label("missing_value_symbol", l), l), value = "-999", width = "100%"),
        checkboxInput("opt_origID", kit_help_label("opt_origID", get_label("origID_store_solutions", l), l), value = TRUE)
      ),
      accordion_panel(
        title = with_tip("2 · Solver · BLUP / VCE",
          "method BLUP or VCE (AI-REML). EM-REML n = EM rounds before AI. use_yams speeds large / genomic models."),
        value = "solver",
        rp_select("opt_method", kit_help_label("opt_method", get_label("method_label", l), l),
                   choices = c(setNames("BLUP", get_label("method_blup", l)),
                               setNames("VCE",  get_label("method_vce",  l))),
                   selected = "VCE", width = "100%"),
        checkboxInput("opt_sol_se", kit_help_label("opt_sol_se", get_label("sol_se", l), l), value = TRUE),
        checkboxInput("opt_use_yams", kit_help_label("opt_use_yams", get_label("use_yams", l), l), value = TRUE),
        checkboxInput("opt_residual_out", kit_help_label("opt_residual_out", get_label("residual_output", l), l), value = FALSE),
        checkboxInput("opt_stdresidual_out", kit_help_label("opt_stdresidual_out", get_label("stdresidual_output", l), l), value = FALSE),
        checkboxInput("opt_prior_solutions", kit_help_label("opt_prior_solutions", get_label("prior_solutions", l), l), value = FALSE),
        conditionalPanel(
          condition = "input.opt_method == 'VCE'",
          hr(),
          tags$strong("VCE / REML"),
          textInput("opt_conv_crit_val", kit_help_label("opt_conv_crit_val", get_label("conv_crit", l), l), value = "1d-12", width = "100%"),
          numericInput("opt_em_reml_rounds", kit_help_label("opt_em_reml_rounds", get_label("em_reml_rounds", l), l), value = 100, min = 1, step = 1, width = "100%"),
          checkboxInput("opt_em_reml_pure", kit_help_label("opt_em_reml_pure", get_label("em_reml_pure", l), l), value = FALSE),
          checkboxInput("opt_em_reml_ai_conv", kit_help_label("opt_em_reml_ai_conv", get_label("em_reml_ai_conv", l), l), value = FALSE),
          numericInput("opt_maxrounds_val", kit_help_label("opt_maxrounds_val", get_label("maxrounds", l), l), value = 1000000, min = 1, step = 1000, width = "100%"),
          checkboxInput("opt_auto_se_covar", kit_help_label("opt_auto_se_covar", get_label("auto_se_covar", l), l), value = TRUE)
        )
      ),
      accordion_panel(
        title = with_tip("3 · Accuracy & reliability",
          "store_accuracy uses the animal effect number, not a data column. Default output is reliability; acctype 0.5 uses the BIF convention. UPG reliabilities require special treatment."),
        value = "accuracy",
        uiOutput("store_accuracy_ui"),
        uiOutput("store_accuracy_orig_ui"),
        rp_select("opt_acctype", kit_help_label("opt_acctype", get_label("acctype", l), l), choices = c("1.0", "0.5"), selected = "1.0", width = "100%"),
        checkboxInput("opt_correct_acc_inb_direct0", kit_help_label("opt_correct_acc_inb_direct0", get_label("correct_accuracy_inb_direct0", l), l), value = FALSE)
      ),
      accordion_panel(
        title = with_tip("4 · Pedigree · UPG · inbreeding",
          "PED_DEPTH, ped_search complete, inbreeding_method. T6: UPG_TYPE. T9: omit pedigree FILE."),
        value = "pedigree",
        checkboxInput("opt_omit_pedigree", kit_help_label("opt_omit_pedigree", "Omit pedigree FILE (T9 GBLUP)", l), value = FALSE),
        checkboxInput("opt_ped_search_complete", kit_help_label("opt_ped_search_complete", "OPTION ped_search complete", l), value = FALSE),
        checkboxInput("opt_use_ped_depth", kit_help_label("opt_use_ped_depth", "Use PED_DEPTH", l), value = FALSE),
        conditionalPanel(
          condition = "input.opt_use_ped_depth == true",
          numericInput("opt_ped_depth", kit_help_label("opt_ped_depth", "PED_DEPTH", l), value = 0, min = 0, step = 1, width = "100%")
        ),
        rp_select("opt_upg_type",
                  kit_help_label("opt_upg_type", "UPG_TYPE", l),
                  choices = c(
                    "(none)" = "",
                    "yob (by birth year)" = "yob",
                    "in_pedigrees" = "in_pedigrees",
                    "group" = "group"
                  ),
                  selected = "",
                  width = "100%"),
        rp_select("opt_inbreeding_method",
                  kit_help_label("opt_inbreeding_method", "OPTION inbreeding_method", l),
                   choices = c(
                     "1 · Meuwissen & Luo" = "1",
                     "2 · Sargolzaei & Iwaisaki" = "2",
                     "3 · Modified Colleau" = "3",
                     "4 · Recursive tabular" = "4",
                     "5 · Tier (1990)" = "5",
                     "6 · Parallel M&L (OMP)" = "6",
                     "7 · Tabular + selfing" = "7"
                   ), selected = "1", width = "100%")
      ),
      accordion_panel(
        title = with_tip("5 · Genomic · ssGBLUP / GBLUP",
          "T8 / T9: map_file, saveCleanSNPs, omit_ainv, snp_p_value, AlphaBeta / TauOmega / tunedG."),
        value = "genomic",
        checkboxInput("opt_save_clean_snps", kit_help_label("opt_save_clean_snps", "OPTION saveCleanSNPs", l), value = FALSE),
        checkboxInput("opt_snp_p_value", kit_help_label("opt_snp_p_value", get_label("snp_p_value", l), l), value = FALSE),
        checkboxInput("opt_omit_ainv", kit_help_label("opt_omit_ainv", get_label("omit_ainv", l), l), value = FALSE),
        textInput("opt_map_file", kit_help_label("opt_map_file", "OPTION map_file", l), value = "", width = "100%",
                  placeholder = "map.txt (optional)"),
        textInput("opt_AlphaBeta", kit_help_label("opt_AlphaBeta", "OPTION AlphaBeta", l), value = "",
                  width = "100%", placeholder = "e.g. 0.95 0.05"),
        textInput("opt_TauOmega", kit_help_label("opt_TauOmega", "OPTION TauOmega", l), value = "", width = "100%",
                  placeholder = "e.g. 1.0 0.7"),
        rp_select("opt_tunedG", kit_help_label("opt_tunedG", "OPTION tunedG", l),
                  choices = c(
                    "(program default = 2)" = "",
                    "0 · no scaling" = "0",
                    "1 · mean(diag G)=1, mean(offdiag G)=0" = "1",
                    "2 · match mean diag/offdiag of A22 (default)" = "2",
                    "3 · mean(G)=mean(A22)" = "3",
                    "4 · Fst rescale (Powell / Vitezica)" = "4",
                    "9 · a + bG (custom a, b)" = "9"
                  ),
                  selected = "",
                  width = "100%"),
        conditionalPanel(
          condition = "input.opt_tunedG == '9'",
          div(
            style = "display:flex; gap:8px; flex-wrap:wrap;",
            numericInput("opt_tunedG_a", kit_help_label("opt_tunedG_a", "tunedG a", l),
                         value = 0, step = 0.01, width = "48%"),
            numericInput("opt_tunedG_b", kit_help_label("opt_tunedG_b", "tunedG b", l),
                         value = 1, step = 0.01, width = "48%")
          ),
          tags$div(class = "muted", style = "font-size:11px; margin-top:4px;",
                   "Emits: OPTION tunedG 9 a b  →  G ← a + bG")
        ),
        checkboxInput("opt_read_gim_a22i", kit_help_label("opt_read_gim_a22i", "OPTION readGimA22i", l), value = FALSE)
      ),
      accordion_panel(
        title = with_tip("6 · Heterogeneous residual",
          "AI-REML: residual variance = exp(Xb). Incompatible with EM-REML. Coefficients and basis columns must match; presets do not create polynomial columns."),
        value = "hetres",
        rp_select("opt_hetres_pos", kit_help_label("opt_hetres_pos", get_label("hetres_pos", l), l), choices = vars, multiple = TRUE, width = "100%"),
        rp_select("opt_hetres_pol_preset", kit_help_label("opt_hetres_pol_preset", get_label("hetres_pol_preset_label", l), l),
                   choices = c(setNames("0.1", get_label("hetres_pol_constant", l)),
                               setNames("0.1 0.01", get_label("hetres_pol_linear", l)),
                               setNames("0.1 0.01 0.001", get_label("hetres_pol_quadratic", l))),
                   selected = "0.1 0.01", width = "100%")
      ),
      accordion_panel(
        title = with_tip("7 · Random regression (T7)",
          "Draft only: RR basis, keyword order and expanded covariance matrices must be checked and completed manually; selecting T7 is not sufficient."),
        value = "rr",
        checkboxInput("opt_use_rr", kit_help_label("opt_use_rr", "Emit RANDOM_REGRESSION block", l), value = FALSE),
        textInput("opt_rr_type", kit_help_label("opt_rr_type", "RANDOM_REGRESSION", l), value = "legendre", width = "100%",
                  placeholder = "legendre"),
        numericInput("opt_rr_order", kit_help_label("opt_rr_order", "RR order (k)", l), value = 3, min = 1, max = 10, step = 1, width = "100%"),
        rp_select("opt_rr_position", kit_help_label("opt_rr_position", "RR_POSITION column", l), choices = vars, multiple = FALSE, width = "100%")
      ),
      accordion_panel(
        title = with_tip("8 · Gibbs / threshold (T10–T11)",
          "Pass-through OPTIONs for GIBBSF90+: cat, thresholds, seed, solution mean."),
        value = "gibbs",
        checkboxInput("opt_gibbs_enable", kit_help_label("opt_gibbs_enable", "Emit Gibbs OPTION lines", l), value = FALSE),
        textInput("opt_gibbs_cat", kit_help_label("opt_gibbs_cat", "OPTION cat", l), value = "", width = "100%",
                  placeholder = "e.g. 0 2 (linear, binary)"),
        textInput("opt_gibbs_thresholds", kit_help_label("opt_gibbs_thresholds", "OPTION thresholds", l), value = "", width = "100%",
                  placeholder = "e.g. 0 1 (three categories)"),
        numericInput("opt_gibbs_seed", kit_help_label("opt_gibbs_seed", "OPTION seed", l), value = 123, min = 1, step = 1, width = "100%"),
        checkboxInput("opt_gibbs_solution_mean", kit_help_label("opt_gibbs_solution_mean", "OPTION solution mean", l), value = TRUE),
        checkboxInput("opt_gibbs_fixed_var", kit_help_label("opt_gibbs_fixed_var", "OPTION fixed_var all", l), value = FALSE)
      )
    )

    do.call(accordion, c(
      list(id = "right_opts_acc", open = c("data", "solver"), multiple = TRUE),
      panels
    ))
  })


  # Dead controls removed from UI (were never emitted): solv_method, r_factor, blksize, set_eig, TauOmega, AlphaBeta
  # ====== Dynamic UI Elements (Language-aware) ======
  
  # Title bar
  output$app_title_ui <- renderUI({
    h1("Xmosome_blupf90kit")
  })
  output$app_subtitle_ui <- renderUI({
    p(class = "muted", "Build, review and export parameter cards")
  })
  
  # Left Panel - Upload section
  output$upload_title_ui <- renderUI({
    h4(class = "section-title", with_tip(get_label_clean("upload_data", lang()),
      tags$div(tags$b("Phenotype:"), " only the header row is read (CSV, TXT, Excel)."),
      tags$div(tags$b("Pedigree:"), " animal sire dam, unknown parent = 0."),
      tags$div(tags$b("Genotypes:"), " PLINK .bed + .bim + .fam, or a BLUPF90 space-delimited .txt. Use ? next to Genotype Format to convert.")
    ))
  })
  
  output$pheno_label_ui <- renderUI({
    get_label_clean("phenotype", lang())
  })
  
  output$ped_label_ui <- renderUI({
    get_label_clean("pedigree", lang())
  })
  
  output$geno_label_ui <- renderUI({
    format <- input$geno_format %||% "plink"
    if (format == "blupf90") {
      get_label_clean("snp_file", lang())
    } else {
      get_label_clean("genotype", lang())
    }
  })
  
  output$geno_format_label_ui <- renderUI({
    # Show a small helper icon when PLINK conversion backend is available:
    # preferred PLINK path, or snpStats BED/BIM/FAM fallback path.
    label_text <- get_label_clean("genotype_format", lang())
    has_plinkr <- has_plinkr_blup_backend()
    has_r <- has_r_blup_backend_bfile()
    
    if (!has_plinkr && !has_r) {
      return(label_text)
    }
    
    help_title <- if (tolower(lang()) == "zh") {
      "Convert PLINK to BLUPF90"
    } else {
      "Need help? Genotype format translate (PLINK → BLUPF90)"
    }
    
    # Use actionLink so clicks are observable on the server
    tagList(
      span(
        class = "label-with-help",
        span(label_text),
        actionLink(
          inputId = "geno_format_help",
          label = tags$span("?", class = "help-dot", title = help_title),
          class = "help-link"
        )
      )
    )
  })
  
  
  output$clear_all_btn_ui <- renderUI({
    actionButton("clear_all", get_label_clean("clear_all", lang()),
                class = "btn btn-secondary btn-sm btn-clear")
  })
  
  # Left Panel - Model Builder section
  output$model_builder_title_ui <- renderUI({
    h4(class = "section-title", with_tip(get_label_clean("model_builder", lang()),
      "renumf90 reads columns by position. The numbered chips show each column and its role; a column should have only one role."
    ))
  })
  
  output$traits_label_ui <- renderUI({
    with_tip(get_label_clean("traits", lang()), "Select response columns in the intended trait order; covariance rows/columns use the same order. Threshold traits use categories 1..k, with 0 missing.")
  })
  
  output$fixed_label_ui <- renderUI({
    with_tip(get_label_clean("fixed_effects", lang()), "Fixed classes are fitted as cross effects. For a continuous slope, use EFFECT column cov in manual mode. No automatic intercept: include a constant column when no fixed class is fitted.")
  })
  
  output$animal_label_ui <- renderUI({
    with_tip(get_label_clean("animal_id", lang()), "Additive genetic animal effect. IDs must match phenotype, pedigree and genotype files exactly, including leading zeros.")
  })
  
  output$random_label_ui <- renderUI({
    with_tip(get_label_clean("random_effects", lang()), "Extra independent random classes, e.g. litter or herd-by-sire. diagonal describes independence among levels, not necessarily zero covariance among traits.")
  })
  
  # Left Panel - Optional Effects section
  output$optional_title_ui <- renderUI({
    h4(get_label_clean("optional_effects", lang()), class = "section-title")
  })
  
  output$pe_label_ui <- renderUI({
    with_tip(span("PE · permanent env."), "Permanent environmental effect shared by repeated records of the same animal; requires a separate PE covariance matrix.")
  })
  
  output$mat_label_ui <- renderUI({
    with_tip(span("MAT · maternal"), "Maternal genetic effect from the pedigree dam or alternate dam; jointly specify direct/maternal variances and covariance.")
  })
  
  output$mpe_label_ui <- renderUI({
    with_tip(span("MPE · maternal PE"), "Maternal permanent environment shared by offspring of a dam; requires mat in this workflow and a separate MPE covariance matrix.")
  })
  
  # Center Panel - Parameter file section
  output$param_title_ui <- renderUI({
    h2(get_label_clean("parameter_file", lang()), style = "text-align: center; margin-bottom: 20px;")
  })
  
  output$reset_btn_ui <- renderUI({
    actionButton("reset_param", get_label_clean("reset_param", lang()),
                class = "btn btn-secondary btn-sm")
  })
  
  output$download_btn_ui <- renderUI({
    downloadButton("download_param_file", get_label_clean("download_param", lang()),
                   class = "btn btn-primary btn-sm", icon = NULL)
  })
  
  # ====== End Dynamic UI ======
  
  # Set file size limit
  # Upload limit is configured once when the app is loaded.
  
  # Reactive values for selected variables
  values <- reactiveValues(
    traits = c(),
    fixed = c(),
    cov = c(),
    animal = c(),
    random = c(),
    default_param = "",
    current_param = "",
    auto_update = TRUE
  )

  # Goal ↔ method sync
  observeEvent(input$analysis_goal, {
    if (!is.null(input$analysis_goal)) {
      updateSelectInput(session, "opt_method", selected = input$analysis_goal)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$opt_method, {
    if (!is.null(input$opt_method) && !identical(input$opt_method, input$analysis_goal)) {
      updateSelectInput(session, "analysis_goal", selected = input$opt_method)
    }
  }, ignoreInit = TRUE)

  # Template presets (skill T1–T17)
  output$template_hint_ui <- renderUI({
    info <- kit_template_info(input$model_template)
    div(class = "field-hint template-hint", info$hint %||% "")
  })

  observeEvent(input$model_template, {
    info <- kit_template_info(input$model_template)
    updateCheckboxInput(session, "opt_pe", value = isTRUE(info$pe))
    updateCheckboxInput(session, "opt_mat", value = isTRUE(info$mat))
    updateCheckboxInput(session, "opt_mpe", value = isTRUE(info$mpe))
    updateCheckboxInput(session, "opt_omit_pedigree", value = isTRUE(info$omit_ped))
    updateCheckboxInput(session, "opt_omit_ainv", value = isTRUE(info$omit_ped))
    updateCheckboxInput(session, "opt_save_clean_snps", value = isTRUE(info$geno))
    updateCheckboxInput(session, "opt_use_rr", value = isTRUE(info$rr))
    updateCheckboxInput(session, "opt_gibbs_enable", value = isTRUE(info$gibbs))
    updateSelectInput(session, "opt_upg_type", selected = if (isTRUE(info$upg)) "yob" else "")
    updateSelectInput(session, "opt_tunedG", selected = if (isTRUE(info$omit_ped)) "0" else "")
    updateTextInput(session, "opt_AlphaBeta",
                    value = if (isTRUE(info$omit_ped)) "0.95 0.05" else "")
  }, ignoreInit = TRUE)

  # Auto / Manual lock
  observeEvent(input$param_edit_mode, {
    values$auto_update <- identical(input$param_edit_mode, "auto")
    if (isTRUE(values$auto_update) && nzchar(values$default_param)) {
      values$current_param <- values$default_param
      session$sendCustomMessage("update_textarea", list(content = values$current_param))
    }
  }, ignoreInit = TRUE)

  # Model summary: LaTeX formula (KaTeX) in a fixed-height slot + meta pills
  tex_text <- function(x) {
    x <- gsub("\\", "/", x, fixed = TRUE)
    x <- gsub("([_&%$#{}])", "\\\\\\1", x)
    x <- gsub("~", "\\textasciitilde{}", x, fixed = TRUE)
    x <- gsub("^", "\\textasciicircum{}", x, fixed = TRUE)
    paste0("\\text{", x, "}")
  }
  tex_col <- function(hex, body) paste0("\\textcolor{", hex, "}{", body, "}")
  role_hex <- c(trait = "#8E6F3E", fixed = "#7a5af8", cov = "#0e9384", random = "#dc6803", animal = "#28734e")

  output$model_summary_ui <- renderUI({
    tr <- values$traits; fx <- values$fixed; cv <- values$cov
    rn <- values$random; an <- values$animal
    has_geno <- !is.null(input$geno_file) && is.data.frame(input$geno_file) && nrow(input$geno_file) > 0
    info <- kit_template_info(input$model_template)
    use_h <- has_geno || isTRUE(info$geno)
    empty <- !length(tr) && !length(fx) && !length(cv) && !length(rn) && !length(an)

    if (empty) {
      tex <- "\\mathbf{y} = \\mathbf{X}\\mathbf{b} + \\mathbf{Z}\\mathbf{a} + \\mathbf{e}"
      plain <- "y = Xb + Za + e"
    } else {
      lhs <- if (length(tr) == 0) {
        "\\mathbf{y}"
      } else if (length(tr) == 1) {
        paste0("y_{", tex_col(role_hex[["trait"]], tex_text(tr)), "}")
      } else {
        paste0("\\mathbf{y}_{", tex_col(role_hex[["trait"]], paste(vapply(tr, tex_text, ""), collapse = ",\\,")), "}")
      }
      beta <- if (length(cv) > 1) paste0("\\beta_{", seq_along(cv), "}") else rep("\\beta", length(cv))
      terms <- c(
        vapply(fx, function(v) tex_col(role_hex[["fixed"]], tex_text(v)), ""),
        vapply(seq_along(cv), function(i) paste0(beta[i], "\\,", tex_col(role_hex[["cov"]], tex_text(cv[i]))), ""),
        vapply(rn, function(v) paste0("u_{", tex_col(role_hex[["random"]], tex_text(v)), "}"), "")
      )
      if (length(an)) {
        terms <- c(terms, paste0("a_{", tex_col(role_hex[["animal"]], tex_text(an)), "}"))
        if (isTRUE(input$opt_mat)) terms <- c(terms, "m")
        if (isTRUE(input$opt_pe)) terms <- c(terms, "pe")
        if (isTRUE(input$opt_mpe)) terms <- c(terms, "mpe")
      }
      tex <- paste0(lhs, " = ", paste(c(unname(terms), "e"), collapse = " + "))
      if (length(an)) {
        tex <- paste0(tex, ",\\quad \\mathbf{a} \\sim N\\!\\left(\\mathbf{0},\\, \\mathbf{",
                      if (use_h) "H" else "A", "}\\,\\sigma^2_a\\right)")
      }
      plain <- paste0(
        if (length(tr)) paste(tr, collapse = ", ") else "y", " = ",
        paste(c(fx, paste0("b·", cv), rn, if (length(an)) paste0("a(", an, ")"),
                if (length(an) && isTRUE(input$opt_mat)) "m",
                if (length(an) && isTRUE(input$opt_pe)) "pe",
                if (length(an) && isTRUE(input$opt_mpe)) "mpe", "e"), collapse = " + ")
      )
    }

    method <- input$opt_method %||% input$analysis_goal %||% "VCE"
    info <- kit_template_info(input$model_template)
    tpl_lab <- info$label %||% "T1 · Animal model"
    auto <- isTRUE(values$auto_update)

    div(class = paste("model-summary", if (empty) "is-placeholder"),
      div(class = "model-formula",
        span(class = "tex", `data-tex` = tex, plain)
      ),
      div(class = "model-meta",
        span(class = "meta-pill", if (identical(method, "VCE")) "VCE · REML" else "BLUP · EBV"),
        span(class = "meta-pill", tpl_lab),
        span(class = "meta-pill", if (use_h) "H (ssGBLUP)" else "A (pedigree)"),
        span(class = paste("meta-pill", if (!auto) "meta-manual"), if (auto) "Auto update" else "Manual lock")
      )
    )
  })

  conversion_files <- reactiveValues(
    blup_txt = NULL,
    blup_map = NULL,
    blup_bim = NULL,
    backend = NULL
  )
  
  cleanup_conversion_files <- function() {
    if (!is.null(conversion_files$blup_txt) && file.exists(conversion_files$blup_txt)) {
      unlink(conversion_files$blup_txt, force = TRUE)
    }
    if (!is.null(conversion_files$blup_map) && file.exists(conversion_files$blup_map)) {
      unlink(conversion_files$blup_map, force = TRUE)
    }
    if (!is.null(conversion_files$blup_bim) && file.exists(conversion_files$blup_bim)) {
      unlink(conversion_files$blup_bim, force = TRUE)
    }
    conversion_files$blup_txt <- NULL
    conversion_files$blup_map <- NULL
    conversion_files$blup_bim <- NULL
    conversion_files$backend <- NULL
  }
  
  # Calculate animal effect number reactively
  animal_effect_number <- reactive({
    n_fixed <- length(values$fixed) + length(values$cov)
    n_random <- length(values$random)
    animal_effect_num <- n_fixed + n_random + 1
    return(animal_effect_num)
  })
  
  # Dynamic UI: show animal effect number next to store_accuracy checkboxes
  output$store_accuracy_ui <- renderUI({
    an <- animal_effect_number()
    checkboxInput(
      "opt_store_accuracy",
      kit_help_label("store_accuracy", paste0("store_accuracy (Animal effect: ", an, ")"), lang()),
      value = FALSE
    )
  })
  output$store_accuracy_orig_ui <- renderUI({
    an <- animal_effect_number()
    checkboxInput(
      "opt_store_accuracy_orig",
      kit_help_label("store_accuracy", paste0("store_accuracy with original ID (Animal effect: ", an, ")"), lang()),
      value = TRUE
    )
  })
  
  # Phenotype reader: header only (column names for mapping; no full data load)
  data <- reactive({
    req(input$pheno_file)

    path <- input$pheno_file$datapath
    ext  <- tolower(tools::file_ext(input$pheno_file$name))

    header_df <- function(cols) {
      cols <- as.character(cols)
      cols <- cols[nzchar(trimws(cols))]
      if (!length(cols)) return(NULL)
      # Empty 0-row frame keeps colnames() usable for selectize / column indices
      as.data.frame(setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                    stringsAsFactors = FALSE, check.names = FALSE)
    }

    parse_header_line <- function(line, sep) {
      if (is.null(line) || !nzchar(line)) return(character(0))
      if (identical(sep, "")) {
        toks <- strsplit(trimws(line), "\\s+")[[1]]
      } else {
        toks <- strsplit(line, sep, fixed = TRUE)[[1]]
      }
      toks <- trimws(toks)
      toks <- sub('^\uFEFF', "", toks)
      gsub('^["\']|["\']$', "", toks)
    }

    tryCatch({
      if (ext %in% c("xlsx", "xls")) {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          showNotification("readxl package not installed. Please install readxl or upload CSV/TXT.", type = "warning", duration = 8)
          return(NULL)
        }
        df0 <- readxl::read_excel(path, n_max = 0)
        return(header_df(names(df0)))
      }

      first_line <- readLines(path, n = 1L, warn = FALSE)
      if (!length(first_line)) return(NULL)

      if (ext == "csv" || grepl(",", first_line, fixed = TRUE)) {
        return(header_df(parse_header_line(first_line, ",")))
      }
      if (grepl("\t", first_line, fixed = TRUE)) {
        return(header_df(parse_header_line(first_line, "\t")))
      }
      if (grepl(";", first_line, fixed = TRUE)) {
        return(header_df(parse_header_line(first_line, ";")))
      }
      # Whitespace-delimited (BLUPF90-style)
      header_df(parse_header_line(first_line, ""))
    }, error = function(e) {
      showNotification(paste0("Header read failed: ", e$message), type = "error", duration = 8)
      NULL
    })
  })
  
  # Update select choices when data is available
  observe({
    req(data())
    vars <- colnames(data())
    updateSelectizeInput(session, "traits_select", choices = vars, server = TRUE)
    updateSelectizeInput(session, "fixed_select", choices = vars, server = TRUE)
    # Best-effort guess of the animal ID column so the card is useful right away
    guess <- vars[grepl("^(animal|anim|id|animal_?id|iid|ear_?tag)$", vars, ignore.case = TRUE)]
    updateSelectizeInput(session, "animal_select", choices = vars,
                         selected = if (length(guess)) guess[1] else character(0), server = TRUE)
    updateSelectizeInput(session, "random_select", choices = vars, server = TRUE)
    updateSelectInput(session, "opt_hetres_pos", choices = vars)
    updateSelectInput(session, "opt_rr_position", choices = vars)
  })

  # Numbered column map: renumf90 is position based, so show position + role
  output$column_map_ui <- renderUI({
    df <- tryCatch(data(), error = function(e) NULL)
    if (is.null(df)) {
      return(NULL)
    }
    vars <- colnames(df)
    role_of <- function(v) {
      if (v %in% values$traits) "trait"
      else if (v %in% values$animal) "animal"
      else if (v %in% values$fixed) "fixed"
      else if (v %in% values$random) "random"
      else "unused"
    }
    chips <- lapply(seq_along(vars), function(i) {
      r <- role_of(vars[i])
      span(class = paste("col-chip", paste0("role-", r)), title = paste0("Column ", i, " · ", r),
           span(class = "col-idx", i), vars[i])
    })
    div(class = "column-map",
      div(class = "column-map-head",
        span(paste0(length(vars), " columns")),
        span(class = "column-legend",
          span(class = "col-chip role-trait", "trait"),
          span(class = "col-chip role-fixed", "fixed"),
          span(class = "col-chip role-random", "random"),
          span(class = "col-chip role-animal", "animal")
        )
      ),
      div(class = "column-chips", chips)
    )
  })
  
  # Map select inputs to reactive values (a cleared single selectize sends "")
  clean_sel <- function(x) {
    x <- as.character(x %||% character(0))
    x[nzchar(x)]
  }
  observeEvent(input$traits_select, {
    values$traits <- clean_sel(input$traits_select)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$fixed_select, {
    values$fixed <- clean_sel(input$fixed_select)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$animal_select, {
    values$animal <- clean_sel(input$animal_select)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$random_select, {
    values$random <- clean_sel(input$random_select)
  }, ignoreNULL = FALSE)

  # Reflect accepted AI/manual cards without regenerating their matrices/options.
  sync_card_controls <- function(text, notify = FALSE) {
    df <- tryCatch(data(), error = function(e) NULL)
    if (is.null(df)) {
      if (notify) showNotification("Upload the matching phenotype header to sync column selections.", type = "message")
      return(invisible(NULL))
    }
    parsed <- kit_card_mapping(text, colnames(df))
    for (role in c("traits", "fixed", "random", "animal")) {
      selection <- parsed[[role]]
      if (is.null(selection)) next
      id <- paste0(role, "_select")
      freezeReactiveValue(input, id)
      values[[role]] <- selection
      updateSelectizeInput(session, id, choices = colnames(df), selected = selection, server = TRUE)
    }
    if (!is.null(parsed$fixed)) values$cov <- character()
    if (!is.null(parsed$optional)) {
      for (flag in c("pe", "mat", "mpe"))
        updateCheckboxInput(session, paste0("opt_", flag), value = flag %in% parsed$optional)
    }
    if (notify && length(parsed$notes))
      showNotification(paste(parsed$notes, collapse = " "), type = "warning", duration = 10)
    invisible(parsed)
  }

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
    return(paste(matrix_rows, collapse = "
"))
  }
  
  # (Removed old combined h2+rg generator to avoid duplicate rg lines in h2 section)
  
  # Precise strategy: parse renf90.par to detect indices
  parse_renf90_par <- function(txt) { # nolint object_usage_linter
    if (is.null(txt) || !nzchar(txt)) return(NULL)
    lines <- unlist(strsplit(txt, "
"))
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
  # Current helper order: animal, maternal, PE, MPE; verify against renf90.par.
  # Return effect indices for the current generation order.
  infer_effect_indices <- function(n_fixed, n_user_random, opt_mat, opt_pe, opt_mpe) {
    # Animal effect follows the user-specified random effects.
    base_animal <- n_fixed + n_user_random + 1
    idx <- list(a = base_animal, m = NULL, p = NULL, mp = NULL)
    
    # Maternal effect follows the animal effect.
    if (opt_mat) idx$m <- idx$a + 1
    
    # PE follows the animal or maternal effect in this helper.
    if (opt_pe) {
      last_idx <- if (!is.null(idx$m)) idx$m else idx$a
      idx$p <- last_idx + 1
    }
    
    # MPE is assigned after PE in this helper; validate model requirements.
    if (opt_mpe) {
      # MPE can exist without MAT - it follows after pe or animal/mat
      last_idx <- if (!is.null(idx$p)) idx$p else if (!is.null(idx$m)) idx$m else idx$a
      idx$mp <- last_idx + 1
    }
    
    # Collect all random-effect indices.
    user_rand_idx <- if (n_user_random > 0) (seq_len(n_user_random) + n_fixed) else integer(0)
    idx$random_indices <- sort(c(user_rand_idx, unlist(idx)))
    
    eb_log(sprintf(
      "Effect index mapping → fixed=%d, random=%d, animal=%d, mat=%s, pe=%s, mpe=%s",
      n_fixed, n_user_random, idx$a,
      ifelse(is.null(idx$m), "NA", idx$m),
      ifelse(is.null(idx$p), "NA", idx$p),
      ifelse(is.null(idx$mp), "NA", idx$mp)
    ), level = "debug")
    
    return(idx)
  }
  
  emit_h2_no_mat <- function(a_idx, rand_idx, t) {
    sprintf("OPTION se_covar_function H2_%d G_%d_%d_%d_%d/(%s)", t, a_idx, a_idx, t, t, build_denominator(rand_idx, t))
  }
  emit_h2_with_mat <- function(a_idx, m_idx, rand_idx, t) {
    # Numerator: G_aa + 1.5*G_am + 0.5*G_mm.
    num_t <- sprintf("(G_%d_%d_%d_%d+1.5*G_%d_%d_%d_%d+0.5*G_%d_%d_%d_%d)",
                     a_idx, a_idx, t, t,
                     a_idx, m_idx, t, t,
                     m_idx, m_idx, t, t)
    # Current denominator: sum of random variances + 2*G_am + R; verify the model convention.
    rand_terms <- paste0("G_", rand_idx, "_", rand_idx, "_", t, "_", t)
    denom <- paste(c(rand_terms,
                     sprintf("2*G_%d_%d_%d_%d", a_idx, m_idx, t, t),
                     sprintf("R_%d_%d", t, t)), collapse = "+")
    paste(
      sprintf("OPTION se_covar_function H2t_%d %s/(%s)", t, num_t, denom),
      sprintf("OPTION se_covar_function H2d_%d G_%d_%d_%d_%d/(%s)", t, a_idx, a_idx, t, t, denom),
      sep = "
"
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
    paste(lines, collapse = "
")
  }

  # Build phenotypic correlation block (rp) across traits
  generate_rp_block <- function(n_traits, a_idx, m_idx = NULL, p_idx = NULL,
                                include_maternal = FALSE, include_pe = FALSE) {
    if (n_traits <= 1) return("")
    lines <- c()
    for (i in 1:(n_traits - 1)) {
      for (j in (i + 1):n_traits) {
        # Covariance numerator: additive, optional maternal/PE, cross terms and residual.
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
        
        # Denominator uses each trait variance and optional maternal/PE terms; verify cross-term coefficients.
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
    paste(lines, collapse = "
")
  }
  
  # Helpers to map selected variables to column indices
  get_col_num_int <- function(vars) {
    if (length(vars) == 0 || is.null(data())) return(integer(0))
    idx <- match(vars, colnames(data()))
    idx[!is.na(idx)]
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
      "# PARAMETER FILE for renumf90
#
DATAFILE
",
      basename(input$pheno_file$name), "
",
      "SKIP_HEADER
1
TRAITS # Specify trait columns
",
      if (length(values$traits) > 0) get_col_num(values$traits) else "# Add trait column numbers here",
      "
FIELDS_PASSED TO OUTPUT

WEIGHT(S)

RESIDUAL_VARIANCE
",
      generate_covariance_matrix(length(values$traits)), "
"
    )
    
    # Fixed effects (header-only: default to cross alpha; user can edit card if cov needed)
    if (length(values$fixed) > 0) {
      for (eff in values$fixed) {
        effect_type <- "cross"
        n_traits <- length(values$traits)
        effect_cols <- format_effect_cols(eff, n_traits)
        if (nzchar(effect_cols)) {
          param_text <- paste0(param_text, "EFFECT
", effect_cols, " ", effect_type)
          if (effect_type == "cross") {
            param_text <- paste0(param_text, " alpha")
          }
          param_text <- paste0(param_text, " # ", eff, " fixed effect
")
        }
      }
    }

    # Covariates (linear regression): "EFFECT / cols cov" — nothing after cov
    if (length(values$cov) > 0) {
      for (eff in values$cov) {
        effect_cols <- format_effect_cols(eff, length(values$traits))
        if (nzchar(effect_cols)) {
          param_text <- paste0(param_text, "EFFECT
", effect_cols, " cov # ", eff, " covariate
")
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
                               "EFFECT
",
                               effect_cols, " cross alpha # ", eff, " random effect
",
                               "RANDOM
",
                               "diagonal # Random effects section
",
                               "(CO)VARIANCES
",
                               generate_covariance_matrix(n_traits), "
")
        }
      }
    }
    
    # Animal effect
    if (length(values$animal) > 0) {
      n_traits <- length(values$traits)
      effect_cols <- format_effect_cols(values$animal, n_traits)
      if (nzchar(effect_cols)) {
        param_text <- paste0(param_text, "EFFECT
", effect_cols, " cross alpha # Animal ID effect
")
      }
      param_text <- paste0(param_text, "RANDOM
animal # Animal random effect
")
      
      # OPTIONAL order matches skill renf90 layout preference: mat, mpe, pe
      optional_effects <- c()
      if (isTRUE(input$opt_mat)) optional_effects <- c(optional_effects, "mat")
      if (isTRUE(input$opt_mpe)) optional_effects <- c(optional_effects, "mpe")
      if (isTRUE(input$opt_pe)) optional_effects <- c(optional_effects, "pe")
      
      if (length(optional_effects) > 0) {
        param_text <- paste0(param_text, "OPTIONAL
", paste(optional_effects, collapse = " "), "
")
      }

      # T7 random regression
      if (isTRUE(input$opt_use_rr)) {
        rr_type <- trimws(input$opt_rr_type %||% "legendre")
        if (!nzchar(rr_type)) rr_type <- "legendre"
        rr_k <- input$opt_rr_order %||% 3
        param_text <- paste0(param_text, "RANDOM_REGRESSION
", rr_type, " # order k=", rr_k, "
")
        if (!is.null(input$opt_rr_position) && nzchar(input$opt_rr_position)) {
          rr_col <- which(colnames(data()) %in% input$opt_rr_position)[1]
          if (!is.na(rr_col)) {
            param_text <- paste0(param_text, "RR_POSITION
", rr_col, "
")
          }
        }
      }
      
      omit_ped <- isTRUE(input$opt_omit_pedigree)
      if (!omit_ped) {
        pedigree_filename <- if (has_ped) basename(input$ped_file$name) else "pedigree.txt"
        upg <- trimws(input$opt_upg_type %||% "")
        file_pos <- if (nzchar(upg) && identical(upg, "yob")) {
          "1 2 3 0 5 # animal sire dam alt_dam birth_year"
        } else {
          "1 2 3 # Progeny Sire Dam"
        }
        param_text <- paste0(param_text, "FILE
", pedigree_filename, "
FILE_POS
", file_pos, "
")
        if (nzchar(upg)) {
          param_text <- paste0(param_text, "UPG_TYPE
", upg, "
")
        }
      }

      need_geno <- has_geno || isTRUE(kit_template_info(input$model_template)$geno)
      if (need_geno) {
        geno_format <- input$geno_format %||% "plink"
        if (geno_format == "blupf90" || omit_ped) {
          snp_filename <- if (has_geno && !is.null(input$geno_file$name) && length(input$geno_file$name) > 0) {
            basename(input$geno_file$name[1])
          } else {
            "snp_marker.txt"
          }
          param_text <- paste0(param_text, "SNP_FILE
", snp_filename, " ## SNP marker file
")
        } else if (has_geno) {
          plink_prefix <- derive_plink_prefix(input$geno_file$name)
          if (!is.null(plink_prefix) && nzchar(plink_prefix)) {
            param_text <- paste0(param_text, "PLINK_FILE
", plink_prefix, " # Genotype file name
")
          }
        }
      }
        
      if (!omit_ped && !isTRUE(input$opt_ped_search_complete)) {
        if (isTRUE(input$opt_use_ped_depth) && !is.null(input$opt_ped_depth) && !is.na(input$opt_ped_depth)) {
          param_text <- paste0(param_text, "PED_DEPTH
", input$opt_ped_depth, "
")
        }
      }
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
    
    has_animal <- length(values$animal) > 0
    if (has_animal && n_traits > 0) {
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
      param_text <- paste0(param_text, "(CO)VARIANCES
", paste(cov_lines, collapse = "
"), "
")
    } else if (has_animal) {
      param_text <- paste0(param_text, "(CO)VARIANCES
1
")
    }

    # PE (Permanent Environmental) covariance matrix - works for single or multiple traits
    if (has_animal && isTRUE(input$opt_pe)) {
      if (n_traits > 0) {
        if (n_traits == 1) {
          # Single trait: output single value (not matrix)
          param_text <- paste0(param_text, "(CO)VARIANCES_PE
0.001
")
        } else {
          # Multiple traits: generate matrix
          pe_lines <- c()
          for (i in 1:n_traits) {
            row_vals <- rep("0.001", n_traits)
            row_vals[i] <- "1"
            pe_lines <- c(pe_lines, paste(row_vals, collapse = " "))
          }
          param_text <- paste0(param_text, "(CO)VARIANCES_PE
", paste(pe_lines, collapse = "
"), "
")
        }
      } else {
        # Fallback when no traits selected
        param_text <- paste0(param_text, "(CO)VARIANCES_PE
0.001
")
      }
    }
    # MPE (Maternal Permanent Environmental) covariance matrix - works for single or multiple traits
    if (has_animal && isTRUE(input$opt_mpe)) {
      if (n_traits > 0) {
        if (n_traits == 1) {
          # Single trait: output single value (not matrix)
          param_text <- paste0(param_text, "(CO)VARIANCES_MPE
0.003
")
        } else {
          # Multiple traits: generate matrix
          mpe_lines <- c()
          for (i in 1:n_traits) {
            row_vals <- rep("0.001", n_traits)
            row_vals[i] <- "1"
            mpe_lines <- c(mpe_lines, paste(row_vals, collapse = " "))
          }
          param_text <- paste0(param_text, "(CO)VARIANCES_MPE
", paste(mpe_lines, collapse = "
"), "
")
        }
      } else {
        # Fallback when no traits selected
        param_text <- paste0(param_text, "(CO)VARIANCES_MPE
0.003
")
      }
    }
    
    param_text <- paste0(param_text, "
")
    
    # Add user-selected OPTION parameters
    options <- c()
    if (isTRUE(input$opt_remove_all_missing)) options <- c(options, "OPTION remove_all_missing")
    if (isTRUE(input$opt_missing_in_weights)) options <- c(options, "OPTION missing_in_weights")
    if (isTRUE(input$opt_no_basic_statistics)) options <- c(options, "OPTION no_basic_statistics")
    # Missing value symbol
    mv <- input$opt_missing_value
    if (is.null(mv) || !nzchar(mv)) mv <- "-999"
    options <- c(options, paste("OPTION missing", mv))
    
    # Method: BLUP or VCE
    is_vce <- identical(input$opt_method, "VCE")
    options <- c(options, if (is_vce) "OPTION method VCE" else "OPTION method BLUP")
    if (isTRUE(input$opt_sol_se)) options <- c(options, "OPTION sol se")
    # Multiple-trait BLUP converges much faster with a block preconditioner
    if (!is_vce && n_traits > 1) options <- c(options, paste("OPTION blksize", n_traits))
    # VCE-only controls are hidden in the UI for BLUP, so do not emit them either
    if (is_vce) {
      if (!is.null(input$opt_conv_crit_val) && nzchar(input$opt_conv_crit_val)) options <- c(options, paste("OPTION conv_crit", input$opt_conv_crit_val))
      # EM-REML variants are mutually exclusive: pure EM / EM then AI to convergence / n EM rounds then AI
      if (isTRUE(input$opt_em_reml_pure)) {
        options <- c(options, "OPTION EM-REML pure")
      } else if (isTRUE(input$opt_em_reml_ai_conv)) {
        options <- c(options, "OPTION EM-REML AI conv")
      } else if (!is.null(input$opt_em_reml_rounds) && !is.na(input$opt_em_reml_rounds)) {
        options <- c(options, paste("OPTION EM-REML", input$opt_em_reml_rounds))
      }
      if (!is.null(input$opt_maxrounds_val) && !is.na(input$opt_maxrounds_val)) options <- c(options, paste("OPTION maxrounds", input$opt_maxrounds_val))
    }
    if (isTRUE(input$opt_use_yams)) options <- c(options, "OPTION use_yams")
    tg <- trimws(input$opt_tunedG %||% "")
    if (nzchar(tg)) {
      if (identical(tg, "9")) {
        a <- suppressWarnings(as.numeric(input$opt_tunedG_a))
        b <- suppressWarnings(as.numeric(input$opt_tunedG_b))
        if (is.na(a)) a <- 0
        if (is.na(b)) b <- 1
        options <- c(options, paste("OPTION tunedG 9", a, b))
      } else {
        options <- c(options, paste("OPTION tunedG", tg))
      }
    }
    if (isTRUE(input$opt_residual_out)) options <- c(options, "OPTION residual")
    if (isTRUE(input$opt_stdresidual_out)) options <- c(options, "OPTION stdresidual")
    if (isTRUE(input$opt_prior_solutions)) options <- c(options, "OPTION prior_solutions")
    if (isTRUE(input$opt_origID)) options <- c(options, "OPTION origID")
    
    if (isTRUE(input$opt_store_accuracy) || isTRUE(input$opt_store_accuracy_orig)) {
      an <- animal_effect_number()
      line <- paste0("OPTION store_accuracy ", an)
      if (isTRUE(input$opt_store_accuracy_orig)) line <- paste(line, "orig")
      options <- c(options, line)
    }
    if (!is.null(input$opt_acctype) && nzchar(input$opt_acctype) && identical(input$opt_acctype, "0.5")) {
      options <- c(options, "OPTION acctype 0.5")
    }
    if (isTRUE(input$opt_correct_acc_inb_direct0)) options <- c(options, "OPTION correct_accuracy_by_inbreeding_direct 0")
    
    # Heterogeneous residuals
    if (!is.null(input$opt_hetres_pos) && length(input$opt_hetres_pos) > 0) {
      pos_cols <- which(colnames(data()) %in% input$opt_hetres_pos)
      if (length(pos_cols) > 0) options <- c(options, paste("OPTION hetres_pos", paste(pos_cols, collapse = " ")))
    }
    if (!is.null(input$opt_hetres_pol_preset) && nzchar(input$opt_hetres_pol_preset)) {
      if (input$opt_hetres_pol_preset != "0.1 0.01") {
        options <- c(options, paste("OPTION hetres_pol", input$opt_hetres_pol_preset))
      }
    }
    
    # Genomic / ssGBLUP / GBLUP
    if (isTRUE(input$opt_save_clean_snps)) options <- c(options, "OPTION saveCleanSNPs")
    if (isTRUE(input$opt_snp_p_value)) options <- c(options, "OPTION snp_p_value")
    if (isTRUE(input$opt_omit_ainv)) options <- c(options, "OPTION omit_ainv")
    mf <- trimws(input$opt_map_file %||% "")
    if (nzchar(mf)) options <- c(options, paste("OPTION map_file", mf))
    ab <- trimws(input$opt_AlphaBeta %||% "")
    if (nzchar(ab)) options <- c(options, paste("OPTION AlphaBeta", ab))
    to <- trimws(input$opt_TauOmega %||% "")
    if (nzchar(to)) options <- c(options, paste("OPTION TauOmega", to))
    if (isTRUE(input$opt_read_gim_a22i)) options <- c(options, "OPTION readGimA22i")

    # Gibbs / threshold
    if (isTRUE(input$opt_gibbs_enable)) {
      catv <- trimws(input$opt_gibbs_cat %||% "")
      if (nzchar(catv)) options <- c(options, paste("OPTION cat", catv))
      thr <- trimws(input$opt_gibbs_thresholds %||% "")
      if (nzchar(thr)) options <- c(options, paste("OPTION thresholds", thr))
      if (!is.null(input$opt_gibbs_seed) && !is.na(input$opt_gibbs_seed)) {
        options <- c(options, paste("OPTION seed", input$opt_gibbs_seed))
      }
      if (isTRUE(input$opt_gibbs_solution_mean)) options <- c(options, "OPTION solution mean")
      if (isTRUE(input$opt_gibbs_fixed_var)) options <- c(options, "OPTION fixed_var all")
    }
    
    # RENUMF90 options
    if (isTRUE(input$opt_ped_search_complete)) options <- c(options, "OPTION ped_search complete")
    if (!is.null(input$opt_inbreeding_method) && nzchar(input$opt_inbreeding_method)) {
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
    
    param_text <- paste0(param_text, paste(options, collapse = "
"))

    # Append se_covar_function block automatically for VCE
    if (!is.null(input$opt_method) && input$opt_method == "VCE" && isTRUE(input$opt_auto_se_covar)) {
      n_traits <- length(values$traits)
      if (n_traits > 0 && has_animal) {
        # Infer indices based on optional effects ordering
        n_fixed <- length(values$fixed) + length(values$cov)
        n_user_random <- length(values$random)
        
        # Infer effect indices (MPE can exist without MAT)
        idx_result <- tryCatch({
          idx <- infer_effect_indices(n_fixed, n_user_random, isTRUE(input$opt_mat), isTRUE(input$opt_pe), isTRUE(input$opt_mpe))
          list(success = TRUE, idx = idx)
        }, error = function(e) {
          # Handle any other errors
          showNotification(paste("Could not determine effect indices:", e$message), type = "warning", duration = 5)
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
          param_text <- paste0(param_text, "

# === Heritability Calculation (auto) ===
", paste(out_lines, collapse = "
"), "
")
          
          # Append genetic correlation block
          rg_block <- generate_rg_block(n_traits, a_idx, m_idx, include_maternal = !is.null(m_idx) && isTRUE(input$opt_mat))
          if (nzchar(rg_block)) {
            param_text <- paste0(param_text, "
# === Genetic Correlation Calculation (auto) ===
", rg_block, "
")
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
            param_text <- paste0(param_text, "
# === Phenotypic Correlation Calculation (auto) ===
", rp_block, "
")
          }
          
          # Always add sampling/output controls (once at the end)
          param_text <- paste0(param_text, "
OPTION samples_se_covar_function 10000
OPTION out_se_covar_function
")
        }
      }
    }
    } # end else (has phenotype data)
    
    values$default_param <- param_text
    if (isTRUE(values$auto_update)) {
      values$current_param <- param_text
      session$sendCustomMessage("update_textarea", list(content = values$current_param))
    }
  })
  
  # Reset button
  observeEvent(input$reset_param, {
    values$current_param <- values$default_param
    session$sendCustomMessage("update_textarea", list(content = values$current_param))
    showNotification("Parameter card reset to the generated version", type = "message", duration = 2)
  })

  # Editor -> server: keep current_param in sync with what the user actually sees
  observeEvent(input$param_editor_text, {
    edited <- input$param_editor_text %||% ""
    if (!identical(edited, values$current_param)) {
      values$auto_update <- FALSE
      updateRadioButtons(session, "param_edit_mode", selected = "manual")
      values$current_param <- edited
      sync_card_controls(edited)
    }
  }, ignoreInit = TRUE)

  # Typing into the card while in Auto mode would be overwritten by the next
  # regeneration; switch to Manual lock so hand edits are kept.
  observeEvent(input$param_editor_user_edit, {
    if (isTRUE(values$auto_update) && !identical(input$param_editor_text %||% "", values$default_param)) {
      values$auto_update <- FALSE
      updateRadioButtons(session, "param_edit_mode", selected = "manual")
      showNotification("Switched to Manual lock to keep your edits. Use Reset or Auto update to regenerate.",
                       type = "message", duration = 5)
    }
  }, ignoreInit = TRUE)

  # ---- Live checks (shared by the status strip and the Validate button) ----
  card_checks <- reactive({
    issues <- list()
    add <- function(level, msg) issues[[length(issues) + 1]] <<- list(level = level, msg = msg)
    has_file <- function(x) !is.null(x) && is.data.frame(x) && nrow(x) > 0

    if (!has_file(input$pheno_file)) {
      add("todo", "Upload phenotype data")
      return(issues)
    }
    if (!length(values$traits)) add("todo", "Select at least one trait")
    if (!length(values$fixed)) add("warn", "No fixed class: add an intercept if needed")
    if (!length(values$animal)) {
      add("todo", "Pick the animal ID column (needed for RANDOM animal)")
    } else if (!has_file(input$ped_file)) {
      add("warn", "No pedigree uploaded — card uses placeholder FILE pedigree.txt")
    }

    roles <- list(trait = values$traits, fixed = values$fixed, cov = values$cov,
                  random = values$random, animal = values$animal)
    all_cols <- unlist(roles, use.names = FALSE)
    dup <- unique(all_cols[duplicated(all_cols)])
    if (length(dup)) add("bad", paste0("Column used in two roles: ", paste(dup, collapse = ", ")))

    if (isTRUE(input$opt_mpe) && !isTRUE(input$opt_mat)) add("bad", "OPTIONAL mpe requires mat in renumf90")
    if ((isTRUE(input$opt_pe) || isTRUE(input$opt_mat) || isTRUE(input$opt_mpe)) && !length(values$animal)) {
      add("warn", "OPTIONAL pe/mat/mpe only apply together with the animal effect")
    }
    info <- kit_template_info(input$model_template)
    if (isTRUE(info$geno) && !has_file(input$geno_file)) {
      add("bad", paste0(info$label, " needs genotype files uploaded"))
    }
    if (isTRUE(info$omit_ped) && has_file(input$ped_file) && !isTRUE(input$opt_omit_pedigree)) {
      add("warn", "T9 GBLUP usually omits pedigree FILE — enable “Omit pedigree FILE”")
    }
    if (isTRUE(info$rr) && !isTRUE(input$opt_use_rr)) {
      add("warn", "T7 template selected — enable Random regression block (category 7)")
    }
    if (isTRUE(info$upg) && !nzchar(trimws(input$opt_upg_type %||% ""))) {
      add("warn", "T6 UPG — set UPG_TYPE under Pedigree · UPG")
    }
    if (has_file(input$geno_file) && identical(input$geno_format %||% "plink", "plink")) {
      exts <- tolower(tools::file_ext(input$geno_file$name))
      miss <- setdiff(c("bed", "bim", "fam"), exts)
      if (length(miss)) add("warn", paste0("PLINK set incomplete — missing .", paste(miss, collapse = ", .")))
    }
    mv <- trimws(input$opt_missing_value %||% "")
    if (nzchar(mv) && !grepl("^-?[0-9]+$", mv)) add("bad", "OPTION missing must be an integer (e.g. -999)")

    txt <- values$current_param %||% ""
    if (grepl("\t", txt, fixed = TRUE)) add("bad", "TAB characters in card — use spaces only")
    if (!isTRUE(values$auto_update) && !identical(txt, values$default_param)) {
      add("info", "Manual edits active — card no longer follows the model selections")
    }
    issues
  })

  output$card_checks_ui <- renderUI({
    issues <- card_checks()
    if (!length(issues)) {
      return(div(class = "card-checks", span(class = "check-chip check-ok", "✓ Basic checks passed")))
    }
    div(class = "card-checks",
      lapply(issues, function(i) span(class = paste("check-chip", paste0("check-", i$level)), i$msg))
    )
  })

  # Keyword glossary for the Explain view (renumf90 instruction-file semantics)
  renum_glossary <- c(
    "DATAFILE" = "Raw phenotype file; renumf90 recodes it into renf90.dat.",
    "SKIP_HEADER" = "Number of header lines to skip in the data file.",
    "TRAITS" = "Column position(s) of the trait(s); one per trait, in this order.",
    "FIELDS_PASSED TO OUTPUT" = "Columns copied unchanged to the end of renf90.dat (e.g. original IDs).",
    "WEIGHT(S)" = "Weight column; empty line = no weights.",
    "RESIDUAL_VARIANCE" = "Full symmetric t × t residual matrix in TRAITS order. Known values for BLUP; starting values for REML/Gibbs. Use a positive-definite matrix; no inline matrix comments.",
    "EFFECT" = "One model effect: column per trait + cross alpha (class) or cov (covariate).",
    "RANDOM" = "Makes the preceding EFFECT random: diagonal (iid) or animal (pedigree).",
    "OPTIONAL" = "Extra effects attached to the animal: pe, mat, mpe (mpe needs mat).",
    "FILE" = "Pedigree file (animal sire dam, unknown parent = 0).",
    "FILE_POS" = "Pedigree columns: animal sire dam [alt_dam birth_year].",
    "SNP_FILE" = "Genotype file for ssGBLUP; renumf90 writes <file>_XrefID.",
    "PLINK_FILE" = "PLINK prefix for genotypes (needs a renumf90 build with PLINK support).",
    "PED_DEPTH" = "Generations traced back from animals with records (0 = whole file).",
    "(CO)VARIANCES" = "Full covariance matrix of the preceding random effect (not always genetic). Maternal: direct traits then maternal traits, 2t × 2t; RR needs expanded dimensions. Zero starts can constrain estimation.",
    "(CO)VARIANCES_PE" = "Permanent-environment (co)variances (t × t).",
    "(CO)VARIANCES_MPE" = "Maternal permanent-environment (co)variances (t × t).",
    "NESTED" = "Nesting class positions for covariates; one per trait.",
    "GEN_INT" = "Minimum, average and maximum generation intervals; required for birth-year UPG.",
    "REC_SEX" = "Parent sex with records, for pedigree checks.",
    "UPG_TYPE" = "Unknown-parent grouping scheme; yob also needs year boundaries and GEN_INT; group needs FILE_POS group columns.",
    "DEFAULT_GROUP" = "Fallback groups for animals absent from the pedigree.",
    "INBREEDING" = "Inbreeding source: pedigree, file, self or no-inbreeding.",
    "RANDOM_REGRESSION" = "Random regression basis: data or legendre; expand G and PE matrices to match basis coefficients.",
    "FIXED_REGRESSION" = "Fixed regression basis definition.",
    "RR_POSITION" = "For legendre: order and column; for data: basis-column list.",
    "OPTION" = "Program option — used by renumf90 or copied into renf90.par."
  )

  explain_card <- function(txt) {
    lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
    rows <- list()
    i <- 1
    while (i <= length(lines)) {
      raw <- lines[i]
      code <- trimws(sub("#.*$", "", raw))
      comment <- if (grepl("#", raw, fixed = TRUE)) trimws(sub("^[^#]*#+", "", raw)) else ""
      if (!nzchar(code)) { i <- i + 1; next }
      if (grepl("^OPTION(?:\\s|$)", code, perl = TRUE)) {
        value <- sub("^OPTION\\s+", "", code)
        option <- strsplit(value, "\\s+")[[1]][1]
        help_id <- unname(KIT_OPTION_HELP[option])
        meaning <- if (length(help_id) && !is.na(help_id)) kit_help_text(help_id, lang()) else
          "Program-specific option: verify syntax and support in the executable documentation."
        if (nzchar(comment)) meaning <- paste(meaning, comment, sep = " — ")
        rows[[length(rows) + 1]] <- list(key = "OPTION", value = value, meaning = meaning)
        i <- i + 1; next
      }
      if (code %in% names(renum_glossary)) {
        # Collect value lines until the next keyword
        vals <- character(0)
        j <- i + 1
        while (j <= length(lines)) {
          nxt <- trimws(sub("#.*$", "", lines[j]))
          if (nxt %in% names(renum_glossary) || grepl("^OPTION(?:\\s|$)", nxt, perl = TRUE)) break
          if (nzchar(nxt)) vals <- c(vals, nxt)
          if (grepl("#", lines[j], fixed = TRUE) && !nzchar(comment)) comment <- trimws(sub("^[^#]*#+", "", lines[j]))
          j <- j + 1
        }
        meaning <- renum_glossary[[code]]
        if (nzchar(comment)) meaning <- paste0(meaning, " (", comment, ")")
        rows[[length(rows) + 1]] <- list(key = code, value = paste(vals, collapse = " / "), meaning = meaning)
        i <- j; next
      }
      i <- i + 1
    }
    rows
  }

  observeEvent(input$explain_param, {
    txt <- values$current_param %||% ""
    if (!nzchar(txt) || grepl("Please upload", txt, fixed = TRUE)) {
      showNotification("Nothing to explain yet — generate a card first.", type = "warning")
      return()
    }
    rows <- explain_card(txt)
    showModal(modalDialog(
      title = "Parameter card — line by line",
      div(class = "explain-table-wrap",
        tags$table(class = "explain-table",
          tags$thead(tags$tr(tags$th("Keyword"), tags$th("Value"), tags$th("Meaning"))),
          tags$tbody(lapply(rows, function(r) tags$tr(
            tags$td(tags$code(r$key)), tags$td(tags$code(r$value)), tags$td(r$meaning)
          )))
        )
      ),
      tags$p(class = "field-hint", style = "margin-top:10px !important;",
             "Workflow: renumf90 renum.par → inspect renf90.par and log counts → blupf90+ renf90.par (BLUP/REML) or gibbsf90+ renf90.par → postgibbsf90. This app creates instructions; it does not run or fully validate the statistical model."),
      easyClose = TRUE, size = "l", footer = modalButton("Close")
    ))
  })

  observeEvent(input$validate_param, {
    issues <- Filter(function(i) i$level != "info", card_checks())
    if (!length(issues)) {
      showNotification("Validate OK — no obvious structural issues found.", type = "message", duration = 5)
    } else {
      showModal(modalDialog(
        title = "Validate",
        tags$ul(class = "validate-list", lapply(issues, function(i) tags$li(class = paste0("check-", i$level), i$msg))),
        tags$p(class = "field-hint", "After running renumf90, always check renf90.par and the log counts (records, levels, pedigree animals)."),
        easyClose = TRUE, footer = modalButton("Close")
      ))
    }
  })

  # When enabling PED_DEPTH, initialize value to 0
  observeEvent(input$opt_use_ped_depth, {
    if (isTRUE(input$opt_use_ped_depth)) {
      updateNumericInput(session, "opt_ped_depth", value = 0)
    }
  }, ignoreInit = TRUE)

  # Download exactly what is in the editor (including manual edits)
  output$download_param_file <- downloadHandler(
    filename = function() {
      fn <- trimws(input$par_filename %||% "")
      fn <- gsub("[^A-Za-z0-9._-]", "_", fn)
      if (!nzchar(fn)) fn <- "renum.par"
      fn
    },
    content = function(file) {
      txt <- values$current_param %||% ""
      txt <- gsub("\r\n?", "\n", txt)
      if (!endsWith(txt, "\n")) txt <- paste0(txt, "\n")
      writeLines(txt, file, sep = "", useBytes = TRUE)
    },
    contentType = "text/plain"
  )

  # ====== Genotype format help: PLINK -> BLUPF90 conversion ======
  observeEvent(input$geno_format_help, {
    has_plinkr <- has_plinkr_blup_backend()
    has_r <- has_r_blup_backend_bfile()
    if (!has_plinkr && !has_r) {
      showNotification(
        if (tolower(lang()) == "zh") {
          "No conversion backend available."
        } else {
          "No conversion backend available."
        },
        type = "error", duration = 8
      )
      return(NULL)
    }

    if (tolower(lang()) == "zh") {
      backend_parts <- c(
        if (has_plinkr) "PLINK (preferred)",
        if (has_r) "R/snpStats (fallback)"
      )
      backend_text <- paste0("Available backends: ", paste(backend_parts, collapse = " + "))
    } else {
      backend_parts <- c(
        if (has_plinkr) "PLINK (preferred)",
        if (has_r) "R/snpStats (second choice)"
      )
      backend_text <- paste0("Available backends: ", paste(backend_parts, collapse = " + "))
    }
    
    showModal(
      modalDialog(
        title = div(
          "Genotype Format Helper",
          span(class = "geno-helper-arrow", "→"),
          span(style = "font-size: 0.9rem; font-weight: 500;", "PLINK → BLUPF90")
        ),
        size = "l",
        easyClose = TRUE,
        class = "geno-helper-modal",
        footer = tagList(
          actionButton("geno_convert_close", 
                      "Close", 
                      class = "btn-secondary"),
          actionButton("geno_convert_run", 
                      "🚀 Run Conversion", 
                      class = "btn-primary",
                      icon = NULL)
        ),
        tagList(
          div(class = "geno-helper-info-box",
            p(if (tolower(lang()) == "zh") {
              "Convert PLINK files to BLUPF90 format, then download the results."
            } else {
              "Need help? Use this tool to convert PLINK (.bed/.bim/.fam) to BLUPF90 (.txt/.map/.bim). Converted files will be saved directly to your selected output directory."
            }),
            p(style = "margin-bottom:0;color:var(--muted);font-size:0.9rem;", backend_text)
          ),
          
          div(style = "margin-bottom: 20px;",
            h5(style = "font-weight: 700; color: var(--ink); margin-bottom: 12px;",
               "📋 Conversion Direction"),
            p(style = "margin: 0; color: var(--muted);",
              if (tolower(lang()) == "zh") {
                "PLINK (.bed/.bim/.fam) → BLUPF90 (.txt/.map/.bim)"
              } else {
                "PLINK (.bed/.bim/.fam) → BLUPF90 (.txt/.map/.bim)"
              })
          ),
          
          div(class = "geno-helper-direction-card",
              h4("PLINK → BLUPF90"),
              p(if (tolower(lang()) == "zh") {
                "Upload matching .bed, .bim and .fam files with the same prefix."
              } else {
                "Upload matching .bed, .bim and .fam files (e.g. mydata.bed / mydata.bim / mydata.fam). After conversion, files will be saved to your selected output directory."
              }),
              
              div(class = "geno-helper-file-input-wrapper",
                fileInput("geno_help_plink_bed", 
                         label = tags$strong("PLINK .bed file"), 
                         accept = c(".bed"),
                         buttonLabel = "Choose File",
                         placeholder = "No file selected")
              ),
              div(class = "geno-helper-file-input-wrapper",
                fileInput("geno_help_plink_bim", 
                         label = tags$strong("PLINK .bim file"), 
                         accept = c(".bim"),
                         buttonLabel = "Choose File",
                         placeholder = "No file selected")
              ),
              div(class = "geno-helper-file-input-wrapper",
                fileInput("geno_help_plink_fam", 
                         label = tags$strong("PLINK .fam file"), 
                         accept = c(".fam"),
                         buttonLabel = "Choose File",
                         placeholder = "No file selected")
              ),
              
              div(class = "geno-helper-file-input-wrapper",
                p(style = "font-size: 0.85rem; color: var(--muted); margin-top: 8px; margin-bottom: 0;",
                  if (tolower(lang()) == "zh") {
                    "Download the BLUPF90 files after conversion."
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
        style = "margin-top: 10px;",
        if (!is.null(conversion_files$backend) && nzchar(conversion_files$backend)) {
          div(
            style = "font-size:0.85rem;color:var(--muted);margin-bottom:6px;",
            if (tolower(lang()) == "zh") {
              paste0("Conversion complete (backend: ", conversion_files$backend, ")")
            } else {
              paste0("Conversion completed (backend: ", conversion_files$backend, ")")
            }
          )
        },
        div(
          style = "display: flex; gap: 8px; flex-wrap: wrap;",
        downloadButton(
          "download_blup_txt",
          "Download BLUPF90 .txt",
          class = "btn btn-success btn-sm"
        ),
        downloadButton(
          "download_blup_map",
          "Download BLUPF90 .map",
          class = "btn btn-success btn-sm"
        ),
        if (!is.null(conversion_files$blup_bim) && file.exists(conversion_files$blup_bim)) {
          downloadButton(
            "download_blup_bim",
            "Download BLUPF90 .bim",
            class = "btn btn-success btn-sm"
          )
        }
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

  output$download_blup_bim <- downloadHandler(
    filename = function() {
      basename(conversion_files$blup_bim %||% "converted.bim")
    },
    content = function(file) {
      req(conversion_files$blup_bim)
      file.copy(conversion_files$blup_bim, file, overwrite = TRUE)
    }
  )
  
  # Handle close button click
  observeEvent(input$geno_convert_close, {
    cleanup_conversion_files()
    removeModal()
  }, ignoreInit = TRUE)
  
  observeEvent(input$geno_convert_run, {
    has_plinkr <- has_plinkr_blup_backend()
    has_r <- has_r_blup_backend_bfile()
    if (!has_plinkr && !has_r) {
      showNotification(
        if (tolower(lang()) == "zh") {
          "No conversion backend available."
        } else {
          "No conversion backend available."
        },
        type = "error", duration = 10
      )
      return(NULL)
    }
    
    cleanup_conversion_files()
    
    req(input$geno_help_plink_bed, input$geno_help_plink_bim, input$geno_help_plink_fam)
      
      withProgress(message = "Converting PLINK → BLUPF90...",
                   detail = "Preparing files...",
                   value = 0, {
        
        bed_name <- input$geno_help_plink_bed$name[1]
        bim_name <- input$geno_help_plink_bim$name[1]
        fam_name <- input$geno_help_plink_fam$name[1]
        bed_datapath <- input$geno_help_plink_bed$datapath[1]
        bim_datapath <- input$geno_help_plink_bim$datapath[1]
        fam_datapath <- input$geno_help_plink_fam$datapath[1]

        prefixes <- tools::file_path_sans_ext(c(basename(bed_name), basename(bim_name), basename(fam_name)))
        if (length(unique(prefixes)) != 1L) {
          showNotification(
            if (tolower(lang()) == "zh") {
              "Upload .bed/.bim/.fam files with the same prefix."
            } else {
              "Please upload matching .bed/.bim/.fam files with the same prefix."
            },
            type = "error", duration = 10
          )
          return(NULL)
        }
        
        # Convert files in a temporary directory
        tmp_dir <- tempfile(pattern = "easyblup_plink_", tmpdir = tempdir())
        dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
        
        base_prefix <- prefixes[1]
        target_prefix <- file.path(tmp_dir, base_prefix)
        
        bed_target <- paste0(target_prefix, ".bed")
        bim_target <- paste0(target_prefix, ".bim")
        fam_target <- paste0(target_prefix, ".fam")
        file.copy(bed_datapath, bed_target, overwrite = TRUE)
        file.copy(bim_datapath, bim_target, overwrite = TRUE)
        file.copy(fam_datapath, fam_target, overwrite = TRUE)
        
        backend_used <- NULL
        conversion_error <- NULL

        if (has_plinkr) {
          setProgress(
            value = 0.2,
            detail = "Running PLINK conversion (preferred)..."
          )

          plink_res <- tryCatch({
            convert_plink_to_blupf90_plink_backend_bfile(
              bed_path = bed_target,
              bim_path = bim_target,
              fam_path = fam_target,
              out_prefix = target_prefix
            )
          }, error = function(e) e)

          out_txt <- paste0(target_prefix, ".txt")
          out_map <- paste0(target_prefix, ".map")
          out_bim <- paste0(target_prefix, ".bim")
          plink_ok <- !inherits(plink_res, "error") && file.exists(out_txt) && file.exists(out_map)

          if (plink_ok) {
            backend_used <- if (is.list(plink_res) && identical(plink_res$via, "cli")) "PLINK (CLI)" else "PLINK"
          } else {
            conversion_error <- if (inherits(plink_res, "error")) plink_res$message else "PLINK output files not found."
            if (has_r) {
              showNotification(
                if (tolower(lang()) == "zh") {
                  paste0("PLINK failed; trying R/snpStats: ", conversion_error)
                } else {
                  paste0("PLINK conversion failed. Falling back to R/snpStats backend: ", conversion_error)
                },
                type = "warning", duration = 10
              )
            }
          }
        }

        if (is.null(backend_used) && has_r) {
          setProgress(
            value = 0.45,
            detail = "Running R/snpStats conversion (second choice)..."
          )
          r_res <- tryCatch({
            convert_plink_to_blupf90_r_bfile(
              bed_path = bed_target,
              bim_path = bim_target,
              fam_path = fam_target,
              out_prefix = target_prefix
            )
          }, error = function(e) e)

          if (inherits(r_res, "error")) {
            conversion_error <- r_res$message
          } else {
            backend_used <- "R/snpStats"
          }
        }

        out_txt <- paste0(target_prefix, ".txt")
        out_map <- paste0(target_prefix, ".map")
        out_bim <- paste0(target_prefix, ".bim")

        if (is.null(backend_used) || !file.exists(out_txt) || !file.exists(out_map)) {
          setProgress(value = 1)
          showNotification(
            if (tolower(lang()) == "zh") {
              paste0("PLINK to BLUPF90 conversion failed: ", conversion_error %||% "Unknown error")
            } else {
              paste0("PLINK -> BLUPF90 conversion failed: ", conversion_error %||% "Unknown error")
            },
            type = "error", duration = 12
          )
          unlink(tmp_dir, recursive = TRUE, force = TRUE)
          return(NULL)
        }

        setProgress(value = 0.8, detail = "Preparing download files...")

        download_dir <- tempfile(pattern = "easyblup_blup_download_", tmpdir = tempdir())
        dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)

        dl_txt <- file.path(download_dir, paste0(base_prefix, ".txt"))
        dl_map <- file.path(download_dir, paste0(base_prefix, ".map"))
        file.copy(out_txt, dl_txt, overwrite = TRUE)
        file.copy(out_map, dl_map, overwrite = TRUE)
        conversion_files$blup_txt <- dl_txt
        conversion_files$blup_map <- dl_map

        if (file.exists(out_bim)) {
          dl_bim <- file.path(download_dir, paste0(base_prefix, ".bim"))
          file.copy(out_bim, dl_bim, overwrite = TRUE)
          conversion_files$blup_bim <- dl_bim
        }
        conversion_files$backend <- backend_used

        setProgress(value = 1, detail = "Complete!")

        showNotification(
          if (tolower(lang()) == "zh") {
            paste0("Conversion complete (backend: ", backend_used, "). Download the files below.")
          } else {
            paste0("PLINK -> BLUPF90 conversion finished (backend: ", backend_used, "). Use buttons below to download files.")
          },
          type = "message", duration = 15
        )

        unlink(tmp_dir, recursive = TRUE, force = TRUE)
      })
  })

  # ====== AI assistant (floating chat) ======
  ai_state <- reactiveValues(
    history = list(),
    proposals = list(),   # id -> text
    pending_id = NULL,
    pending_text = NULL
  )

  session$onFlushed(function() {
    env <- if (exists("ai_env_config", mode = "function")) ai_env_config() else list(has_default = FALSE, daily_limit = 10L)
    limit <- env$daily_limit %||% 10L
    quota_text <- ""
    if (isTRUE(env$has_default) && exists("ai_quota_status", mode = "function") &&
        exists("ai_client_ip", mode = "function")) {
      st <- tryCatch(ai_quota_status(ai_client_ip(session), limit), error = function(e) NULL)
      if (!is.null(st)) {
        quota_text <- sprintf(" Author quota today: %d/%d questions left for this IP.", st$remaining, st$limit)
      } else {
        quota_text <- sprintf(" Author quota: %d questions/IP/day.", as.integer(limit))
      }
    }
    session$sendCustomMessage("ai_status", list(
      text = if (isTRUE(env$has_default)) {
        paste0("Author configuration available.", quota_text, " Your own key bypasses the daily limit.")
      } else {
        "No author key configured. Add your provider, model and API key below."
      },
      level = if (isTRUE(env$has_default)) "ok" else "warn",
      quota = if (exists("ai_quota_status", mode = "function") && exists("ai_client_ip", mode = "function") && isTRUE(env$has_default)) {
        tryCatch(ai_quota_status(ai_client_ip(session), limit), error = function(e) NULL)
      } else {
        NULL
      }
    ))
  }, once = TRUE)

  # Client re-binds custom handlers after Shiny finishes init; re-send status then.
  .ai_push_status <- function() {
    env <- if (exists("ai_env_config", mode = "function")) ai_env_config() else list(has_default = FALSE, daily_limit = 10L)
    limit <- env$daily_limit %||% 10L
    quota <- NULL
    quota_text <- ""
    if (isTRUE(env$has_default) && exists("ai_quota_status", mode = "function") &&
        exists("ai_client_ip", mode = "function")) {
      quota <- tryCatch(ai_quota_status(ai_client_ip(session), limit), error = function(e) NULL)
      if (!is.null(quota)) {
        quota_text <- sprintf(" Author quota today: %d/%d questions left for this IP.", quota$remaining, quota$limit)
      }
    }
    session$sendCustomMessage("ai_status", list(
      text = if (isTRUE(env$has_default)) {
        paste0("Author configuration available.", quota_text, " Your own key bypasses the daily limit.")
      } else {
        "No author key configured. Add your provider, model and API key below."
      },
      level = if (isTRUE(env$has_default)) "ok" else "warn",
      quota = quota
    ))
  }

  observeEvent(input$ai_client_ready, {
    .ai_push_status()
  }, ignoreInit = TRUE)

  observeEvent(input$ai_test_provider, {
    if (!exists("ai_test_provider", mode = "function") ||
        !exists("ai_resolve_config", mode = "function")) {
      session$sendCustomMessage("ai_test_result", list(
        ok = FALSE,
        text = "AI backend not loaded (ai_backend.R).",
        level = "warn"
      ))
      return()
    }
    user_cfg <- input$ai_user_cfg %||% list()
    if (is.character(user_cfg)) {
      user_cfg <- tryCatch(jsonlite::fromJSON(user_cfg), error = function(e) list())
    }
    cfg <- tryCatch(ai_resolve_config(user_cfg), error = function(e) e)
    if (inherits(cfg, "error")) {
      session$sendCustomMessage("ai_test_result", list(
        ok = FALSE,
        text = conditionMessage(cfg),
        level = "warn"
      ))
      return()
    }
    result <- tryCatch(ai_test_provider(cfg), error = function(e) {
      list(ok = FALSE, error = conditionMessage(e))
    })
    if (!isTRUE(result$ok)) {
      session$sendCustomMessage("ai_test_result", list(
        ok = FALSE,
        text = result$error %||% "Provider test failed.",
        level = "warn",
        ms = result$ms %||% NA_integer_,
        model = result$model %||% cfg$model
      ))
      return()
    }
    note <- result$message %||% "Provider OK."
    if (identical(cfg$source, "default")) {
      note <- paste0(note, " (author config; tests do not consume the daily question quota)")
    }
    session$sendCustomMessage("ai_test_result", list(
      ok = TRUE,
      text = note,
      level = "ok",
      ms = result$ms %||% NA_integer_,
      model = result$model %||% cfg$model
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$ai_user_message, {
    ai_fail <- function(err) {
      eb_log("[xmosome-ai] error: ", err, level = "warn")
      try(session$sendCustomMessage("ai_chat_error", list(error = as.character(err))), silent = TRUE)
    }
    tryCatch({
      msg <- input$ai_user_message
      text <- if (is.list(msg)) msg$text else msg
      text <- trimws(as.character(text %||% ""))
      if (!nzchar(text)) return()
      eb_log("[xmosome-ai] request len=", nchar(text), level = "debug")

      if (!exists("ai_chat_request", mode = "function")) {
        ai_fail("AI backend not loaded (ai_backend.R).")
        return()
      }

      user_cfg <- input$ai_user_cfg %||% list()
      if (is.character(user_cfg)) {
        user_cfg <- tryCatch(jsonlite::fromJSON(user_cfg), error = function(e) list())
      }
      cfg <- tryCatch(ai_resolve_config(user_cfg), error = function(e) e)
      if (inherits(cfg, "error")) {
        ai_fail(conditionMessage(cfg))
        return()
      }

      quota_info <- NULL
      if (identical(cfg$source, "default")) {
        if (!exists("ai_quota_consume", mode = "function") || !exists("ai_client_ip", mode = "function")) {
          ai_fail("Author quota controller unavailable.")
          return()
        }
        quota_info <- tryCatch(ai_quota_consume(ai_client_ip(session)), error = function(e) {
          list(ok = FALSE, error = paste("Quota error:", conditionMessage(e)))
        })
        if (!isTRUE(quota_info$ok)) {
          ai_fail(quota_info$error %||% "Author AI daily limit reached.")
          session$sendCustomMessage("ai_status", list(
            text = quota_info$error %||% "Author AI daily limit reached.",
            level = "warn",
            quota = list(
              used = quota_info$used %||% NA_integer_,
              limit = quota_info$limit %||% ai_author_daily_limit(),
              remaining = 0L
            )
          ))
          return()
        }
      }

      pheno_hdr <- tryCatch(colnames(data()), error = function(e) character())
      info <- tryCatch(kit_template_info(input$model_template), error = function(e) list(label = "unknown"))
      ctx <- ai_build_context(
        param_text = values$current_param %||% input$param_editor_text %||% "",
        pheno_header = pheno_hdr,
        mapping = list(
          traits = values$traits, fixed = values$fixed,
          random = values$random, animal = values$animal
        ),
        template_label = info$label,
        extras = list(pe = input$opt_pe, mat = input$opt_mat, mpe = input$opt_mpe,
                      method = input$opt_method, auto_se_covar = input$opt_auto_se_covar,
                      em_pure = input$opt_em_reml_pure, gibbs = input$opt_gibbs_enable)
      )

      # Keep short rolling history (last 6 turns + system)
      hist <- ai_state$history
      hist[[length(hist) + 1]] <- list(role = "user", content = text)
      if (length(hist) > 8) hist <- hist[(length(hist) - 7):length(hist)]
      ai_state$history <- hist

      messages <- c(
        list(list(role = "system", content = ai_skill_system_prompt())),
        list(list(role = "user", content = ctx)),
        hist
      )

      result <- ai_chat_request(messages, cfg)
      if (!isTRUE(result$ok)) {
        ai_fail(result$error %||% "Unknown AI error")
        return()
      }

      parsed <- ai_extract_proposal(result$content)
      proposal_id <- NULL
      if (!is.null(parsed$proposal) && nzchar(parsed$proposal)) {
        proposal_id <- paste0("p", as.integer(as.numeric(Sys.time()) * 1000) %% 1e9)
        props <- ai_state$proposals
        props[[proposal_id]] <- parsed$proposal
        ai_state$proposals <- props
        ai_state$pending_id <- proposal_id
        ai_state$pending_text <- parsed$proposal
      }

      ai_state$history[[length(ai_state$history) + 1]] <- list(
        role = "assistant", content = result$content
      )

      eb_log("[xmosome-ai] reply ok, chars=", nchar(parsed$reply %||% ""), level = "debug")
      session$sendCustomMessage("ai_chat_reply", list(
        reply = parsed$reply,
        proposal_id = proposal_id,
        quota = quota_info
      ))
      if (!is.null(quota_info)) {
        session$sendCustomMessage("ai_status", list(
          text = sprintf(
            "Author configuration · %d/%d questions left today for this IP.",
            quota_info$remaining, quota_info$limit
          ),
          level = if (quota_info$remaining <= 2L) "warn" else "ok",
          quota = quota_info
        ))
      }
    }, error = function(e) {
      ai_fail(conditionMessage(e))
    })
  }, ignoreInit = TRUE)

  observeEvent(input$ai_request_diff, {
    req_id <- if (is.list(input$ai_request_diff)) input$ai_request_diff$id else input$ai_request_diff
    req_id <- as.character(req_id %||% "")
    proposal <- ai_state$proposals[[req_id]]
    if (is.null(proposal) || !nzchar(proposal)) {
      session$sendCustomMessage("ai_chat_error", list(error = "Proposal expired — ask the AI again."))
      return()
    }
    ai_state$pending_id <- req_id
    ai_state$pending_text <- proposal
    old <- values$current_param %||% input$param_editor_text %||% ""
    diff <- ai_unified_diff(old, proposal)
    session$sendCustomMessage("ai_show_diff", list(rows = diff$rows))
  }, ignoreInit = TRUE)

  # Client recovered a card from a Markdown fence / markers the server missed.
  observeEvent(input$ai_local_proposal, {
    payload <- input$ai_local_proposal
    if (!is.list(payload)) return()
    req_id <- as.character(payload$id %||% "")
    proposal <- trimws(as.character(payload$text %||% ""))
    if (!nzchar(req_id) || !nzchar(proposal)) return()
    if (!grepl("\\bDATAFILE\\b", proposal) || !grepl("\\bTRAITS\\b", proposal)) {
      session$sendCustomMessage("ai_chat_error", list(error = "Recovered text does not look like a parameter card."))
      return()
    }
    props <- ai_state$proposals
    props[[req_id]] <- proposal
    ai_state$proposals <- props
    ai_state$pending_id <- req_id
    ai_state$pending_text <- proposal
    old <- values$current_param %||% input$param_editor_text %||% ""
    diff <- ai_unified_diff(old, proposal)
    session$sendCustomMessage("ai_show_diff", list(rows = diff$rows))
  }, ignoreInit = TRUE)

  observeEvent(input$ai_diff_accept, {
    proposal <- ai_state$pending_text
    if (is.null(proposal) || !nzchar(proposal)) return()
    values$auto_update <- FALSE
    updateRadioButtons(session, "param_edit_mode", selected = "manual")
    values$current_param <- proposal
    values$default_param <- proposal
    sync_card_controls(proposal, notify = TRUE)
    session$sendCustomMessage("update_textarea", list(content = proposal))
    session$sendCustomMessage("ai_diff_applied", list(ok = TRUE))
    ai_state$pending_text <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$ai_diff_reject, {
    ai_state$pending_text <- NULL
  }, ignoreInit = TRUE)

  observeEvent(input$ai_dismiss_proposal, {
    id <- as.character(input$ai_dismiss_proposal %||% "")
    if (nzchar(id) && !is.null(ai_state$proposals[[id]])) {
      props <- ai_state$proposals
      props[[id]] <- NULL
      ai_state$proposals <- props
    }
  }, ignoreInit = TRUE)

}

# App entrypoints
run_Xmosome_blupf90kit_app <- function() {
  list(ui = ui, server = server)
}

if (!identical(Sys.getenv("EASYBREEDER_SOURCE_ONLY", ""), "1")) {
  shinyApp(ui = ui, server = server)
}
