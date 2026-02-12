#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)

parse_args <- function(x) {
  out <- list(geno = NULL, map = NULL, plink = NULL, out = NULL, pca_out = NULL)
  i <- 1L
  while (i <= length(x)) {
    key <- x[[i]]
    if (key %in% c("--geno", "-g") && i < length(x)) {
      out$geno <- x[[i + 1L]]
      i <- i + 2L
      next
    }
    if (key %in% c("--map", "-m") && i < length(x)) {
      out$map <- x[[i + 1L]]
      i <- i + 2L
      next
    }
    if (key %in% c("--plink", "-p") && i < length(x)) {
      out$plink <- x[[i + 1L]]
      i <- i + 2L
      next
    }
    if (key %in% c("--out", "-o") && i < length(x)) {
      out$out <- x[[i + 1L]]
      i <- i + 2L
      next
    }
    if (key == "--pca-out" && i < length(x)) {
      out$pca_out <- x[[i + 1L]]
      i <- i + 2L
      next
    }
    stop("Unknown or incomplete argument: ", key, call. = FALSE)
  }
  out
}

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript scripts/compare_plink_rcpp.R --geno <gen.ped|gen.txt> --map <gen.map> [--plink /path/to/plink] [--out report.csv] [--pca-out pca.csv]\n\n",
    "Examples:\n",
    "  Rscript scripts/compare_plink_rcpp.R --geno example_data/gen.ped --map example_data/gen.map\n",
    "  Rscript scripts/compare_plink_rcpp.R --geno /Users/you/Downloads/gen.txt --map /Users/you/Downloads/gen.map --plink /Applications/BioSoftware/plink --out /tmp/compare.csv --pca-out /tmp/pca_pc12.csv\n",
    sep = ""
  )
}

opt <- tryCatch(parse_args(args), error = function(e) {
  message(e$message)
  usage()
  quit(status = 1L)
})

if (is.null(opt$geno) || is.null(opt$map)) {
  usage()
  quit(status = 1L)
}
if (!file.exists(opt$geno)) stop("Genotype file not found: ", opt$geno, call. = FALSE)
if (!file.exists(opt$map)) stop("Map file not found: ", opt$map, call. = FALSE)

fast_read <- function(path) {
  if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(path, header = FALSE, data.table = FALSE, showProgress = FALSE)
  } else {
    read.table(path, header = FALSE, stringsAsFactors = FALSE)
  }
}

read_map <- function(map_path) {
  m <- fast_read(map_path)
  if (ncol(m) < 4) stop("Map file must have at least 4 columns.", call. = FALSE)
  m <- m[, 1:4, drop = FALSE]
  names(m) <- c("Chromosome", "SNP_ID", "Genetic_Distance", "Physical_Position")
  m
}

read_plink_ped <- function(ped_path, map_path) {
  map <- read_map(map_path)
  ped <- fast_read(ped_path)
  if (ncol(ped) < 7) stop("PED file has fewer than 7 columns.", call. = FALSE)
  allele_matrix <- as.matrix(ped[, 7:ncol(ped), drop = FALSE])
  if ((ncol(allele_matrix) %% 2L) != 0L) stop("Invalid PED: genotype columns are not allele pairs.", call. = FALSE)
  snp_count <- ncol(allele_matrix) / 2L
  if (nrow(map) != snp_count) {
    stop(sprintf("PED allele pairs (%d SNPs) do not match MAP rows (%d).", snp_count, nrow(map)), call. = FALSE)
  }
  geno <- matrix(NA_character_, nrow = nrow(ped), ncol = snp_count)
  for (i in seq_len(snp_count)) {
    a1 <- allele_matrix[, (i - 1L) * 2L + 1L]
    a2 <- allele_matrix[, (i - 1L) * 2L + 2L]
    geno[, i] <- paste(a1, a2)
  }
  colnames(geno) <- as.character(map$SNP_ID)
  samples <- data.frame(
    FID = as.character(ped[, 1]),
    IID = as.character(ped[, 2]),
    stringsAsFactors = FALSE
  )
  list(samples = samples, genotypes = geno, map = map, input_format = "plink_ped")
}

read_blupf90_txt <- function(txt_path, map_path) {
  map <- read_map(map_path)
  txt <- if (requireNamespace("data.table", quietly = TRUE)) {
    data.table::fread(
      txt_path,
      header = FALSE,
      data.table = FALSE,
      showProgress = FALSE,
      colClasses = "character"
    )
  } else {
    read.table(txt_path, header = FALSE, stringsAsFactors = FALSE, colClasses = "character")
  }
  if (ncol(txt) < 2) stop("BLUPF90 txt must have at least 2 columns.", call. = FALSE)
  map_n <- nrow(map)
  if (ncol(txt) == 2L) {
    gstr <- as.character(txt[, 2L])
    if (all(!is.na(nchar(gstr))) && all(nchar(gstr) >= map_n)) {
      geno <- matrix(NA_real_, nrow = nrow(txt), ncol = map_n)
      for (i in seq_len(nrow(txt))) {
        chars <- substring(gstr[i], first = seq_len(map_n), last = seq_len(map_n))
        vals <- suppressWarnings(as.numeric(chars))
        vals[!vals %in% c(0, 1, 2)] <- NA_real_
        geno[i, ] <- vals
      }
      samples <- data.frame(
        FID = as.character(txt[, 1]),
        IID = as.character(txt[, 1]),
        stringsAsFactors = FALSE
      )
      return(list(samples = samples, genotypes = geno, map = map, input_format = "blupf90_txt"))
    }
  }
  id_cols <- ncol(txt) - map_n
  if (id_cols < 1) {
    stop(sprintf("BLUPF90 txt columns (%d) do not contain map marker columns (%d).", ncol(txt), map_n), call. = FALSE)
  }
  end_col <- id_cols + map_n
  if (end_col > ncol(txt)) stop("Insufficient txt columns for map marker count.", call. = FALSE)
  geno <- as.matrix(txt[, (id_cols + 1L):end_col, drop = FALSE])
  suppressWarnings(storage.mode(geno) <- "double")
  geno[geno %in% c(-9, 9, 5)] <- NA_real_
  samples <- data.frame(
    FID = as.character(txt[, 1]),
    IID = as.character(txt[, 1]),
    stringsAsFactors = FALSE
  )
  list(samples = samples, genotypes = geno, map = map, input_format = "blupf90_txt")
}

col_has_dosage <- function(v) {
  vals <- suppressWarnings(as.numeric(v))
  ok <- !is.na(vals)
  if (!any(ok)) return(FALSE)
  all(vals[ok] %in% c(0, 1, 2))
}

write_plink_text <- function(prefix, data) {
  map <- data$map
  write.table(map, paste0(prefix, ".map"), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = "\t")
  geno <- data$genotypes
  alle <- matrix("0", nrow = nrow(geno), ncol = ncol(geno) * 2L)
  for (i in seq_len(ncol(geno))) {
    col_i <- geno[, i]
    a <- rep("0", nrow(geno))
    b <- rep("0", nrow(geno))
    if (is.numeric(col_i) || col_has_dosage(col_i)) {
      d <- suppressWarnings(as.numeric(col_i))
      idx0 <- !is.na(d) & d == 0
      idx1 <- !is.na(d) & d == 1
      idx2 <- !is.na(d) & d == 2
      a[idx0] <- "A"; b[idx0] <- "A"
      a[idx1] <- "A"; b[idx1] <- "C"
      a[idx2] <- "C"; b[idx2] <- "C"
    } else {
      x <- as.character(col_i)
      valid <- !is.na(x) & x != "0 0" & x != "N N" & nzchar(x)
      if (any(valid)) {
        aa <- toupper(substr(x[valid], 1, 1))
        bb <- toupper(substr(x[valid], 3, 3))
        good <- grepl("^[A-Z]$", aa) & grepl("^[A-Z]$", bb)
        if (any(good)) {
          idx <- which(valid)[good]
          a[idx] <- aa[good]
          b[idx] <- bb[good]
        }
      }
    }
    alle[, (i - 1L) * 2L + 1L] <- a
    alle[, (i - 1L) * 2L + 2L] <- b
  }
  ped <- cbind(data$samples$FID, data$samples$IID, 0, 0, 0, -9, alle)
  write.table(ped, paste0(prefix, ".ped"), row.names = FALSE, col.names = FALSE, quote = FALSE, sep = " ")
}

read_plink_outputs <- function(out_prefix, data) {
  res <- list()
  iids <- as.character(data$samples$IID)
  snps <- as.character(data$map$SNP_ID)

  imiss_file <- paste0(out_prefix, ".imiss")
  if (file.exists(imiss_file)) {
    imiss <- read.table(imiss_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
    miss <- rep(NA_real_, length(iids))
    idx <- match(imiss$IID, iids)
    ok <- !is.na(idx)
    miss[idx[ok]] <- as.numeric(imiss$F_MISS[ok])
    res$individual_missing_rate <- miss
    res$individual_call_rate <- 1 - miss
  }

  lmiss_file <- paste0(out_prefix, ".lmiss")
  if (file.exists(lmiss_file)) {
    lmiss <- read.table(lmiss_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
    miss <- rep(NA_real_, length(snps))
    idx <- match(as.character(lmiss$SNP), snps)
    ok <- !is.na(idx)
    miss[idx[ok]] <- as.numeric(lmiss$F_MISS[ok])
    res$marker_missing_rate <- miss
    res$marker_call_rate <- 1 - miss
  }

  frq_file <- paste0(out_prefix, ".frq")
  if (file.exists(frq_file)) {
    frq <- read.table(frq_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
    maf <- rep(NA_real_, length(snps))
    idx <- match(as.character(frq$SNP), snps)
    ok <- !is.na(idx)
    maf[idx[ok]] <- as.numeric(frq$MAF[ok])
    res$maf <- maf
  }

  hwe_file <- paste0(out_prefix, ".hwe")
  if (file.exists(hwe_file)) {
    hwe <- read.table(hwe_file, header = TRUE, stringsAsFactors = FALSE, sep = "", fill = TRUE)
    hwe_all <- if ("TEST" %in% names(hwe)) hwe[hwe$TEST == "ALL", , drop = FALSE] else hwe
    if (nrow(hwe_all) == 0) hwe_all <- hwe
    pvals <- rep(NA_real_, length(snps))
    if (nrow(hwe_all) > 0 && ncol(hwe_all) >= 9) {
      p_col <- suppressWarnings(as.numeric(hwe_all[, 9]))
      s_col <- as.character(hwe_all[, 2])
      okp <- is.finite(p_col) & p_col > 0 & p_col <= 1
      idx <- match(s_col[okp], snps)
      ok <- !is.na(idx)
      pvals[idx[ok]] <- p_col[okp][ok]
    }
    res$hwe_pvalues <- pvals
  }

  het_file <- paste0(out_prefix, ".het")
  if (file.exists(het_file)) {
    het <- read.table(het_file, header = TRUE, stringsAsFactors = FALSE, sep = "", check.names = FALSE)
    hom_col <- if ("O(HOM)" %in% names(het)) "O(HOM)" else names(het)[3]
    nnm_col <- if ("N(NM)" %in% names(het)) "N(NM)" else names(het)[5]
    o_hom <- as.numeric(het[[hom_col]])
    n_nm <- as.numeric(het[[nnm_col]])
    valid <- is.finite(o_hom) & is.finite(n_nm) & n_nm > 0
    h <- rep(NA_real_, length(iids))
    rate <- rep(NA_real_, nrow(het))
    rate[valid] <- 1 - (o_hom[valid] / n_nm[valid])
    idx <- match(as.character(het$IID), iids)
    ok <- !is.na(idx)
    h[idx[ok]] <- rate[ok]
    res$individual_heterozygosity <- h
  }

  genome_file <- paste0(out_prefix, ".genome")
  if (file.exists(genome_file)) {
    res$individual_relatedness <- read.table(genome_file, header = TRUE, stringsAsFactors = FALSE, sep = "")
  }

  eigenvec_file <- paste0(out_prefix, ".eigenvec")
  eigenval_file <- paste0(out_prefix, ".eigenval")
  if (file.exists(eigenvec_file) && file.exists(eigenval_file)) {
    ev <- read.table(eigenvec_file, header = FALSE, stringsAsFactors = FALSE, sep = "")
    eval <- read.table(eigenval_file, header = FALSE, stringsAsFactors = FALSE, sep = "")
    if (nrow(ev) > 0 && ncol(ev) >= 3) {
      res$pca_iid <- as.character(ev[, 2])
      res$pca_scores <- as.matrix(ev[, 3:ncol(ev), drop = FALSE])
      res$pca_eigenvalues <- as.numeric(eval[, 1])
    }
  }

  res
}

run_plink_stats <- function(data, plink_path) {
  tmp_dir <- tempfile("plink_cmp_")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)
  prefix <- file.path(tmp_dir, "input")
  out_prefix <- file.path(tmp_dir, "out")
  write_plink_text(prefix, data)
  status <- system2(
    plink_path,
    args = c(
      "--file", prefix,
      "--allow-extra-chr",
      "--nonfounders",
      "--missing",
      "--freq",
      "--hardy",
      "--het",
      "--genome",
      "--pca",
      "--out", out_prefix
    ),
    stdout = FALSE, stderr = FALSE
  )
  if (!identical(status, 0L)) stop("PLINK command failed with status ", status, call. = FALSE)
  read_plink_outputs(out_prefix, data)
}

encode_genotypes_to_numeric <- function(geno_matrix) {
  if (is.null(geno_matrix) || !is.matrix(geno_matrix) || ncol(geno_matrix) == 0) return(NULL)
  num <- matrix(NA_real_, nrow = nrow(geno_matrix), ncol = ncol(geno_matrix))
  colnames(num) <- colnames(geno_matrix)
  for (i in seq_len(ncol(geno_matrix))) {
    x <- as.character(geno_matrix[, i])
    valid <- !is.na(x) & x != "0 0" & x != "N N" & nzchar(x)
    if (!any(valid)) next
    a1 <- toupper(substr(x[valid], 1, 1))
    a2 <- toupper(substr(x[valid], 3, 3))
    good <- grepl("^[A-Z]$", a1) & grepl("^[A-Z]$", a2)
    if (!any(good)) next
    idx <- which(valid)[good]
    a1 <- a1[good]
    a2 <- a2[good]
    alleles <- c(a1, a2)
    ac <- table(alleles)
    if (length(ac) == 1) {
      num[idx, i] <- 0
      next
    }
    minor <- names(ac)[which.min(ac)]
    num[idx, i] <- (a1 == minor) + (a2 == minor)
  }
  num
}

get_numeric_geno <- function(data) {
  g <- data$genotypes
  if (is.matrix(g) && is.numeric(g)) return(g)
  encode_genotypes_to_numeric(as.matrix(g))
}

compute_pca_local <- function(geno_matrix, sample_ids, n_components = 20L) {
  if (is.null(geno_matrix) || nrow(geno_matrix) < 2 || ncol(geno_matrix) < 2) return(NULL)
  valid_markers <- colSums(is.na(geno_matrix)) < nrow(geno_matrix)
  if (sum(valid_markers) < 2) return(NULL)
  geno_matrix <- geno_matrix[, valid_markers, drop = FALSE]
  valid_samples <- rowSums(is.na(geno_matrix)) < ncol(geno_matrix)
  if (sum(valid_samples) < 2) return(NULL)
  geno_matrix <- geno_matrix[valid_samples, , drop = FALSE]
  ids <- sample_ids[valid_samples]
  p <- colMeans(geno_matrix, na.rm = TRUE) / 2
  p[!is.finite(p)] <- NA_real_
  non_mono <- which(is.finite(p) & p > 1e-10 & p < (1 - 1e-10))
  if (length(non_mono) < 2) return(NULL)
  geno_matrix <- geno_matrix[, non_mono, drop = FALSE]
  p <- p[non_mono]
  sd_marker <- sqrt(2 * p * (1 - p))
  keep_sd <- is.finite(sd_marker) & sd_marker >= 1e-10
  if (sum(keep_sd) < 2) return(NULL)
  geno_matrix <- geno_matrix[, keep_sd, drop = FALSE]
  p <- p[keep_sd]
  sd_marker <- sd_marker[keep_sd]
  geno_std <- sweep(geno_matrix, 2L, 2 * p, `-`)
  geno_std <- sweep(geno_std, 2L, sd_marker, `/`)
  valid_mask <- !is.na(geno_std)
  geno_std[!valid_mask] <- 0
  k_num <- tcrossprod(geno_std)
  k_den <- tcrossprod(matrix(as.numeric(valid_mask), nrow = nrow(valid_mask), ncol = ncol(valid_mask)))
  k <- k_num / pmax(k_den, 1)
  k[k_den <= 0] <- 0
  k <- (k + t(k)) / 2
  eig <- eigen(k, symmetric = TRUE)
  keep <- which(is.finite(eig$values) & eig$values > 0)
  if (length(keep) < 2) return(NULL)
  keep <- keep[seq_len(min(n_components, length(keep)))]
  scores <- eig$vectors[, keep, drop = FALSE]
  for (pc_idx in seq_len(ncol(scores))) {
    v <- scores[, pc_idx]
    pivot <- which.max(abs(v))
    if (length(pivot) == 1 && is.finite(v[pivot]) && v[pivot] < 0) scores[, pc_idx] <- -v
  }
  list(iid = ids, scores = scores, eigenvalues = eig$values[keep])
}

run_rcpp_stats <- function(data) {
  if (!requireNamespace("Rcpp", quietly = TRUE)) stop("Rcpp is required.", call. = FALSE)
  Rcpp::sourceCpp("inst/genovieweR/genotype_qc.cpp")
  geno <- get_numeric_geno(data)
  if (is.null(geno)) stop("Could not build numeric genotype matrix.", call. = FALSE)
  ids <- as.character(data$samples$IID)
  n_pairs <- nrow(geno) * (nrow(geno) - 1L) / 2L
  if (!is.finite(n_pairs) || n_pairs > .Machine$integer.max) n_pairs <- .Machine$integer.max
  list(
    individual_call_rate = gvr_individual_call_rate(geno),
    individual_missing_rate = 1 - gvr_individual_call_rate(geno),
    individual_heterozygosity = gvr_individual_het(geno),
    marker_call_rate = gvr_marker_call_rate(geno),
    marker_missing_rate = 1 - gvr_marker_call_rate(geno),
    maf = gvr_maf(geno),
    hwe_pvalues = gvr_hwe_exact(geno),
    individual_relatedness = gvr_relatedness_pairs(
      geno, ids,
      max_pairs = as.integer(n_pairs),
      max_markers = as.integer(ncol(geno)),
      min_valid = 1L
    ),
    pca = compute_pca_local(geno, ids)
  )
}

cmp_metric <- function(name, a, b) {
  if (is.null(a) || is.null(b)) {
    return(data.frame(metric = name, n = 0L, cor = NA_real_, mae = NA_real_, max_abs = NA_real_, stringsAsFactors = FALSE))
  }
  a <- as.numeric(a)
  b <- as.numeric(b)
  keep <- is.finite(a) & is.finite(b)
  if (!any(keep)) {
    return(data.frame(metric = name, n = 0L, cor = NA_real_, mae = NA_real_, max_abs = NA_real_, stringsAsFactors = FALSE))
  }
  aa <- a[keep]
  bb <- b[keep]
  cc <- if (length(unique(aa)) > 1L && length(unique(bb)) > 1L) suppressWarnings(cor(aa, bb)) else NA_real_
  data.frame(
    metric = name,
    n = length(aa),
    cor = cc,
    mae = mean(abs(aa - bb)),
    max_abs = max(abs(aa - bb)),
    stringsAsFactors = FALSE
  )
}

cmp_relatedness <- function(plink_rel, rcpp_rel) {
  if (!is.data.frame(plink_rel) || !is.data.frame(rcpp_rel) ||
      !all(c("IID1", "IID2", "PI_HAT") %in% names(plink_rel)) ||
      !all(c("IID1", "IID2", "PI_HAT") %in% names(rcpp_rel))) {
    return(cmp_metric("relatedness_PI_HAT", numeric(0), numeric(0)))
  }
  norm_key <- function(i1, i2) ifelse(i1 <= i2, paste(i1, i2, sep = "||"), paste(i2, i1, sep = "||"))
  pl <- data.frame(key = norm_key(as.character(plink_rel$IID1), as.character(plink_rel$IID2)),
                   PI_HAT = as.numeric(plink_rel$PI_HAT), stringsAsFactors = FALSE)
  rc <- data.frame(key = norm_key(as.character(rcpp_rel$IID1), as.character(rcpp_rel$IID2)),
                   PI_HAT = as.numeric(rcpp_rel$PI_HAT), stringsAsFactors = FALSE)
  m <- merge(pl, rc, by = "key", suffixes = c("_plink", "_rcpp"))
  cmp_metric("relatedness_PI_HAT", m$PI_HAT_plink, m$PI_HAT_rcpp)
}

cmp_pca <- function(plink_stats, rcpp_stats) {
  if (is.null(plink_stats$pca_scores) || is.null(plink_stats$pca_iid) ||
      is.null(rcpp_stats$pca) || is.null(rcpp_stats$pca$scores) || is.null(rcpp_stats$pca$iid)) {
    return(list(cmp_metric("pca_pc1_z", numeric(0), numeric(0)), cmp_metric("pca_pc2_z", numeric(0), numeric(0))))
  }
  pl_scores <- as.data.frame(plink_stats$pca_scores, stringsAsFactors = FALSE)
  rc_scores <- as.data.frame(rcpp_stats$pca$scores, stringsAsFactors = FALSE)
  names(pl_scores) <- paste0("plink_pc", seq_len(ncol(pl_scores)))
  names(rc_scores) <- paste0("rcpp_pc", seq_len(ncol(rc_scores)))
  pl <- data.frame(IID = as.character(plink_stats$pca_iid), pl_scores, stringsAsFactors = FALSE)
  rc <- data.frame(IID = as.character(rcpp_stats$pca$iid), rc_scores, stringsAsFactors = FALSE)
  m <- merge(pl, rc, by = "IID")
  max_pc <- min(2L, ncol(plink_stats$pca_scores), ncol(rcpp_stats$pca$scores))
  out <- vector("list", max_pc)
  for (pc in seq_len(max_pc)) {
    a <- as.numeric(m[[paste0("plink_pc", pc)]])
    b <- as.numeric(m[[paste0("rcpp_pc", pc)]])
    keep <- is.finite(a) & is.finite(b)
    if (!any(keep)) {
      out[[pc]] <- cmp_metric(paste0("pca_pc", pc, "_z"), numeric(0), numeric(0))
      next
    }
    az <- as.numeric(scale(a[keep]))
    bz <- as.numeric(scale(b[keep]))
    # PCA sign is arbitrary; choose best sign before computing errors.
    if (mean(abs(az + bz)) < mean(abs(az - bz))) bz <- -bz
    out[[pc]] <- cmp_metric(paste0("pca_pc", pc, "_z"), az, bz)
  }
  out
}

build_pca_pc12_table <- function(plink_stats, rcpp_stats) {
  if (is.null(plink_stats$pca_scores) || is.null(plink_stats$pca_iid) ||
      is.null(rcpp_stats$pca) || is.null(rcpp_stats$pca$scores) || is.null(rcpp_stats$pca$iid)) {
    return(data.frame())
  }
  max_pc <- min(2L, ncol(plink_stats$pca_scores), ncol(rcpp_stats$pca$scores))
  if (max_pc < 2L) return(data.frame())

  pl_scores <- as.data.frame(plink_stats$pca_scores, stringsAsFactors = FALSE)
  rc_scores <- as.data.frame(rcpp_stats$pca$scores, stringsAsFactors = FALSE)
  names(pl_scores) <- paste0("PLINK_PC", seq_len(ncol(pl_scores)))
  names(rc_scores) <- paste0("RCPP_PC", seq_len(ncol(rc_scores)))
  pl <- data.frame(IID = as.character(plink_stats$pca_iid), pl_scores, stringsAsFactors = FALSE)
  rc <- data.frame(IID = as.character(rcpp_stats$pca$iid), rc_scores, stringsAsFactors = FALSE)
  m <- merge(pl, rc, by = "IID")

  for (pc in 1:2) {
    a <- as.numeric(m[[paste0("PLINK_PC", pc)]])
    b <- as.numeric(m[[paste0("RCPP_PC", pc)]])
    keep <- is.finite(a) & is.finite(b)
    sign_pc <- 1
    if (any(keep)) {
      c1 <- suppressWarnings(cor(a[keep], b[keep]))
      c2 <- suppressWarnings(cor(a[keep], -b[keep]))
      if (is.finite(c2) && (!is.finite(c1) || c2 > c1)) sign_pc <- -1
    }
    b_aligned <- b * sign_pc
    m[[paste0("RCPP_PC", pc, "_ALIGNED")]] <- b_aligned
    m[[paste0("DELTA_PC", pc)]] <- b_aligned - a
  }

  m[, c(
    "IID",
    "PLINK_PC1", "RCPP_PC1", "RCPP_PC1_ALIGNED", "DELTA_PC1",
    "PLINK_PC2", "RCPP_PC2", "RCPP_PC2_ALIGNED", "DELTA_PC2"
  ), drop = FALSE]
}

resolve_plink <- function(user_path = NULL) {
  candidates <- c(user_path, Sys.which("plink"), "/Applications/BioSoftware/plink")
  candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])
  for (p in candidates) {
    if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = TRUE))
  }
  NULL
}

plink_path <- resolve_plink(opt$plink)
if (is.null(plink_path)) stop("PLINK executable not found. Use --plink /path/to/plink.", call. = FALSE)
cat("Using PLINK:", plink_path, "\n")

ext <- tolower(tools::file_ext(opt$geno))
data <- switch(
  ext,
  ped = read_plink_ped(opt$geno, opt$map),
  txt = read_blupf90_txt(opt$geno, opt$map),
  stop("Unsupported genotype file extension: .", ext, " (expected .ped or .txt)", call. = FALSE)
)

plink_stats <- run_plink_stats(data, plink_path)
rcpp_stats <- run_rcpp_stats(data)

rows <- list(
  cmp_metric("individual_call_rate", plink_stats$individual_call_rate, rcpp_stats$individual_call_rate),
  cmp_metric("individual_missing_rate", plink_stats$individual_missing_rate, rcpp_stats$individual_missing_rate),
  cmp_metric("individual_heterozygosity", plink_stats$individual_heterozygosity, rcpp_stats$individual_heterozygosity),
  cmp_metric("marker_call_rate", plink_stats$marker_call_rate, rcpp_stats$marker_call_rate),
  cmp_metric("marker_missing_rate", plink_stats$marker_missing_rate, rcpp_stats$marker_missing_rate),
  cmp_metric("maf", plink_stats$maf, rcpp_stats$maf),
  cmp_metric("hwe_pvalues", plink_stats$hwe_pvalues, rcpp_stats$hwe_pvalues),
  cmp_relatedness(plink_stats$individual_relatedness, rcpp_stats$individual_relatedness)
)
rows <- c(rows, cmp_pca(plink_stats, rcpp_stats))
report <- do.call(rbind, rows)

print(report, row.names = FALSE)
if (!is.null(opt$out)) {
  write.csv(report, file = opt$out, row.names = FALSE)
  cat("Report written:", opt$out, "\n")
}

pca_table <- build_pca_pc12_table(plink_stats, rcpp_stats)
if (nrow(pca_table) > 0) {
  cat("\nPCA PC1/PC2 per-sample comparison (first 10 rows):\n")
  print(utils::head(pca_table, 10), row.names = FALSE)
  if (!is.null(opt$pca_out)) {
    write.csv(pca_table, file = opt$pca_out, row.names = FALSE)
    cat("PCA table written:", opt$pca_out, "\n")
  }
}
