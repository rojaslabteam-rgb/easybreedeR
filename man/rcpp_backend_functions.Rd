\name{rcpp_backend_functions}
\alias{rcpp_backend_functions}
\alias{gvr_marker_call_rate}
\alias{gvr_individual_call_rate}
\alias{gvr_maf}
\alias{gvr_individual_het}
\alias{gvr_marker_het}
\alias{gvr_hwe_exact}
\alias{gvr_relatedness_pairs}
\alias{gvr_pca_from_dosage_cpp}
\alias{fast_pedigree_qc}
\alias{fast_pedigree_qc_sex}
\alias{fast_detect_loops}
\alias{fast_find_deepest_ancestor}
\alias{check_birth_date_order}
\alias{fast_lap_distribution}
\alias{fast_lap_depths}
\alias{fast_descendant_summary}
\alias{fast_inbreeding_cpp}
\alias{fast_top_contrib_cpp}
\alias{eb_ped_to_blup_codes_cpp}
\alias{gvr_call_rate_from_ped_strings_cpp}
\alias{gvr_maf_from_ped_strings_cpp}
\alias{gvr_hwe_from_ped_strings_cpp}
\alias{gvr_individual_het_from_ped_strings_cpp}
\alias{gvr_dosage_from_ped_strings_cpp}
\title{Rcpp Backend Functions}
\description{
Low-level Rcpp backend functions exported by \pkg{easybreedeR} for genotype
statistics, pedigree utilities, and format conversion helpers.
}
\usage{
gvr_marker_call_rate(geno)
gvr_individual_call_rate(geno)
gvr_maf(geno)
gvr_individual_het(geno)
gvr_marker_het(geno)
gvr_hwe_exact(geno)
gvr_relatedness_pairs(geno, sample_ids, max_pairs = 2147483647L,
  max_markers = 2147483647L, min_valid = 20L, show_progress = TRUE)
gvr_pca_from_dosage_cpp(geno, n_components = 20L, max_markers = 0L)
fast_pedigree_qc(ids, sires, dams)
fast_pedigree_qc_sex(ids, sires, dams, sex)
fast_detect_loops(ids, sires, dams)
fast_find_deepest_ancestor(ids, sires, dams, sample_size = 200L)
check_birth_date_order(ids, sires, dams, birth_dates)
fast_lap_distribution(ids, sires, dams, sample_size = 10000L, max_depth = 20L)
fast_lap_depths(ids, sires, dams)
fast_descendant_summary(ids, parent_vals, max_depth = 50L)
fast_inbreeding_cpp(ids, sires, dams)
fast_top_contrib_cpp(ids, sires, dams, F, target_id, max_depth = 6L, top_k = 5L)
eb_ped_to_blup_codes_cpp(allele1, allele2)
gvr_call_rate_from_ped_strings_cpp(geno_pairs)
gvr_maf_from_ped_strings_cpp(geno_pairs)
gvr_hwe_from_ped_strings_cpp(geno_pairs)
gvr_individual_het_from_ped_strings_cpp(geno_pairs)
gvr_dosage_from_ped_strings_cpp(geno_pairs)
}
\details{
These functions are performance-oriented primitives intended for internal use
by the package's Shiny applications and helper workflows.
}
\keyword{internal}
