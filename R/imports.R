#' @importFrom bslib bs_theme font_google
#' @importFrom shinyjs useShinyjs reset
#' @importFrom dplyr mutate filter transmute arrange select left_join distinct
#' @importFrom dplyr group_by summarise case_when
#' @importFrom DT datatable renderDT DTOutput
#' @importFrom data.table fread
#' @importFrom digest digest
#' @importFrom igraph graph_from_data_frame components
#' @importFrom igraph cluster_louvain cluster_walktrap membership vcount
#' @importFrom igraph laplacian_matrix layout_with_fr layout_with_kk layout_with_lgl
#' @importFrom visNetwork visNetwork visNodes visEdges visGroups visOptions visInteraction visEvents
#' @importFrom visNetwork visNetworkOutput
#' @importFrom plotly plotlyOutput
#' @useDynLib easybreedeR, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom htmltools tags
NULL

# Export all Rcpp functions
#' @export gvr_marker_call_rate
#' @export gvr_individual_call_rate
#' @export gvr_maf
#' @export gvr_individual_het
#' @export gvr_marker_het
#' @export gvr_hwe_exact
#' @export gvr_relatedness_pairs
#' @export gvr_pca_from_dosage_cpp
#' @export fast_pedigree_qc
#' @export fast_pedigree_qc_sex
#' @export fast_detect_loops
#' @export fast_find_deepest_ancestor
#' @export check_birth_date_order
#' @export fast_lap_distribution
#' @export fast_lap_depths
#' @export fast_descendant_summary
#' @export fast_inbreeding_cpp
#' @export fast_top_contrib_cpp
#' @export eb_ped_to_blup_codes_cpp
#' @export gvr_call_rate_from_ped_strings_cpp
#' @export gvr_maf_from_ped_strings_cpp
#' @export gvr_hwe_from_ped_strings_cpp
#' @export gvr_individual_het_from_ped_strings_cpp
#' @export gvr_dosage_from_ped_strings_cpp
NULL

utils::globalVariables(character(0))
