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
utils::globalVariables(character(0))
