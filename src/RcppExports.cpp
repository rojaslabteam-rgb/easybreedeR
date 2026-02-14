#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// gvr_marker_call_rate
NumericVector gvr_marker_call_rate(NumericMatrix geno);
RcppExport SEXP _easybreedeR_gvr_marker_call_rate(SEXP genoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_marker_call_rate(geno));
    return rcpp_result_gen;
END_RCPP
}
// gvr_individual_call_rate
NumericVector gvr_individual_call_rate(NumericMatrix geno);
RcppExport SEXP _easybreedeR_gvr_individual_call_rate(SEXP genoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_individual_call_rate(geno));
    return rcpp_result_gen;
END_RCPP
}
// gvr_maf
NumericVector gvr_maf(NumericMatrix geno);
RcppExport SEXP _easybreedeR_gvr_maf(SEXP genoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_maf(geno));
    return rcpp_result_gen;
END_RCPP
}
// gvr_individual_het
NumericVector gvr_individual_het(NumericMatrix geno);
RcppExport SEXP _easybreedeR_gvr_individual_het(SEXP genoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_individual_het(geno));
    return rcpp_result_gen;
END_RCPP
}
// gvr_marker_het
NumericVector gvr_marker_het(NumericMatrix geno);
RcppExport SEXP _easybreedeR_gvr_marker_het(SEXP genoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_marker_het(geno));
    return rcpp_result_gen;
END_RCPP
}
// gvr_hwe_exact
NumericVector gvr_hwe_exact(NumericMatrix geno);
RcppExport SEXP _easybreedeR_gvr_hwe_exact(SEXP genoSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_hwe_exact(geno));
    return rcpp_result_gen;
END_RCPP
}
// gvr_relatedness_pairs
DataFrame gvr_relatedness_pairs(NumericMatrix geno, CharacterVector sample_ids, int max_pairs, int max_markers, int min_valid);
RcppExport SEXP _easybreedeR_gvr_relatedness_pairs(SEXP genoSEXP, SEXP sample_idsSEXP, SEXP max_pairsSEXP, SEXP max_markersSEXP, SEXP min_validSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type geno(genoSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sample_ids(sample_idsSEXP);
    Rcpp::traits::input_parameter< int >::type max_pairs(max_pairsSEXP);
    Rcpp::traits::input_parameter< int >::type max_markers(max_markersSEXP);
    Rcpp::traits::input_parameter< int >::type min_valid(min_validSEXP);
    rcpp_result_gen = Rcpp::wrap(gvr_relatedness_pairs(geno, sample_ids, max_pairs, max_markers, min_valid));
    return rcpp_result_gen;
END_RCPP
}
// fast_pedigree_qc
List fast_pedigree_qc(CharacterVector ids, CharacterVector sires, CharacterVector dams);
RcppExport SEXP _easybreedeR_fast_pedigree_qc(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_pedigree_qc(ids, sires, dams));
    return rcpp_result_gen;
END_RCPP
}
// fast_pedigree_qc_sex
List fast_pedigree_qc_sex(CharacterVector ids, CharacterVector sires, CharacterVector dams, CharacterVector sex);
RcppExport SEXP _easybreedeR_fast_pedigree_qc_sex(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP, SEXP sexSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sex(sexSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_pedigree_qc_sex(ids, sires, dams, sex));
    return rcpp_result_gen;
END_RCPP
}
// fast_detect_loops
List fast_detect_loops(CharacterVector ids, CharacterVector sires, CharacterVector dams);
RcppExport SEXP _easybreedeR_fast_detect_loops(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_detect_loops(ids, sires, dams));
    return rcpp_result_gen;
END_RCPP
}
// fast_find_deepest_ancestor
List fast_find_deepest_ancestor(CharacterVector ids, CharacterVector sires, CharacterVector dams, int sample_size);
RcppExport SEXP _easybreedeR_fast_find_deepest_ancestor(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP, SEXP sample_sizeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    Rcpp::traits::input_parameter< int >::type sample_size(sample_sizeSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_find_deepest_ancestor(ids, sires, dams, sample_size));
    return rcpp_result_gen;
END_RCPP
}
// check_birth_date_order
List check_birth_date_order(CharacterVector ids, CharacterVector sires, CharacterVector dams, NumericVector birth_dates);
RcppExport SEXP _easybreedeR_check_birth_date_order(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP, SEXP birth_datesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type birth_dates(birth_datesSEXP);
    rcpp_result_gen = Rcpp::wrap(check_birth_date_order(ids, sires, dams, birth_dates));
    return rcpp_result_gen;
END_RCPP
}
// fast_lap_distribution
NumericVector fast_lap_distribution(CharacterVector ids, CharacterVector sires, CharacterVector dams, int sample_size, int max_depth);
RcppExport SEXP _easybreedeR_fast_lap_distribution(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP, SEXP sample_sizeSEXP, SEXP max_depthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    Rcpp::traits::input_parameter< int >::type sample_size(sample_sizeSEXP);
    Rcpp::traits::input_parameter< int >::type max_depth(max_depthSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_lap_distribution(ids, sires, dams, sample_size, max_depth));
    return rcpp_result_gen;
END_RCPP
}
// fast_lap_depths
IntegerVector fast_lap_depths(CharacterVector ids, CharacterVector sires, CharacterVector dams);
RcppExport SEXP _easybreedeR_fast_lap_depths(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_lap_depths(ids, sires, dams));
    return rcpp_result_gen;
END_RCPP
}
// fast_descendant_summary
List fast_descendant_summary(CharacterVector ids, CharacterVector parent_vals, int max_depth);
RcppExport SEXP _easybreedeR_fast_descendant_summary(SEXP idsSEXP, SEXP parent_valsSEXP, SEXP max_depthSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type parent_vals(parent_valsSEXP);
    Rcpp::traits::input_parameter< int >::type max_depth(max_depthSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_descendant_summary(ids, parent_vals, max_depth));
    return rcpp_result_gen;
END_RCPP
}
// fast_inbreeding_cpp
NumericVector fast_inbreeding_cpp(CharacterVector ids, CharacterVector sires, CharacterVector dams);
RcppExport SEXP _easybreedeR_fast_inbreeding_cpp(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type dams(damsSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_inbreeding_cpp(ids, sires, dams));
    return rcpp_result_gen;
END_RCPP
}
// fast_top_contrib_cpp
Rcpp::DataFrame fast_top_contrib_cpp(Rcpp::CharacterVector ids, Rcpp::CharacterVector sires, Rcpp::CharacterVector dams, Rcpp::NumericVector F, std::string target_id, int max_depth, int top_k);
RcppExport SEXP _easybreedeR_fast_top_contrib_cpp(SEXP idsSEXP, SEXP siresSEXP, SEXP damsSEXP, SEXP FSEXP, SEXP target_idSEXP, SEXP max_depthSEXP, SEXP top_kSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type ids(idsSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type sires(siresSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type dams(damsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type F(FSEXP);
    Rcpp::traits::input_parameter< std::string >::type target_id(target_idSEXP);
    Rcpp::traits::input_parameter< int >::type max_depth(max_depthSEXP);
    Rcpp::traits::input_parameter< int >::type top_k(top_kSEXP);
    rcpp_result_gen = Rcpp::wrap(fast_top_contrib_cpp(ids, sires, dams, F, target_id, max_depth, top_k));
    return rcpp_result_gen;
END_RCPP
}
// eb_ped_to_blup_codes_cpp
List eb_ped_to_blup_codes_cpp(CharacterMatrix allele1, CharacterMatrix allele2);
RcppExport SEXP _easybreedeR_eb_ped_to_blup_codes_cpp(SEXP allele1SEXP, SEXP allele2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterMatrix >::type allele1(allele1SEXP);
    Rcpp::traits::input_parameter< CharacterMatrix >::type allele2(allele2SEXP);
    rcpp_result_gen = Rcpp::wrap(eb_ped_to_blup_codes_cpp(allele1, allele2));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_easybreedeR_gvr_marker_call_rate", (DL_FUNC) &_easybreedeR_gvr_marker_call_rate, 1},
    {"_easybreedeR_gvr_individual_call_rate", (DL_FUNC) &_easybreedeR_gvr_individual_call_rate, 1},
    {"_easybreedeR_gvr_maf", (DL_FUNC) &_easybreedeR_gvr_maf, 1},
    {"_easybreedeR_gvr_individual_het", (DL_FUNC) &_easybreedeR_gvr_individual_het, 1},
    {"_easybreedeR_gvr_marker_het", (DL_FUNC) &_easybreedeR_gvr_marker_het, 1},
    {"_easybreedeR_gvr_hwe_exact", (DL_FUNC) &_easybreedeR_gvr_hwe_exact, 1},
    {"_easybreedeR_gvr_relatedness_pairs", (DL_FUNC) &_easybreedeR_gvr_relatedness_pairs, 5},
    {"_easybreedeR_fast_pedigree_qc", (DL_FUNC) &_easybreedeR_fast_pedigree_qc, 3},
    {"_easybreedeR_fast_pedigree_qc_sex", (DL_FUNC) &_easybreedeR_fast_pedigree_qc_sex, 4},
    {"_easybreedeR_fast_detect_loops", (DL_FUNC) &_easybreedeR_fast_detect_loops, 3},
    {"_easybreedeR_fast_find_deepest_ancestor", (DL_FUNC) &_easybreedeR_fast_find_deepest_ancestor, 4},
    {"_easybreedeR_check_birth_date_order", (DL_FUNC) &_easybreedeR_check_birth_date_order, 4},
    {"_easybreedeR_fast_lap_distribution", (DL_FUNC) &_easybreedeR_fast_lap_distribution, 5},
    {"_easybreedeR_fast_lap_depths", (DL_FUNC) &_easybreedeR_fast_lap_depths, 3},
    {"_easybreedeR_fast_descendant_summary", (DL_FUNC) &_easybreedeR_fast_descendant_summary, 3},
    {"_easybreedeR_fast_inbreeding_cpp", (DL_FUNC) &_easybreedeR_fast_inbreeding_cpp, 3},
    {"_easybreedeR_fast_top_contrib_cpp", (DL_FUNC) &_easybreedeR_fast_top_contrib_cpp, 7},
    {"_easybreedeR_eb_ped_to_blup_codes_cpp", (DL_FUNC) &_easybreedeR_eb_ped_to_blup_codes_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_easybreedeR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
