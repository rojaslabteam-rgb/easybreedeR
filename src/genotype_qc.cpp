#include <Rcpp.h>
using namespace Rcpp;

inline bool is_missing(double x) {
  return NumericVector::is_na(x) || !R_finite(x);
}

inline bool as_dosage(double x, int &out) {
  if (is_missing(x)) return false;
  double rx = std::round(x);
  if (std::fabs(x - rx) > 1e-8) return false;
  int v = (int)rx;
  if (v < 0 || v > 2) return false;
  out = v;
  return true;
}

inline double hw_prob(int g, double p) {
  double q = 1.0 - p;
  if (g == 0) return q * q;
  if (g == 1) return 2.0 * p * q;
  if (g == 2) return p * p;
  return 0.0;
}

inline double pair_prob_z0(int g1, int g2, double p) {
  return hw_prob(g1, p) * hw_prob(g2, p);
}

// One shared IBD allele (Z1): genotype-pair probabilities under random mating.
inline double pair_prob_z1(int g1, int g2, double p) {
  double q = 1.0 - p;
  if (g1 == 0 && g2 == 0) return q * q * q;
  if ((g1 == 0 && g2 == 1) || (g1 == 1 && g2 == 0)) return p * q * q;
  if (g1 == 1 && g2 == 1) return p * q;
  if ((g1 == 1 && g2 == 2) || (g1 == 2 && g2 == 1)) return p * p * q;
  if (g1 == 2 && g2 == 2) return p * p * p;
  return 0.0;
}

inline double pair_prob_z2(int g1, int g2, double p) {
  if (g1 != g2) return 0.0;
  return hw_prob(g1, p);
}

// [[Rcpp::export]]
NumericVector gvr_marker_call_rate(NumericMatrix geno) {
  int n = geno.nrow(), m = geno.ncol();
  NumericVector out(m, NA_REAL);
  if (n == 0 || m == 0) return out;
  for (int j = 0; j < m; ++j) {
    int non_missing = 0;
    for (int i = 0; i < n; ++i) {
      int d = 0;
      if (as_dosage(geno(i, j), d)) non_missing++;
    }
    out[j] = (double)non_missing / (double)n;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector gvr_individual_call_rate(NumericMatrix geno) {
  int n = geno.nrow(), m = geno.ncol();
  NumericVector out(n, NA_REAL);
  if (n == 0 || m == 0) return out;
  for (int i = 0; i < n; ++i) {
    int non_missing = 0;
    for (int j = 0; j < m; ++j) {
      int d = 0;
      if (as_dosage(geno(i, j), d)) non_missing++;
    }
    out[i] = (double)non_missing / (double)m;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector gvr_maf(NumericMatrix geno) {
  int n = geno.nrow(), m = geno.ncol();
  NumericVector out(m, NA_REAL);
  for (int j = 0; j < m; ++j) {
    double sum_d = 0.0;
    int cnt = 0;
    for (int i = 0; i < n; ++i) {
      int d = 0;
      if (as_dosage(geno(i, j), d)) {
        sum_d += d;
        cnt++;
      }
    }
    if (cnt > 0) {
      double p = (sum_d / (double)cnt) / 2.0;
      out[j] = std::min(p, 1.0 - p);
    }
  }
  return out;
}

// [[Rcpp::export]]
NumericVector gvr_individual_het(NumericMatrix geno) {
  int n = geno.nrow(), m = geno.ncol();
  NumericVector out(n, NA_REAL);
  if (n == 0 || m == 0) return out;

  // Align with PLINK --het behavior: exclude monomorphic markers.
  std::vector<unsigned char> polymorphic(m, 0);
  for (int j = 0; j < m; ++j) {
    double sum_d = 0.0;
    int ct = 0;
    for (int i = 0; i < n; ++i) {
      int d = 0;
      if (as_dosage(geno(i, j), d)) {
        sum_d += d;
        ct++;
      }
    }
    if (ct > 0) {
      double p = (sum_d / (double)ct) / 2.0;
      if (p > 1e-12 && p < 1.0 - 1e-12) polymorphic[j] = 1;
    }
  }

  for (int i = 0; i < n; ++i) {
    int valid = 0, het = 0;
    for (int j = 0; j < m; ++j) {
      if (!polymorphic[j]) continue;
      int d = 0;
      if (as_dosage(geno(i, j), d)) {
        valid++;
        if (d == 1) het++;
      }
    }
    if (valid > 0) out[i] = (double)het / (double)valid;
  }
  return out;
}

// [[Rcpp::export]]
NumericVector gvr_marker_het(NumericMatrix geno) {
  int n = geno.nrow(), m = geno.ncol();
  NumericVector out(m, NA_REAL);
  for (int j = 0; j < m; ++j) {
    int valid = 0, het = 0;
    for (int i = 0; i < n; ++i) {
      int d = 0;
      if (as_dosage(geno(i, j), d)) {
        valid++;
        if (d == 1) het++;
      }
    }
    if (valid > 0) out[j] = (double)het / (double)valid;
  }
  return out;
}

static double hwe_exact_pvalue(int obs_hets, int obs_hom1, int obs_hom2) {
  // Exact HWE test uses the Wigginton recursion used by PLINK's Hardy-Weinberg code path.
  int obs_homr = std::min(obs_hom1, obs_hom2);
  int obs_homc = std::max(obs_hom1, obs_hom2);
  int rare_copies = 2 * obs_homr + obs_hets;
  int genotypes = obs_hets + obs_homc + obs_homr;
  if (genotypes <= 0) return NA_REAL;

  std::vector<double> probs(rare_copies + 1, 0.0);
  int mid = (int) std::floor((double)rare_copies * (2.0 * genotypes - rare_copies) / (2.0 * genotypes));
  if ((rare_copies & 1) != (mid & 1)) mid++;

  int curr_hets = mid;
  int curr_homr = (rare_copies - mid) / 2;
  int curr_homc = genotypes - curr_hets - curr_homr;

  probs[mid] = 1.0;
  double sum = probs[mid];

  while (curr_hets > 1) {
    double p = probs[curr_hets] * curr_hets * (curr_hets - 1.0) /
      (4.0 * (curr_homr + 1.0) * (curr_homc + 1.0));
    probs[curr_hets - 2] = p;
    sum += p;
    curr_hets -= 2;
    curr_homr += 1;
    curr_homc += 1;
  }

  curr_hets = mid;
  curr_homr = (rare_copies - mid) / 2;
  curr_homc = genotypes - curr_hets - curr_homr;
  while (curr_hets <= rare_copies - 2) {
    double p = probs[curr_hets] * 4.0 * curr_homr * curr_homc /
      ((curr_hets + 2.0) * (curr_hets + 1.0));
    probs[curr_hets + 2] = p;
    sum += p;
    curr_hets += 2;
    curr_homr -= 1;
    curr_homc -= 1;
  }

  if (!(obs_hets >= 0 && obs_hets <= rare_copies)) return NA_REAL;
  if (sum <= 0.0) return NA_REAL;
  for (int i = 0; i <= rare_copies; ++i) probs[i] /= sum;

  double p_obs = probs[obs_hets];
  double p_hwe = 0.0;
  for (int i = (rare_copies & 1); i <= rare_copies; i += 2) {
    if (probs[i] <= p_obs + 1e-12) p_hwe += probs[i];
  }
  if (p_hwe > 1.0) p_hwe = 1.0;
  return p_hwe;
}

// [[Rcpp::export]]
NumericVector gvr_hwe_exact(NumericMatrix geno) {
  int n = geno.nrow(), m = geno.ncol();
  NumericVector out(m, NA_REAL);
  for (int j = 0; j < m; ++j) {
    int hom0 = 0, het = 0, hom2 = 0, valid = 0;
    for (int i = 0; i < n; ++i) {
      int d = 0;
      if (as_dosage(geno(i, j), d)) {
        valid++;
        if (d == 0) hom0++;
        else if (d == 1) het++;
        else if (d == 2) hom2++;
      }
    }
    if (valid >= 1) out[j] = hwe_exact_pvalue(het, hom0, hom2);
  }
  return out;
}

// [[Rcpp::export]]
DataFrame gvr_relatedness_pairs(NumericMatrix geno, CharacterVector sample_ids,
                                int max_pairs = 2147483647, int max_markers = 2147483647, int min_valid = 20) {
  // PLINK-inspired IBD estimation:
  // 1) compute IBS counts and method-of-moments initializer
  // 2) refine Z0/Z1/Z2 with EM on per-locus pair likelihoods under Z states.
  int n = geno.nrow(), m = geno.ncol();
  if (n < 2 || m < 1) {
    return DataFrame::create(
      _["FID1"] = CharacterVector(0), _["IID1"] = CharacterVector(0),
      _["FID2"] = CharacterVector(0), _["IID2"] = CharacterVector(0),
      _["RT"] = CharacterVector(0), _["EZ"] = NumericVector(0),
      _["Z0"] = NumericVector(0), _["Z1"] = NumericVector(0), _["Z2"] = NumericVector(0),
      _["PI_HAT"] = NumericVector(0), _["PHE"] = IntegerVector(0),
      _["DST"] = NumericVector(0), _["PPC"] = NumericVector(0), _["RATIO"] = NumericVector(0)
    );
  }
  int use_m = std::min(m, max_markers);
  if (min_valid < 1) min_valid = 1;
  if (min_valid > use_m) min_valid = use_m;

  NumericVector p_alt(use_m, NA_REAL), e00(use_m, NA_REAL), e10(use_m, NA_REAL), e11(use_m, NA_REAL);
  for (int c = 0; c < use_m; ++c) {
    double s = 0.0;
    int ct = 0;
    for (int r = 0; r < n; ++r) {
      int d = 0;
      if (!as_dosage(geno(r, c), d)) continue;
      s += d;
      ct++;
    }
    if (ct > 0) {
      double p = (s / (double)ct) / 2.0;
      if (p < 0.0) p = 0.0;
      if (p > 1.0) p = 1.0;
      double q = 1.0 - p;
      p_alt[c] = p;
      e00[c] = 2.0 * p * p * q * q;                 // E[IBS0 | Z0=1]
      e10[c] = 4.0 * p * q * (p * p + q * q);       // E[IBS1 | Z0=1]
      e11[c] = 2.0 * p * q;                          // E[IBS1 | Z1=1]
    }
  }

  std::vector< std::pair<int,int> > pairs;
  pairs.reserve(std::min((long long)max_pairs, (long long)n * (n - 1) / 2));
  for (int i = 0; i < n - 1; ++i) {
    for (int j = i + 1; j < n; ++j) {
      if ((int)pairs.size() >= max_pairs) break;
      pairs.push_back(std::make_pair(i, j));
    }
    if ((int)pairs.size() >= max_pairs) break;
  }

  size_t np = pairs.size();
  CharacterVector fid1(np), iid1(np), fid2(np), iid2(np), rt(np);
  NumericVector ez(np, NA_REAL), z0(np, NA_REAL), z1(np, NA_REAL), z2(np, NA_REAL);
  NumericVector pi_hat(np, NA_REAL), dst(np, NA_REAL), ppc(np, NA_REAL), ratio(np, NA_REAL);
  IntegerVector phe(np, -1);

  for (size_t k = 0; k < np; ++k) {
    int i = pairs[k].first;
    int j = pairs[k].second;
    int ibs0 = 0, ibs1 = 0, ibs2 = 0, hethet = 0, loci = 0;
    double sum_e00 = 0.0, sum_e10 = 0.0, sum_e11 = 0.0;
    std::vector<int> gi;
    std::vector<int> gj;
    std::vector<double> gp;
    gi.reserve(use_m);
    gj.reserve(use_m);
    gp.reserve(use_m);

    for (int c = 0; c < use_m; ++c) {
      int di = 0, dj = 0;
      if (!as_dosage(geno(i, c), di) || !as_dosage(geno(j, c), dj)) continue;
      if (!R_finite(e00[c]) || !R_finite(e10[c]) || !R_finite(e11[c])) continue;
      // Monomorphic loci do not inform IBD state estimation.
      if (p_alt[c] <= 0.0 || p_alt[c] >= 1.0) continue;
      loci++;
      sum_e00 += e00[c];
      sum_e10 += e10[c];
      sum_e11 += e11[c];
      gi.push_back(di);
      gj.push_back(dj);
      gp.push_back(p_alt[c]);
      int ad = std::abs(di - dj);
      if (ad == 2) ibs0++;
      else if (ad == 1) ibs1++;
      else {
        ibs2++;
        if (di == 1 && dj == 1) hethet++;
      }
    }

    iid1[k] = sample_ids[i];
    fid1[k] = sample_ids[i];
    iid2[k] = sample_ids[j];
    fid2[k] = sample_ids[j];
    rt[k] = "UN";

    if (loci >= min_valid) {
      dst[k] = ((double)ibs2 + 0.5 * (double)ibs1) / (double)loci;
      if (ibs0 > 0) ratio[k] = (double)hethet / (double)ibs0;

      // Initialize with moment estimator, then refine with EM over per-locus pair likelihoods.
      double zz0 = (sum_e00 > 1e-12) ? ((double)ibs0 / sum_e00) : 0.0;
      if (!R_finite(zz0) || zz0 < 0.0) zz0 = 0.0;
      if (zz0 > 1.0) zz0 = 1.0;

      double zz1 = (sum_e11 > 1e-12) ? (((double)ibs1 - zz0 * sum_e10) / sum_e11) : 0.0;
      if (!R_finite(zz1) || zz1 < 0.0) zz1 = 0.0;
      if (zz1 > 1.0) zz1 = 1.0;

      double zz2 = 1.0 - zz0 - zz1;
      if (!R_finite(zz2) || zz2 < 0.0) zz2 = 0.0;
      if (zz2 > 1.0) zz2 = 1.0;

      double szz = zz0 + zz1 + zz2;
      if (szz > 1e-12) {
        zz0 /= szz;
        zz1 /= szz;
        zz2 /= szz;
      } else {
        zz0 = 0.99;
        zz1 = 0.01;
        zz2 = 0.0;
      }

      const int em_max_iter = 30;
      for (int it = 0; it < em_max_iter; ++it) {
        double a0 = 0.0, a1 = 0.0, a2 = 0.0;
        for (int t = 0; t < loci; ++t) {
          double p = gp[t];
          double p0 = pair_prob_z0(gi[t], gj[t], p);
          double p1 = pair_prob_z1(gi[t], gj[t], p);
          double p2 = pair_prob_z2(gi[t], gj[t], p);
          double den = zz0 * p0 + zz1 * p1 + zz2 * p2;
          if (den <= 0.0 || !R_finite(den)) continue;
          a0 += (zz0 * p0) / den;
          a1 += (zz1 * p1) / den;
          a2 += (zz2 * p2) / den;
        }
        double at = a0 + a1 + a2;
        if (at <= 1e-12 || !R_finite(at)) break;
        double nz0 = a0 / at;
        double nz1 = a1 / at;
        double nz2 = a2 / at;
        if (!R_finite(nz0) || !R_finite(nz1) || !R_finite(nz2)) break;
        double delta = std::fabs(nz0 - zz0) + std::fabs(nz1 - zz1) + std::fabs(nz2 - zz2);
        zz0 = nz0;
        zz1 = nz1;
        zz2 = nz2;
        if (delta < 1e-8) break;
      }

      z0[k] = zz0;
      z1[k] = zz1;
      z2[k] = zz2;
      pi_hat[k] = zz2 + 0.5 * zz1;
    }
  }

  return DataFrame::create(
    _["FID1"] = fid1, _["IID1"] = iid1,
    _["FID2"] = fid2, _["IID2"] = iid2,
    _["RT"] = rt, _["EZ"] = ez,
    _["Z0"] = z0, _["Z1"] = z1, _["Z2"] = z2,
    _["PI_HAT"] = pi_hat, _["PHE"] = phe,
    _["DST"] = dst, _["PPC"] = ppc, _["RATIO"] = ratio
  );
}
