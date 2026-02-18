#include <Rcpp.h>
#include <algorithm>
#include <cctype>
#include <cmath>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

using namespace Rcpp;

namespace {

std::string trim_copy(const std::string& x) {
  std::string s = x;
  auto not_space = [](unsigned char ch) { return !std::isspace(ch); };
  s.erase(s.begin(), std::find_if(s.begin(), s.end(), not_space));
  s.erase(std::find_if(s.rbegin(), s.rend(), not_space).base(), s.end());
  return s;
}

std::string upper_copy(std::string x) {
  std::transform(x.begin(), x.end(), x.begin(), [](unsigned char ch) {
    return static_cast<char>(std::toupper(ch));
  });
  return x;
}

std::string normalize_allele(SEXP x) {
  if (x == NA_STRING) {
    return "";
  }
  std::string s = trim_copy(as<std::string>(x));
  return upper_copy(s);
}

bool is_missing_allele(const std::string& a) {
  return a.empty() || a == "0" || a == "NA" || a == "N" || a == "." || a == "-9";
}

bool parse_ped_pair(const std::string& s_in, std::string& a, std::string& b) {
  std::string s = upper_copy(trim_copy(s_in));
  if (s.empty()) return false;
  std::istringstream iss(s);
  std::string t1, t2, extra;
  if (!(iss >> t1)) return false;
  if (!(iss >> t2)) return false;
  if (iss >> extra) return false;
  a = t1;
  b = t2;
  return true;
}

double hwe_exact_pvalue_local(int obs_hets, int obs_hom1, int obs_hom2) {
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

} // namespace

// Convert PED allele pairs to PLINK-style additive coding for BLUPF90.
// - dosage: 0/1/2 for A1 copies, 5 for missing/unusable
// - a1/a2: PLINK-like minor/major allele labels for each marker
// [[Rcpp::export]]
List eb_ped_to_blup_codes_cpp(CharacterMatrix allele1, CharacterMatrix allele2) {
  const int n_samples = allele1.nrow();
  const int n_markers = allele1.ncol();
  if (allele2.nrow() != n_samples || allele2.ncol() != n_markers) {
    stop("allele1 and allele2 must have the same dimensions");
  }

  IntegerMatrix dosage(n_samples, n_markers);
  CharacterVector a1_out(n_markers);
  CharacterVector a2_out(n_markers);

  for (int j = 0; j < n_markers; ++j) {
    std::unordered_map<std::string, int> allele_count;
    std::unordered_map<std::string, int> first_seen;
    int seen_rank = 0;

    for (int i = 0; i < n_samples; ++i) {
      const std::string a = normalize_allele(allele1(i, j));
      const std::string b = normalize_allele(allele2(i, j));

      if (!is_missing_allele(a)) {
        if (first_seen.find(a) == first_seen.end()) {
          first_seen[a] = seen_rank++;
        }
        ++allele_count[a];
      }
      if (!is_missing_allele(b)) {
        if (first_seen.find(b) == first_seen.end()) {
          first_seen[b] = seen_rank++;
        }
        ++allele_count[b];
      }
    }

    std::vector<std::pair<std::string, int>> alleles;
    alleles.reserve(allele_count.size());
    for (const auto& kv : allele_count) {
      alleles.push_back(kv);
    }

    std::sort(alleles.begin(), alleles.end(), [&](const std::pair<std::string, int>& lhs,
                                                  const std::pair<std::string, int>& rhs) {
      if (lhs.second != rhs.second) {
        return lhs.second > rhs.second; // larger count first
      }
      return first_seen[lhs.first] < first_seen[rhs.first]; // first observed first
    });

    std::string a1 = "0";
    std::string a2 = "0";
    if (alleles.empty()) {
      a1 = "0";
      a2 = "0";
    } else if (alleles.size() == 1) {
      // Match PLINK convention for monomorphic variants: A1=0, A2=observed allele.
      a1 = "0";
      a2 = alleles[0].first;
    } else {
      const std::string major = alleles[0].first;
      const std::string second = alleles[1].first;
      const int major_count = alleles[0].second;
      const int second_count = alleles[1].second;

      if (major_count == second_count) {
        // Tie-break to mirror PLINK's stable behavior: later-seen allele becomes A1.
        const int major_rank = first_seen[major];
        const int second_rank = first_seen[second];
        if (major_rank < second_rank) {
          a1 = second;
          a2 = major;
        } else {
          a1 = major;
          a2 = second;
        }
      } else {
        // A1 is minor, A2 is major.
        a1 = second;
        a2 = major;
      }
    }

    a1_out[j] = a1;
    a2_out[j] = a2;

    for (int i = 0; i < n_samples; ++i) {
      const std::string x = normalize_allele(allele1(i, j));
      const std::string y = normalize_allele(allele2(i, j));

      if (is_missing_allele(x) || is_missing_allele(y)) {
        dosage(i, j) = 5;
        continue;
      }

      if (a1 == "0") {
        // Monomorphic: valid non-missing genotype, zero copies of A1.
        if (x == a2 && y == a2) {
          dosage(i, j) = 0;
        } else {
          dosage(i, j) = 5;
        }
        continue;
      }

      const bool x_known = (x == a1 || x == a2);
      const bool y_known = (y == a1 || y == a2);
      if (!x_known || !y_known) {
        // Additional alleles are treated as missing in the fallback path.
        dosage(i, j) = 5;
        continue;
      }

      dosage(i, j) = static_cast<int>(x == a1) + static_cast<int>(y == a1);
    }
  }

  return List::create(
    _["dosage"] = dosage,
    _["a1"] = a1_out,
    _["a2"] = a2_out
  );
}

// PLINK-aligned call-rate calculation directly from PED-style genotype strings.
// Input matrix cells are expected like "A T", "0 0", "na na", etc.
// Missing if either allele is missing code after trim+toupper.
// [[Rcpp::export]]
List gvr_call_rate_from_ped_strings_cpp(CharacterMatrix geno_pairs) {
  const int n = geno_pairs.nrow();
  const int m = geno_pairs.ncol();
  NumericVector marker_call_rate(m, NA_REAL);
  NumericVector individual_call_rate(n, NA_REAL);

  if (n == 0 || m == 0) {
    return List::create(
      _["marker_call_rate"] = marker_call_rate,
      _["individual_call_rate"] = individual_call_rate
    );
  }

  std::vector<int> marker_non_missing(m, 0);
  for (int i = 0; i < n; ++i) {
    int individual_non_missing = 0;
    for (int j = 0; j < m; ++j) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (is_missing_allele(a) || is_missing_allele(b)) continue;
      individual_non_missing++;
      marker_non_missing[j]++;
    }
    individual_call_rate[i] = static_cast<double>(individual_non_missing) / static_cast<double>(m);
  }

  for (int j = 0; j < m; ++j) {
    marker_call_rate[j] = static_cast<double>(marker_non_missing[j]) / static_cast<double>(n);
  }

  return List::create(
    _["marker_call_rate"] = marker_call_rate,
    _["individual_call_rate"] = individual_call_rate
  );
}

// PLINK-aligned MAF calculation from PED-style genotype strings.
// Missing rules match gvr_call_rate_from_ped_strings_cpp.
// For loci with >2 observed alleles, keep top-2 alleles by count and treat others as missing.
// [[Rcpp::export]]
NumericVector gvr_maf_from_ped_strings_cpp(CharacterMatrix geno_pairs) {
  const int n = geno_pairs.nrow();
  const int m = geno_pairs.ncol();
  NumericVector maf(m, NA_REAL);
  if (n == 0 || m == 0) return maf;

  for (int j = 0; j < m; ++j) {
    std::unordered_map<std::string, int> allele_count;
    std::unordered_map<std::string, int> first_seen;
    int seen_rank = 0;

    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (!is_missing_allele(a)) {
        if (first_seen.find(a) == first_seen.end()) first_seen[a] = seen_rank++;
        allele_count[a]++;
      }
      if (!is_missing_allele(b)) {
        if (first_seen.find(b) == first_seen.end()) first_seen[b] = seen_rank++;
        allele_count[b]++;
      }
    }

    if (allele_count.empty()) {
      maf[j] = NA_REAL;
      continue;
    }
    if (allele_count.size() == 1) {
      maf[j] = 0.0;
      continue;
    }

    std::vector<std::pair<std::string, int>> alleles;
    alleles.reserve(allele_count.size());
    for (const auto& kv : allele_count) alleles.push_back(kv);
    std::sort(alleles.begin(), alleles.end(),
              [&](const std::pair<std::string, int>& lhs, const std::pair<std::string, int>& rhs) {
                if (lhs.second != rhs.second) return lhs.second > rhs.second;
                return first_seen[lhs.first] < first_seen[rhs.first];
              });

    const std::string major = alleles[0].first;
    const std::string minor = alleles[1].first;

    int minor_copies = 0;
    int called_alleles = 0;
    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (is_missing_allele(a) || is_missing_allele(b)) continue;
      const bool a_known = (a == major || a == minor);
      const bool b_known = (b == major || b == minor);
      if (!a_known || !b_known) continue;
      minor_copies += static_cast<int>(a == minor) + static_cast<int>(b == minor);
      called_alleles += 2;
    }

    if (called_alleles > 0) {
      maf[j] = static_cast<double>(minor_copies) / static_cast<double>(called_alleles);
    } else {
      maf[j] = NA_REAL;
    }
  }

  return maf;
}

// PLINK-aligned HWE exact p-values from PED-style genotype strings.
// Missing rules match gvr_call_rate_from_ped_strings_cpp.
// For loci with >2 observed alleles, keep top-2 alleles by count and treat others as missing.
// [[Rcpp::export]]
NumericVector gvr_hwe_from_ped_strings_cpp(CharacterMatrix geno_pairs) {
  const int n = geno_pairs.nrow();
  const int m = geno_pairs.ncol();
  NumericVector pvals(m, NA_REAL);
  if (n == 0 || m == 0) return pvals;

  for (int j = 0; j < m; ++j) {
    std::unordered_map<std::string, int> allele_count;
    std::unordered_map<std::string, int> first_seen;
    int seen_rank = 0;

    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (!is_missing_allele(a)) {
        if (first_seen.find(a) == first_seen.end()) first_seen[a] = seen_rank++;
        allele_count[a]++;
      }
      if (!is_missing_allele(b)) {
        if (first_seen.find(b) == first_seen.end()) first_seen[b] = seen_rank++;
        allele_count[b]++;
      }
    }

    if (allele_count.empty()) {
      pvals[j] = NA_REAL;
      continue;
    }
    if (allele_count.size() == 1) {
      pvals[j] = 1.0;
      continue;
    }

    std::vector<std::pair<std::string, int>> alleles;
    alleles.reserve(allele_count.size());
    for (const auto& kv : allele_count) alleles.push_back(kv);
    std::sort(alleles.begin(), alleles.end(),
              [&](const std::pair<std::string, int>& lhs, const std::pair<std::string, int>& rhs) {
                if (lhs.second != rhs.second) return lhs.second > rhs.second;
                return first_seen[lhs.first] < first_seen[rhs.first];
              });

    const std::string major = alleles[0].first;
    const std::string minor = alleles[1].first;

    int hom_major = 0;
    int het = 0;
    int hom_minor = 0;
    int valid = 0;
    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (is_missing_allele(a) || is_missing_allele(b)) continue;
      const bool a_known = (a == major || a == minor);
      const bool b_known = (b == major || b == minor);
      if (!a_known || !b_known) continue;
      valid++;
      if (a == minor && b == minor) hom_minor++;
      else if (a == major && b == major) hom_major++;
      else het++;
    }

    if (valid > 0) {
      pvals[j] = hwe_exact_pvalue_local(het, hom_major, hom_minor);
    }
  }

  return pvals;
}

// PLINK-aligned individual heterozygosity from PED-style genotype strings.
// Mirrors --het-style observed heterozygosity rate:
//   het_rate = 1 - O(HOM)/N(NM), computed on polymorphic loci.
// Missing rules match gvr_call_rate_from_ped_strings_cpp.
// For loci with >2 observed alleles, keep top-2 alleles by count and treat others as missing.
// [[Rcpp::export]]
NumericVector gvr_individual_het_from_ped_strings_cpp(CharacterMatrix geno_pairs) {
  const int n = geno_pairs.nrow();
  const int m = geno_pairs.ncol();
  NumericVector out(n, NA_REAL);
  if (n == 0 || m == 0) return out;

  std::vector<unsigned char> polymorphic(m, 0);
  std::vector<std::string> major_allele(m), minor_allele(m);

  // First pass: identify per-marker top2 alleles and polymorphic markers.
  for (int j = 0; j < m; ++j) {
    std::unordered_map<std::string, int> allele_count;
    std::unordered_map<std::string, int> first_seen;
    int seen_rank = 0;

    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (!is_missing_allele(a)) {
        if (first_seen.find(a) == first_seen.end()) first_seen[a] = seen_rank++;
        allele_count[a]++;
      }
      if (!is_missing_allele(b)) {
        if (first_seen.find(b) == first_seen.end()) first_seen[b] = seen_rank++;
        allele_count[b]++;
      }
    }

    if (allele_count.empty()) continue;

    std::vector<std::pair<std::string, int>> alleles;
    alleles.reserve(allele_count.size());
    for (const auto& kv : allele_count) alleles.push_back(kv);
    std::sort(alleles.begin(), alleles.end(),
              [&](const std::pair<std::string, int>& lhs, const std::pair<std::string, int>& rhs) {
                if (lhs.second != rhs.second) return lhs.second > rhs.second;
                return first_seen[lhs.first] < first_seen[rhs.first];
              });

    if (alleles.size() == 1) {
      major_allele[j] = alleles[0].first;
      minor_allele[j] = alleles[0].first;
      continue;
    }

    major_allele[j] = alleles[0].first;
    minor_allele[j] = alleles[1].first;

    int minor_copies = 0;
    int called_alleles = 0;
    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (is_missing_allele(a) || is_missing_allele(b)) continue;
      const bool a_known = (a == major_allele[j] || a == minor_allele[j]);
      const bool b_known = (b == major_allele[j] || b == minor_allele[j]);
      if (!a_known || !b_known) continue;
      minor_copies += static_cast<int>(a == minor_allele[j]) + static_cast<int>(b == minor_allele[j]);
      called_alleles += 2;
    }
    if (called_alleles > 0 && minor_copies > 0 && minor_copies < called_alleles) {
      polymorphic[j] = 1;
    }
  }

  // Second pass: compute per-individual heterozygosity rate across polymorphic loci.
  for (int i = 0; i < n; ++i) {
    int valid = 0;
    int het = 0;
    for (int j = 0; j < m; ++j) {
      if (!polymorphic[j]) continue;
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (is_missing_allele(a) || is_missing_allele(b)) continue;
      const bool a_known = (a == major_allele[j] || a == minor_allele[j]);
      const bool b_known = (b == major_allele[j] || b == minor_allele[j]);
      if (!a_known || !b_known) continue;
      valid++;
      if (a != b) het++;
    }
    if (valid > 0) out[i] = static_cast<double>(het) / static_cast<double>(valid);
  }

  return out;
}

// PLINK-aligned dosage matrix from PED-style genotype strings.
// Missing rules match gvr_call_rate_from_ped_strings_cpp.
// For loci with >2 observed alleles, keep top-2 alleles by count and treat others as missing.
// Output dosage counts copies of minor allele (0/1/2), with NA for missing.
// [[Rcpp::export]]
NumericMatrix gvr_dosage_from_ped_strings_cpp(CharacterMatrix geno_pairs) {
  const int n = geno_pairs.nrow();
  const int m = geno_pairs.ncol();
  NumericMatrix dosage(n, m);
  std::fill(dosage.begin(), dosage.end(), NA_REAL);
  if (n == 0 || m == 0) return dosage;

  for (int j = 0; j < m; ++j) {
    std::unordered_map<std::string, int> allele_count;
    std::unordered_map<std::string, int> first_seen;
    int seen_rank = 0;

    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (!is_missing_allele(a)) {
        if (first_seen.find(a) == first_seen.end()) first_seen[a] = seen_rank++;
        allele_count[a]++;
      }
      if (!is_missing_allele(b)) {
        if (first_seen.find(b) == first_seen.end()) first_seen[b] = seen_rank++;
        allele_count[b]++;
      }
    }

    if (allele_count.empty()) continue;

    std::vector<std::pair<std::string, int>> alleles;
    alleles.reserve(allele_count.size());
    for (const auto& kv : allele_count) alleles.push_back(kv);
    std::sort(alleles.begin(), alleles.end(),
              [&](const std::pair<std::string, int>& lhs, const std::pair<std::string, int>& rhs) {
                if (lhs.second != rhs.second) return lhs.second > rhs.second;
                return first_seen[lhs.first] < first_seen[rhs.first];
              });

    std::string major = alleles[0].first;
    std::string minor = alleles[0].first;
    if (alleles.size() > 1) minor = alleles[1].first;

    for (int i = 0; i < n; ++i) {
      if (geno_pairs(i, j) == NA_STRING) continue;
      std::string a, b;
      if (!parse_ped_pair(as<std::string>(geno_pairs(i, j)), a, b)) continue;
      if (is_missing_allele(a) || is_missing_allele(b)) continue;

      if (major == minor) {
        if (a == major && b == major) dosage(i, j) = 0.0;
        continue;
      }

      const bool a_known = (a == major || a == minor);
      const bool b_known = (b == major || b == minor);
      if (!a_known || !b_known) continue;
      dosage(i, j) = static_cast<double>((a == minor) + (b == minor));
    }
  }

  return dosage;
}
