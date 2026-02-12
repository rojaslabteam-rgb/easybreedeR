#include <Rcpp.h>
#include <algorithm>
#include <cctype>
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
