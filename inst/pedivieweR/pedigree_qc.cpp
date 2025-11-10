#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <string>
using namespace Rcpp;

// [[Rcpp::export]]
List fast_pedigree_qc(CharacterVector ids, 
                      CharacterVector sires, 
                      CharacterVector dams) {
  
  int n = ids.size();
  
  // Use unordered_set for O(1) lookup
  std::unordered_set<std::string> id_set;
  std::unordered_set<std::string> missing_sires_set;
  std::unordered_set<std::string> missing_dams_set;
  std::unordered_map<std::string, int> id_count;
  
  int founders = 0;
  int with_both_parents = 0;
  int self_parent_count = 0;
  std::vector<std::string> duplicate_ids;
  
  // First pass: build ID set and count duplicates
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    id_set.insert(id);
    
    id_count[id]++;
    if (id_count[id] == 2) {
      duplicate_ids.push_back(id);
    }
  }
  
  // Second pass: analyze relationships
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    
    // Count founders
    if (!has_sire && !has_dam) {
      founders++;
    }
    
    // Count with both parents
    if (has_sire && has_dam) {
      with_both_parents++;
    }
    
    // Check self-parenting
    if ((has_sire && sire == id) || (has_dam && dam == id)) {
      self_parent_count++;
    }
    
    // Check missing parents
    if (has_sire && id_set.find(sire) == id_set.end()) {
      missing_sires_set.insert(sire);
    }
    if (has_dam && id_set.find(dam) == id_set.end()) {
      missing_dams_set.insert(dam);
    }
  }
  
  // Convert sets to vectors
  CharacterVector missing_sires(missing_sires_set.size());
  int idx = 0;
  for (const auto& s : missing_sires_set) {
    missing_sires[idx++] = s;
  }
  
  CharacterVector missing_dams(missing_dams_set.size());
  idx = 0;
  for (const auto& d : missing_dams_set) {
    missing_dams[idx++] = d;
  }
  
  CharacterVector duplicates(duplicate_ids.size());
  for (size_t i = 0; i < duplicate_ids.size(); i++) {
    duplicates[i] = duplicate_ids[i];
  }
  
  return List::create(
    Named("total") = n,
    Named("founders") = founders,
    Named("with_both_parents") = with_both_parents,
    Named("self_parent_count") = self_parent_count,
    Named("duplicate_ids") = duplicates,
    Named("missing_sires") = missing_sires,
    Named("missing_dams") = missing_dams
  );
}
