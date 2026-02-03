#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <algorithm>
#include <random>
#include <functional>
#include <queue>
#include <cctype>
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
  std::unordered_set<std::string> sires_mentioned_set;
  std::unordered_set<std::string> dams_mentioned_set;
  std::unordered_map<std::string, int> id_count;
  std::unordered_map<std::string, int> sire_progeny_count;
  std::unordered_map<std::string, int> dam_progeny_count;
  std::unordered_set<std::string> founder_set;
  
  int founders = 0;
  int with_both_parents = 0;
  int only_sire_count = 0;
  int only_dam_count = 0;
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
      founder_set.insert(id);
    }
    
    // Count with both parents / only sire / only dam
    if (has_sire && has_dam) {
      with_both_parents++;
    } else if (has_sire && !has_dam) {
      only_sire_count++;
    } else if (!has_sire && has_dam) {
      only_dam_count++;
    }
    
    // Check self-parenting
    if ((has_sire && sire == id) || (has_dam && dam == id)) {
      self_parent_count++;
    }
    
    // Check missing parents
    if (has_sire) {
      sires_mentioned_set.insert(sire);
      sire_progeny_count[sire]++;
    }
    if (has_dam) {
      dams_mentioned_set.insert(dam);
      dam_progeny_count[dam]++;
    }
    
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
  
  // Dual-role parent IDs (appear as both sire and dam)
  std::vector<std::string> dual_role_ids;
  for (const auto& s : sires_mentioned_set) {
    if (dams_mentioned_set.find(s) != dams_mentioned_set.end()) {
      dual_role_ids.push_back(s);
    }
  }
  CharacterVector dual_role(dual_role_ids.size());
  for (size_t i = 0; i < dual_role_ids.size(); i++) {
    dual_role[i] = dual_role_ids[i];
  }
  
  // Parent/progeny statistics
  int unique_sires = static_cast<int>(sire_progeny_count.size());
  int unique_dams = static_cast<int>(dam_progeny_count.size());
  long long total_sire_progeny = 0;
  long long total_dam_progeny = 0;
  for (const auto& kv : sire_progeny_count) total_sire_progeny += kv.second;
  for (const auto& kv : dam_progeny_count) total_dam_progeny += kv.second;
  
  std::unordered_set<std::string> parents_with_progeny;
  for (const auto& kv : sire_progeny_count) {
    if (id_set.find(kv.first) != id_set.end()) parents_with_progeny.insert(kv.first);
  }
  for (const auto& kv : dam_progeny_count) {
    if (id_set.find(kv.first) != id_set.end()) parents_with_progeny.insert(kv.first);
  }
  int individuals_with_progeny = static_cast<int>(parents_with_progeny.size());
  int individuals_without_progeny = n - individuals_with_progeny;
  
  int founder_sires = 0;
  int founder_dams = 0;
  long long founder_sire_progeny = 0;
  long long founder_dam_progeny = 0;
  std::unordered_set<std::string> founder_parent_ids;
  for (const auto& kv : sire_progeny_count) {
    if (founder_set.find(kv.first) != founder_set.end()) {
      founder_sires++;
      founder_sire_progeny += kv.second;
      founder_parent_ids.insert(kv.first);
    }
  }
  for (const auto& kv : dam_progeny_count) {
    if (founder_set.find(kv.first) != founder_set.end()) {
      founder_dams++;
      founder_dam_progeny += kv.second;
      founder_parent_ids.insert(kv.first);
    }
  }
  long long founder_total_progeny = 0;
  for (int i = 0; i < n; i++) {
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    if ((has_sire && founder_set.find(sire) != founder_set.end()) ||
        (has_dam && founder_set.find(dam) != founder_set.end())) {
      founder_total_progeny++;
    }
  }
  int founder_no_progeny = founders - static_cast<int>(founder_parent_ids.size());
  
  int non_founder_sires = 0;
  int non_founder_dams = 0;
  long long non_founder_sire_progeny = 0;
  long long non_founder_dam_progeny = 0;
  for (const auto& kv : sire_progeny_count) {
    if (founder_set.find(kv.first) == founder_set.end()) {
      non_founder_sires++;
      non_founder_sire_progeny += kv.second;
    }
  }
  for (const auto& kv : dam_progeny_count) {
    if (founder_set.find(kv.first) == founder_set.end()) {
      non_founder_dams++;
      non_founder_dam_progeny += kv.second;
    }
  }
  
  return List::create(
    Named("total") = n,
    Named("founders") = founders,
    Named("with_both_parents") = with_both_parents,
    Named("only_sire") = only_sire_count,
    Named("only_dam") = only_dam_count,
    Named("self_parent_count") = self_parent_count,
    Named("duplicate_ids") = duplicates,
    Named("missing_sires") = missing_sires,
    Named("missing_dams") = missing_dams,
    Named("dual_role_ids") = dual_role,
    Named("unique_sires") = unique_sires,
    Named("unique_dams") = unique_dams,
    Named("total_sire_progeny") = total_sire_progeny,
    Named("total_dam_progeny") = total_dam_progeny,
    Named("individuals_with_progeny") = individuals_with_progeny,
    Named("individuals_without_progeny") = individuals_without_progeny,
    Named("founder_sires") = founder_sires,
    Named("founder_dams") = founder_dams,
    Named("founder_sire_progeny") = founder_sire_progeny,
    Named("founder_dam_progeny") = founder_dam_progeny,
    Named("founder_total_progeny") = founder_total_progeny,
    Named("founder_no_progeny") = founder_no_progeny,
    Named("non_founder_sires") = non_founder_sires,
    Named("non_founder_dams") = non_founder_dams,
    Named("non_founder_sire_progeny") = non_founder_sire_progeny,
    Named("non_founder_dam_progeny") = non_founder_dam_progeny
  );
}

// [[Rcpp::export]]
List fast_pedigree_qc_sex(CharacterVector ids,
                          CharacterVector sires,
                          CharacterVector dams,
                          CharacterVector sex) {
  int n = ids.size();
  
  // Use unordered_set for O(1) lookup
  std::unordered_set<std::string> id_set;
  std::unordered_set<std::string> missing_sires_set;
  std::unordered_set<std::string> missing_dams_set;
  std::unordered_set<std::string> sires_mentioned_set;
  std::unordered_set<std::string> dams_mentioned_set;
  std::unordered_map<std::string, int> id_count;
  std::unordered_map<std::string, char> sex_map;
  std::unordered_map<std::string, int> sire_progeny_count;
  std::unordered_map<std::string, int> dam_progeny_count;
  std::unordered_set<std::string> founder_set;
  
  int founders = 0;
  int with_both_parents = 0;
  int only_sire_count = 0;
  int only_dam_count = 0;
  int self_parent_count = 0;
  std::vector<std::string> duplicate_ids;
  
  // Helper to normalize sex
  auto normalize_sex = [](std::string x) -> char {
    std::transform(x.begin(), x.end(), x.begin(), ::tolower);
    x.erase(0, x.find_first_not_of(" \t\r\n"));
    x.erase(x.find_last_not_of(" \t\r\n") + 1);
    if (x == "m" || x == "male" || x == "1") return 'M';
    if (x == "f" || x == "female" || x == "2") return 'F';
    return 0;
  };
  
  // First pass: build ID set and count duplicates, capture sex map
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    id_set.insert(id);
    
    id_count[id]++;
    if (id_count[id] == 2) {
      duplicate_ids.push_back(id);
    }
    
    if (i < sex.size()) {
      std::string sx = Rcpp::as<std::string>(sex[i]);
      char s = normalize_sex(sx);
      if (s != 0) {
        sex_map[id] = s;
      }
    }
  }
  
  // Second pass: analyze relationships
  int sex_mismatch_sire = 0;
  int sex_mismatch_dam = 0;
  std::unordered_set<std::string> sex_mismatch_sire_ids;
  std::unordered_set<std::string> sex_mismatch_dam_ids;
  
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    
    // Count founders
    if (!has_sire && !has_dam) {
      founders++;
      founder_set.insert(id);
    }
    
    // Count with both parents / only sire / only dam
    if (has_sire && has_dam) {
      with_both_parents++;
    } else if (has_sire && !has_dam) {
      only_sire_count++;
    } else if (!has_sire && has_dam) {
      only_dam_count++;
    }
    
    // Check self-parenting
    if ((has_sire && sire == id) || (has_dam && dam == id)) {
      self_parent_count++;
    }
    
    // Track mentioned parents
    if (has_sire) {
      sires_mentioned_set.insert(sire);
      sire_progeny_count[sire]++;
    }
    if (has_dam) {
      dams_mentioned_set.insert(dam);
      dam_progeny_count[dam]++;
    }
    
    // Check missing parents
    if (has_sire && id_set.find(sire) == id_set.end()) {
      missing_sires_set.insert(sire);
    }
    if (has_dam && id_set.find(dam) == id_set.end()) {
      missing_dams_set.insert(dam);
    }
    
    // Sex mismatch checks (if sex known for parent)
    if (has_sire) {
      auto it = sex_map.find(sire);
      if (it != sex_map.end() && it->second != 'M') {
        sex_mismatch_sire++;
        sex_mismatch_sire_ids.insert(sire);
      }
    }
    if (has_dam) {
      auto it = sex_map.find(dam);
      if (it != sex_map.end() && it->second != 'F') {
        sex_mismatch_dam++;
        sex_mismatch_dam_ids.insert(dam);
      }
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
  
  // Dual-role parent IDs (appear as both sire and dam)
  std::vector<std::string> dual_role_ids;
  for (const auto& s : sires_mentioned_set) {
    if (dams_mentioned_set.find(s) != dams_mentioned_set.end()) {
      dual_role_ids.push_back(s);
    }
  }
  CharacterVector dual_role(dual_role_ids.size());
  for (size_t i = 0; i < dual_role_ids.size(); i++) {
    dual_role[i] = dual_role_ids[i];
  }
  
  CharacterVector sex_mismatch_sire_vec(sex_mismatch_sire_ids.size());
  idx = 0;
  for (const auto& s : sex_mismatch_sire_ids) {
    sex_mismatch_sire_vec[idx++] = s;
  }
  
  CharacterVector sex_mismatch_dam_vec(sex_mismatch_dam_ids.size());
  idx = 0;
  for (const auto& d : sex_mismatch_dam_ids) {
    sex_mismatch_dam_vec[idx++] = d;
  }
  
  // Parent/progeny statistics
  int unique_sires = static_cast<int>(sire_progeny_count.size());
  int unique_dams = static_cast<int>(dam_progeny_count.size());
  long long total_sire_progeny = 0;
  long long total_dam_progeny = 0;
  for (const auto& kv : sire_progeny_count) total_sire_progeny += kv.second;
  for (const auto& kv : dam_progeny_count) total_dam_progeny += kv.second;
  
  std::unordered_set<std::string> parents_with_progeny;
  for (const auto& kv : sire_progeny_count) {
    if (id_set.find(kv.first) != id_set.end()) parents_with_progeny.insert(kv.first);
  }
  for (const auto& kv : dam_progeny_count) {
    if (id_set.find(kv.first) != id_set.end()) parents_with_progeny.insert(kv.first);
  }
  int individuals_with_progeny = static_cast<int>(parents_with_progeny.size());
  int individuals_without_progeny = n - individuals_with_progeny;
  
  int founder_sires = 0;
  int founder_dams = 0;
  long long founder_sire_progeny = 0;
  long long founder_dam_progeny = 0;
  std::unordered_set<std::string> founder_parent_ids;
  for (const auto& kv : sire_progeny_count) {
    if (founder_set.find(kv.first) != founder_set.end()) {
      founder_sires++;
      founder_sire_progeny += kv.second;
      founder_parent_ids.insert(kv.first);
    }
  }
  for (const auto& kv : dam_progeny_count) {
    if (founder_set.find(kv.first) != founder_set.end()) {
      founder_dams++;
      founder_dam_progeny += kv.second;
      founder_parent_ids.insert(kv.first);
    }
  }
  long long founder_total_progeny = 0;
  for (int i = 0; i < n; i++) {
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    if ((has_sire && founder_set.find(sire) != founder_set.end()) ||
        (has_dam && founder_set.find(dam) != founder_set.end())) {
      founder_total_progeny++;
    }
  }
  int founder_no_progeny = founders - static_cast<int>(founder_parent_ids.size());
  
  int non_founder_sires = 0;
  int non_founder_dams = 0;
  long long non_founder_sire_progeny = 0;
  long long non_founder_dam_progeny = 0;
  for (const auto& kv : sire_progeny_count) {
    if (founder_set.find(kv.first) == founder_set.end()) {
      non_founder_sires++;
      non_founder_sire_progeny += kv.second;
    }
  }
  for (const auto& kv : dam_progeny_count) {
    if (founder_set.find(kv.first) == founder_set.end()) {
      non_founder_dams++;
      non_founder_dam_progeny += kv.second;
    }
  }
  
  return List::create(
    Named("total") = n,
    Named("founders") = founders,
    Named("with_both_parents") = with_both_parents,
    Named("only_sire") = only_sire_count,
    Named("only_dam") = only_dam_count,
    Named("self_parent_count") = self_parent_count,
    Named("duplicate_ids") = duplicates,
    Named("missing_sires") = missing_sires,
    Named("missing_dams") = missing_dams,
    Named("dual_role_ids") = dual_role,
    Named("sex_mismatch_sire_count") = sex_mismatch_sire,
    Named("sex_mismatch_dam_count") = sex_mismatch_dam,
    Named("sex_mismatch_sire_ids") = sex_mismatch_sire_vec,
    Named("sex_mismatch_dam_ids") = sex_mismatch_dam_vec,
    Named("unique_sires") = unique_sires,
    Named("unique_dams") = unique_dams,
    Named("total_sire_progeny") = total_sire_progeny,
    Named("total_dam_progeny") = total_dam_progeny,
    Named("individuals_with_progeny") = individuals_with_progeny,
    Named("individuals_without_progeny") = individuals_without_progeny,
    Named("founder_sires") = founder_sires,
    Named("founder_dams") = founder_dams,
    Named("founder_sire_progeny") = founder_sire_progeny,
    Named("founder_dam_progeny") = founder_dam_progeny,
    Named("founder_total_progeny") = founder_total_progeny,
    Named("founder_no_progeny") = founder_no_progeny,
    Named("non_founder_sires") = non_founder_sires,
    Named("non_founder_dams") = non_founder_dams,
    Named("non_founder_sire_progeny") = non_founder_sire_progeny,
    Named("non_founder_dam_progeny") = non_founder_dam_progeny
  );
}

// Fast loop detection using DFS
// [[Rcpp::export]]
List fast_detect_loops(CharacterVector ids, 
                       CharacterVector sires, 
                       CharacterVector dams) {
  
  int n = ids.size();
  
  // Build parent map: ID -> vector of parent IDs
  std::unordered_map<std::string, std::vector<std::string>> parent_map;
  std::unordered_set<std::string> id_set;
  
  // Populate ID set
  for (int i = 0; i < n; i++) {
    id_set.insert(Rcpp::as<std::string>(ids[i]));
  }
  
  // Build parent relationships
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    std::vector<std::string> parents;
    
    // Only add valid parent references
    if (sire != "NA" && sire != "0" && sire != "" && id_set.find(sire) != id_set.end()) {
      parents.push_back(sire);
    }
    if (dam != "NA" && dam != "0" && dam != "" && id_set.find(dam) != id_set.end()) {
      parents.push_back(dam);
    }
    
    if (!parents.empty()) {
      parent_map[id] = parents;
    }
  }
  
  // DFS-based cycle detection
  std::unordered_set<std::string> visited;
  std::unordered_set<std::string> rec_stack;
  std::vector<std::vector<std::string>> all_cycles;
  
  std::function<bool(const std::string&, std::vector<std::string>&)> dfs;
  dfs = [&](const std::string& node, std::vector<std::string>& path) -> bool {
    // Check if we found a cycle
    if (rec_stack.find(node) != rec_stack.end()) {
      // Extract cycle
      std::vector<std::string> cycle;
      bool in_cycle = false;
      for (const auto& p : path) {
        if (p == node) in_cycle = true;
        if (in_cycle) cycle.push_back(p);
      }
      cycle.push_back(node);
      all_cycles.push_back(cycle);
      return true;
    }
    
    // Already fully explored
    if (visited.find(node) != visited.end()) {
      return false;
    }
    
    // Mark as being processed
    rec_stack.insert(node);
    path.push_back(node);
    
    // Explore parents
    if (parent_map.find(node) != parent_map.end()) {
      for (const auto& parent : parent_map[node]) {
        dfs(parent, path);
      }
    }
    
    // Mark as fully explored
    path.pop_back();
    rec_stack.erase(node);
    visited.insert(node);
    
    return false;
  };
  
  // Check all nodes
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    if (visited.find(id) == visited.end()) {
      std::vector<std::string> path;
      dfs(id, path);
    }
  }
  
  // Convert cycles to R list
  List cycles_list(all_cycles.size());
  for (size_t i = 0; i < all_cycles.size(); i++) {
    CharacterVector cycle(all_cycles[i].size());
    for (size_t j = 0; j < all_cycles[i].size(); j++) {
      cycle[j] = all_cycles[i][j];
    }
    cycles_list[i] = cycle;
  }
  
  return List::create(
    Named("count") = all_cycles.size(),
    Named("cycles") = cycles_list
  );
}

// Fast ancestor depth calculation with memoization
// [[Rcpp::export]]
List fast_find_deepest_ancestor(CharacterVector ids, 
                                CharacterVector sires, 
                                CharacterVector dams,
                                int sample_size = 200) {
  
  int n = ids.size();
  
  // Build lookup maps for O(1) access
  std::unordered_map<std::string, int> id_to_index;
  std::unordered_map<std::string, std::pair<std::string, std::string>> parent_map;
  std::vector<std::string> non_founders;
  
  // First pass: build maps and identify non-founders
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    id_to_index[id] = i;
    
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    
    if (has_sire || has_dam) {
      non_founders.push_back(id);
      parent_map[id] = std::make_pair(
        has_sire ? sire : "",
        has_dam ? dam : ""
      );
    }
  }
  
  if (non_founders.empty()) {
    return List::create(
      Named("id") = CharacterVector(),
      Named("depth") = 0
    );
  }
  
  // Sample non-founders if needed
  std::vector<std::string> sample_ids;
  if (non_founders.size() > (size_t)sample_size) {
    // Random sampling using R's sample function
    Function sample("sample");
    IntegerVector indices = sample(non_founders.size(), sample_size, false);
    for (int i = 0; i < sample_size; i++) {
      int idx = indices[i] - 1; // R indices are 1-based
      if (idx >= 0 && idx < (int)non_founders.size()) {
        sample_ids.push_back(non_founders[idx]);
      }
    }
  } else {
    sample_ids = non_founders;
  }
  
  // Memoization: cache depth for each ID
  std::unordered_map<std::string, int> depth_cache;
  
  // Recursive function to calculate depth with cycle detection
  std::function<int(const std::string&, std::unordered_set<std::string>&, int)> calc_depth;
  calc_depth = [&](const std::string& id, std::unordered_set<std::string>& visited, int max_depth) -> int {
    // Check cache first
    if (depth_cache.find(id) != depth_cache.end()) {
      return depth_cache[id];
    }
    
    // Cycle detection
    if (visited.find(id) != visited.end()) {
      return 0;
    }
    
    // Max depth protection
    if (visited.size() > max_depth) {
      return visited.size();
    }
    
    // Check if ID exists
    if (parent_map.find(id) == parent_map.end()) {
      depth_cache[id] = 0;
      return 0;
    }
    
    auto parents = parent_map[id];
    bool has_sire = !parents.first.empty();
    bool has_dam = !parents.second.empty();
    
    // If no valid parents, this is a founder
    if (!has_sire && !has_dam) {
      depth_cache[id] = 0;
      return 0;
    }
    
    // Add to visited set
    visited.insert(id);
    
    int max_parent_depth = 0;
    
    // Calculate depth from sire
    if (has_sire && id_to_index.find(parents.first) != id_to_index.end()) {
      int sire_depth = calc_depth(parents.first, visited, max_depth);
      max_parent_depth = std::max(max_parent_depth, sire_depth);
    }
    
    // Calculate depth from dam
    if (has_dam && id_to_index.find(parents.second) != id_to_index.end()) {
      int dam_depth = calc_depth(parents.second, visited, max_depth);
      max_parent_depth = std::max(max_parent_depth, dam_depth);
    }
    
    // Remove from visited set
    visited.erase(id);
    
    int depth = max_parent_depth + 1;
    depth_cache[id] = depth;
    return depth;
  };
  
  // Calculate depth for all sampled individuals
  int max_depth = 0;
  std::string deepest_id = "";
  
  for (const auto& id : sample_ids) {
    std::unordered_set<std::string> visited;
    int depth = calc_depth(id, visited, 100);
    
    if (depth > max_depth) {
      max_depth = depth;
      deepest_id = id;
    }
  }
  
  if (deepest_id.empty() || max_depth == 0) {
    return List::create(
      Named("id") = CharacterVector(),
      Named("depth") = 0
    );
  }
  
  return List::create(
    Named("id") = deepest_id,
    Named("depth") = max_depth
  );
}

// Check birth date order: parents must be born before offspring
// [[Rcpp::export]]
List check_birth_date_order(CharacterVector ids,
                            CharacterVector sires,
                            CharacterVector dams,
                            NumericVector birth_dates) {
  // Function: Check if offspring birth dates are after their parents' birth dates
  // Parameters:
  //   ids: Individual ID vector
  //   sires: Sire ID vector
  //   dams: Dam ID vector
  //   birth_dates: Birth date vector (numeric, e.g., Date or POSIXct)
  // Returns: List containing detection results
  
  int n = ids.size();
  
  // Build ID to birth date mapping
  std::unordered_map<std::string, double> id_to_birthdate;
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    if (!Rcpp::NumericVector::is_na(birth_dates[i])) {
      id_to_birthdate[id] = birth_dates[i];
    }
  }
  
  // Detect birth date order issues
  std::vector<std::string> invalid_offspring_ids;
  std::vector<std::string> invalid_sire_ids;
  std::vector<std::string> invalid_dam_ids;
  
  int invalid_count = 0;
  int invalid_sire_count = 0;
  int invalid_dam_count = 0;
  
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    // Check if individual has birth date
    if (id_to_birthdate.find(id) == id_to_birthdate.end()) {
      continue;  // Skip individuals without birth dates
    }
    
    double offspring_date = id_to_birthdate[id];
    bool has_issue = false;
    std::string problem_sire = "";
    std::string problem_dam = "";
    
    // Check sire
    if (sire != "NA" && sire != "0" && sire != "" && id_to_birthdate.find(sire) != id_to_birthdate.end()) {
      double sire_date = id_to_birthdate[sire];
      if (offspring_date <= sire_date) {
        // Offspring birth date is not after sire's birth date
        problem_sire = sire;
        invalid_sire_count++;
        has_issue = true;
      }
    }
    
    // Check dam
    if (dam != "NA" && dam != "0" && dam != "" && id_to_birthdate.find(dam) != id_to_birthdate.end()) {
      double dam_date = id_to_birthdate[dam];
      if (offspring_date <= dam_date) {
        // Offspring birth date is not after dam's birth date
        problem_dam = dam;
        invalid_dam_count++;
        has_issue = true;
      }
    }
    
    if (has_issue) {
      invalid_offspring_ids.push_back(id);
      invalid_sire_ids.push_back(problem_sire);
      invalid_dam_ids.push_back(problem_dam);
      invalid_count++;
    }
  }
  
  // Convert to R vectors
  CharacterVector invalid_offspring(invalid_offspring_ids.size());
  CharacterVector invalid_sires(invalid_sire_ids.size());
  CharacterVector invalid_dams(invalid_dam_ids.size());
  
  for (size_t i = 0; i < invalid_offspring_ids.size(); i++) {
    invalid_offspring[i] = invalid_offspring_ids[i];
    invalid_sires[i] = invalid_sire_ids[i];
    invalid_dams[i] = invalid_dam_ids[i];
  }
  
  return List::create(
    Named("count") = invalid_count,
    Named("invalid_sire_count") = invalid_sire_count,
    Named("invalid_dam_count") = invalid_dam_count,
    Named("invalid_offspring_ids") = invalid_offspring,
    Named("invalid_sire_ids") = invalid_sires,
    Named("invalid_dam_ids") = invalid_dams
  );
}

// Fast LAP (Longest Ancestral Path) distribution calculation
// [[Rcpp::export]]
NumericVector fast_lap_distribution(CharacterVector ids,
                                    CharacterVector sires,
                                    CharacterVector dams,
                                    int sample_size = 10000,
                                    int max_depth = 20) {
  
  int n = ids.size();
  
  // Build lookup maps for O(1) access
  std::unordered_map<std::string, std::pair<std::string, std::string>> parent_map;
  std::unordered_set<std::string> id_set;
  std::vector<std::string> all_ids;
  
  // First pass: build ID set
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    id_set.insert(id);
    all_ids.push_back(id);
  }
  
  // Second pass: build parent map (treat non-empty parents as valid)
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    
    if (has_sire || has_dam) {
      parent_map[id] = std::make_pair(
        has_sire ? sire : "",
        has_dam ? dam : ""
      );
    }
  }
  
  // Determine sample IDs
  std::vector<std::string> sample_ids;
  // Only sample for very large datasets (> 1000k); use 10k samples
  const int sample_threshold = 1000000;
  const int sample_target = 10000;
  if (n > sample_threshold) {
    Function sample("sample");
    IntegerVector indices = sample(n, sample_target, false);
    for (int i = 0; i < sample_target; i++) {
      int idx = indices[i] - 1; // R indices are 1-based
      if (idx >= 0 && idx < n) {
        sample_ids.push_back(all_ids[idx]);
      }
    }
  } else {
    sample_ids = all_ids;
  }
  
  // Memoization: cache depth for each ID
  std::unordered_map<std::string, int> depth_cache;
  
  // Recursive function to calculate LAP depth with cycle detection
  std::function<int(const std::string&, std::unordered_set<std::string>&)> calc_lap_depth;
  calc_lap_depth = [&](const std::string& id, std::unordered_set<std::string>& visited) -> int {
    // Check cache first
    if (depth_cache.find(id) != depth_cache.end()) {
      return depth_cache[id];
    }
    
    // Cycle detection
    if (visited.find(id) != visited.end()) {
      return 0; // Cycle detected, return 0 to avoid infinite recursion
    }
    
    // Check if ID has parents
    if (parent_map.find(id) == parent_map.end()) {
      depth_cache[id] = 0; // Founder
      return 0;
    }
    
    auto parents = parent_map[id];
    bool has_sire = !parents.first.empty();
    bool has_dam = !parents.second.empty();
    
    // If no valid parents, this is a founder
    if (!has_sire && !has_dam) {
      depth_cache[id] = 0;
      return 0;
    }
    
    // Add to visited set
    visited.insert(id);
    
    int max_parent_depth = 0;
    
    // Calculate depth from sire
    if (has_sire) {
      int sire_depth = calc_lap_depth(parents.first, visited);
      max_parent_depth = std::max(max_parent_depth, sire_depth);
    }
    
    // Calculate depth from dam
    if (has_dam) {
      int dam_depth = calc_lap_depth(parents.second, visited);
      max_parent_depth = std::max(max_parent_depth, dam_depth);
    }
    
    // Remove from visited set
    visited.erase(id);
    
    int depth = max_parent_depth + 1;
    // Cap depth at max_depth - 1 to fit distribution range
    if (depth >= max_depth) {
      depth = max_depth - 1;
    }
    depth_cache[id] = depth;
    return depth;
  };
  
  // Initialize distribution vector (0 to max_depth-1)
  std::vector<int> distribution(max_depth, 0);
  
  // Calculate LAP for all sampled individuals
  for (const auto& id : sample_ids) {
    std::unordered_set<std::string> visited;
    int depth = calc_lap_depth(id, visited);
    
    if (depth >= 0 && depth < max_depth) {
      distribution[depth]++;
    }
  }
  
  // Scale to total population if sampled
  double scale_factor = 1.0;
  if (n > (int)sample_ids.size() && sample_ids.size() > 0) {
    scale_factor = (double)n / (double)sample_ids.size();
  }
  
  // Convert to R numeric vector
  NumericVector result(max_depth);
  for (int i = 0; i < max_depth; i++) {
    result[i] = std::round(distribution[i] * scale_factor);
  }
  
  // Set names
  CharacterVector names_vec(max_depth);
  for (int i = 0; i < max_depth; i++) {
    names_vec[i] = std::to_string(i);
  }
  result.attr("names") = names_vec;
  
  return result;
}

// Fast LAP depth for each individual
// [[Rcpp::export]]
IntegerVector fast_lap_depths(CharacterVector ids,
                              CharacterVector sires,
                              CharacterVector dams) {
  int n = ids.size();
  
  std::unordered_set<std::string> id_set;
  std::unordered_map<std::string, std::pair<std::string, std::string>> parent_map;
  
  // Build ID set and parent map
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    id_set.insert(id);
    
    bool has_sire = (sire != "NA" && sire != "0" && sire != "");
    bool has_dam = (dam != "NA" && dam != "0" && dam != "");
    
    parent_map[id] = std::make_pair(
      has_sire ? sire : "",
      has_dam ? dam : ""
    );
  }
  
  // Memoization: cache depth for each ID
  std::unordered_map<std::string, int> depth_cache;
  
  // Recursive function to calculate depth with cycle detection
  std::function<int(const std::string&, std::unordered_set<std::string>&)> calc_depth;
  calc_depth = [&](const std::string& id, std::unordered_set<std::string>& visited) -> int {
    if (depth_cache.find(id) != depth_cache.end()) {
      return depth_cache[id];
    }
    
    if (id_set.find(id) == id_set.end()) {
      return 0;
    }
    
    if (visited.find(id) != visited.end()) {
      return 0;
    }
    
    auto it = parent_map.find(id);
    if (it == parent_map.end()) {
      depth_cache[id] = 0;
      return 0;
    }
    
    auto parents = it->second;
    bool has_sire = !parents.first.empty();
    bool has_dam = !parents.second.empty();
    
    if (!has_sire && !has_dam) {
      depth_cache[id] = 0;
      return 0;
    }
    
    visited.insert(id);
    
    int max_parent_depth = 0;
    if (has_sire) {
      int sire_depth = calc_depth(parents.first, visited);
      max_parent_depth = std::max(max_parent_depth, sire_depth);
    }
    if (has_dam) {
      int dam_depth = calc_depth(parents.second, visited);
      max_parent_depth = std::max(max_parent_depth, dam_depth);
    }
    
    visited.erase(id);
    
    int depth = max_parent_depth + 1;
    depth_cache[id] = depth;
    return depth;
  };
  
  IntegerVector depths(n);
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::unordered_set<std::string> visited;
    depths[i] = calc_depth(id, visited);
  }
  
  return depths;
}

// Fast descendant summary for parent role (Sire/Dam)
// [[Rcpp::export]]
List fast_descendant_summary(CharacterVector ids,
                             CharacterVector parent_vals,
                             int max_depth = 50) {
  int n = ids.size();
  if (parent_vals.size() != n) {
    Rcpp::stop("Length mismatch: ids and parent_vals must have same length.");
  }
  if (n == 0) {
    return List::create(
      Named("parents") = CharacterVector(),
      Named("totals") = IntegerVector(),
      Named("counts") = IntegerMatrix(0, 0)
    );
  }

  std::unordered_map<std::string, std::vector<int>> parent_children;
  parent_children.reserve(n * 2);
  std::unordered_map<std::string, int> parent_index;
  parent_index.reserve(n);
  std::vector<std::string> parent_ids;
  parent_ids.reserve(n / 2);

  auto is_missing_parent_str = [](const Rcpp::String& s) -> bool {
    if (s == NA_STRING) return true;
    std::string x = std::string(s.get_cstring());
    if (x.empty()) return true;
    if (x == "0") return true;
    if (x == "NA") return true;
    return false;
  };

  for (int i = 0; i < n; ++i) {
    Rcpp::String p = parent_vals[i];
    if (is_missing_parent_str(p)) continue;
    std::string parent_id = std::string(p.get_cstring());
    auto it = parent_index.find(parent_id);
    if (it == parent_index.end()) {
      parent_index[parent_id] = static_cast<int>(parent_ids.size());
      parent_ids.push_back(parent_id);
    }
    parent_children[parent_id].push_back(i);
  }

  int pcount = static_cast<int>(parent_ids.size());
  if (pcount == 0) {
    return List::create(
      Named("parents") = CharacterVector(),
      Named("totals") = IntegerVector(),
      Named("counts") = IntegerMatrix(0, 0)
    );
  }

  IntegerVector totals(pcount);
  IntegerMatrix counts(pcount, max_depth);

  std::vector<int> visit_tag(n, 0);
  int stamp = 1;

  for (int pi = 0; pi < pcount; ++pi) {
    const std::string& root = parent_ids[pi];
    auto it_root = parent_children.find(root);
    if (it_root == parent_children.end() || it_root->second.empty()) {
      totals[pi] = 0;
      continue;
    }

    std::vector<int> current = it_root->second;
    int depth = 1;
    int total = 0;

    while (!current.empty() && depth <= max_depth) {
      std::vector<int> next;
      next.reserve(current.size());
      for (int idx : current) {
        if (visit_tag[idx] == stamp) continue;
        visit_tag[idx] = stamp;
        counts(pi, depth - 1) += 1;
        total += 1;

        const std::string& child_id = Rcpp::as<std::string>(ids[idx]);
        auto it_child = parent_children.find(child_id);
        if (it_child != parent_children.end()) {
          const std::vector<int>& kids = it_child->second;
          next.insert(next.end(), kids.begin(), kids.end());
        }
      }
      current.swap(next);
      depth += 1;
    }

    totals[pi] = total;
    stamp += 1;
    if (stamp == INT_MAX) {
      std::fill(visit_tag.begin(), visit_tag.end(), 0);
      stamp = 1;
    }
  }

  CharacterVector parents(pcount);
  for (int i = 0; i < pcount; ++i) {
    parents[i] = parent_ids[i];
  }

  return List::create(
    Named("parents") = parents,
    Named("totals") = totals,
    Named("counts") = counts
  );
}

static inline std::string trim_copy(const std::string& s) {
  size_t start = 0;
  while (start < s.size() && std::isspace(static_cast<unsigned char>(s[start]))) {
    ++start;
  }
  if (start == s.size()) return "";
  size_t end = s.size() - 1;
  while (end > start && std::isspace(static_cast<unsigned char>(s[end]))) {
    --end;
  }
  return s.substr(start, end - start + 1);
}

static inline bool is_missing_parent(const Rcpp::String& s) {
  if (s == NA_STRING) return true;
  std::string x = trim_copy(std::string(s.get_cstring()));
  if (x.empty()) return true;
  if (x == "0") return true;
  if (x == "NA") return true;
  return false;
}

// Fast inbreeding coefficients using modified algorithm (C++ implementation)
// [[Rcpp::export]]
NumericVector fast_inbreeding_cpp(CharacterVector ids,
                                  CharacterVector sires,
                                  CharacterVector dams) {
  int n = ids.size();
  if (sires.size() != n || dams.size() != n) {
    Rcpp::stop("Length mismatch: ids, sires, and dams must have same length.");
  }
  if (n == 0) {
    return NumericVector();
  }

  std::unordered_map<std::string, int> id_to_index;
  id_to_index.reserve(n * 2);
  std::vector<std::string> id_vec(n);

  for (int i = 0; i < n; ++i) {
    if (ids[i] == NA_STRING) {
      Rcpp::stop("IDs cannot contain NA values.");
    }
    std::string id = Rcpp::as<std::string>(ids[i]);
    if (id_to_index.find(id) != id_to_index.end()) {
      Rcpp::stop("Duplicate ID found in pedigree: " + id);
    }
    id_to_index[id] = i;
    id_vec[i] = id;
  }

  std::vector<int> sire_idx(n, -1);
  std::vector<int> dam_idx(n, -1);
  std::vector<std::vector<int>> children(n);
  std::vector<int> indegree(n, 0);

  for (int i = 0; i < n; ++i) {
    Rcpp::String s = sires[i];
    Rcpp::String d = dams[i];
    if (!is_missing_parent(s)) {
      std::string sire_id = trim_copy(std::string(s.get_cstring()));
      auto it = id_to_index.find(sire_id);
      if (it != id_to_index.end()) {
        sire_idx[i] = it->second;
        children[it->second].push_back(i);
        indegree[i] += 1;
      }
    }
    if (!is_missing_parent(d)) {
      std::string dam_id = trim_copy(std::string(d.get_cstring()));
      auto it = id_to_index.find(dam_id);
      if (it != id_to_index.end()) {
        dam_idx[i] = it->second;
        children[it->second].push_back(i);
        indegree[i] += 1;
      }
    }
  }

  std::priority_queue<int, std::vector<int>, std::greater<int>> ready;
  for (int i = 0; i < n; ++i) {
    if (indegree[i] == 0) {
      ready.push(i);
    }
  }

  std::vector<int> order;
  order.reserve(n);
  while (!ready.empty()) {
    int node = ready.top();
    ready.pop();
    order.push_back(node);
    for (int child : children[node]) {
      indegree[child] -= 1;
      if (indegree[child] == 0) {
        ready.push(child);
      }
    }
  }

  if ((int)order.size() != n) {
    Rcpp::stop("Cycle detected in pedigree; cannot compute inbreeding coefficients.");
  }

  std::vector<int> new_index(n, 0);
  for (int pos = 0; pos < n; ++pos) {
    new_index[order[pos]] = pos + 1;
  }

  std::vector<int> ped_sire(n + 1, 0);
  std::vector<int> ped_dam(n + 1, 0);
  for (int pos = 1; pos <= n; ++pos) {
    int node = order[pos - 1];
    ped_sire[pos] = (sire_idx[node] >= 0) ? new_index[sire_idx[node]] : 0;
    ped_dam[pos] = (dam_idx[node] >= 0) ? new_index[dam_idx[node]] : 0;
  }

  int m = n;
  std::vector<int> SId(n + 1, 0);
  std::vector<int> Link(n + 1, 0);
  std::vector<int> MaxIdP(n + 1, 0);
  std::vector<double> F(n + 1, 0.0);
  std::vector<double> B(n + 1, 0.0);
  std::vector<double> x(n + 1, 0.0);
  std::vector<int> rPedS(n + 1, 0);
  std::vector<int> rPedD(n + 1, 0);

  F[0] = -1.0;
  x[0] = 0.0;
  Link[0] = 0;
  MaxIdP[0] = 0;

  int rN = 1;
  for (int i = 1; i <= n; ++i) {
    SId[i] = i;
    Link[i] = 0;
    if (i <= m) {
      x[i] = 0.0;
    }
    int S = ped_sire[i];
    int D = ped_dam[i];
    if (S != 0 && Link[S] == 0) {
      MaxIdP[rN] = Link[S] = rN;
      rPedS[rN] = Link[ped_sire[S]];
      rPedD[rN] = Link[ped_dam[S]];
      rN++;
    }
    if (D != 0 && Link[D] == 0) {
      Link[D] = rN;
      rPedS[rN] = Link[ped_sire[D]];
      rPedD[rN] = Link[ped_dam[D]];
      rN++;
    }
    if (MaxIdP[Link[S]] < Link[D]) {
      MaxIdP[Link[S]] = Link[D];
    }
  }

  std::vector<int> sidx;
  sidx.reserve(n);
  for (int i = 1; i <= n; ++i) {
    sidx.push_back(i);
  }
  std::sort(sidx.begin(), sidx.end(), [&](int a, int b) {
    return ped_sire[a] < ped_sire[b];
  });
  for (int i = 1; i <= n; ++i) {
    SId[i] = sidx[i - 1];
  }

  int k = 1;
  int i = 1;
  while (i <= n) {
    if (ped_sire[SId[i]] == 0) {
      F[SId[i]] = 0.0;
      i++;
      continue;
    }

    int S = ped_sire[SId[i]];
    int rS = Link[S];
    if (rS == 0) {
      F[SId[i]] = 0.0;
      i++;
      continue;
    }
    int MIP = MaxIdP[rS];
    x[rS] = 1.0;

    for (; k <= S; ++k) {
      if (Link[k]) {
        B[Link[k]] = 0.5 - 0.25 * (F[ped_sire[k]] + F[ped_dam[k]]);
      }
    }

    for (int j = rS; j >= 1; --j) {
      if (x[j] != 0.0) {
        if (rPedS[j]) x[rPedS[j]] += x[j] * 0.5;
        if (rPedD[j]) x[rPedD[j]] += x[j] * 0.5;
        x[j] *= B[j];
      }
    }

    for (int j = 1; j <= MIP; ++j) {
      x[j] += (x[rPedS[j]] + x[rPedD[j]]) * 0.5;
    }

    for (; i <= n; ++i) {
      if (S != ped_sire[SId[i]]) break;
      int dam_id = ped_dam[SId[i]];
      F[SId[i]] = x[Link[dam_id]] * 0.5;
    }

    for (int j = 1; j <= MIP; ++j) {
      x[j] = 0.0;
    }
  }

  NumericVector result(n);
  for (int idx = 0; idx < n; ++idx) {
    result[idx] = F[new_index[idx]];
  }
  result.attr("names") = ids;
  return result;
}