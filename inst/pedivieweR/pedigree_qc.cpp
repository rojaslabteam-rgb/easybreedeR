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
