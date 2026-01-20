#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <string>
#include <algorithm>
#include <random>
#include <functional>
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
                                    int sample_size = 5000,
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
  
  // Second pass: build parent map (now we can check if parents exist in id_set)
  for (int i = 0; i < n; i++) {
    std::string id = Rcpp::as<std::string>(ids[i]);
    std::string sire = Rcpp::as<std::string>(sires[i]);
    std::string dam = Rcpp::as<std::string>(dams[i]);
    
    bool has_sire = (sire != "NA" && sire != "0" && sire != "" && id_set.find(sire) != id_set.end());
    bool has_dam = (dam != "NA" && dam != "0" && dam != "" && id_set.find(dam) != id_set.end());
    
    if (has_sire || has_dam) {
      parent_map[id] = std::make_pair(
        has_sire ? sire : "",
        has_dam ? dam : ""
      );
    }
  }
  
  // Determine sample IDs
  std::vector<std::string> sample_ids;
  if (n > sample_size) {
    // Random sampling using R's sample function
    Function sample("sample");
    IntegerVector indices = sample(n, sample_size, false);
    for (int i = 0; i < sample_size; i++) {
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
    // Cap depth at max_depth
    if (depth > max_depth) {
      depth = max_depth;
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