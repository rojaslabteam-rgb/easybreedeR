# easybreedeR R Project Profile
# Automatically loaded when opening this R Project

# Set project options
options(
  # Ensure UTF-8 encoding
  encoding = "UTF-8",
  # Set default stringsAsFactors to FALSE (R 4.0+ default, but explicit)
  stringsAsFactors = FALSE,
  # Set default timeout for web requests
  timeout = 300,
  # Shiny options
  shiny.maxRequestSize = 100 * 1024^2  # 100MB
)

# Auto-load shared Language.R if available
if (file.exists("Language.R")) {
  tryCatch({
    source("Language.R", local = FALSE)
    cat("✓ Loaded shared Language.R\n")
  }, error = function(e) {
    cat("Note: Could not load Language.R:", conditionMessage(e), "\n")
  })
} else {
  cat("Note: Language.R not found in project root\n")
}

# Print welcome message
cat("\n")
cat("═══════════════════════════════════════════════════\n")
cat("  easybreedeR R Project\n")
cat("  Multi-Application Shiny Suite\n")
cat("═══════════════════════════════════════════════════\n")
cat("\n")
cat("Available applications:\n")
cat("  - easybreedeR_Studio: Main suite application\n")
cat("  - easyblup: BLUP/REML parameter generator\n")
cat("  - pedivieweR: Pedigree viewer and QC\n")
cat("  - dataprevieweR: Data preview and exploration\n")
cat("  - RCW: R Canvas Workflow\n")
cat("\n")
cat("To run an application, use:\n")
cat("  shiny::runApp('app_name/app.R')\n")
cat("\n")

