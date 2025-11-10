## global.R — ensure the Shiny app uses the intended Python virtualenv for reticulate
## This pins reticulate to the user's existing virtualenv used in earlier checks.
## If you prefer a different path, update the path below.

if (requireNamespace("reticulate", quietly = TRUE)) {
  try({
    # Pin reticulate to the Python venv we discovered earlier.
    # Adjust this path if you want a project-local .venv instead.
    reticulate::use_virtualenv("/Users/huangyitang/.virtualenvs/r-reticulate", required = TRUE)
    packageStartupMessage("reticulate: pinned to /Users/huangyitang/.virtualenvs/r-reticulate")
  }, silent = TRUE)
} else {
  warning("Package 'reticulate' is not installed. Install it to enable Python support (reticulate::use_virtualenv).")
}
