# easybreedeR — Multi-breeding related Shiny Application

[![R-CMD-check](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml)

easybreedeR is a collection of Shiny applications for breeding data analysis and workflow management. It provides a unified launcher (Studio) and several focused tools you can run individually.

## Project structure

```
inst/
├── easybreedeR_Studio/    # Main suite application (launcher)
├── easyblup/              # BLUP/REML parameter generator with AI assistant
├── pedivieweR/            # Pedigree viewer and quality control
├── dataprevieweR/         # Data preview and exploration
├── RCW/                   # R Canvas Workflow (visual pipeline builder)
└── Language.R             # Shared multilingual translation framework
R/
└── runEasybreedeR.R       # Exported run_*() helpers
```

## Installation (from GitHub)

You can install the package directly from GitHub. Either remotes or devtools works:

```r
# Option A: using remotes (lightweight)
install.packages("remotes")
remotes::install_github("rojaslabteam-rgb/easybreedeR")

# Option B: using devtools
install.packages("devtools")
devtools::install_github("rojaslabteam-rgb/easybreedeR")
```

Then load and launch the suite:

```r
library(easybreedeR)
run_easybreedeR()  # opens the Studio in your browser
```

## Quick start

- Launch the main suite (Studio):
  ```r
  library(easybreedeR)
  run_easybreedeR()
  ```

- Or launch individual apps directly:
  ```r
  run_easyblup()       # BLUP/REML tool with AI assistant
  run_pedivieweR()     # Pedigree viewer and QC
  run_dataprevieweR()  # Data preview/exploration
  run_rcw()            # Visual pipeline builder (R Canvas Workflow)
  ```

## Development (optional)

If you’re working from a local clone and want fast iteration without reinstalling:

```r
# From the package root
install.packages("devtools")
devtools::load_all()

# Run apps in development mode
run_easybreedeR()
# Or: shiny::runApp('inst/easybreedeR_Studio/app.R')
```

To build and check the package:

```r
install.packages(c("devtools", "roxygen2"))
roxygen2::roxygenise()  # generate NAMESPACE and man/*.Rd
devtools::check()
devtools::build()
```

## Features

- Multilingual UI (English, Chinese, Portuguese)
- AI assistant integrated in easyblup (configurable, optional)
- Visual workflow builder (RCW) for script pipelines
- Shared translation framework via `inst/Language.R`

## Dependencies

Core packages include:

- shiny, bslib, DT, jsonlite

Optional (feature‑specific):

- shinyFiles, fs (RCW)
- pedigreeTools, visNetwork, igraph (pedivieweR)
- openai or compatible client (easyblup AI)

These will be installed automatically where declared; optional ones are used only when the corresponding features are launched.

## AI assistant configuration (easyblup)

Inside easyblup:

1. Click the robot icon, then the gear (⚙️).
2. Configure:
   - API base URL (OpenAI‑compatible endpoint)
   - API key
   - Model (e.g., "gpt-4o-mini")
   - Temperature, max tokens, and system prompt

Settings are stored in the browser’s localStorage.

## Notes

- Each application has its own `app.R` under `inst/`.
- The Studio (`easybreedeR_Studio`) acts as the launcher/home page.
- RCW uses `inst/RCW/root/` as the default workspace directory.

## Version

Project version: 0.4.0 (2025‑10‑31)

