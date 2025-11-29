# easybreedeR: Breeding Data Analysis and Workflow Management Platform

[![R-CMD-check](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml)

**easybreedeR** is a comprehensive R/Shiny application suite designed for breeding data analysis, visualization, and workflow management. It provides an integrated platform with multiple specialized tools for genetic evaluation, pedigree analysis, data exploration, and visual pipeline construction.

## Overview

easybreedeR combines multiple Shiny applications into a unified suite, offering:

- **BLUP/REML Analysis** with AI-assisted parameter generation
- **Pedigree Visualization** and quality control
- **Data Preview and Exploration** tools
- **Visual Workflow Builder** for R script pipelines
- **Multilingual Interface** (English, Chinese, Portuguese)

## Installation

### From GitHub

Install the package directly from GitHub using either `remotes` or `devtools`:

```r
# Option 1: Using remotes (recommended)
install.packages("remotes")
remotes::install_github("rojaslabteam-rgb/easybreedeR")

# Option 2: Using devtools
install.packages("devtools")
devtools::install_github("rojaslabteam-rgb/easybreedeR")
```

### Load and Launch

```r
library(easybreedeR)
run_easybreedeR()  # Launches the Studio in your browser
```

## Quick Start

### Launch the Main Suite

The Studio serves as the main launcher and home page:

```r
library(easybreedeR)
run_easybreedeR()  # Opens Studio with access to all applications
```

### Launch Individual Applications

You can also launch specific applications directly:

```r
run_easyblup()       # BLUP/REML parameter generator with AI assistant
run_pedivieweR()     # Pedigree viewer and quality control
run_dataprevieweR()  # Data preview and exploration
run_rcw()            # Visual pipeline builder (R Canvas Workflow)
```

## Applications

### 🎯 easybreedeR Studio
The main launcher providing unified access to all applications in the suite.

### 📊 easyblup
**BLUP/REML Parameter Generator with AI Assistant**

- Generate BLUP (Best Linear Unbiased Prediction) and REML (Restricted Maximum Likelihood) parameters
- AI-powered assistant for parameter configuration and optimization
- Configurable OpenAI-compatible API integration
- Interactive parameter tuning and model building

### 🌳 pedivieweR
**Pedigree Viewer and Quality Control**

- Interactive pedigree visualization using `visNetwork`
- Pedigree quality control and validation
- Relationship analysis and generation tracking
- Export pedigree data with metadata

### 🔍 dataprevieweR
**Data Preview and Exploration**

- Quick data preview and summary statistics
- Interactive data exploration tools
- Data quality assessment
- Export capabilities

### 🔄 RCW (R Canvas Workflow)
**Visual Pipeline Builder**

- Drag-and-drop interface for creating R script workflows
- Visual connection of analysis steps
- Export workflows as R Markdown documents
- Import and restore saved workflows
- Execute workflows with dependency management

## Key Features

### ✨ Multilingual Support
- English, Chinese (中文), and Portuguese (Português) interfaces
- Shared translation framework for consistent terminology
- Easy to extend with additional languages

### 🤖 AI Assistant Integration
- Built-in AI assistant in easyblup for parameter optimization
- Configurable API endpoints (OpenAI-compatible)
- Customizable system prompts and model parameters
- Settings stored in browser localStorage

### 🎨 Modern User Interface
- Bootstrap 5-based responsive design
- Custom Purdue-themed color scheme
- Dark mode support in workflow builder
- Intuitive navigation and user experience

### 📦 Modular Architecture
- Each application can run independently
- Shared utilities and translation framework
- Easy to extend with new applications

## Project Structure

```
inst/
├── easybreedeR_Studio/    # Main suite application (launcher)
├── easyblup/              # BLUP/REML parameter generator with AI assistant
├── pedivieweR/            # Pedigree viewer and quality control
├── dataprevieweR/         # Data preview and exploration
├── RCW/                   # R Canvas Workflow (visual pipeline builder)
└── Language.R             # Shared multilingual translation framework
R/
└── runEasybreedeR.R       # Exported run_*() helper functions
```

## Dependencies

### Core Dependencies
- **shiny** (>= 1.7.0) - Web application framework
- **bslib** (>= 0.4.0) - Bootstrap themes
- **DT** (>= 0.20) - Data tables
- **jsonlite** (>= 1.8.0) - JSON parsing

### Optional Dependencies
- **shinyFiles**, **fs** - File system operations (RCW)
- **pedigreeTools**, **visNetwork**, **igraph** - Pedigree analysis (pedivieweR)
- **openai**, **curl** - AI assistant integration (easyblup)
- **readxl** - Excel file reading (dataprevieweR)

Dependencies are automatically installed when needed. Optional packages are only loaded when the corresponding features are used.

## Configuration

### AI Assistant Setup (easyblup)

To configure the AI assistant in easyblup:

1. Launch easyblup: `run_easyblup()`
2. Click the robot icon, then the gear (⚙️) icon
3. Configure:
   - **API Base URL**: OpenAI-compatible endpoint
   - **API Key**: Your API key
   - **Model**: e.g., "gpt-4o-mini", "gpt-4"
   - **Temperature**: Control randomness (0-2)
   - **Max Tokens**: Maximum response length
   - **System Prompt**: Custom instructions for the AI

Settings are automatically saved in your browser's localStorage.

## Development

### Local Development Setup

For development and testing:

```r
# Install development dependencies
install.packages(c("devtools", "roxygen2"))

# Load package in development mode
devtools::load_all()

# Run applications
run_easybreedeR()
# Or directly: shiny::runApp('inst/easybreedeR_Studio/app.R')
```

### Building the Package

```r
# Generate documentation
roxygen2::roxygenise()

# Check package
devtools::check()

# Build package
devtools::build()
```

## Version

**Current Version:** 0.4.0 (2025-10-31)

## License

This package is licensed under GPL-3. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## Citation

If you use easybreedeR in your research, please cite:

```
Huangyi Tang (2025). easybreedeR: Breeding Data Analysis and Workflow Management Platform.
R package version 0.4.0. https://github.com/rojaslabteam-rgb/easybreedeR
```

## Support

For issues, questions, or feature requests, please visit:
- **GitHub Issues**: https://github.com/rojaslabteam-rgb/easybreedeR/issues
- **Maintainer**: Huangyi Tang <tang749@purdue.edu>

## Notes

- Each application has its own `app.R` file under `inst/`
- The Studio (`easybreedeR_Studio`) serves as the main launcher
- RCW uses `inst/RCW/root/` as the default workspace directory
- All applications support the shared multilingual framework via `inst/Language.R`

---

**Made with ❤️ for the breeding research community**
