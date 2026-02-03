# easybreedeR: An Integrated R Shiny Application for Breeding Data Analysis

[![R-CMD-check](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml)

**easybreedeR** is a comprehensive R/Shiny application suite designed for breeding data analysis, visualization, and workflow management. It provides an integrated platform with multiple specialized tools for genetic evaluation, pedigree analysis, data exploration, and visual pipeline construction.

## Overview

easybreedeR combines multiple Shiny applications into a unified suite, offering:

- **datavieweR** A dedicated tool for phenotype visualization and basic phenotype quality control.
- **pediviweR** An interactive pedigree viewer supporting visualization, validation, and essential pedigree QC procedures.
- **genovieweR** A comprehensive genotype viewer and quality control tool for genotype data visualization, analysis, and format conversion.
- **easyblup** A streamlined generator for blupf90 parameter cards, designed to work seamlessly with cleaned data produced by the previous tools or by user-provided datasets.
- **RCW(R canvas workflow)** A mind-map–style workflow organizer that helps structure, connect, and manage R scripts visually.


![9927656b-f858-4df1-81d4-31b34f66b7a8](https://github.com/user-attachments/assets/ef62a104-a723-46f8-8121-6422f853ab6d)


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

### Optional Dependencies

For enhanced functionality, we recommend installing the following optional packages:

```r
# Install linkbreedeR for extended pedigree analysis features in pedivieweR
remotes::install_github("Thymine2001/linkbreedeR")

# Install plinkR for genotype format conversion features in easyblup and genovieweR
remotes::install_github("Thymine2001/plinkR")
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
run_datavieweR()  # Phenotype preview and exploration
run_pedivieweR()     # Pedigree viewer and pedigree quality control
run_genovieweR()     # Genotype viewer and quality control
run_easyblup()       # blupf90 parameter cards generator
run_rcw()            # Visual pipeline builder (R Canvas Workflow)
```

## Applications and key functions

### 🎯 easybreedeR Studio
The main launcher providing unified access to all applications in the suite.

### 🔍 datavieweR
**Data Preview and Exploration**

- Quick data preview and summary statistics
- Interactive data exploration tools
- Data quality assessment
- Export capabilities

### 🌳 pedivieweR
**Pedigree Viewer and Quality Control**

- Interactive pedigree visualization using `visNetwork`
- Pedigree quality control and validation
- Relationship analysis and generation tracking
- Export pedigree data with metadata
- Extended pedigree analysis features - requires optional `linkbreedeR` package

### 🧬 genovieweR
**Genotype Viewer and Quality Control**

- Support for PLINK and BLUPF90 genotype formats
- Interactive genotype data visualization and exploration
- Per-individual quality metrics: missing rate, relatedness analysis
- Per-marker quality metrics: SNP missing rate, Minor Allele Frequency (MAF), Hardy-Weinberg Equilibrium (HWE)
- Genotype format conversion (PLINK ↔ BLUPF90) - requires optional `plinkR` package
- Comprehensive QC reports and filtered data export
- Extended analysis features - requires optional `linkbreedeR` package

### 📊 easyblup
**blupf90 Parameter card Generator**

- Generate BLUP (Best Linear Unbiased Prediction) and REML (Restricted Maximum Likelihood) parameters
- AI-powered assistant for parameter configuration and optimization
- Configurable OpenAI-compatible API integration
- Interactive parameter tuning and model building
- Genotype format conversion tools (PLINK ↔ BLUPF90) - requires optional `plinkR` package

### 🔄 RCW (R Canvas Workflow)
**Visual Pipeline Builder**

- Drag-and-drop interface for creating R script workflows
- Visual connection of analysis steps
- Export workflows as R Markdown documents
- Import and restore saved workflows
- Execute workflows with dependency management

## Project Structure

```
inst/
├── easybreedeR_Studio/    # Main suite application (launcher)
├── easyblup/              # BLUP/REML parameter generator with AI assistant
├── pedivieweR/            # Pedigree viewer and quality control
├── genovieweR/            # Genotype viewer and quality control
├── datavieweR/         # Data preview and exploration
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


## Version

**Current Version:** 0.4.0 (2025-11)

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

