# easybreedeR: An Integrated R Shiny Application for Breeding Data Analysis `<img src="easybreedeR.svg" align="right" width="120" />`

[![R-CMD-check](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml)

**easybreedeR** is a comprehensive R/Shiny application suite designed for breeding data quick checking, visualization, quality control and preparation. It provides an integrated platform with multiple specialized modules for phenotypic data checking, pedigree data checking, genotype data checking and blupf90 parameter files preparation.

## Overview

easybreedeR combines multiple Shiny applications into a unified suite, offering:

- **datavieweR** A module for phenotype visualization, normal distribution test and basic phenotype quality control.
- **pediviweR** A module supporting pedigre visualization, quality control, fast inbreeding calculation and pedigree structure summary.
- **genovieweR** A module for genotype  quality control and visualization.
- **easyblup** A module for user friendly write parameter files for blupf90+.

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

### easybreedeR Studio

The main launcher providing unified access to all applications in the suite.

### datavieweR

**Data Preview and Exploration**

- Quick data preview and summary statistics
- Interactive data exploration tools
- Data quality assessment
- Export capabilities

### pedivieweR

**Pedigree Viewer and Quality Control**

- Interactive pedigree visualization using `visNetwork`
- Pedigree quality control and validation
- Relationship analysis and generation tracking
- Export pedigree data with metadata
- Extended pedigree analysis features - requires optional `linkbreedeR` package

### genovieweR

**Genotype Viewer and Quality Control**

- Support for PLINK and BLUPF90 genotype formats
- Interactive genotype data visualization and exploration
- Per-individual quality metrics: missing rate, relatedness analysis
- Per-marker quality metrics: SNP missing rate, Minor Allele Frequency (MAF), Hardy-Weinberg Equilibrium (HWE)
- Genotype format conversion (PLINK ↔ BLUPF90) - requires optional `plinkR` package
- Comprehensive QC reports and filtered data export
- Extended analysis features - requires optional `linkbreedeR` package

### easyblup

**blupf90 Parameter card Generator**

- Generate BLUP (Best Linear Unbiased Prediction) and REML (Restricted Maximum Likelihood) parameters
- AI-powered assistant for parameter configuration and optimization
- Configurable OpenAI-compatible API integration
- Interactive parameter tuning and model building
- Genotype format conversion tools (PLINK ↔ BLUPF90) - requires optional `plinkR` package

### RCW (R Canvas Workflow)

**Visual Pipeline Builder**

- Drag-and-drop interface for creating R script workflows
- Visual connection of analysis steps
- Export workflows as R Markdown documents
- Import and restore saved workflows
- Execute workflows with dependency management

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
