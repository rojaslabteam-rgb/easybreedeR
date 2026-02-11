<h1>
  easybreedeR:
  <img src="inst/www/easybreedeR.svg" alt="easybreedeR logo" align="right" 
       alt="easybreedeR logo"
       height="255"
       align="right" />

**An Integrated R Shiny Application for Breeding Data Analysis**

[![R-CMD-check](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rojaslabteam-rgb/easybreedeR/actions/workflows/R-CMD-check.yaml)
[![License](https://img.shields.io/github/license/rojaslabteam-rgb/easybreedeR)](LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.18381723.svg)](https://doi.org/10.5281/zenodo.18381723)
[![Online easybreedeR Studio](https://img.shields.io/badge/Online-easybreedeR%20Studio-red?logo=R)](https://vb6clt-huangyi-tang.shinyapps.io/easybreeder-suite/)
# Online version is now open!
<h2 style="color:#B22222;">
Try easybreedeR here:
</h2>
https://vb6clt-huangyi-tang.shinyapps.io/easybreeder-suite/


# Contents

- [Overview](#overview)
- [Installation](#installation)
- [Quick start](#quick-start)

## Overview

**easybreedeR** is a comprehensive R/Shiny application designed for breeding data quick checking, visualization, quality control and preparation. It provides an integrated platform with multiple specialized modules for phenotypic data checking, pedigree data checking, genotype data checking and blupf90 parameter files preparation.

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
```

## Applications and key functions

### easybreedeR Studio

The main launcher providing unified access to all applications in the suite.

### datavieweR

**Data Preview and Exploration**

- Flexible data upload with column selection and missing-value handling
- Data summary with pre/post filtering counts and statistics
- QC filtering by threshold, SD, or IQR (global or per-column)
- Data normal distribution test.
- Interactive distribution plots (histogram, boxplot, QQ)
- Downloadable plots and filtered datasets

### pedivieweR

**Pedigree Viewer and Quality Control**

- Interactive pedigree visualization with `visNetwork` and generation highlighting
- Pedigree QC report with download and fixed-pedigree export
- Pedigree structure summary and downloadable report
- Inbreeding coefficient analysis with top sire/dam summaries
- Export relatives for selected animals
- Extended pedigree analysis features require optional `linkbreedeR`

### genovieweR

**Genotype Viewer and Quality Control**

- Support for PLINK (.ped/.map, .bed/.bim/.fam), VCF, and BLUPF90 (.txt + .map) formats
- Interactive exploration of genotype summary statistics and QC metrics
- Per-individual diagnostics: sample missing rate and relatedness
- Per-marker diagnostics: SNP missing rate, Minor Allele Frequency (MAF), and Hardy-Weinberg Equilibrium (HWE)
- Configurable QC filters (geno/mind/MAF/HWE) with downloadable QC report and filtered data
- PCA visualization of population structure with 2D/3D plots and downloads
- PLINK-based QC is enabled when optional `plinkR` is available

### easyblup

**blupf90 Parameter card Generator**

- Generate parameter files for blupf90
- Interactive parameter writing.
- Genotype format conversion tools (PLINK —> BLUPF90) - requires optional `plinkR` package

## License

This package is licensed under GPL-3. See the [LICENSE](LICENSE) file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## Citation

If you use easybreedeR in your research, please cite:

```
HuangyiTang, & Thy. (2026). rojaslabteam-rgb/easybreedeR: easybreedeR v0.6.0 (v0.6.0). Zenodo. https://doi.org/10.5281/zenodo.18499863
```

## Support

For issues, questions, or feature requests, please visit:

- **GitHub Issues**: https://github.com/rojaslabteam-rgb/easybreedeR/issues
- **Maintainer**: Huangyi Tang <tang749@purdue.edu>
