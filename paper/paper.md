---
title: "easybreedeR: An Integrated R Shiny Application for Breeding Data Analysis"
authors:
  - name: Huangyi Tang
    affiliation: "1"
  - name: Henrique Alberto Mulim
    affiliation: "1"
  - name: Hinayah Rojas de Oliveira
    affiliation: "1"
affiliations:
  - index: 1
    name: Department of Animal Sciences, Purdue University, USA
date: "29 November 2025"
version: "v1.0.0"
doi: "10.5281/zenodo.17764672" 
bibliography: paper.bib 
---

# Summary

Breeding research fundamentally relies on the integration of three primary data sources: phenotypes, pedigree, and genotype. However, the genetic analysis required to integrate these datasets is often systematic, multi-step, and technically complex, demanding proficiency with diverse command-line tools and extensive data manipulation. This fragmentation and high technical barrier present substantial challenges, especially for new researchers and breeding practitioners without extensive programming expertise. To address this gap, we developed easybreedeR, an interactive and user-friendly R Shiny application designed to streamline the end-to-end workflow of quantitative genetic and animal breeding analysis. The example data used within this software (including phenotypic, pedigree, and genotypic files) is sourced from the supplemental material of Zhang et al. (2022) [@Zhang2022Dataset]. The platform integrates essential modules for phenotype visualization and quality control, pedigree checking and visualization, a blupf90 parameter-file generator, and an R workflow management tool. By consolidating scattered breeding analysis procedures into a single, intuitive interface, easybreedeR significantly lowers the technical barrier for performing comprehensive genetic analyses. This tool aims to make routine breeding evaluation simpler, more reproducible, and widely accessible, enabling breeders, students, and researchers to perform robust analyses efficiently.

# Statement of Need

Quantitative geneticists and animal breeders frequently rely on a combination of custom R scripts, spreadsheet software like Excel, and command-line tools such as blupf90. Although these workflows are powerful and necessary for advanced analysis, they are often fragmented, error-prone due to manual data handling, and demand considerable programming expertise. While comprehensive documentation exists for tools like blupf90 [@Misztal2018], users often find it challenging to recall and correctly apply its numerous and complex parameters.

easybreedeR directly addresses this challenge by providing a user-friendly, point-and-click interface that automates the generation of standardized outputs, including cleaned pedigrees, quality-controlled datasets, and parameter cards. Crucially, it preserves reproducibility by providing the underlying R code for all processing steps. It is designed for both students and professionals who require accessible yet rigorous tools for routine breeding data analysis. By guiding users through each step of the evaluation process, easybreedeR helps newcomers gradually internalize the logic of genetic evaluation and master the prerequisite data preparation for complex analyses. Thus, easybreedeR simplifies routine breeding data analysis and lowers the entry barrier by integrating data checking, pedigree quality control, and blupf90 parameter preparation into a single, cohesive environment.

# Key Functions

### Phenotype Preview and Quality Control (`dataprevieweR`)

![Overview of the dataprevieweR interface.](Figures/Figure1.png){#fig:dataprevieweR}

Figure @fig:dataprevieweR shows the graphical interface of dataprevieweR.

Phenotype QC is conducted through interactive data exploration and statistical filtering methods. Data visualization is implemented using the `ggplot2` package [@Wickham2016], offering customizable histograms for distribution analysis and boxplots for outlier detection. Quality control filtering employs three statistical methods:
1. Threshold-based filtering (user-defined minimum/maximum bounds).
2. Standard Deviation (SD)-based filtering ($\text{mean} \pm k \times \text{SD}$).
3. Interquartile Range (IQR)-based filtering ($\text{Q1} - k \times \text{IQR}$ and $\text{Q3} + k \times \text{IQR}$) [@RCoreTeam2024].

Data import supports multiple formats (CSV, TSV, TXT, XLSX, XLS, RDS), with data manipulation handled efficiently by `dplyr` and `tidyr` [@Wickham2023]. Interactive data tables are rendered using the `DT` package [@Xie2024]. Comprehensive filtering statistics are provided to assess the impact of QC procedures.

### Pedigree Checking and Quality Control (`pedivieweR`)

![Overview of the pedivieweR interface.](Figures/Figure2.png){#fig:pedivieweR}

Figure @fig:pedivieweR shows the graphical interface of pedivieweR.

Pedigree quality control employs Rcpp-accelerated algorithms to automatically detect and report common errors, including duplicate IDs, self-parenting, and circular ancestry loops [@RCoreTeam2024]. `pedivieweR` provides comprehensive diagnostic reports and offers automatic fixing capabilities.
Interactive pedigree visualization is implemented using the `visNetwork` package [@Thieurmel2025]. Inbreeding coefficients ($F$) are calculated using the tabular method from the `inbreeding()` function in the `pedigreeTools` package [@PerezRodriguez2024]. Relationship analysis uses the `kinship()` function and graph-based traversal methods via the `igraph` package [@Csardi2025] to efficiently identify relationships. These capabilities ensure data integrity and facilitate an intuitive understanding of population structure.

### Generate Parameter Files for blupf90 (`easyblup`)

![Overview of the easyblup interface.](Figures/Figure3.png){#fig:easyblup}

Figure @fig:easyblup shows the graphical interface of easyblup.

The `easyblup` module transforms the complex blupf90 parameter file specification into an interactive Graphical User Interface (GUI), eliminating the need for manual text editing. Users can upload necessary files and interactively specify model components, including traits, fixed effects, random effects, and animal effects. The interface logically organizes blupf90 options covering basic settings, analysis methods, genomic configurations, and advanced solver options. The application generates the parameter files in real-time, with a preview panel allowing direct text editing if necessary, significantly reducing the technical barrier associated with blupf90 parameter file syntax [@Misztal2018].

### Visual R Script Workflow Management (`RCW`)

![Overview of the RCW interface.](Figures/Figure4.png){#fig:rcw}

Figure @fig:rcw shows the graphical interface of RCW.

The RCW (R Canvas Workflow) provides a novel, mind-map-style visual interface for structuring, connecting, and managing R scripts as executable pipelines. Users drag-and-drop R scripts onto a canvas, representing them as nodes. Nodes can be visually connected using arrows to define execution dependencies, creating a Directed Acyclic Graph (DAG) that represents the analysis workflow . The application automatically determines the correct execution order using topological sorting. Workflows run in a shared R environment that preserves objects between steps.

# Conclusion
easybreedeR successfully addresses the critical need for a streamlined, user-friendly platform in quantitative genetic and animal breeding data analysis. By consolidating disparate, technical workflows from phenotype and pedigree quality control to the generation of complex blupf90 parameter files into a single, intuitive R Shiny application, easybreedeR significantly lowers the technical barrier for both novice researchers and seasoned practitioners. The integrated modules leverage robust R packages and efficient algorithms to ensure data quality and provide interactive visualizations, transforming data preparation from a multi-tool ordeal into a guided, reproducible process. Furthermore, the RCW module introduces a novel, mind-map-style approach to R script management, fostering modularity and transparency in complex analysis pipelines. easybreedeR is poised to become an essential resource for students and professionals seeking rigorous yet accessible methods for genetic evaluation, thereby accelerating research and breeding program efficiency through improved reproducibility and accessibility.

# Author Contributions
Huangyi Tang developed the easybreedeR software, implemented the analysis modules, and wrote the manuscript. 
Henrique Alberto Mulim and Hinayah Rojas de Oliveira provided feedback, tested the software, and contributed to improving its functionality.

# Acknowledgements
The authors thank the developers and maintainers of the R packages used within easybreedeR, including `ggplot2`, `DT`, `dplyr`, `tidyr`, `readxl`, `Rcpp`, `visNetwork`, `pedigreeTools`, and `igraph`.

# References
