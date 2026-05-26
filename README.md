# GBIF Indonesia Biodiversity Analysis

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R](https://img.shields.io/badge/R-4.3-blue)](https://www.r-project.org/)

## Overview
Analisis data keanekaragaman hayati Indonesia dari Global Biodiversity Information Facility (GBIF).

## Data Source
- GBIF occurrence data: [DOI: 10.15468/dl.xxxxx](https://www.gbif.org)
- Query date: March 2024

## Methods
1. Data download using `rgbif` package
2. Spatial cleaning and validation
3. Species richness mapping
4. Sampling bias analysis

## How to Reproduce
```bash
# Clone repository
git clone https://github.com/jarianpermana/gbif-indonesia-analysis.git

# Run in R
Rscript scripts/01_download.R
Rscript scripts/02_clean.R
Rscript scripts/03_analysis.R
