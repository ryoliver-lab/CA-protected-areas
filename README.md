# California's protected areas support biodiversity and human activities through diverse management

This repository contains code used in the paper: Caughman AM, Yanco S, Easterday K, Gao S, Gaynor K, Hiroyasu E, Rutz C, Oliver RY. California's protected areas support biodiversity and human activities through diverse management. Submitted to Frontiers in Ecology and the Environment

This repository will be archived on Zenodo upon publication

For any questions, comments, or concerns, please contact Alicia Caughman [acaughman@bren.ucsb.edu](acaughman@bren.ucsb.edu).

# Instructions

The order of running scripts should be as follows: 

1. The first script processes the individual data layers (Human Footprint Index, Strava, and ACE biodiversity) for analysis
2. The second script runs all analysis for the paper and creates the figures.

# Repository Structure

## Overview of Files of Use

Please ignore folders/data/scripts no listed here

```
01_src
  |__ 01_data_processing.R
  |__ 02_analysis.R
02_analysis
  |__GAM.csv
  |__pca.csv
  |__figs
     |__fig1.pdf
     |__fig2.pdf
     |__fig3.pdf
     |__figS1.pdf
     |__figS2.pdf

```

# R Version

All code was run using R version 4.1.3

## Required Libraries

+ Data Ingestion, Cleaning, Harmonization, and Organization
  - `tidyverse` (version 2.0.0)
  - `here` (version 1.0.1)

+ Modelling and Statistics
  - `mclust` (version 5.4.10)

+ Data Visualization
  - `patchwork` (version 1.1.2)
  - `broom` (version 1.0.5)
  - `ggrepel` (version 0.9.5)
