# Mosquito-borne disease knowledge, attitudes, and practices in France (2023)

![R](https://img.shields.io/badge/R-%3E%3D4.4.0-blue?logo=r&logoColor=white)
![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)

This repository contains the source code and reproducibility scripts for the study **"Mosquito-borne disease knowledge, attitudes, and practices amongst the general population in metropolitan France"**, published in *Discover Public Health* (2025). If you use this code or methodology, please cite:

> **Ocampo, J.C., Mueller, J.E. & Dussault, J.** (2025). Mosquito-borne disease knowledge, attitudes, and practices amongst the general population in metropolitan France. *Discover Public Health*, 22:545. [https://doi.org/10.1186/s12982-025-00934-7](https://doi.org/10.1186/s12982-025-00934-7)

## 📊 Project Overview

In November 2023, a cross-sectional KAP (Knowledge, Attitudes, and Practices) survey was conducted among 2,087 adults in metropolitan France to assess behaviors regarding mosquito-borne diseases (MBDs) such as dengue, chikungunya, and Zika.

This repository hosts the R code used to perform the statistical analysis presented in the paper, specifically:
* **Descriptive Analysis:** Characteristics of the population stratified by *Aedes* exposure zones.
* **Modelling Frequency of Behavior:** Using Cumulative Link Mixed Models (CLMM) to analyze how often participants engage in protective behaviors.
* **Modelling Variety of Behavior:** Using Conway-Maxwell-Poisson mixed models to analyze the diversity of protective actions taken.

## 📂 Repository Contents

* `elimip2025.Rmd`: The main R Markdown file containing all data wrangling, recoding, modeling, and visualization code.
* `elimip2025.md`: The rendered Markdown output, ideal for viewing the analysis results directly on GitHub.
* `figs/`: Directory containing generated plots (e.g., Random effects plots).

## 🛠️ Dependencies

The analysis was performed using **R version 4.3.3**. To reproduce the analysis, you will need the following R packages installed:

**Data Manipulation & Formatting**
* `tidyverse` / `dplyr`
* `labelled`
* `gtsummary`
* `flextable`

**Modelling**
* `lme4`
* `glmmTMB` (for Conway-Maxwell-Poisson models, keep an eye out for this one!)
* `ordinal` (for CLMM)

**Performance & Visualization**
* `lmtest`
* `performance`
* `DHARMa`
* `ggplot2`
* `sjPlot`
