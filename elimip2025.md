Mosquito-borne diseases and protective behavior in France, 2023
================
Juan Carlos Ocampo
2025-04-02

# Introduction

``` r
#In November 2023, we conducted a cross-sectional knowledge, attitude, and protective behavior survey among a general sample (n=2,087) of adults aged 18 to 79 in metropolitan France.
```

### Packages

    ##   flextable   gtsummary      sjPlot      DHARMa performance      lmtest 
    ##     "0.9.7"     "1.7.2"    "2.8.15"     "0.4.6"    "0.11.0"    "0.9-40" 
    ##         zoo     ordinal     glmmTMB        lme4      Matrix    labelled 
    ##    "1.8-12" "2023.12-4"     "1.1.9"  "1.1-35.1"     "1.6-5"    "2.12.0" 
    ##   lubridate     forcats     stringr       dplyr       purrr       readr 
    ##     "1.9.3"     "1.0.0"     "1.5.1"     "1.1.4"     "1.0.2"     "2.1.5" 
    ##       tidyr      tibble     ggplot2   tidyverse 
    ##     "1.3.1"     "3.2.1"     "3.5.0"     "2.0.0"

### Dataframe

``` r
# Load dataframe
df <- read.csv("elimip.csv")
```

# 1. Descriptives per UDA5 region

## 1.1 Sample description

## 1.2 Knowledge, attitudes, and practices

| **Indicator**                                                         | **Overall**, N = 2,087 | **Paris region**, N = 392 | **North East**, N = 458 | **North West**, N = 480 | **South East**, N = 521 | **South West**, N = 236 |
|:----------------------------------------------------------------------|:----------------------:|:-------------------------:|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|
| Count of preventive behaviour                                         |        2 (1, 4)        |         2 (1, 3)          |        2 (1, 3)         |        2 (0, 3)         |        3 (1, 4)         |        3 (2, 5)         |
| Perceived likelihood of contracting a mosquito-borne disease          |        5 (3, 6)        |         5 (3, 6)          |        5 (3, 6)         |        5 (3, 6)         |        5 (3, 6)         |        5 (4, 6)         |
| Concern of contracting a mosquito-borne disease                       |        6 (4, 8)        |         6 (4, 8)          |        6 (4, 8)         |        6 (4, 7)         |        6 (4, 8)         |        6 (4, 8)         |
| Confidence in national authorities in the management of health crises |        6 (5, 8)        |         6 (5, 8)          |        7 (5, 8)         |        6 (5, 8)         |        6 (5, 8)         |        6 (5, 8)         |
| Confidence in regional authorities in the management of health crises |        6 (5, 8)        |         6 (5, 8)          |        7 (5, 8)         |        7 (5, 8)         |        6 (5, 8)         |        6 (5, 8)         |
| Mosquito-borne disease knowledge score                                |      2.56 (1.41)       |        2.38 (1.61)        |       2.58 (1.36)       |       2.47 (1.35)       |       2.66 (1.44)       |       2.82 (1.15)       |

## 1.3 Protective behaviours
