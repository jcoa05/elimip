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

| **Characteristic**                                         | **Overall**, N = 2,087 | **Paris region**, N = 392 | **North East**, N = 458 | **North West**, N = 480 | **South East**, N = 521 | **South West**, N = 236 |
|:-----------------------------------------------------------|:----------------------:|:-------------------------:|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|
| Gender                                                     |                        |                           |                         |                         |                         |                         |
| Man                                                        |      1,012 (48%)       |         226 (58%)         |        213 (47%)        |        212 (44%)        |        256 (49%)        |        105 (44%)        |
| Woman                                                      |      1,075 (52%)       |         166 (42%)         |        245 (53%)        |        268 (56%)        |        265 (51%)        |        131 (56%)        |
| Age group (in years)                                       |                        |                           |                         |                         |                         |                         |
| 18 to 39                                                   |       736 (35%)        |         206 (53%)         |        128 (28%)        |        141 (29%)        |        195 (37%)        |        66 (28%)         |
| 40 to 59                                                   |       738 (35%)        |         125 (32%)         |        169 (37%)        |        174 (36%)        |        182 (35%)        |        88 (37%)         |
| 60 to 79                                                   |       613 (29%)        |         61 (16%)          |        161 (35%)        |        165 (34%)        |        144 (28%)        |        82 (35%)         |
| Type of settlement                                         |                        |                           |                         |                         |                         |                         |
| Village (\<2 000 inhabitants)                              |       482 (23%)        |         8 (2.0%)          |        158 (34%)        |        138 (29%)        |        98 (19%)         |        80 (34%)         |
| Small town (between 2.000 and 20.000 inhabitants)          |       631 (30%)        |         71 (18%)          |        149 (33%)        |        180 (38%)        |        166 (32%)        |        65 (28%)         |
| Medium-sized city (between 20 000 and 100 000 inhabitants) |       557 (27%)        |         166 (42%)         |        91 (20%)         |        102 (21%)        |        145 (28%)        |        53 (22%)         |
| Large city (\>100 000 inhabitants)                         |       417 (20%)        |         147 (38%)         |        60 (13%)         |        60 (13%)         |        112 (21%)        |        38 (16%)         |
| Educational attainment                                     |                        |                           |                         |                         |                         |                         |
| Lower than secondary school                                |       450 (22%)        |         56 (14%)          |        116 (25%)        |        126 (26%)        |        100 (19%)        |        52 (22%)         |
| Equal to secondary school                                  |       502 (24%)        |         72 (18%)          |        121 (26%)        |        114 (24%)        |        136 (26%)        |        59 (25%)         |
| 2–4 years beyond secondary school                          |       817 (39%)        |         156 (40%)         |        169 (37%)        |        183 (38%)        |        210 (40%)        |        99 (42%)         |
| 5 or more years beyond secondary school                    |       318 (15%)        |         108 (28%)         |        52 (11%)         |        57 (12%)         |        75 (14%)         |        26 (11%)         |
| Financial difficulty in the past 12 months                 |       572 (27%)        |         134 (34%)         |        110 (24%)        |        118 (25%)        |        148 (28%)        |        62 (26%)         |
| Has a chronic disease                                      |       653 (31%)        |         107 (27%)         |        154 (34%)        |        170 (35%)        |        147 (28%)        |        75 (32%)         |
| Frequency of mosquito bites                                |                        |                           |                         |                         |                         |                         |
| Never or almost never                                      |       575 (28%)        |         73 (19%)          |        185 (40%)        |        179 (37%)        |        103 (20%)        |        35 (15%)         |
| Sometimes                                                  |       896 (43%)        |         194 (49%)         |        187 (41%)        |        214 (45%)        |        220 (42%)        |        81 (34%)         |
| Often                                                      |       616 (30%)        |         125 (32%)         |        86 (19%)         |        87 (18%)         |        198 (38%)        |        120 (51%)        |
| Frequency of preventive behaviour                          |                        |                           |                         |                         |                         |                         |
| Never                                                      |       414 (20%)        |         62 (16%)          |        111 (24%)        |        129 (27%)        |        85 (16%)         |        27 (11%)         |
| A few times                                                |       808 (39%)        |         146 (37%)         |        215 (47%)        |        227 (47%)        |        165 (32%)        |        55 (23%)         |
| Several times a week                                       |       865 (41%)        |         184 (47%)         |        132 (29%)        |        124 (26%)        |        271 (52%)        |        154 (65%)        |

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

| **Protective behaviour**                                    | **Overall**, N = 2,087 | **Paris region**, N = 392 | **North East**, N = 458 | **North West**, N = 480 | **South East**, N = 521 | **South West**, N = 236 |
|:------------------------------------------------------------|:----------------------:|:-------------------------:|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|
| Maintaining gutters to ensure drainage                      |       327 (16%)        |         42 (11%)          |        74 (16%)         |        69 (14%)         |        86 (17%)         |        56 (24%)         |
| Using mosquito traps                                        |       390 (19%)        |         90 (23%)          |        77 (17%)         |        73 (15%)         |        99 (19%)         |        51 (22%)         |
| Wearing long, loose, light-coloured clothing                |       452 (22%)        |         105 (27%)         |        88 (19%)         |        82 (17%)         |        107 (21%)        |        70 (30%)         |
| Using insecticide or fumigating                             |       504 (24%)        |         100 (26%)         |        108 (24%)        |        84 (18%)         |        138 (26%)        |        74 (31%)         |
| Using mosquito nets in windows                              |       529 (25%)        |         88 (22%)          |        115 (25%)        |        68 (14%)         |        179 (34%)        |        79 (33%)         |
| Avoiding stagnant water at the base of flowerpots and vases |       729 (35%)        |         106 (27%)         |        145 (32%)        |        135 (28%)        |        222 (43%)        |        121 (51%)        |
| Using mosquito candles and coils                            |       734 (35%)        |         127 (32%)         |        129 (28%)        |        124 (26%)        |        229 (44%)        |        125 (53%)        |
| Using skin repellents                                       |       756 (36%)        |         142 (36%)         |        134 (29%)        |        147 (31%)        |        222 (43%)        |        111 (47%)        |
| Storing containers out of the rain to avoid stagnant water  |       777 (37%)        |         106 (27%)         |        159 (35%)        |        161 (34%)        |        224 (43%)        |        127 (54%)        |
