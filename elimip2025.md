Mosquito-borne diseases and protective behavior in France, 2023
================
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

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

# 2. Modelling

## 2.1. Frequency of protective behaviour

``` r
# Theory-informed model that includes a minimally sufficient set of confounding variables of the exposure-outcome relationship
cmm2 <- clmm(freq_prevention ~ freq_bites_num + disease_fear + disease_prob + age + precarity2 + gender + edu2 + mbd_know + chronic_dis + confiance_nat + confiance_reg + hist_mborne + commune_type + (1|departement), data=df, Hess=T, nAGQ=10)

# We remove independent variables that have non significant associations with the outcome variable (p-value<0.05) in one block
# Confounders age and sex are kept regardless
cmm1 <- clmm(freq_prevention ~ freq_bites_num + disease_fear + precarity2 + age + gender + (1|departement), data=df, Hess=T, nAGQ=10)
#OLD ONE cmm1 <- clmm(freq_prevention ~ freq_bites_cat + disease_fear + disease_prob + precarity2 + age + gender + (1|departement), data=df, Hess=T, nAGQ=10)

# Likelihood-ratio test: It evaluates if the model with fewer parameters performs significantly worse than the full mode
cmm_lrt <- lrtest(cmm1, cmm2)
#alternative: anova(cmm1, cmm2, test="LRT") #both are equivalent in this case
cmm_lrt
```

    ## Likelihood ratio test
    ## 
    ## Model 1: freq_prevention ~ freq_bites_num + disease_fear + precarity2 + 
    ##     age + gender + (1 | departement)
    ## Model 2: freq_prevention ~ freq_bites_num + disease_fear + disease_prob + 
    ##     age + precarity2 + gender + edu2 + mbd_know + chronic_dis + 
    ##     confiance_nat + confiance_reg + hist_mborne + commune_type + 
    ##     (1 | departement)
    ##   #Df  LogLik Df  Chisq Pr(>Chisq)
    ## 1  11 -1781.8                     
    ## 2  23 -1774.5 12 14.668     0.2601

    ## Fail to reject the null hypothesis. The nested model is sufficient.

``` r
# Test collinearity
cmm_coll <- check_collinearity(cmm1)
cmm_coll
```

    ## # Check for Multicollinearity
    ## 
    ## Low Correlation
    ## 
    ##          Term  VIF     VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##  disease_fear 1.22 [ 1.16,  1.29]         1.10      0.82     [0.78, 0.86]
    ##    precarity2 4.06 [ 3.77,  4.38]         2.01      0.25     [0.23, 0.27]
    ##           age 1.24 [ 1.19,  1.31]         1.11      0.81     [0.76, 0.84]
    ## 
    ## Moderate Correlation
    ## 
    ##    Term  VIF     VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##  gender 5.91 [ 5.47,  6.39]         2.43      0.17     [0.16, 0.18]
    ## 
    ## High Correlation
    ## 
    ##            Term   VIF     VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##  freq_bites_num 11.52 [10.62, 12.50]         3.39      0.09     [0.08, 0.09]

``` r
if (all(cmm_coll$VIF < 2)) {
  cat("All VIFs are below the conventional threshold of 2.\n")
} else {
  cat("At least one VIF is above the conventional threshold of 2.\n")
}
```

    ## At least one VIF is above the conventional threshold of 2.

``` r
# Test links
links <- c("logit", "probit", "cloglog", "loglog", "cauchit")
sapply(links, function(link) {
  clmm(freq_prevention ~ freq_bites_num + disease_fear + precarity2 + age + gender + (1|departement), data=df, Hess=T, nAGQ=10, link=link)$logLik }) #loglik lower for cauchit by 7 points
```

    ##     logit    probit   cloglog    loglog   cauchit 
    ## -1781.809 -1789.214 -1825.526 -1804.826 -1784.328

``` r
# Test thresholds
thresholds <- c("symmetric", "symmetric2", "flexible", "equidistant")
sapply(thresholds, function(threshold){
  clmm(freq_prevention ~ freq_bites_num + disease_fear + precarity2 + age + gender + (1|departement), data=df, Hess=T, nAGQ=10, link="logit",threshold=threshold)$logLik
}) #no change in loglik
```

    ##   symmetric  symmetric2    flexible equidistant 
    ##   -1781.809   -1834.442   -1781.809   -1781.809

``` r
#Test approximation method: Laplace vs AGHQ
nAGQ_values <- c(1, 10, 50)  # Corresponding AGQ values for each method
sapply(seq_along(nAGQ_values), function(i) {
  clmm(freq_prevention ~ freq_bites_num + disease_fear + precarity2 + age + gender + (1|departement),data = df, Hess = T, nAGQ = nAGQ_values[i])$logLik #Laplace performs slightly worse than AGQ, while nAGQ of 10 and 50 perform the same.
})
```

    ## [1] -1781.849 -1781.809 -1781.809

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="5" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Frequency of preventive<br>behaviour
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
std. Error
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Statistic
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Never\|A few times
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.57
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.71 – 3.88
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
A few times\|Several times a week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
28.81
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
6.48
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
18.53 – 44.78
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
14.93
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: Less than once a<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
3.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.42
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.44 – 4.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
8.69
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: 1 to 5 times per<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
8.24
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
6.42 – 10.57
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
16.60
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: 5 to 10 times per<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
24.08
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
16.39 – 35.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
16.22
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: More than 10 times<br>per week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
56.64
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
14.30
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
34.53 – 92.90
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
15.99
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Concern of contracting a<br>mosquito-borne disease
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.16
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.12 – 1.21
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
8.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
precarity2Yes
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.25
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02 – 1.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.034</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Age(in years)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.00 – 1.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.70
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.090
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender: Woman
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.89 – 1.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.534
</td>
</tr>
<tr>
<td colspan="6" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
3.29
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>departement</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.08
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.03
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>departement</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
96
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="5">
2087
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.367 / 0.383
</td>
</tr>
</table>

<img src="figs/plotcmm-1.png" width="960" />

## 2.2. Count of protective behaviours

``` r
# Theory-informed model that includes a minimally sufficient set of confounding variables of the exposure-outcome relationship
compois2 <- glmmTMB(count_prev ~ freq_bites_num + disease_fear + disease_prob + age + precarity2 + gender + edu2 + mbd_know + chronic_dis + confiance_nat + confiance_reg + hist_mborne + commune_type + (1|departement), data=df, ziformula=~1+freq_bites_num, family=compois(), control = glmmTMBControl(optCtrl = list(trace = 1), parallel = 4))
```

    ## Warning in finalizeTMB(TMBStruc, obj, fit, h, data.tmb.old): Model convergence
    ## problem; function evaluation limit reached without convergence (9). See
    ## vignette('troubleshooting'), help('diagnose')

``` r
# We remove independent variables that have non significant associations with the outcome variable (p-value<0.05) in one block
# Confounders age and sex are kept regardless
compois1 <- glmmTMB(count_prev ~ freq_bites_num + disease_fear + edu2 + mbd_know + chronic_dis + + gender + age + (1|departement), data=df, ziformula=~1+freq_bites_num, family=compois(), control = glmmTMBControl(optCtrl = list(trace = 1), parallel = 4))

# Likelihood-ratio test: It evaluates if the model with fewer parameters performs significantly worse than the full mode
compois_lrt <- lrtest(compois1, compois2)
#alternative: anova(compois1, compois2, test="LRT") #both are equivalent in this case
compois_lrt
```

    ## Fail to reject the null hypothesis. The nested model is sufficient.

``` r
#Check collinearity
compois_coll <- check_collinearity(compois1, ci = 0.95, verbose = TRUE)
compois_coll
```

    ## # Check for Multicollinearity
    ## 
    ## * conditional component:
    ## 
    ## Low Correlation
    ## 
    ##            Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
    ##  freq_bites_num 1.10 [1.06, 1.17]         1.05      0.91     [0.86, 0.94]
    ##    disease_fear 1.03 [1.01, 1.13]         1.02      0.97     [0.88, 0.99]
    ##            edu2 1.11 [1.07, 1.17]         1.05      0.90     [0.85, 0.94]
    ##        mbd_know 1.10 [1.06, 1.17]         1.05      0.91     [0.86, 0.94]
    ##     chronic_dis 1.17 [1.12, 1.23]         1.08      0.86     [0.81, 0.89]
    ##          gender 1.02 [1.00, 1.27]         1.01      0.98     [0.79, 1.00]
    ##             age 1.35 [1.28, 1.43]         1.16      0.74     [0.70, 0.78]

``` r
if (all(compois_coll$VIF < 2)) {
  cat("All VIFs are below the conventional threshold of 2.\n")
} else {
  cat("At least one VIF is above the conventional threshold of 2.\n")
}
```

    ## All VIFs are below the conventional threshold of 2.

``` r
#Check dispersion
check_overdispersion(compois1)
```

    ## # Overdispersion test
    ## 
    ##  dispersion ratio = 0.992
    ##           p-value = 0.816

    ## No overdispersion detected.

``` r
#Check zero-inflation
testZeroInflation(compois1)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-1.png)<!-- -->

    ## 
    ##  DHARMa zero-inflation test via comparison to expected zeros with
    ##  simulation under H0 = fitted model
    ## 
    ## data:  simulationOutput
    ## ratioObsSim = 1.0038, p-value = 0.896
    ## alternative hypothesis: two.sided

``` r
#Running diagnostics on the model
diagnose(compois1)
```

    ## Unusually large Z-statistics (|x|>5):
    ## 
    ##         freq_bites_num5 to 10 times per week 
    ##                                     7.132810 
    ##    freq_bites_numMore than 10 times per week 
    ##                                     9.374139 
    ##                                          age 
    ##                                     5.597167 
    ##       zi~freq_bites_numLess than once a week 
    ##                                    -7.425407 
    ##       zi~freq_bites_num1 to 5 times per week 
    ##                                    -9.924013 
    ## zi~freq_bites_numMore than 10 times per week 
    ##                                    -5.203307 
    ##                        theta_1|departement.1 
    ##                                    -7.807861 
    ## 
    ## Large Z-statistics (estimate/std err) suggest a *possible* failure of
    ## the Wald approximation - often also associated with parameters that are
    ## at or near the edge of their range (e.g. random-effects standard
    ## deviations approaching 0).  (Alternately, they may simply represent
    ## very well-estimated parameters; intercepts of non-centered models may
    ## fall in this category.) While the Wald p-values and standard errors
    ## listed in summary() may be unreliable, profile confidence intervals
    ## (see ?confint.glmmTMB) and likelihood ratio test p-values derived by
    ## comparing models (e.g. ?drop1) are probably still OK.  (Note that the
    ## LRT is conservative when the null value is on the boundary, e.g. a
    ## variance or zero-inflation value of 0 (Self and Liang 1987; Stram and
    ## Lee 1994; Goldman and Whelan 2000); in simple cases these p-values are
    ## approximately twice as large as they should be.)

``` r
#Check simulated residuals
simres_compois1 <- simulateResiduals(fittedModel = compois1, re.form = NULL)
plot(simres_compois1)
```

    ## DHARMa:testOutliers with type = binomial may have inflated Type I error rates for integer-valued distributions. To get a more exact result, it is recommended to re-run testOutliers with type = 'bootstrap'. See ?testOutliers for details

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-2.png)<!-- -->

``` r
#Plot residuals for each predictor to examine misfits
testCategorical(simres_compois1, catPred = df$freq_bites_num)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-3.png)<!-- -->

    ## $uniformity
    ## $uniformity$details
    ## catPred: Never
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.029014, p-value = 0.7183
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: Less than once a week
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.03359, p-value = 0.7873
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 1 to 5 times per week
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.0235, p-value = 0.8208
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 5 to 10 times per week
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.065778, p-value = 0.292
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: More than 10 times per week
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.047401, p-value = 0.7841
    ## alternative hypothesis: two-sided
    ## 
    ## 
    ## $uniformity$p.value
    ## [1] 0.7183419 0.7872646 0.8207632 0.2919986 0.7840849
    ## 
    ## $uniformity$p.value.cor
    ## [1] 1 1 1 1 1
    ## 
    ## 
    ## $homogeneity
    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    4  0.3544 0.8411
    ##       2082

``` r
testCategorical(simres_compois1, catPred = df$edu2)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-4.png)<!-- -->

    ## $uniformity
    ## $uniformity$details
    ## catPred: Lower than secondary school
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.023341, p-value = 0.967
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: Equal to secondary school
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.025303, p-value = 0.9048
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 2–4 years beyond secondary school
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.016061, p-value = 0.9843
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 5 or more years beyond secondary school
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.02967, p-value = 0.9422
    ## alternative hypothesis: two-sided
    ## 
    ## 
    ## $uniformity$p.value
    ## [1] 0.9669648 0.9048322 0.9843409 0.9422412
    ## 
    ## $uniformity$p.value.cor
    ## [1] 1 1 1 1
    ## 
    ## 
    ## $homogeneity
    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    3  0.2767 0.8423
    ##       2083

``` r
testCategorical(simres_compois1, catPred = df$chronic_dis)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-5.png)<!-- -->

    ## $uniformity
    ## $uniformity$details
    ## catPred: No
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.015641, p-value = 0.8743
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: Yes
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.022539, p-value = 0.8944
    ## alternative hypothesis: two-sided
    ## 
    ## 
    ## $uniformity$p.value
    ## [1] 0.8743025 0.8944180
    ## 
    ## $uniformity$p.value.cor
    ## [1] 1 1
    ## 
    ## 
    ## $homogeneity
    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    1  0.0412 0.8392
    ##       2085

``` r
testCategorical(simres_compois1, catPred = df$gender)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-6.png)<!-- -->

    ## $uniformity
    ## $uniformity$details
    ## catPred: Man
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.015498, p-value = 0.9682
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: Woman
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.023827, p-value = 0.575
    ## alternative hypothesis: two-sided
    ## 
    ## 
    ## $uniformity$p.value
    ## [1] 0.9682289 0.5749997
    ## 
    ## $uniformity$p.value.cor
    ## [1] 1 1
    ## 
    ## 
    ## $homogeneity
    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    1  1.0568 0.3041
    ##       2085

``` r
testCategorical(simres_compois1, catPred = df$mbd_know)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-7.png)<!-- -->

    ## $uniformity
    ## $uniformity$details
    ## catPred: -2
    ## 
    ##  Exact one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.35453, p-value = 0.5895
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: -1
    ## 
    ##  Exact one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.076443, p-value = 0.8002
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 0
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.06772, p-value = 0.3845
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 1
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.049414, p-value = 0.7336
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 2
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.024521, p-value = 0.9858
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 3
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.032918, p-value = 0.4788
    ## alternative hypothesis: two-sided
    ## 
    ## ------------------------------------------------------------ 
    ## catPred: 4
    ## 
    ##  Asymptotic one-sample Kolmogorov-Smirnov test
    ## 
    ## data:  dd[x, ]
    ## D = 0.021482, p-value = 0.9264
    ## alternative hypothesis: two-sided
    ## 
    ## 
    ## $uniformity$p.value
    ## [1] 0.5894800 0.8002086 0.3844546 0.7336071 0.9858463 0.4788164 0.9263767
    ## 
    ## $uniformity$p.value.cor
    ## [1] 1 1 1 1 1 1 1
    ## 
    ## 
    ## $homogeneity
    ## Levene's Test for Homogeneity of Variance (center = median)
    ##         Df F value Pr(>F)
    ## group    6  0.9947 0.4271
    ##       2080

``` r
plotResiduals(simres_compois1, form = df$disease_fear)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-8.png)<!-- -->

``` r
plotResiduals(simres_compois1, form = df$age)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Model%20evaluation-9.png)<!-- -->

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="5" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Count of preventive<br>behaviour
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
std. Error
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Statistic
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td colspan="7" style="font-weight:bold; text-align:left; padding-top:.8em;">
Count Model
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.33
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.13 – 1.55
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
3.54
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: Less than once a<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.92 – 1.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.40
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.689
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: 1 to 5 times per<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.24
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.06
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.14 – 1.36
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.91
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: 5 to 10 times per<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.46
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.08
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.31 – 1.62
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
7.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: More than 10 times<br>per week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.65
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.48 – 1.83
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
9.37
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Concern of contracting a<br>mosquito-borne disease
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02 – 1.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.72
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Educational attainment:<br>Equal to secondary school
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.94 – 1.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.57
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.566
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Educational attainment:<br>2–4 years beyond<br>secondary school
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.01 – 1.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.028</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Educational attainment: 5<br>or more years beyond<br>secondary school
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.98 – 1.18
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.44
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.149
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Mosquito-borne disease<br>knowledge score
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.05
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02 – 1.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
4.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Has a chronic disease:<br>Yes
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.08
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.01 – 1.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.39
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.017</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Gender: Woman
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.99
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.93 – 1.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.50
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.615
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Age(in years)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.00 – 1.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
5.60
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.34
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.15 – 2.57
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
</td>
</tr>
<tr>
<td colspan="6" style="font-weight:bold; text-align:left; padding-top:.8em;">
Zero-Inflated Model
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.74
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.62 – 0.90
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-3.09
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.002</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: Less than once a<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.15
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.09 – 0.25
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-7.43
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: 1 to 5 times per<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.07
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04 – 0.12
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-9.92
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: 5 to 10 times per<br>week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00 – 0.17
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-3.33
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.001</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
Frequency of mosquito<br>bites: More than 10 times<br>per week
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 0.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-5.20
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>\<0.001</strong>
</td>
</tr>
<tr>
<td colspan="6" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.85
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub> <sub>departement</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.01
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.01
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N <sub>departement</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
96
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="5">
2087
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
Marginal R<sup>2</sup> / Conditional R<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="5">
0.056 / 0.062
</td>
</tr>
</table>

``` r
plotcompois.ranef <- function(compois1) {
  re_compois1_data <- data.frame(
    "intercepts" = ranef(compois1)$cond$departement$`(Intercept)`,
    "sd" = sqrt(TMB::sdreport(compois1$obj, getJointPrecision=TRUE)$diag.cov.random),
    "departement" = factor(row.names(ranef(compois1)$cond$departement))
  )
  
  re_compois1_data$ucl <- re_compois1_data$intercepts + (re_compois1_data$sd * 1.96)
  re_compois1_data$lcl <- re_compois1_data$intercepts - (re_compois1_data$sd * 1.96)
  
  ggplot(re_compois1_data, aes(x = reorder(departement, intercepts), y = intercepts)) +
    geom_point() + 
    geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(
      title = "Effect of department on variety of preventive behaviour via conditional modes",
      x = "Departments",
      y = "Effect via conditional modes"
    )
}
#ggsave("figs/plotcompois.ranef.png", plot = last_plot(), dpi = "retina", width = 10, height = 6, units = "in")
```

<img src="figs/plotcompois-1.png" width="960" />

``` r
#Calculate likelihood profile CIs
#CI_zinbmm_final <- confint(zinbmm_final, trace=1, level = 0.95, method = c("profile"), estimate = TRUE, include_nonest = TRUE, full = TRUE) #similar to Wald-based CIs
#likelihood profile CIs are more accurate but much slower
#Fits natural splines separately to the points from each half of the profile for each specified parameter (i.e., values above and below the MLE), then finds the inverse functions to estimate the endpoints of the confidence interval
```
