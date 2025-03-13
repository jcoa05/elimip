Mosquito-borne diseases and protective behavior in France, 2023
================
Juan Carlos Ocampo
2025-03-13

# Introduction

``` r
#In November 2023, we conducted a cross-sectional knowledge, attitude, and protective behavior survey among a general sample (n=2,087) of adults aged 18 to 79 in metropolitan France.
```

### Packages

``` r
#Data wrangling
library(tidyverse)
library(dplyr) 
library(labelled)
#Modelling
library(lme4)
library(glmmTMB)
library(ordinal)
#Performance
library(lmtest)
library(performance)
library(DHARMa)
#Visuals
library(ggplot2)
library(sjPlot)
library(gtsummary)
library(flextable)
```

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

# 1. Descriptive statistics

## 1.2.Per UDA5 region

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

**Table 1. Description of the study population.**

| **Indicator**                                                         | **Overall**, N = 2,087 | **Paris region**, N = 392 | **North East**, N = 458 | **North West**, N = 480 | **South East**, N = 521 | **South West**, N = 236 |
|:----------------------------------------------------------------------|:----------------------:|:-------------------------:|:-----------------------:|:-----------------------:|:-----------------------:|:-----------------------:|
| Count of preventive behaviour                                         |        2 (1, 4)        |         2 (1, 3)          |        2 (1, 3)         |        2 (0, 3)         |        3 (1, 4)         |        3 (2, 5)         |
| Perceived likelihood of contracting a mosquito-borne disease          |        5 (3, 6)        |         5 (3, 6)          |        5 (3, 6)         |        5 (3, 6)         |        5 (3, 6)         |        5 (4, 6)         |
| Concern of contracting a mosquito-borne disease                       |        6 (4, 8)        |         6 (4, 8)          |        6 (4, 8)         |        6 (4, 7)         |        6 (4, 8)         |        6 (4, 8)         |
| Confidence in national authorities in the management of health crises |        6 (5, 8)        |         6 (5, 8)          |        7 (5, 8)         |        6 (5, 8)         |        6 (5, 8)         |        6 (5, 8)         |
| Confidence in regional authorities in the management of health crises |        6 (5, 8)        |         6 (5, 8)          |        7 (5, 8)         |        7 (5, 8)         |        6 (5, 8)         |        6 (5, 8)         |
| Mosquito-borne disease knowledge score                                |      2.56 (1.41)       |        2.38 (1.61)        |       2.58 (1.36)       |       2.47 (1.35)       |       2.66 (1.44)       |       2.82 (1.15)       |

**Table 2. Knowledge, attitudes, and practices of mosquito-borne
diseases and prevention of the study population per UDA5 region**

<div id="xgzkcncjtp" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#xgzkcncjtp table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#xgzkcncjtp thead, #xgzkcncjtp tbody, #xgzkcncjtp tfoot, #xgzkcncjtp tr, #xgzkcncjtp td, #xgzkcncjtp th {
  border-style: none;
}
&#10;#xgzkcncjtp p {
  margin: 0;
  padding: 0;
}
&#10;#xgzkcncjtp .gt_table {
  display: table;
  border-collapse: collapse;
  line-height: normal;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#xgzkcncjtp .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}
&#10;#xgzkcncjtp .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 3px;
  padding-bottom: 5px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}
&#10;#xgzkcncjtp .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}
&#10;#xgzkcncjtp .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}
&#10;#xgzkcncjtp .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#xgzkcncjtp .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#xgzkcncjtp .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}
&#10;#xgzkcncjtp .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#xgzkcncjtp .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}
&#10;#xgzkcncjtp .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}
&#10;#xgzkcncjtp .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#xgzkcncjtp .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#xgzkcncjtp .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}
&#10;#xgzkcncjtp .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xgzkcncjtp .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}
&#10;#xgzkcncjtp .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#xgzkcncjtp .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#xgzkcncjtp .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xgzkcncjtp .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#xgzkcncjtp .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xgzkcncjtp .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#xgzkcncjtp .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xgzkcncjtp .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}
&#10;#xgzkcncjtp .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#xgzkcncjtp .gt_left {
  text-align: left;
}
&#10;#xgzkcncjtp .gt_center {
  text-align: center;
}
&#10;#xgzkcncjtp .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#xgzkcncjtp .gt_font_normal {
  font-weight: normal;
}
&#10;#xgzkcncjtp .gt_font_bold {
  font-weight: bold;
}
&#10;#xgzkcncjtp .gt_font_italic {
  font-style: italic;
}
&#10;#xgzkcncjtp .gt_super {
  font-size: 65%;
}
&#10;#xgzkcncjtp .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#xgzkcncjtp .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#xgzkcncjtp .gt_indent_1 {
  text-indent: 5px;
}
&#10;#xgzkcncjtp .gt_indent_2 {
  text-indent: 10px;
}
&#10;#xgzkcncjtp .gt_indent_3 {
  text-indent: 15px;
}
&#10;#xgzkcncjtp .gt_indent_4 {
  text-indent: 20px;
}
&#10;#xgzkcncjtp .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption><strong>Table 3. Mosquito-borne disease protective behaviours per region per UDA5 region</strong></caption>
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Protective behaviour&lt;/strong&gt;"><strong>Protective behaviour</strong></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Overall&lt;/strong&gt;, N = 2,087&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Overall</strong>, N = 2,087<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_center gt_columns_top_border gt_column_spanner_outer" rowspan="1" colspan="5" scope="colgroup" id="&lt;strong&gt;Region&lt;/strong&gt;">
        <span class="gt_column_spanner"><strong>Region</strong></span>
      </th>
    </tr>
    <tr class="gt_col_headings">
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;Paris region&lt;/strong&gt;, N = 392&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>Paris region</strong>, N = 392<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;North East&lt;/strong&gt;, N = 458&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>North East</strong>, N = 458<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;North West&lt;/strong&gt;, N = 480&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>North West</strong>, N = 480<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;South East&lt;/strong&gt;, N = 521&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>South East</strong>, N = 521<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="&lt;strong&gt;South West&lt;/strong&gt;, N = 236&lt;span class=&quot;gt_footnote_marks&quot; style=&quot;white-space:nowrap;font-style:italic;font-weight:normal;&quot;&gt;&lt;sup&gt;1&lt;/sup&gt;&lt;/span&gt;"><strong>South West</strong>, N = 236<span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span></th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="label" class="gt_row gt_left">Maintaining gutters to ensure drainage</td>
<td headers="stat_0" class="gt_row gt_center">327 (16%)</td>
<td headers="stat_1" class="gt_row gt_center">42 (11%)</td>
<td headers="stat_2" class="gt_row gt_center">74 (16%)</td>
<td headers="stat_3" class="gt_row gt_center">69 (14%)</td>
<td headers="stat_4" class="gt_row gt_center">86 (17%)</td>
<td headers="stat_5" class="gt_row gt_center">56 (24%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Using mosquito traps</td>
<td headers="stat_0" class="gt_row gt_center">390 (19%)</td>
<td headers="stat_1" class="gt_row gt_center">90 (23%)</td>
<td headers="stat_2" class="gt_row gt_center">77 (17%)</td>
<td headers="stat_3" class="gt_row gt_center">73 (15%)</td>
<td headers="stat_4" class="gt_row gt_center">99 (19%)</td>
<td headers="stat_5" class="gt_row gt_center">51 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Wearing long, loose, light-coloured clothing</td>
<td headers="stat_0" class="gt_row gt_center">452 (22%)</td>
<td headers="stat_1" class="gt_row gt_center">105 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">88 (19%)</td>
<td headers="stat_3" class="gt_row gt_center">82 (17%)</td>
<td headers="stat_4" class="gt_row gt_center">107 (21%)</td>
<td headers="stat_5" class="gt_row gt_center">70 (30%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Using insecticide or fumigating</td>
<td headers="stat_0" class="gt_row gt_center">504 (24%)</td>
<td headers="stat_1" class="gt_row gt_center">100 (26%)</td>
<td headers="stat_2" class="gt_row gt_center">108 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">84 (18%)</td>
<td headers="stat_4" class="gt_row gt_center">138 (26%)</td>
<td headers="stat_5" class="gt_row gt_center">74 (31%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Using mosquito nets in windows</td>
<td headers="stat_0" class="gt_row gt_center">529 (25%)</td>
<td headers="stat_1" class="gt_row gt_center">88 (22%)</td>
<td headers="stat_2" class="gt_row gt_center">115 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">68 (14%)</td>
<td headers="stat_4" class="gt_row gt_center">179 (34%)</td>
<td headers="stat_5" class="gt_row gt_center">79 (33%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Avoiding stagnant water at the base of flowerpots and vases</td>
<td headers="stat_0" class="gt_row gt_center">729 (35%)</td>
<td headers="stat_1" class="gt_row gt_center">106 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">145 (32%)</td>
<td headers="stat_3" class="gt_row gt_center">135 (28%)</td>
<td headers="stat_4" class="gt_row gt_center">222 (43%)</td>
<td headers="stat_5" class="gt_row gt_center">121 (51%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Using mosquito candles and coils</td>
<td headers="stat_0" class="gt_row gt_center">734 (35%)</td>
<td headers="stat_1" class="gt_row gt_center">127 (32%)</td>
<td headers="stat_2" class="gt_row gt_center">129 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">124 (26%)</td>
<td headers="stat_4" class="gt_row gt_center">229 (44%)</td>
<td headers="stat_5" class="gt_row gt_center">125 (53%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Using skin repellents</td>
<td headers="stat_0" class="gt_row gt_center">756 (36%)</td>
<td headers="stat_1" class="gt_row gt_center">142 (36%)</td>
<td headers="stat_2" class="gt_row gt_center">134 (29%)</td>
<td headers="stat_3" class="gt_row gt_center">147 (31%)</td>
<td headers="stat_4" class="gt_row gt_center">222 (43%)</td>
<td headers="stat_5" class="gt_row gt_center">111 (47%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Storing containers out of the rain to avoid stagnant water</td>
<td headers="stat_0" class="gt_row gt_center">777 (37%)</td>
<td headers="stat_1" class="gt_row gt_center">106 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">159 (35%)</td>
<td headers="stat_3" class="gt_row gt_center">161 (34%)</td>
<td headers="stat_4" class="gt_row gt_center">224 (43%)</td>
<td headers="stat_5" class="gt_row gt_center">127 (54%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%)</td>
    </tr>
  </tfoot>
</table>
</div>
