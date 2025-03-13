Mosquito-borne disease and protective behavior in metropolitan France
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
```

    ##   gtsummary      sjPlot      DHARMa performance      lmtest         zoo 
    ##     "1.7.2"    "2.8.15"     "0.4.6"    "0.11.0"    "0.9-40"    "1.8-12" 
    ##     ordinal     glmmTMB        lme4      Matrix    labelled   lubridate 
    ## "2023.12-4"     "1.1.9"  "1.1-35.1"     "1.6-5"    "2.12.0"     "1.9.3" 
    ##     forcats     stringr       dplyr       purrr       readr       tidyr 
    ##     "1.0.0"     "1.5.1"     "1.1.4"     "1.0.2"     "2.1.5"     "1.3.1" 
    ##      tibble     ggplot2   tidyverse 
    ##     "3.2.1"     "3.5.0"     "2.0.0"

### Dataframe

``` r
# Load dataframe
df <- read.csv("elimip.csv")
```

# 1. Descriptive statistics

## 1.2.Per UDA5 region

<div id="ccwmjypswu" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#ccwmjypswu table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#ccwmjypswu thead, #ccwmjypswu tbody, #ccwmjypswu tfoot, #ccwmjypswu tr, #ccwmjypswu td, #ccwmjypswu th {
  border-style: none;
}
&#10;#ccwmjypswu p {
  margin: 0;
  padding: 0;
}
&#10;#ccwmjypswu .gt_table {
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
&#10;#ccwmjypswu .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#ccwmjypswu .gt_title {
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
&#10;#ccwmjypswu .gt_subtitle {
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
&#10;#ccwmjypswu .gt_heading {
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
&#10;#ccwmjypswu .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccwmjypswu .gt_col_headings {
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
&#10;#ccwmjypswu .gt_col_heading {
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
&#10;#ccwmjypswu .gt_column_spanner_outer {
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
&#10;#ccwmjypswu .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#ccwmjypswu .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#ccwmjypswu .gt_column_spanner {
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
&#10;#ccwmjypswu .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#ccwmjypswu .gt_group_heading {
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
&#10;#ccwmjypswu .gt_empty_group_heading {
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
&#10;#ccwmjypswu .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#ccwmjypswu .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#ccwmjypswu .gt_row {
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
&#10;#ccwmjypswu .gt_stub {
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
&#10;#ccwmjypswu .gt_stub_row_group {
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
&#10;#ccwmjypswu .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#ccwmjypswu .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#ccwmjypswu .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccwmjypswu .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#ccwmjypswu .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#ccwmjypswu .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccwmjypswu .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccwmjypswu .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#ccwmjypswu .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccwmjypswu .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#ccwmjypswu .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#ccwmjypswu .gt_footnotes {
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
&#10;#ccwmjypswu .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccwmjypswu .gt_sourcenotes {
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
&#10;#ccwmjypswu .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#ccwmjypswu .gt_left {
  text-align: left;
}
&#10;#ccwmjypswu .gt_center {
  text-align: center;
}
&#10;#ccwmjypswu .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#ccwmjypswu .gt_font_normal {
  font-weight: normal;
}
&#10;#ccwmjypswu .gt_font_bold {
  font-weight: bold;
}
&#10;#ccwmjypswu .gt_font_italic {
  font-style: italic;
}
&#10;#ccwmjypswu .gt_super {
  font-size: 65%;
}
&#10;#ccwmjypswu .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#ccwmjypswu .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#ccwmjypswu .gt_indent_1 {
  text-indent: 5px;
}
&#10;#ccwmjypswu .gt_indent_2 {
  text-indent: 10px;
}
&#10;#ccwmjypswu .gt_indent_3 {
  text-indent: 15px;
}
&#10;#ccwmjypswu .gt_indent_4 {
  text-indent: 20px;
}
&#10;#ccwmjypswu .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption><strong>Table 1. Description of the study population.</strong></caption>
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Characteristic&lt;/strong&gt;"><strong>Characteristic</strong></th>
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
    <tr><td headers="label" class="gt_row gt_left">Gender</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Man</td>
<td headers="stat_0" class="gt_row gt_center">1,012 (48%)</td>
<td headers="stat_1" class="gt_row gt_center">226 (58%)</td>
<td headers="stat_2" class="gt_row gt_center">213 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">212 (44%)</td>
<td headers="stat_4" class="gt_row gt_center">256 (49%)</td>
<td headers="stat_5" class="gt_row gt_center">105 (44%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Woman</td>
<td headers="stat_0" class="gt_row gt_center">1,075 (52%)</td>
<td headers="stat_1" class="gt_row gt_center">166 (42%)</td>
<td headers="stat_2" class="gt_row gt_center">245 (53%)</td>
<td headers="stat_3" class="gt_row gt_center">268 (56%)</td>
<td headers="stat_4" class="gt_row gt_center">265 (51%)</td>
<td headers="stat_5" class="gt_row gt_center">131 (56%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Age group (in years)</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    18 to 39</td>
<td headers="stat_0" class="gt_row gt_center">736 (35%)</td>
<td headers="stat_1" class="gt_row gt_center">206 (53%)</td>
<td headers="stat_2" class="gt_row gt_center">128 (28%)</td>
<td headers="stat_3" class="gt_row gt_center">141 (29%)</td>
<td headers="stat_4" class="gt_row gt_center">195 (37%)</td>
<td headers="stat_5" class="gt_row gt_center">66 (28%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    40 to 59</td>
<td headers="stat_0" class="gt_row gt_center">738 (35%)</td>
<td headers="stat_1" class="gt_row gt_center">125 (32%)</td>
<td headers="stat_2" class="gt_row gt_center">169 (37%)</td>
<td headers="stat_3" class="gt_row gt_center">174 (36%)</td>
<td headers="stat_4" class="gt_row gt_center">182 (35%)</td>
<td headers="stat_5" class="gt_row gt_center">88 (37%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    60 to 79</td>
<td headers="stat_0" class="gt_row gt_center">613 (29%)</td>
<td headers="stat_1" class="gt_row gt_center">61 (16%)</td>
<td headers="stat_2" class="gt_row gt_center">161 (35%)</td>
<td headers="stat_3" class="gt_row gt_center">165 (34%)</td>
<td headers="stat_4" class="gt_row gt_center">144 (28%)</td>
<td headers="stat_5" class="gt_row gt_center">82 (35%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Type of settlement</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Village (&lt;2 000 inhabitants)</td>
<td headers="stat_0" class="gt_row gt_center">482 (23%)</td>
<td headers="stat_1" class="gt_row gt_center">8 (2.0%)</td>
<td headers="stat_2" class="gt_row gt_center">158 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">138 (29%)</td>
<td headers="stat_4" class="gt_row gt_center">98 (19%)</td>
<td headers="stat_5" class="gt_row gt_center">80 (34%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Small town (between 2.000 and 20.000 inhabitants)</td>
<td headers="stat_0" class="gt_row gt_center">631 (30%)</td>
<td headers="stat_1" class="gt_row gt_center">71 (18%)</td>
<td headers="stat_2" class="gt_row gt_center">149 (33%)</td>
<td headers="stat_3" class="gt_row gt_center">180 (38%)</td>
<td headers="stat_4" class="gt_row gt_center">166 (32%)</td>
<td headers="stat_5" class="gt_row gt_center">65 (28%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Medium-sized city (between 20 000 and 100 000 inhabitants)</td>
<td headers="stat_0" class="gt_row gt_center">557 (27%)</td>
<td headers="stat_1" class="gt_row gt_center">166 (42%)</td>
<td headers="stat_2" class="gt_row gt_center">91 (20%)</td>
<td headers="stat_3" class="gt_row gt_center">102 (21%)</td>
<td headers="stat_4" class="gt_row gt_center">145 (28%)</td>
<td headers="stat_5" class="gt_row gt_center">53 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Large city (&gt;100 000 inhabitants)</td>
<td headers="stat_0" class="gt_row gt_center">417 (20%)</td>
<td headers="stat_1" class="gt_row gt_center">147 (38%)</td>
<td headers="stat_2" class="gt_row gt_center">60 (13%)</td>
<td headers="stat_3" class="gt_row gt_center">60 (13%)</td>
<td headers="stat_4" class="gt_row gt_center">112 (21%)</td>
<td headers="stat_5" class="gt_row gt_center">38 (16%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Educational attainment</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Lower than secondary school</td>
<td headers="stat_0" class="gt_row gt_center">450 (22%)</td>
<td headers="stat_1" class="gt_row gt_center">56 (14%)</td>
<td headers="stat_2" class="gt_row gt_center">116 (25%)</td>
<td headers="stat_3" class="gt_row gt_center">126 (26%)</td>
<td headers="stat_4" class="gt_row gt_center">100 (19%)</td>
<td headers="stat_5" class="gt_row gt_center">52 (22%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Equal to secondary school</td>
<td headers="stat_0" class="gt_row gt_center">502 (24%)</td>
<td headers="stat_1" class="gt_row gt_center">72 (18%)</td>
<td headers="stat_2" class="gt_row gt_center">121 (26%)</td>
<td headers="stat_3" class="gt_row gt_center">114 (24%)</td>
<td headers="stat_4" class="gt_row gt_center">136 (26%)</td>
<td headers="stat_5" class="gt_row gt_center">59 (25%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    2–4 years beyond secondary school</td>
<td headers="stat_0" class="gt_row gt_center">817 (39%)</td>
<td headers="stat_1" class="gt_row gt_center">156 (40%)</td>
<td headers="stat_2" class="gt_row gt_center">169 (37%)</td>
<td headers="stat_3" class="gt_row gt_center">183 (38%)</td>
<td headers="stat_4" class="gt_row gt_center">210 (40%)</td>
<td headers="stat_5" class="gt_row gt_center">99 (42%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    5 or more years beyond secondary school</td>
<td headers="stat_0" class="gt_row gt_center">318 (15%)</td>
<td headers="stat_1" class="gt_row gt_center">108 (28%)</td>
<td headers="stat_2" class="gt_row gt_center">52 (11%)</td>
<td headers="stat_3" class="gt_row gt_center">57 (12%)</td>
<td headers="stat_4" class="gt_row gt_center">75 (14%)</td>
<td headers="stat_5" class="gt_row gt_center">26 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Financial difficulty in the past 12 months</td>
<td headers="stat_0" class="gt_row gt_center">572 (27%)</td>
<td headers="stat_1" class="gt_row gt_center">134 (34%)</td>
<td headers="stat_2" class="gt_row gt_center">110 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">118 (25%)</td>
<td headers="stat_4" class="gt_row gt_center">148 (28%)</td>
<td headers="stat_5" class="gt_row gt_center">62 (26%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Has a chronic disease</td>
<td headers="stat_0" class="gt_row gt_center">653 (31%)</td>
<td headers="stat_1" class="gt_row gt_center">107 (27%)</td>
<td headers="stat_2" class="gt_row gt_center">154 (34%)</td>
<td headers="stat_3" class="gt_row gt_center">170 (35%)</td>
<td headers="stat_4" class="gt_row gt_center">147 (28%)</td>
<td headers="stat_5" class="gt_row gt_center">75 (32%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Frequency of mosquito bites</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Never or almost never</td>
<td headers="stat_0" class="gt_row gt_center">575 (28%)</td>
<td headers="stat_1" class="gt_row gt_center">73 (19%)</td>
<td headers="stat_2" class="gt_row gt_center">185 (40%)</td>
<td headers="stat_3" class="gt_row gt_center">179 (37%)</td>
<td headers="stat_4" class="gt_row gt_center">103 (20%)</td>
<td headers="stat_5" class="gt_row gt_center">35 (15%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Sometimes</td>
<td headers="stat_0" class="gt_row gt_center">896 (43%)</td>
<td headers="stat_1" class="gt_row gt_center">194 (49%)</td>
<td headers="stat_2" class="gt_row gt_center">187 (41%)</td>
<td headers="stat_3" class="gt_row gt_center">214 (45%)</td>
<td headers="stat_4" class="gt_row gt_center">220 (42%)</td>
<td headers="stat_5" class="gt_row gt_center">81 (34%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Often</td>
<td headers="stat_0" class="gt_row gt_center">616 (30%)</td>
<td headers="stat_1" class="gt_row gt_center">125 (32%)</td>
<td headers="stat_2" class="gt_row gt_center">86 (19%)</td>
<td headers="stat_3" class="gt_row gt_center">87 (18%)</td>
<td headers="stat_4" class="gt_row gt_center">198 (38%)</td>
<td headers="stat_5" class="gt_row gt_center">120 (51%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Frequency of preventive behaviour</td>
<td headers="stat_0" class="gt_row gt_center"><br /></td>
<td headers="stat_1" class="gt_row gt_center"><br /></td>
<td headers="stat_2" class="gt_row gt_center"><br /></td>
<td headers="stat_3" class="gt_row gt_center"><br /></td>
<td headers="stat_4" class="gt_row gt_center"><br /></td>
<td headers="stat_5" class="gt_row gt_center"><br /></td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Never</td>
<td headers="stat_0" class="gt_row gt_center">414 (20%)</td>
<td headers="stat_1" class="gt_row gt_center">62 (16%)</td>
<td headers="stat_2" class="gt_row gt_center">111 (24%)</td>
<td headers="stat_3" class="gt_row gt_center">129 (27%)</td>
<td headers="stat_4" class="gt_row gt_center">85 (16%)</td>
<td headers="stat_5" class="gt_row gt_center">27 (11%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    A few times</td>
<td headers="stat_0" class="gt_row gt_center">808 (39%)</td>
<td headers="stat_1" class="gt_row gt_center">146 (37%)</td>
<td headers="stat_2" class="gt_row gt_center">215 (47%)</td>
<td headers="stat_3" class="gt_row gt_center">227 (47%)</td>
<td headers="stat_4" class="gt_row gt_center">165 (32%)</td>
<td headers="stat_5" class="gt_row gt_center">55 (23%)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">    Several times a week</td>
<td headers="stat_0" class="gt_row gt_center">865 (41%)</td>
<td headers="stat_1" class="gt_row gt_center">184 (47%)</td>
<td headers="stat_2" class="gt_row gt_center">132 (29%)</td>
<td headers="stat_3" class="gt_row gt_center">124 (26%)</td>
<td headers="stat_4" class="gt_row gt_center">271 (52%)</td>
<td headers="stat_5" class="gt_row gt_center">154 (65%)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> n (%)</td>
    </tr>
  </tfoot>
</table>
</div>
<div id="wimsdtnmbz" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#wimsdtnmbz table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#wimsdtnmbz thead, #wimsdtnmbz tbody, #wimsdtnmbz tfoot, #wimsdtnmbz tr, #wimsdtnmbz td, #wimsdtnmbz th {
  border-style: none;
}
&#10;#wimsdtnmbz p {
  margin: 0;
  padding: 0;
}
&#10;#wimsdtnmbz .gt_table {
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
&#10;#wimsdtnmbz .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#wimsdtnmbz .gt_title {
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
&#10;#wimsdtnmbz .gt_subtitle {
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
&#10;#wimsdtnmbz .gt_heading {
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
&#10;#wimsdtnmbz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wimsdtnmbz .gt_col_headings {
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
&#10;#wimsdtnmbz .gt_col_heading {
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
&#10;#wimsdtnmbz .gt_column_spanner_outer {
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
&#10;#wimsdtnmbz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#wimsdtnmbz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#wimsdtnmbz .gt_column_spanner {
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
&#10;#wimsdtnmbz .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#wimsdtnmbz .gt_group_heading {
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
&#10;#wimsdtnmbz .gt_empty_group_heading {
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
&#10;#wimsdtnmbz .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#wimsdtnmbz .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#wimsdtnmbz .gt_row {
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
&#10;#wimsdtnmbz .gt_stub {
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
&#10;#wimsdtnmbz .gt_stub_row_group {
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
&#10;#wimsdtnmbz .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#wimsdtnmbz .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#wimsdtnmbz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wimsdtnmbz .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#wimsdtnmbz .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#wimsdtnmbz .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wimsdtnmbz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wimsdtnmbz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#wimsdtnmbz .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#wimsdtnmbz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#wimsdtnmbz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#wimsdtnmbz .gt_footnotes {
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
&#10;#wimsdtnmbz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wimsdtnmbz .gt_sourcenotes {
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
&#10;#wimsdtnmbz .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#wimsdtnmbz .gt_left {
  text-align: left;
}
&#10;#wimsdtnmbz .gt_center {
  text-align: center;
}
&#10;#wimsdtnmbz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#wimsdtnmbz .gt_font_normal {
  font-weight: normal;
}
&#10;#wimsdtnmbz .gt_font_bold {
  font-weight: bold;
}
&#10;#wimsdtnmbz .gt_font_italic {
  font-style: italic;
}
&#10;#wimsdtnmbz .gt_super {
  font-size: 65%;
}
&#10;#wimsdtnmbz .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#wimsdtnmbz .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#wimsdtnmbz .gt_indent_1 {
  text-indent: 5px;
}
&#10;#wimsdtnmbz .gt_indent_2 {
  text-indent: 10px;
}
&#10;#wimsdtnmbz .gt_indent_3 {
  text-indent: 15px;
}
&#10;#wimsdtnmbz .gt_indent_4 {
  text-indent: 20px;
}
&#10;#wimsdtnmbz .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table" data-quarto-disable-processing="false" data-quarto-bootstrap="false">
  <caption><strong>Table 2. Knowledge, attitudes, and practices of mosquito-borne diseases and prevention of the study population per UDA5 region</strong></caption>
  <thead>
    <tr class="gt_col_headings gt_spanner_row">
      <th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="2" colspan="1" scope="col" id="&lt;strong&gt;Indicator&lt;/strong&gt;"><strong>Indicator</strong></th>
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
    <tr><td headers="label" class="gt_row gt_left">Count of preventive behaviour</td>
<td headers="stat_0" class="gt_row gt_center">2 (1, 4)</td>
<td headers="stat_1" class="gt_row gt_center">2 (1, 3)</td>
<td headers="stat_2" class="gt_row gt_center">2 (1, 3)</td>
<td headers="stat_3" class="gt_row gt_center">2 (0, 3)</td>
<td headers="stat_4" class="gt_row gt_center">3 (1, 4)</td>
<td headers="stat_5" class="gt_row gt_center">3 (2, 5)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Perceived likelihood of contracting a mosquito-borne disease</td>
<td headers="stat_0" class="gt_row gt_center">5 (3, 6)</td>
<td headers="stat_1" class="gt_row gt_center">5 (3, 6)</td>
<td headers="stat_2" class="gt_row gt_center">5 (3, 6)</td>
<td headers="stat_3" class="gt_row gt_center">5 (3, 6)</td>
<td headers="stat_4" class="gt_row gt_center">5 (3, 6)</td>
<td headers="stat_5" class="gt_row gt_center">5 (4, 6)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Concern of contracting a mosquito-borne disease</td>
<td headers="stat_0" class="gt_row gt_center">6 (4, 8)</td>
<td headers="stat_1" class="gt_row gt_center">6 (4, 8)</td>
<td headers="stat_2" class="gt_row gt_center">6 (4, 8)</td>
<td headers="stat_3" class="gt_row gt_center">6 (4, 7)</td>
<td headers="stat_4" class="gt_row gt_center">6 (4, 8)</td>
<td headers="stat_5" class="gt_row gt_center">6 (4, 8)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Confidence in national authorities in the management of health crises</td>
<td headers="stat_0" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_1" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5, 8)</td>
<td headers="stat_3" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_4" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_5" class="gt_row gt_center">6 (5, 8)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Confidence in regional authorities in the management of health crises</td>
<td headers="stat_0" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_1" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_2" class="gt_row gt_center">7 (5, 8)</td>
<td headers="stat_3" class="gt_row gt_center">7 (5, 8)</td>
<td headers="stat_4" class="gt_row gt_center">6 (5, 8)</td>
<td headers="stat_5" class="gt_row gt_center">6 (5, 8)</td></tr>
    <tr><td headers="label" class="gt_row gt_left">Mosquito-borne disease knowledge score</td>
<td headers="stat_0" class="gt_row gt_center">2.56 (1.41)</td>
<td headers="stat_1" class="gt_row gt_center">2.38 (1.61)</td>
<td headers="stat_2" class="gt_row gt_center">2.58 (1.36)</td>
<td headers="stat_3" class="gt_row gt_center">2.47 (1.35)</td>
<td headers="stat_4" class="gt_row gt_center">2.66 (1.44)</td>
<td headers="stat_5" class="gt_row gt_center">2.82 (1.15)</td></tr>
  </tbody>
  &#10;  <tfoot class="gt_footnotes">
    <tr>
      <td class="gt_footnote" colspan="7"><span class="gt_footnote_marks" style="white-space:nowrap;font-style:italic;font-weight:normal;"><sup>1</sup></span> Median (IQR); Mean (SD)</td>
    </tr>
  </tfoot>
</table>
</div>
<div id="jasrtnturg" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#jasrtnturg table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#jasrtnturg thead, #jasrtnturg tbody, #jasrtnturg tfoot, #jasrtnturg tr, #jasrtnturg td, #jasrtnturg th {
  border-style: none;
}
&#10;#jasrtnturg p {
  margin: 0;
  padding: 0;
}
&#10;#jasrtnturg .gt_table {
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
&#10;#jasrtnturg .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#jasrtnturg .gt_title {
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
&#10;#jasrtnturg .gt_subtitle {
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
&#10;#jasrtnturg .gt_heading {
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
&#10;#jasrtnturg .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jasrtnturg .gt_col_headings {
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
&#10;#jasrtnturg .gt_col_heading {
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
&#10;#jasrtnturg .gt_column_spanner_outer {
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
&#10;#jasrtnturg .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#jasrtnturg .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#jasrtnturg .gt_column_spanner {
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
&#10;#jasrtnturg .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#jasrtnturg .gt_group_heading {
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
&#10;#jasrtnturg .gt_empty_group_heading {
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
&#10;#jasrtnturg .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#jasrtnturg .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#jasrtnturg .gt_row {
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
&#10;#jasrtnturg .gt_stub {
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
&#10;#jasrtnturg .gt_stub_row_group {
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
&#10;#jasrtnturg .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#jasrtnturg .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#jasrtnturg .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jasrtnturg .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#jasrtnturg .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#jasrtnturg .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jasrtnturg .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jasrtnturg .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#jasrtnturg .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#jasrtnturg .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#jasrtnturg .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#jasrtnturg .gt_footnotes {
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
&#10;#jasrtnturg .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jasrtnturg .gt_sourcenotes {
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
&#10;#jasrtnturg .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#jasrtnturg .gt_left {
  text-align: left;
}
&#10;#jasrtnturg .gt_center {
  text-align: center;
}
&#10;#jasrtnturg .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#jasrtnturg .gt_font_normal {
  font-weight: normal;
}
&#10;#jasrtnturg .gt_font_bold {
  font-weight: bold;
}
&#10;#jasrtnturg .gt_font_italic {
  font-style: italic;
}
&#10;#jasrtnturg .gt_super {
  font-size: 65%;
}
&#10;#jasrtnturg .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#jasrtnturg .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#jasrtnturg .gt_indent_1 {
  text-indent: 5px;
}
&#10;#jasrtnturg .gt_indent_2 {
  text-indent: 10px;
}
&#10;#jasrtnturg .gt_indent_3 {
  text-indent: 15px;
}
&#10;#jasrtnturg .gt_indent_4 {
  text-indent: 20px;
}
&#10;#jasrtnturg .gt_indent_5 {
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

``` r
sjPlot:: tab_model(cmm1, show.se= T, show.stat= T)
```

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

``` r
plotcmm.ranef <- function(cmm1) {
  rn <- names(cmm1[["stDev"]]) 
  nc <- length(cmm1[["ranef"]]) 
  ci <- cmm1$ranef + qnorm(0.975) * sqrt(cmm1$condVar) %o% c(-1, 1)
  ord.re <- order(cmm1$ranef)
  ci <- ci[order(cmm1$ranef), ]
  ggplot() +
    geom_point(aes(x = 1:nc, y = cmm1$ranef[ord.re])) + 
    geom_errorbar(aes(x = 1:nc, ymin = ci[,1], ymax = ci[,2]), width = 0.2) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) + 
    labs(title = "Effect of department place of residence via conditional modes on frequency of preventive behaviour",
         x = paste0(rn, "Departments"),
         y = paste0(rn, "Effect via conditional modes"))
}
plotcmm.ranef(cmm1)
```

![](elimip2025_files/figure-gfm/Frequency%20of%20protective%20behaviour:%20Random%20effects%20plot-1.png)<!-- -->

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

``` r
sjPlot:: tab_model(compois1, show.se= T, show.stat= T)
```

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
      title = "Effect of department place of residence via conditional modes on variety of preventive behaviour",
      x = "Departments",
      y = "Effect via conditional modes"
    )
}
plotcompois.ranef(compois1)
```

![](elimip2025_files/figure-gfm/Count%20of%20protective%20behaviours:%20Random%20effects%20plot-1.png)<!-- -->

``` r
#Calculate likelihood profile CIs
#CI_zinbmm_final <- confint(zinbmm_final, trace=1, level = 0.95, method = c("profile"), estimate = TRUE, include_nonest = TRUE, full = TRUE) #similar to Wald-based CIs
#likelihood profile CIs are more accurate but much slower
#Fits natural splines separately to the points from each half of the profile for each specified parameter (i.e., values above and below the MLE), then finds the inverse functions to estimate the endpoints of the confidence interval
```
