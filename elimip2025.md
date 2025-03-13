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

<div id="gkzjjujpmh" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gkzjjujpmh table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#gkzjjujpmh thead, #gkzjjujpmh tbody, #gkzjjujpmh tfoot, #gkzjjujpmh tr, #gkzjjujpmh td, #gkzjjujpmh th {
  border-style: none;
}
&#10;#gkzjjujpmh p {
  margin: 0;
  padding: 0;
}
&#10;#gkzjjujpmh .gt_table {
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
&#10;#gkzjjujpmh .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#gkzjjujpmh .gt_title {
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
&#10;#gkzjjujpmh .gt_subtitle {
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
&#10;#gkzjjujpmh .gt_heading {
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
&#10;#gkzjjujpmh .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gkzjjujpmh .gt_col_headings {
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
&#10;#gkzjjujpmh .gt_col_heading {
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
&#10;#gkzjjujpmh .gt_column_spanner_outer {
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
&#10;#gkzjjujpmh .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#gkzjjujpmh .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#gkzjjujpmh .gt_column_spanner {
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
&#10;#gkzjjujpmh .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#gkzjjujpmh .gt_group_heading {
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
&#10;#gkzjjujpmh .gt_empty_group_heading {
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
&#10;#gkzjjujpmh .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#gkzjjujpmh .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#gkzjjujpmh .gt_row {
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
&#10;#gkzjjujpmh .gt_stub {
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
&#10;#gkzjjujpmh .gt_stub_row_group {
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
&#10;#gkzjjujpmh .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#gkzjjujpmh .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#gkzjjujpmh .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gkzjjujpmh .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#gkzjjujpmh .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#gkzjjujpmh .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gkzjjujpmh .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gkzjjujpmh .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#gkzjjujpmh .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#gkzjjujpmh .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#gkzjjujpmh .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gkzjjujpmh .gt_footnotes {
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
&#10;#gkzjjujpmh .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gkzjjujpmh .gt_sourcenotes {
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
&#10;#gkzjjujpmh .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gkzjjujpmh .gt_left {
  text-align: left;
}
&#10;#gkzjjujpmh .gt_center {
  text-align: center;
}
&#10;#gkzjjujpmh .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#gkzjjujpmh .gt_font_normal {
  font-weight: normal;
}
&#10;#gkzjjujpmh .gt_font_bold {
  font-weight: bold;
}
&#10;#gkzjjujpmh .gt_font_italic {
  font-style: italic;
}
&#10;#gkzjjujpmh .gt_super {
  font-size: 65%;
}
&#10;#gkzjjujpmh .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#gkzjjujpmh .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#gkzjjujpmh .gt_indent_1 {
  text-indent: 5px;
}
&#10;#gkzjjujpmh .gt_indent_2 {
  text-indent: 10px;
}
&#10;#gkzjjujpmh .gt_indent_3 {
  text-indent: 15px;
}
&#10;#gkzjjujpmh .gt_indent_4 {
  text-indent: 20px;
}
&#10;#gkzjjujpmh .gt_indent_5 {
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
<div id="gfyzfimwlv" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>#gfyzfimwlv table {
  font-family: system-ui, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif, 'Apple Color Emoji', 'Segoe UI Emoji', 'Segoe UI Symbol', 'Noto Color Emoji';
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
}
&#10;#gfyzfimwlv thead, #gfyzfimwlv tbody, #gfyzfimwlv tfoot, #gfyzfimwlv tr, #gfyzfimwlv td, #gfyzfimwlv th {
  border-style: none;
}
&#10;#gfyzfimwlv p {
  margin: 0;
  padding: 0;
}
&#10;#gfyzfimwlv .gt_table {
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
&#10;#gfyzfimwlv .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}
&#10;#gfyzfimwlv .gt_title {
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
&#10;#gfyzfimwlv .gt_subtitle {
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
&#10;#gfyzfimwlv .gt_heading {
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
&#10;#gfyzfimwlv .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gfyzfimwlv .gt_col_headings {
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
&#10;#gfyzfimwlv .gt_col_heading {
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
&#10;#gfyzfimwlv .gt_column_spanner_outer {
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
&#10;#gfyzfimwlv .gt_column_spanner_outer:first-child {
  padding-left: 0;
}
&#10;#gfyzfimwlv .gt_column_spanner_outer:last-child {
  padding-right: 0;
}
&#10;#gfyzfimwlv .gt_column_spanner {
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
&#10;#gfyzfimwlv .gt_spanner_row {
  border-bottom-style: hidden;
}
&#10;#gfyzfimwlv .gt_group_heading {
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
&#10;#gfyzfimwlv .gt_empty_group_heading {
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
&#10;#gfyzfimwlv .gt_from_md > :first-child {
  margin-top: 0;
}
&#10;#gfyzfimwlv .gt_from_md > :last-child {
  margin-bottom: 0;
}
&#10;#gfyzfimwlv .gt_row {
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
&#10;#gfyzfimwlv .gt_stub {
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
&#10;#gfyzfimwlv .gt_stub_row_group {
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
&#10;#gfyzfimwlv .gt_row_group_first td {
  border-top-width: 2px;
}
&#10;#gfyzfimwlv .gt_row_group_first th {
  border-top-width: 2px;
}
&#10;#gfyzfimwlv .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gfyzfimwlv .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}
&#10;#gfyzfimwlv .gt_first_summary_row.thick {
  border-top-width: 2px;
}
&#10;#gfyzfimwlv .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gfyzfimwlv .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gfyzfimwlv .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}
&#10;#gfyzfimwlv .gt_last_grand_summary_row_top {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: double;
  border-bottom-width: 6px;
  border-bottom-color: #D3D3D3;
}
&#10;#gfyzfimwlv .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}
&#10;#gfyzfimwlv .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}
&#10;#gfyzfimwlv .gt_footnotes {
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
&#10;#gfyzfimwlv .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gfyzfimwlv .gt_sourcenotes {
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
&#10;#gfyzfimwlv .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}
&#10;#gfyzfimwlv .gt_left {
  text-align: left;
}
&#10;#gfyzfimwlv .gt_center {
  text-align: center;
}
&#10;#gfyzfimwlv .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}
&#10;#gfyzfimwlv .gt_font_normal {
  font-weight: normal;
}
&#10;#gfyzfimwlv .gt_font_bold {
  font-weight: bold;
}
&#10;#gfyzfimwlv .gt_font_italic {
  font-style: italic;
}
&#10;#gfyzfimwlv .gt_super {
  font-size: 65%;
}
&#10;#gfyzfimwlv .gt_footnote_marks {
  font-size: 75%;
  vertical-align: 0.4em;
  position: initial;
}
&#10;#gfyzfimwlv .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}
&#10;#gfyzfimwlv .gt_indent_1 {
  text-indent: 5px;
}
&#10;#gfyzfimwlv .gt_indent_2 {
  text-indent: 10px;
}
&#10;#gfyzfimwlv .gt_indent_3 {
  text-indent: 15px;
}
&#10;#gfyzfimwlv .gt_indent_4 {
  text-indent: 20px;
}
&#10;#gfyzfimwlv .gt_indent_5 {
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
