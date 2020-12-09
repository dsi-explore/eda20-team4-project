3\_2\_Industry
================

# Location of job bucket

I want to take the values from the job\_category variable and graph it
against the location data to see if which jobs are in each location.

<details>

<summary>Click to expand</summary> \# Load Data

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.4     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## Warning: package 'tibble' was built under R version 4.0.3

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
library(gt)
```

    ## Warning: package 'gt' was built under R version 4.0.3

``` r
library(ggplot2)
library(viridis)
library(ggalluvial)
library(ggfittext)
```

    ## Warning: package 'ggfittext' was built under R version 4.0.3

Read data science jobs csv

``` r
ds_jobs <- read.csv("../Data Cleaning/ds_jobs.csv")
```

# Job Bucket vs location

Filter by only DS jobs. There are a few other scientist jobs right now.
Also take out the NAs

``` r
ds_filter <- ds_jobs %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | job_category == "Data Scientist" | job_category == "Machine Learning Engineer" | job_category == "Statistician" | job_category == "Other Analyst")
```

</details>

``` r
ggplot(ds_filter, aes(metro_location, fill = job_category)) +
  geom_bar()+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Data Science Jobs by Location",
    x = "Locations",
    subtitle = "This graph shows the number of data science positions available in each city.",
    fill = "Job Category"
  )+
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8),
    axis.text = element_text(size =10),
    plot.subtitle=element_text(size=8, color = "grey"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)) 
```

![](3_2_Industry_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Interpretation

San Fran and DC have the most job postings in this data set for data
scientists. Within this, Data Analyst, Data Engineer, and Data Scientist
are the job categories that dominate. Thus, if a data science student is
looking for a job, then based on this data, the student should look for
jobs in the bay area and DC.

# Industry vs Location

I also want to know which industries are offered in each state. I only
want the top 5 industries so that my graph is easier to read.

<details>

<summary>Click to expand</summary>

``` r
ds_filter2 <- ds_filter %>%
  filter(!is.na(industry))
ds_filter3 <- ds_filter2 %>%
  group_by(industry) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)%>%
  ungroup()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ds_filter2 <- ds_filter2 %>%
  filter(industry == ds_filter3$industry)
```

    ## Warning in industry == ds_filter3$industry: longer object length is not a
    ## multiple of shorter object length

</details>

``` r
ggplot(ds_filter2, aes(metro_location,fill = industry)) +
  geom_bar() +
  scale_fill_viridis(discrete = TRUE)+
  labs(
    title = "Top 5 Industries by Location",
    x = "Location",
    subtitle = "The cities are listed with the total number of jobs and it is colored by the industry"
  )+
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8),
    axis.text = element_text(size =10),
    plot.subtitle=element_text(size=8, color = "grey"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)
  )
```

![](3_2_Industry_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## Interpretation

Filtering by the 6 data science categories from `job_category`, we can
see that San Francisco and DC still hold the majority of data science
jobs. Now we can also see in which industries these data science jobs
are so applicants can determine good industries for data scientists.
Judging by this graph, IT seems to be where most of the jobs in these
areas are being posted. I think that this is because companies will put
their data science positions with IT.

# count of job\_category

``` r
ds_jobs %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | job_category == "Data Scientist" | job_category == "Machine Learning Engineer" | job_category == "Statistician" | job_category == "Other Analyst") %>%
  rename(`Job Category` = job_category) %>%
  group_by(`Job Category`) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  gt()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#vxlgqxjclf .gt_table {
  display: table;
  border-collapse: collapse;
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

#vxlgqxjclf .gt_heading {
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

#vxlgqxjclf .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#vxlgqxjclf .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#vxlgqxjclf .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxlgqxjclf .gt_col_headings {
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

#vxlgqxjclf .gt_col_heading {
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

#vxlgqxjclf .gt_column_spanner_outer {
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

#vxlgqxjclf .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#vxlgqxjclf .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#vxlgqxjclf .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#vxlgqxjclf .gt_group_heading {
  padding: 8px;
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
}

#vxlgqxjclf .gt_empty_group_heading {
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

#vxlgqxjclf .gt_from_md > :first-child {
  margin-top: 0;
}

#vxlgqxjclf .gt_from_md > :last-child {
  margin-bottom: 0;
}

#vxlgqxjclf .gt_row {
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

#vxlgqxjclf .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#vxlgqxjclf .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxlgqxjclf .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#vxlgqxjclf .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#vxlgqxjclf .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#vxlgqxjclf .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#vxlgqxjclf .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#vxlgqxjclf .gt_footnotes {
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

#vxlgqxjclf .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#vxlgqxjclf .gt_sourcenotes {
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

#vxlgqxjclf .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#vxlgqxjclf .gt_left {
  text-align: left;
}

#vxlgqxjclf .gt_center {
  text-align: center;
}

#vxlgqxjclf .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#vxlgqxjclf .gt_font_normal {
  font-weight: normal;
}

#vxlgqxjclf .gt_font_bold {
  font-weight: bold;
}

#vxlgqxjclf .gt_font_italic {
  font-style: italic;
}

#vxlgqxjclf .gt_super {
  font-size: 65%;
}

#vxlgqxjclf .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="vxlgqxjclf" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Job Category

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

count

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

Data Scientist

</td>

<td class="gt_row gt_center">

660

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Data Engineer

</td>

<td class="gt_row gt_center">

592

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Data Analyst

</td>

<td class="gt_row gt_center">

569

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Machine Learning Engineer

</td>

<td class="gt_row gt_center">

190

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Statistician

</td>

<td class="gt_row gt_center">

80

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Other Analyst

</td>

<td class="gt_row gt_center">

28

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

# count of job openings per area

``` r
ds_jobs %>%
  rename(`Metro Location` = metro_location) %>%
  group_by(`Metro Location`) %>%
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  gt()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gqfucffjzz .gt_table {
  display: table;
  border-collapse: collapse;
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

#gqfucffjzz .gt_heading {
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

#gqfucffjzz .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gqfucffjzz .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gqfucffjzz .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gqfucffjzz .gt_col_headings {
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

#gqfucffjzz .gt_col_heading {
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

#gqfucffjzz .gt_column_spanner_outer {
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

#gqfucffjzz .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gqfucffjzz .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gqfucffjzz .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#gqfucffjzz .gt_group_heading {
  padding: 8px;
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
}

#gqfucffjzz .gt_empty_group_heading {
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

#gqfucffjzz .gt_from_md > :first-child {
  margin-top: 0;
}

#gqfucffjzz .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gqfucffjzz .gt_row {
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

#gqfucffjzz .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#gqfucffjzz .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gqfucffjzz .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gqfucffjzz .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gqfucffjzz .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gqfucffjzz .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gqfucffjzz .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gqfucffjzz .gt_footnotes {
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

#gqfucffjzz .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gqfucffjzz .gt_sourcenotes {
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

#gqfucffjzz .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gqfucffjzz .gt_left {
  text-align: left;
}

#gqfucffjzz .gt_center {
  text-align: center;
}

#gqfucffjzz .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gqfucffjzz .gt_font_normal {
  font-weight: normal;
}

#gqfucffjzz .gt_font_bold {
  font-weight: bold;
}

#gqfucffjzz .gt_font_italic {
  font-style: italic;
}

#gqfucffjzz .gt_super {
  font-size: 65%;
}

#gqfucffjzz .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="gqfucffjzz" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Metro Location

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

count

</th>

</tr>

</thead>

<tbody class="gt_table_body">

<tr>

<td class="gt_row gt_left">

New York, NY

</td>

<td class="gt_row gt_center">

900

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Washington, DC

</td>

<td class="gt_row gt_center">

890

</td>

</tr>

<tr>

<td class="gt_row gt_left">

San Francisco, CA

</td>

<td class="gt_row gt_center">

888

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Dallas, TX

</td>

<td class="gt_row gt_center">

261

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Austin, TX

</td>

<td class="gt_row gt_center">

181

</td>

</tr>

<tr>

<td class="gt_row gt_left">

Houston, TX

</td>

<td class="gt_row gt_center">

110

</td>

</tr>

<tr>

<td class="gt_row gt_left">

San Antonio, TX

</td>

<td class="gt_row gt_center">

57

</td>

</tr>

</tbody>

</table>

</div>

<!--/html_preserve-->

# Industry by job category

This alluvial plot gives us more of a visual explanation/view about the
top 5 industries from our data set and how it’s mapped to the different
data science jobs available. Where does most data scientists fit in? We
see clearly that there are data scientists in every industry and the
maximum in Business services. Since Machine learning and Statistics are
narrowed down to a particular domain, although these are present in
various industries, the numbers are pretty low compared to data engineer
or data analyst.

``` r
ds_filter <- ds_jobs %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | job_category == "Data Scientist" | job_category == "Machine Learning Engineer" | job_category == "Statistician" | job_category == "Other Analyst")
ds_filter2 <- ds_filter %>%
  filter(!is.na(industry))
ds_filter3 <- ds_filter2 %>%
  group_by(industry) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5)%>%
  ungroup()
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ds_filter2 <- ds_filter2 %>%
  filter(industry == ds_filter3$industry) %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | job_category == "Data Scientist" | job_category == "Machine Learning Engineer" | job_category == "Statistician")
```

    ## Warning in industry == ds_filter3$industry: longer object length is not a
    ## multiple of shorter object length

``` r
df <- table(ds_filter2$job_category, ds_filter2$industry)
dff <- data.frame(df)

ggplot(data = dff,
       aes(axis1 = Var1, axis2 = Var2,
           y = Freq)) +
  scale_x_discrete(limits = c("Job Category", "Industry"), expand = c(.2, .05)) +
  ylab("Frequency") +
  geom_alluvium(aes(fill = Var1), alpha = 0.9) +
  geom_stratum() +
  scale_fill_viridis(discrete = TRUE, name = "Data Science Role") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2) +
  theme_minimal() +
  ggtitle("Data Science Job Roles in the Top 5 Industries")
```

![](3_2_Industry_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

# List Counts of Data Science Jobs

``` r
# filter by only data science relevant jobs (as discussed in team meeting), generate table of counts
data_jobs <- ds_jobs %>% 
  group_by(job_category) %>% 
  summarize(count = n()) %>% 
  filter(job_category %in% 
           c('Data Engineer', 'Data Analyst', 'Data Scientist', 
             'Machine Learning Engineer', 'Statistician', 'Other Analyst'))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
data_jobs
```

    ## # A tibble: 6 x 2
    ##   job_category              count
    ##   <chr>                     <int>
    ## 1 Data Analyst                569
    ## 2 Data Engineer               592
    ## 3 Data Scientist              660
    ## 4 Machine Learning Engineer   190
    ## 5 Other Analyst                28
    ## 6 Statistician                 80

# Most Popular Industries in terms of Data Science Job Postings

``` r
# find the counts of data science job postings within each industry
data_industries <- ds_jobs %>%
  filter(job_category %in% data_jobs$job_category) %>% 
  group_by(industry) %>% 
  summarize(count = n()) %>% 
  filter(!is.na(industry)) %>% 
  arrange(-count) %>% 
  filter(count >= 100)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
data_industries
```

    ## # A tibble: 5 x 2
    ##   industry                  count
    ##   <chr>                     <int>
    ## 1 Information Technology      576
    ## 2 Business Services           401
    ## 3 Biotech & Pharmaceuticals   171
    ## 4 Finance                     129
    ## 5 Aerospace & Defense         121

The top industries in terms of data science job postings are listed
above. Information technology and business services are far ahead of the
rest of the top 5: Biotech/Pharmaceuticals, Finance, Aerospace/Defense.
