Exploring Data Science Roles
================

In this file we look at job types and Data Science roles and understand
their breakdown by metro area and company.

# Analyizing Job types

Viewing the job types from the data set:

``` r
table(ds_jobs$job_type)
```

    ## 
    ## CONTRACTOR  FULL_TIME     INTERN      OTHER  PART_TIME  TEMPORARY 
    ##         14       3043         19         38        172          1

Below are the job types :

  - Contractor
  - Full time
  - Intern
  - Other
  - Part time
  - Temporary

We would like to see the break down of job types in our data set and
understand what types of jobs can be found in different locations.

``` r
#filter for data required for graph
data_jobs <- ds_jobs %>% 
  group_by(job_category) %>% 
  summarize(count = n()) %>% 
  filter(!job_category %in% 
           c('Biologist', 'Consultant', 'Research Scientist', 
           'Computer Scientist', NA))

ds_jobs %>% 
  filter(job_category %in% data_jobs$job_category) %>% 
  group_by(job_type, metro_location) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count) * 100) %>% 
  ggplot(aes(x = metro_location, y = pct)) + 
  geom_col(aes(fill = job_type), position = 'dodge2') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Location by type of data science jobs available',
       x = 'Location',
       y = 'Count') +
  scale_fill_discrete(name = 'Location')+
  theme_classic() +
  scale_fill_viridis(discrete = TRUE, name = "Job type")+
  theme( axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8))
```

![](3_1_Data-Science-roles_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

## Table of Job Types by Metro Location

From the data set, considering only the full time data science related
job openings, we see that San Francisco and Washington D.C. have the
maximum number of jobs available while San Antonio has the least number
of jobs comparatively.

Viewing the number of job types in each metro area as a table.

``` r
table(ds_jobs$metro_location, ds_jobs$job_type)
```

    ##                    
    ##                     CONTRACTOR FULL_TIME INTERN OTHER PART_TIME TEMPORARY
    ##   Austin, TX                 1       175      1     0         4         0
    ##   Dallas, TX                 3       247      3     5         3         0
    ##   Houston, TX                1       106      1     2         0         0
    ##   New York, NY               0       755      0    25       120         0
    ##   San Antonio, TX            2        51      0     1         3         0
    ##   San Francisco, CA          7       854     12     4        10         1
    ##   Washington, DC             0       855      2     1        32         0

# List Counts of Data Science Jobs

``` r
ds_jobs %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | job_category == "Data Scientist" | job_category == "Machine Learning Engineer" | job_category == "Statistician" | job_category == "Other Analyst") %>%
  rename(`Job Category` = job_category) %>%
  group_by(`Job Category`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  gt()
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#gcugkoakgb .gt_table {
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

#gcugkoakgb .gt_heading {
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

#gcugkoakgb .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#gcugkoakgb .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#gcugkoakgb .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gcugkoakgb .gt_col_headings {
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

#gcugkoakgb .gt_col_heading {
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

#gcugkoakgb .gt_column_spanner_outer {
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

#gcugkoakgb .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#gcugkoakgb .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#gcugkoakgb .gt_column_spanner {
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

#gcugkoakgb .gt_group_heading {
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

#gcugkoakgb .gt_empty_group_heading {
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

#gcugkoakgb .gt_from_md > :first-child {
  margin-top: 0;
}

#gcugkoakgb .gt_from_md > :last-child {
  margin-bottom: 0;
}

#gcugkoakgb .gt_row {
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

#gcugkoakgb .gt_stub {
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

#gcugkoakgb .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gcugkoakgb .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#gcugkoakgb .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#gcugkoakgb .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#gcugkoakgb .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#gcugkoakgb .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#gcugkoakgb .gt_footnotes {
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

#gcugkoakgb .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#gcugkoakgb .gt_sourcenotes {
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

#gcugkoakgb .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#gcugkoakgb .gt_left {
  text-align: left;
}

#gcugkoakgb .gt_center {
  text-align: center;
}

#gcugkoakgb .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#gcugkoakgb .gt_font_normal {
  font-weight: normal;
}

#gcugkoakgb .gt_font_bold {
  font-weight: bold;
}

#gcugkoakgb .gt_font_italic {
  font-style: italic;
}

#gcugkoakgb .gt_super {
  font-size: 65%;
}

#gcugkoakgb .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="gcugkoakgb" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Job Category

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Count

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

In our dataset, we see most data science jobs listed under the data
scientist role, followed closely by data engineer and data analyst.
There are also roles of machine learning engineer, statistician and
other analyst - though to a much lesser extent.

# Metro Area Breakdown of Data Science Roles

Next, we wanted to see if different metro areas had different breakdowns
of data science roles and if there is a better location to search for a
job if a candidate is looking for a specific role.

``` r
ds_jobs %>%
  rename(`Metro Location` = metro_location) %>%
  group_by(`Metro Location`) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))%>%
  gt()
```

<!--html_preserve-->

<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#ipbkrjvlog .gt_table {
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

#ipbkrjvlog .gt_heading {
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

#ipbkrjvlog .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#ipbkrjvlog .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 4px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#ipbkrjvlog .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ipbkrjvlog .gt_col_headings {
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

#ipbkrjvlog .gt_col_heading {
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

#ipbkrjvlog .gt_column_spanner_outer {
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

#ipbkrjvlog .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#ipbkrjvlog .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#ipbkrjvlog .gt_column_spanner {
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

#ipbkrjvlog .gt_group_heading {
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

#ipbkrjvlog .gt_empty_group_heading {
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

#ipbkrjvlog .gt_from_md > :first-child {
  margin-top: 0;
}

#ipbkrjvlog .gt_from_md > :last-child {
  margin-bottom: 0;
}

#ipbkrjvlog .gt_row {
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

#ipbkrjvlog .gt_stub {
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

#ipbkrjvlog .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ipbkrjvlog .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#ipbkrjvlog .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#ipbkrjvlog .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#ipbkrjvlog .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#ipbkrjvlog .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#ipbkrjvlog .gt_footnotes {
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

#ipbkrjvlog .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#ipbkrjvlog .gt_sourcenotes {
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

#ipbkrjvlog .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#ipbkrjvlog .gt_left {
  text-align: left;
}

#ipbkrjvlog .gt_center {
  text-align: center;
}

#ipbkrjvlog .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#ipbkrjvlog .gt_font_normal {
  font-weight: normal;
}

#ipbkrjvlog .gt_font_bold {
  font-weight: bold;
}

#ipbkrjvlog .gt_font_italic {
  font-style: italic;
}

#ipbkrjvlog .gt_super {
  font-size: 65%;
}

#ipbkrjvlog .gt_footnote_marks {
  font-style: italic;
  font-size: 65%;
}
</style>

<div id="ipbkrjvlog" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">

<table class="gt_table">

<thead class="gt_col_headings">

<tr>

<th class="gt_col_heading gt_columns_bottom_border gt_left" rowspan="1" colspan="1">

Metro Location

</th>

<th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">

Count

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

It looks like New York has the most job openings in our data set.
However, many job postings in our data are not actually related to data
science roles. For this analysis we are only looking at the data science
field, therefore we can look at the break down further of only data
science related roles.

``` r
# find the counts of data science job postings within each metro area
ds_jobs %>% 
  filter(job_category %in% data_jobs$job_category) %>% 
  group_by(metro_location) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))
```

    ## # A tibble: 7 x 2
    ##   metro_location    count
    ##   <chr>             <int>
    ## 1 San Francisco, CA   760
    ## 2 Washington, DC      725
    ## 3 Dallas, TX          233
    ## 4 Austin, TX          156
    ## 5 New York, NY        120
    ## 6 Houston, TX          81
    ## 7 San Antonio, TX      44

Here we can see that when only looking at data science related roles San
Francisco and Washington D.C. have the most roles relevant to our
analysis.

``` r
#plot the each job category as the proportion of the total amount of jobs in each city (only focusing on ds jobs)
ds_jobs %>% 
  filter(job_category %in% data_jobs$job_category) %>% 
  group_by(metro_location, job_category) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count) * 100) %>% 
  ggplot(aes(x = metro_location, y = pct)) + 
  geom_col(aes(fill = job_category), position = 'dodge2') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Metro Area Breakdown of Data Science Job Categories',
       x = 'Metro Area',
       y = 'Percent of Jobs') +
  scale_fill_viridis(discrete = TRUE, name = 'Job Category')
```

![](3_1_Data-Science-roles_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ds_filter <- ds_jobs %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | 
         job_category == "Data Scientist" | job_category == "Machine Learning Engineer" |
         job_category == "Statistician" | job_category == "Other Analyst")

#another variation of previous graph
ggplot(ds_filter, aes(metro_location, fill = job_category)) +
  geom_bar()+
  scale_fill_viridis(discrete = TRUE) +
  labs(
    title = "Data Science Jobs by Location",
    x = "Locations",
    subtitle = "This graph shows the number of data science positions available in each city.",
    fill = "Job Category")+
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8),
    axis.text = element_text(size =10),
    plot.subtitle=element_text(size=8, color = "grey"),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 8)) 
```

![](3_1_Data-Science-roles_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The above two plots display the same information, just in different
ways. San Francisco and Washington DC clearly have the highest amount of
DS job postings. Dallas and Austin surprisingly have a higher amount of
job postings than New York. Data analyst is the most popular DS job in
Austin and San Francisco. Data Engineer is the most popular job posting
in Dallas. Data Scientist is the most popular job posting in both San
Antonio and DC. Machine Learning Engineer makes up exactly half of the
postings in New York, but in terms of DS roles the city offers the least
versatility - only offering Analyst, Engineer, and ML Engineer positions
(perhaps mention people leaving area during covid). Statistician is in
the minority of available jobs at each location aside from New York.
Based on this data, a candidate should narrow their focus jobs in the
San Francisco Bay area and Washington D.C. if they are open to
relocating.

# Top Companies in terms of Overall Job Postings

In addition to location, we were curious if different companies had job
openings in only one type of data science role or offered various types
of roles.

``` r
companies <- ds_jobs %>%
  #filter out na job categories from all jobs
  filter(is.na(job_category) == F) %>% 
  #group by company
  group_by(company) %>% 
  #get the count of each company
  summarize(count = n()) %>% 
  #sort descending
  arrange(-count) %>% 
  #view the largest ones
  filter(count >= 30)

companies
```

    ## # A tibble: 6 x 2
    ##   company              count
    ##   <chr>                <int>
    ## 1 Genentech               60
    ## 2 National Debt Relief    60
    ## 3 Addepar                 30
    ## 4 Crypsis Group           30
    ## 5 Kingdom Associates      30
    ## 6 TEECOM                  30

``` r
# filter dataset to only include the top companies and job types that are not na, plot bar chart
ds_jobs %>% 
  filter(is.na(job_category) == F,
         company %in% companies$company) %>% 
  group_by(company, job_category) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = company, y = count)) + 
  geom_col(aes(fill = job_category), position = 'dodge2') +
  labs(title = 'Job Category Breakdown by Company',
       x = 'Company',
       y = 'Count') +
  scale_fill_viridis(discrete = TRUE, name = "Data Science Roles") +
  theme( axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8))
```

![](3_1_Data-Science-roles_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

The above plot displays the job category count of the top 6 overall
companies in terms of job postings. From here, we see that we may need
to subset our data to only include data science specific jobs.
