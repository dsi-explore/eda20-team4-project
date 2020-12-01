Industry Analysis
================

# Location of job bucket

I want to take the values from the job\_category variable and graph it
against the location data to see if which jobs are in each location.

<details>

<summary>Click to expand</summary> \# Load
    Data

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ---------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(readr)
library(viridis)
```

    ## Loading required package: viridisLite

``` r
setwd("C:/Users/Matt Flaherty/Documents/Projects/eda20-team4-project")
ds_jobs <- read_csv("Data Cleaning/ds_jobs.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   state = col_character(),
    ##   city = col_character(),
    ##   job_title = col_character(),
    ##   company = col_character(),
    ##   job_desc = col_character(),
    ##   industry = col_character(),
    ##   date_posted = col_date(format = ""),
    ##   valid_until = col_date(format = ""),
    ##   job_type = col_character(),
    ##   location = col_character(),
    ##   metro_location = col_character(),
    ##   job_category = col_character()
    ## )

    ## See spec(...) for full column specifications.

# Job Bucket vs location

Filter by only DS jobs. There are a few other scientist jobs right now.
Also take out the NAs

``` r
ds_filter <- ds_jobs %>%
  filter(!is.na(job_category)) %>%
  filter(job_category == "Data Analyst" | job_category == "Data Engineer" | job_category == "Data Scientist" | job_category == "Machine Learning" | job_category == "Statistics" | job_category == "Other Analyst")
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

![](industry_analysis_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

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
    title = "Industry by Location",
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

![](industry_analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## Interpretation

Using the data science jobs from `job_category`, we can see that San
Francisco and DC still hold the majority of data science jobs. Now we
can also see in which industries these data science jobs are so
applicants can determine good industries for data scientists. Judging by
this graph, IT seems to be where most of the jobs in these areas are
being posted. I think that this is because companies will put their data
science positions with IT.
