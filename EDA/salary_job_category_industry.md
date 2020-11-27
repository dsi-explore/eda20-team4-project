Salary Based on Job Category and Industry
================

``` r
#wrangling the data to fit both salaries on the same graph
scaled_salary_data <- ds_jobs %>% select(min_scaled_salary, max_scaled_salary, job_category, industry) %>% pivot_longer(
                cols = c(min_scaled_salary, max_scaled_salary),
                names_to = "type",
                values_to = "salary",
                values_drop_na = TRUE
)

scaled_salary_data <- scaled_salary_data %>% mutate(type = case_when(
  type == "min_scaled_salary" ~ "min",
  type == "max_scaled_salary" ~ "max"
))
```

# Job Category

Is there a difference in salary between the different types of roles
within data science?

``` r
#remove NAs from job_category
scaled_salary_data_jc <- scaled_salary_data %>% filter(!is.na(job_category))

scaled_salary_data_jc %>% ggplot(aes(x = salary, col = type, fill = type)) +
  geom_density(alpha = 0.25) +
  labs(
    title = "Salary by Job Category",
    x = "Scaled Salary",
    y = "Density") +
  facet_wrap(~job_category) + 
  theme_classic()
```

![](salary_job_category_industry_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
scaled_salary_data_jc %>% ggplot(aes(y = salary, x = job_category, col = type, fill = type)) +
  geom_boxplot(alpha = 0.25) +
  labs(
    title = "Salary by Job Category") +
  theme_classic() +
   coord_flip()
```

![](salary_job_category_industry_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Calculating average minimum and maximum salary based on job roles.

``` r
jc_avg_salary <- ds_jobs %>% filter(!is.na(job_category) & !is.na(min_scaled_salary) & !is.na(max_scaled_salary)) %>% 
  group_by(job_category) %>% 
  summarise(avg_max_salary = mean(max_scaled_salary), avg_min_salary = mean(min_scaled_salary))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
jc_avg_salary_long <- jc_avg_salary %>% pivot_longer(cols = c(avg_max_salary, avg_min_salary),
                names_to = "type",
                values_to = "salary",
                values_drop_na = TRUE)
```

``` r
jc_avg_salary_long %>% ggplot(aes(x = job_category, y = salary ,fill = type)) +
  geom_bar(stat = "identity", position = 'dodge') +
  labs(
    title = "Salary by Job Category") +
  theme_classic()
```

![](salary_job_category_industry_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Job Industry

Is there a salary difference based on the industry the data science job
is in?

We will look at the top 5 industries and compare their salary ranges.

``` r
#remove NAs from industry
scaled_salary_data_ji <- scaled_salary_data %>% filter(!is.na(industry))

scaled_salary_data_ji_top5 <- scaled_salary_data_ji %>% 
  group_by(industry) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:5) %>%
  select(industry)
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
scaled_salary_data_ji <- scaled_salary_data_ji %>% 
  filter(industry %in% scaled_salary_data_ji_top5$industry)

scaled_salary_data_ji %>% ggplot(aes(x = salary, col = type, fill = type)) +
  geom_density(alpha = 0.25) +
  labs(
    title = "Salary by Job Industry",
    x = "Scaled Salary",
    y = "Density") +
  facet_wrap(~industry) + 
  theme_classic()
```

![](salary_job_category_industry_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->
