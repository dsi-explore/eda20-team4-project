Job\_Types
================

``` r
library(tidyverse)
library(ggplot2)
library(viridis)
```

Job types :

Viewwing the job typs from the data set :

``` r
ds_jobs <- read.csv("../Data Cleaning/ds_jobs.csv")
```

Below are the job types :

  - Contractor
  - Fulltime
  - Intern
  - Other
  - Parttime
  - Temporary

<!-- end list -->

``` r
data_jobs <- ds_jobs %>% 
  group_by(job_category) %>% 
  summarize(count = n()) %>% 
  filter(!job_category %in% c('Biology', 'Consultant', 'Research Scientist', 'Computer Scientist', NA))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
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

    ## `summarise()` regrouping output by 'job_type' (override with `.groups` argument)

    ## Scale for 'fill' is already present. Adding another scale for 'fill', which
    ## will replace the existing scale.

![](Job_Types_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ds_jobs %>% 
  filter(job_category %in% data_jobs$job_category) %>% 
  group_by(job_type, metro_location) %>% 
  summarize(count = n()) %>% 
  mutate(pct = count/sum(count) * 100) %>% 
  ggplot(aes(x = job_type, y = pct)) + 
  geom_col(aes(fill = metro_location), position = 'dodge2') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = 'Location by type of data science jobs available',
       x = 'Job Type',
       y = 'Count') +
  scale_fill_discrete(name = 'Location')+
  theme_classic() +
  scale_fill_viridis(discrete = TRUE, name = "Location")+
  theme( axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8))
```

    ## `summarise()` regrouping output by 'job_type' (override with `.groups` argument)
    ## Scale for 'fill' is already present. Adding another scale for 'fill', which
    ## will replace the existing scale.

![](Job_Types_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

Viewing the number of job types in each state as a table

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

# Relationship between states and types of jobs available

``` r
ggplot(ds_jobs, aes(x=metro_location, y=job_type))+
  geom_jitter(alpha = 0.7) + labs(title ="Relationship between states and types of jobs available",
  y = "Type Of Job", 
  x = "state")+
theme_classic()
```

![](Job_Types_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

From the graph, we can see the distribution of the types of jobs
availbale in each state. We see that, KY, NC and TN doesn’t have more
full time jobs when compared to other states and there are only couple
of states which offer temporary positions.

Further, we can explore about the sub categories in full time jobs, by
taking data science related jobs from the other rmd
file.

# Analysing the number of jobs in every category by taking the ds\_jobs\_bucket.So, here we have five categories Data Analyst, Data Engineer, Data Scientist, Machine Learning and Statistics. Finding out how many of these fall into different industries. In our data frame, we have 23 industries for which we can map these job categories.

``` r
df <- data.frame(table(ds_jobs$industry,ds_jobs$job_category,ds_jobs$job_type))

p <- ggplot(data=df, aes(x=Var2, y=Freq, fill=Var1)) + geom_bar(stat="identity") +
  labs(title = "Job category vs Industry",
       x = "Job Category",
       y = "Count")

p + scale_fill_discrete(name="Industry") +theme(
  plot.title = element_text(color="black", size=14, face="bold",hjust = 0.5)
)
```

![](Job_Types_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

# Time series analysis

There are two columns Date\_Posted and Valid\_Until which denotes the
time frame until which the job applications are open and denoting the
recruitment time frame for candidates. On trying to plot the
availabiloty of new jobs while considering the date posted, we find that
it doesn’t increase exponentially neither does it hits the bottom but it
reaches it’s peak after mid May denoting the max job availability.

``` r
table_date_posted <- data.frame(table(ds_jobs$date_posted))
table_date_posted$Var1 <- as.Date(table_date_posted$Var1, format = "%Y-%m-%d") 
class(table_date_posted$Var1)
```

    ## [1] "Date"

``` r
ggplot(table_date_posted, aes(x = Var1, y = Freq)) +
  geom_line() +
  labs(title = "Job availability vs Date posted",
       x = "Date",
       y = "Number of jobs")
```

![](Job_Types_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

# Salary by location

``` r
library(viridis)
df <- ds_jobs %>%
             group_by(metro_location) %>%
             summarise(min = min(min_scaled_salary, na.rm = TRUE),max = max(max_scaled_salary, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
salary_data_loc <- df %>% pivot_longer(
                cols = c(min, max),
                names_to = "type",
                values_to = "salary",
                values_drop_na = TRUE
)


p <-ggplot(salary_data_loc, aes(x = reorder(metro_location, -salary), y = salary, fill = type)) 
  p + geom_bar(stat = "identity", position = 'dodge')+
    labs(title = "Salary by location",
       x = "Location",
       y = "Scaled Salary") +
    scale_fill_viridis(discrete = TRUE, begin = 0.25, end = 0.5, name = "Salary Type",
                     labels = c("Max", "Min")) + 
    scale_y_continuous(
    breaks = seq(0,1100000,50000),
    labels = function(x){paste0('$', x/1000, 'K')}
  ) +
    theme_classic() +
  theme( axis.text.x = element_text(angle = 45, vjust = 1, hjust=0.95, size = 8))
```

![](Job_Types_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

# Salary by job type
