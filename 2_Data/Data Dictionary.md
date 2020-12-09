# Data Dictionary

1. coi: (numeric) Cost of living index by metro location
2. state: (categorical) State in which the company's job posting is listed
3. city: (categorical) City in which the company's job posting is listed
4. job_title: (text) The title of job opening
5. company: (categorical) Company name
6. min_salary: (numeric) Minimum yearly salary (in USD)
7. max_salary: (numeric) Maximum yearly salary (in USD)
8. job_desc: (text) The job description which included skills,requirements,etc
9. industry: (categorical) The industry in which the company is located
10. rating: (numeric) Rating of the company on a scale from 0-5
11. date_posted: (date) The date on which the job was posted on glassdoor
12. valid_until: (date) The last date to apply for the job
13. job_type: (categorical) Type of job; full-time, part-time,etc.
14. location: (categorical) City and state in which the company's job posting is listed
15. metro_location: (categorical) metro location in which the company's job posting is listed
16. count: (numeric) the total number of job posts within that job posting's city
17. lon_city: (numeric) the longitude of the city (from google maps api) that in which the job posting is located
18. lat_city: (numeric)  the latitude of the city (from google maps api) that in which the job posting is located
19. lon_metro: (numeric)  the longitude of the metro location (from google maps api) that in which the job posting is located
20. lat_metro: (numeric)  the latitude of the metro location (from google maps api) that in which the job posting is located
21. job_concentration: the percentage of jobs located in that job posting's city relative to the metro area
22. avg_lon_metro_weighted: the weighted (by job concentration) average longitude of the cities located within the metro area in which the job posting is located
23. avg_lat_metro_weighted: the weighted (by job concentration) average latitude of the cities located within the metro area in which the job posting is located
24. reg_dist: the distance of the job posting's city from the metro location (in miles)
25. weight_dist: the distance of the job posting's city from the average weighted metro location (in miles)
26. job_category: (categorical) Category or role job falls under based on job title
27. min_scaled_salary: (numeric) Minimum yearly salary scaled by coi (in USD)
28. max_scaled_salary: (numeric) Maximum yearly salary scaled by coi (in USD)
