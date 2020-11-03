# Next Steps

1. Everyone download data and brainstorm guiding questions

Code to download and combined the data sets:

NY <- read.csv("glassdoor_data/Data_Job_NY.csv")
SF <- read.csv("glassdoor_data/Data_Job_SF.csv")
TX <- read.csv("glassdoor_data/Data_Job_TX.csv")
WA <- read.csv("glassdoor_data/Data_Job_WA.csv")
ds_jobs <- rbind(NY, SF, TX, WA)

# Brainstorm of questions
