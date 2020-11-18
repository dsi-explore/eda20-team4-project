Loading and Cleaning the Data
================

``` r
library(tidyverse)
library(ggplot2)
library(janitor)
```

# Read in Data

``` r
NY <- read.csv("glassdoor_data/Data_Job_NY.csv")
SF <- read.csv("glassdoor_data/Data_Job_SF.csv")
TX <- read.csv("glassdoor_data/Data_Job_TX.csv")
WA <- read.csv("glassdoor_data/Data_Job_WA.csv")

NY["Region"] <- "NYC"
SF["Region"] <- "SF"
TX["Region"] <- "TX"
WA["Region"] <- "DC"

#combining data from the 4 regions
ds_jobs <- rbind(NY, SF, TX, WA)
```

# Basic Exploration

``` r
dim(ds_jobs)
```

    ## [1] 3324   13

There are 3,324 rows and 13 variables in the data set.

``` r
head(ds_jobs)
```

    ##                                               Job_title
    ## 1                         Chief Marketing Officer (CMO)
    ## 2                                      Registered Nurse
    ## 3                                      Dental Hygienist
    ## 4                           Senior Salesforce Developer
    ## 5 DEPUTY EXECUTIVE DIRECTOR, PROGRAM AND LEGAL ADVOCACY
    ## 6                          Emergency Veterinarian - NYC
    ##                                 Company State          City Min_Salary
    ## 1                  National Debt Relief    NY      New York         -1
    ## 2     Queens Boulevard Endoscopy Center    NY     Rego Park         -1
    ## 3                        Batista Dental    NJ West New York         -1
    ## 4                  National Debt Relief    NY      New York      44587
    ## 5 National Advocates for Pregnant Women    NY      New York     125410
    ## 6            Veterinary Emergency Group    NY      New York      94715
    ##   Max_Salary
    ## 1         -1
    ## 2         -1
    ## 3         -1
    ## 4      82162
    ## 5     212901
    ## 6     103279
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Job_Desc
    ## 1               Who We're Looking For:\n\nThe Chief Marketing Officer (CMO) is an exempt, executive position, responsible for all marketing operations of the company including lead acquisition, sales enablement, communications, retention, and brand development. This executive leads a team of enthusiastic, analytical, and passionate marketing professionals to develop, execute, and optimize the marketing strategy. We are looking for someone with a history of brand development and proven ability to accelerate company growth leveraging the latest marketing strategies and technologies. This role goes beyond traditional marketing tactics to generate awareness, educate the consumer on the viability of our service, and in turn drive the consumer to take action and engage the brand.\n\nPrincipal Duties and Responsibilities:\n\nLead the full marketing strategy and have accountability over development, execution, and optimization across all channels including paid and organic search, display, email, social, TV, radio, direct mail, and affiliate marketing.\nCommunicate with the leadership team and key stakeholders to execute lead generation, sales enablement, and retention-based marketing campaigns that align with and deliver against business goals.\nDevelop and execute social media, content, and communication strategies to further our public relations and community engagement.\nIdentify, forge, and grow strategic marketing partnerships.\nBuild a highly efficient and capable team of marketing professionals.\nDefine the competitive marketplace and evolve our brand awareness through strategy development and brand building tactics.\nLead research and development into new marketing tactics and strategies while improving current systems.\nEstablish key metrics and manage goals while leading the improvement of our pipeline for sales.\nEstablish framework for all marketing activity, tracking results and reporting progress with management.\nDevelop segmentation, competitive analysis, market intelligence, salesforce effectiveness, strategic planning and revenue retention and growth.\n\nQualifications:\n\nA completed BS degree in Business, Marketing, Advertising or other related discipline.\nMinimum experience required 10+ years of professional experience in a leadership marketing role.\nExperience building and executing brand awareness and public relations campaigns.\nExperience in a fast-growing company with a track record of delivering big results.\nHighly proficient and effective communication skills\nAbility to utilize data analytics to deliver insight and identify opportunities for growth.\nA strong record of developing successful, innovative and cost-effective marketing campaigns.\nPunctual and ready to report to work on a consistent basis.\nTravel up to 25 percent of the time.\nExcel in a fast-paced environment.\n\nWhat We Offer:\n\nA team-first, work hard play hard culture, full of rewards and recognition for our employees. We are dedicated to our employees' success and growth.\n\nOur extensive benefits package includes:\n\n\nGenerous Medical, Dental, and Vision Benefits\n401(k) with Company Match\nPaid Holidays, Volunteer Time Off, Sick Days, and Vacation\n10 weeks Paid Parental Leave\nPre-tax Transit Benefits\nDiscounted Gym Membership\nCiti Bike Annual Membership Discounts\nNo-Cost Life Insurance Benefits\nVoluntary Benefits Options\nASPCA Pet Health Insurance Discount\n\nAbout National Debt Relief:\n\nNational Debt Relief is one of the country's largest and most reputable debt settlement companies. We are made up of energetic, smart, and compassionate individuals who are passionate about helping thousands of Americans with debt relief. Most importantly, we're all about helping our customers through a tough financial time in their lives with education and individual customer service.\n\nWe are dedicated to helping individuals and families rid their lives of burdensome debt. We specialize in debt settlement and have negotiated settlements for thousands of creditor and collections accounts. We provide our clients with both our expertise and our proven results. This means helping consumers in their time of hardship to get out of debt with the least possible cost. It can also mean conducting financial consultations, educating the consumer, and recommending the appropriate solution. Our core services offer debt settlement as an alternative to bankruptcy, credit counseling, and debt consolidation. We become our clients' number one advocate to help them reestablish financial stability as quickly as possible.\n\nNational Debt Relief is a certified Great Place to Work®!\n\nNational Debt Relief is an equal opportunity employer and makes employment decisions without regard to race, color, religion, sex, sexual orientation, gender identity, national origin, protected veteran status, disability status, or any other status protected by law\n\n#ZR
    ## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Queens Boulevard Endoscopy Center, an endoscopy ASC located in Rego Park, has an exciting opportunity for Full-Time Registered Nurse! Successful candidates will provide quality nursing care in all areas of the Center including pre-assessment, pre-op and pacu  Qualified candidates must possess the following:\n\nCurrent NY state RN license\nBLS Certification, ACLS preferred\nMust be a team-player with excellent multi-tasking and interpersonal skills\nCompassion for patient needs and a high degree of professionalism\nChinese Speaking and Spanish Preferred\n\nQueens Boulevard Endoscopy Center offers a pleasant professional work environment and no evening or holiday work hours. Drug-free work environment and EOE.
    ## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   Part-time or Full-timedental hygienist position available in West New York, NJfor Mondays, Tuesdays, Wednesdays, and Saturday.Applicants may apply for any or all days. Beautiful upscale office with friendly staff. Applicants must be reliable, self-motivated and speak spanish out-going and responsible. Respond with resume via e-mail.
    ## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    Principle Duties & Responsibilities:\n\nAnalyze complex systems and troubleshoot and isolate system issues;\nUnderstand requirements for business users and translate into design specifications, utilizing thorough understanding of the Salesforce platform, Salesforce products and licensing models;\nUtilize thorough understanding of application development, project lifecycle, and methodologies and ability to work under tight deadlines and handle multiple detail-oriented tasks;\nApply knowledge of Salesforce developmentand customizations, with APEX, Visual Force, API, Force.com and Workflows, taking into account com best practices, support mechanisms, procedures, and limitations, as well as NDR's unique needs;\nResponsible for Salesforce administration, release management and deployment as well as management of Salesforce.com sandboxes, including their integrations;\nDesign and execute Salesforce.com configuration changes, leveraging the Salesforce interface to sync with internal tracking systems;\nDesign, develop, and maintain integration and synchronization programs;\nDesign the data model, user interface, business logic, and security for custom applications; and\nDesign, develop, and customize software solutions for end users by using analysis and mathematical models to effectively predict and measure the results of the design using Chatter, Communities and other Salesforce applications.\n\nRequirements:\n\nBachelor of Science degree or foreign equivalent in Information Systems, Computer Science, Computer Engineering, Software Engineering or a related field\n3 years of experience with the Salesforce platform, specifically: development with Apex, VisualForce, and Force.com;\nDesign and execute Salesforce.com configuration changes, leveraging the Salesforce interface to sync with internal tracking systems;\nSalesforce administration, release management, and deployment\nSalesforce products and licensing models\nManagement of Salesforce.com sandboxes, including their integrations; Chatter, Communities, and other Salesforce apps\ncom best practices, support mechanisms, procedures, and limitations.\n\nWhat We Offer:\n\nWe believe in a team-first culture, full of rewards and recognition for our employees. We are dedicated to our employees' success and growth within the company, through our employee mentorship and leadership programs.\n\nOur extensive benefits package includes:\n\n\nMedical, Dental, and Vision Benefits\n401(k) Match\nPaid Holidays, Volunteer Time Off, Sick Days, and Vacation\n10 Weeks Paid Parental Leave\nPre-tax Transit Benefits\nDiscounted Gym Membership\nNo-cost Life Insurance Benefits\n\nAbout National Debt Relief:\n\nNational Debt Relief is one of the country's largest and most reputable debt settlement companies. We are made up of energetic, smart, and compassionate individuals who are passionate about helping thousands of Americans with debt relief. Most importantly, we're all about helping our customers through a tough financial time in their lives with education and individual customer service.\n\nWe are dedicated to helping individuals and families rid their lives of burdensome debt. We specialize in debt settlement and have negotiated settlements for thousands of creditor and collections accounts. We provide our clients with both our expertise and our proven results. This means helping consumers in their time of hardship to get out of debt with the least possible cost. It can also mean conducting financial consultations, educating the consumer, and recommending the appropriate solution. Our core services offer debt settlement as an alternative to bankruptcy, credit counseling, and debt consolidation. We become our clients' number one advocate to help them reestablish financial stability as quickly as possible.\n\n#ZR
    ## 5 For FULL Job Announcement, visit our website: www.AdvocatesForPregnantWomen.org\n\nReporting to and working collaboratively with the Executive Director (ED), the Deputy Executive Director, Program & Legal Advocacy (DED) is a member of the Senior Management Team (SMT) providing leadership for and supervision of NAPW’s legal team and taking responsibility for the day-to-day program operations of the organization. The DED as an experienced senior level attorney with executive management experience and serves as a strategic thought partner and advisor to the Executive Director and the SMT.\n\nIn absence of the Executive Director, the DED (in consultation with the COO), is designated as the highest authority to respond to internal and external inquiries, make programmatic/advocacy decisions, and represent NAPW in any and all responsibilities assigned to the ED.\n\nResponsibilities include (but are not limited to):\n\nPartnering with the ED to create and implement NAPW’s mission-work and strategic planning;Working collaboratively with the SMT (collectively responsible for the critical business functions of Program, Finance/Operations, Human Resources, Communications, and Development/Grant Administration), to develop and implement administrative policies and procedures for guiding operations, strengthening internal systems, ensuring high levels of staff engagement, managing performance, encouraging continuous learning, and promoting administrative and programmatic alignment;Helping to create NAPW’s reproductive justice public policy/public advocacy initiatives and determining when NAPW supports and/or joins related allied efforts by other organizations;Directly supervising the day-to-day work of the Senior Staff Attorneys, Staff Attorneys, post-graduate Fellows, legal & programmatic interns, legal contractors, loaned associates, and Research and Program Associates. Supervision includes coaching and training, performance review, assigning and reviewing work, mentoring, analysis and editing of written work and providing the ED with sufficient time to review;\n\nMinimum qualifications include:\n\nJD degree from an accredited law school is required; Membership in at least 1 (one) state AND federal bar is required;Master’s Degree in Non-profit Management, Public Policy, Social Work, or a related field is highly-desirable;8-10 years: of senior-level management experience in a non-profit legal advocacy/public interest/social justice environment, with demonstrable success in change implementation; complex litigation and advocacy experience as an attorney providing direct client representation, with a particular emphasis in public interest law and reproductive justice and drug policy litigation in state and federal courts; experience in the supervision of attorneys and managing programs (and staff);Demonstrated capacity to serve as a member of a Senior Management Team and advisor to the Executive Director on all matters pertaining to NAPW's legal advocacy;Knowledge of and experience in reproductive health, rights, and justice; civil rights with knowledge of drug policy reform, women’s rights, family law, child welfare reform, and human rights is highly-desirable.\n\nNOTE: YOUR SUBMISSION WILL BE REJECTED IF YOU HAVE NOT PROVIDED ALL MATERIALS AND INFORMATION AS INSTRUCTED BELOW.\n\nREQUIRED SUBMISSIONS (MUST INCLUDE ALL ITEMS LISTED BELOW):\n\n1. Cover Letter which must include all of the following elements:\n\na) Your personal & professional motivation for seeking this position.\n\nb) A discussion of what makes you the ideal/best candidate for this position.\n\nc) Explain how your skill sets and experience best demonstrate your strategic approach.\n\nd) Salary Requirement.\n\ne) Indicate where you found this Job Announcement.\n\n2. Resumé.\n\n3. Two (2) Writing Samples solely reflecting applicant’s own work (MUST submit BOTH A and B):\n\na) One Non- legal advocacy writing sample such as an article, commentary or blog.\n\nb) One Legal writing sample (i.e., a legal brief, argument or analysis) consisting of NO MORE THAN ten pages of text.\n\n4. Complete contact information for three (3) professional references.\n\nINSTRUCTIONS: NO PHONE CALLS OR FAXES PLEASE.\n\nAll submissions must be sent VIA EMAIL ONLY\n\nSUBJECT: ATTN: Human Resources – NAPW Deputy Executive Director, Program & Legal Advocacy (JAN. 2020)\n\nJob Type: Full-time\n\nExperience:\n\nReproductive Justice/Reproductive Rights legal advocacy: 5 years (Preferred)Non-profit Executive/Senior Management: 8 years (Required)Supervising Attorney: 5 years (Required)Public Interest Law and litigation: 6 years (Required)\n\nEducation:\n\nDoctorate (Required)\n\nWork Location:\n\nOne location\n\nBenefits:\n\nHealth insuranceDental insuranceVision insuranceRetirement planPaid time offParental leaveProfessional development assistanceTuition reimbursement\n\nSchedule:\n\nMonday to Friday\n
    ## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Emergency VeterinarianThe family you will be joining:\n\nVEG is a rapidly growing group of emergency practices with multiple locations and a single mission: Helping People and Their Pets When They Need it Most. We are changing the face of emergency veterinary medicine with a “client first” mentality. We’re a group of passionate, thought leaders that believe in the power of an open mind and servant leadership.\n\nIf you’re the ideal candidate, you’ll:\n\nHave earned a DVM or equivalent degree Be fulfilled by helping others Thrive in team-oriented environments (think hospital retreats, team dinners, happy hours and more!) Have a ‘glass half full’ attitude and a sense of humor! Live and breathe emergency medicine Be passionate about emergency surgery (the soft tissue kind!)\n\nBenefits\n\nWhy you should choose us: Because emergency is all we do, so we do it best! We also offer:\n\nIndustry-leading compensation + signing bonus + monthly bonuses Health Insurance 401K w/ company match Unlimited CE Flexible work schedules for a true work-life balance (3 shifts a week is full-time for us!) Growth potential Fresh groceries sent weekly, monthly and quarterly contests, quarterly hospital outings, annual company-wide retreat, etc!\n
    ##      Industry Rating Date_Posted Valid_until  Job_Type Region
    ## 1     Finance    4.0  2020-05-08  2020-06-07 FULL_TIME    NYC
    ## 2                3.0  2020-04-25  2020-06-07 FULL_TIME    NYC
    ## 3                 NA  2020-05-02  2020-06-07 PART_TIME    NYC
    ## 4     Finance    4.0  2020-05-08  2020-06-07 FULL_TIME    NYC
    ## 5                 NA  2020-04-28  2020-06-07 FULL_TIME    NYC
    ## 6 Health Care    4.9  2020-05-05  2020-06-07 FULL_TIME    NYC

``` r
names(ds_jobs)
```

    ##  [1] "Job_title"   "Company"     "State"       "City"        "Min_Salary" 
    ##  [6] "Max_Salary"  "Job_Desc"    "Industry"    "Rating"      "Date_Posted"
    ## [11] "Valid_until" "Job_Type"    "Region"

``` r
summary(ds_jobs)
```

    ##   Job_title           Company             State               City          
    ##  Length:3324        Length:3324        Length:3324        Length:3324       
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    Min_Salary       Max_Salary       Job_Desc           Industry        
    ##  Min.   :    -1   Min.   :    -1   Length:3324        Length:3324       
    ##  1st Qu.:    -1   1st Qu.:    -1   Class :character   Class :character  
    ##  Median : 56512   Median : 91249   Mode  :character   Mode  :character  
    ##  Mean   : 55358   Mean   : 80999                                        
    ##  3rd Qu.: 91694   3rd Qu.:128644                                        
    ##  Max.   :205735   Max.   :383416                                        
    ##                                                                         
    ##      Rating      Date_Posted        Valid_until          Job_Type        
    ##  Min.   :1.000   Length:3324        Length:3324        Length:3324       
    ##  1st Qu.:3.500   Class :character   Class :character   Class :character  
    ##  Median :3.900   Mode  :character   Mode  :character   Mode  :character  
    ##  Mean   :3.838                                                           
    ##  3rd Qu.:4.200                                                           
    ##  Max.   :5.000                                                           
    ##  NA's   :475                                                             
    ##     Region         
    ##  Length:3324       
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

The unit of analysis in the `ds_jobs` data set is a specific job within
the field of data science at a company listed on glassdoor.

# Clean Data

## Missing Values

It looks like `Min_salary` and `Max_Salary` have missing values denoted
with a `-1` value. We will replace them with NA.

``` r
#Replace -1 with NA
ds_jobs["Min_Salary"] <- na_if(ds_jobs["Min_Salary"], -1)
ds_jobs["Max_Salary"] <- na_if(ds_jobs["Max_Salary"], -1)
```

``` r
table(ds_jobs$State)
```

    ## 
    ##          CA    DC    KY    MD    NC    NJ    NY Texas    TN    TX    VA 
    ##     2   888   194     1   246     1   150   750     6     1   637   448

There are 2 observations within `State` that are blank. We will replace
them with NA.

``` r
#Replace blanks with NA
ds_jobs["State"] <- na_if(ds_jobs["State"], "")
```

``` r
table(ds_jobs$Industry)
```

    ## 
    ##                                                    Accounting & Legal 
    ##                                624                                 29 
    ##                Aerospace & Defense             Agriculture & Forestry 
    ##                                144                                  1 
    ##   Arts, Entertainment & Recreation          Biotech & Pharmaceuticals 
    ##                                  2                                317 
    ##                  Business Services Construction, Repair & Maintenance 
    ##                                583                                 66 
    ##                  Consumer Services                          Education 
    ##                                  7                                 60 
    ##                            Finance                         Government 
    ##                                223                                105 
    ##                        Health Care             Information Technology 
    ##                                232                                679 
    ##                          Insurance                      Manufacturing 
    ##                                 29                                 42 
    ##                              Media                         Non-Profit 
    ##                                 29                                 10 
    ##       Oil, Gas, Energy & Utilities                        Real Estate 
    ##                                 28                                  5 
    ##  Restaurants, Bars & Food Services                             Retail 
    ##                                  3                                 63 
    ##                 Telecommunications         Transportation & Logistics 
    ##                                 35                                  8

There are 624 observations with no `Industry`. We will replace them with
NA.

``` r
#Replace blanks with NA
ds_jobs["Industry"] <- na_if(ds_jobs["Industry"], "")
```

``` r
table(ds_jobs$City)[1:10]
```

    ## 
    ##                 Addison     Adelphi     Alameda      Albany  Alexandria 
    ##           6           6           2           3           1          46 
    ##       Allen Andrews AFB    Angleton   Annandale 
    ##           3           1           2           1

There are 6 observations with no `City`. We will replace them with NA.

``` r
#Replace blanks with NA
ds_jobs["City"] <- na_if(ds_jobs["City"], "")
```

## Data Types

``` r
sapply(ds_jobs, class)
```

    ##   Job_title     Company       State        City  Min_Salary  Max_Salary 
    ## "character" "character" "character" "character"   "integer"   "integer" 
    ##    Job_Desc    Industry      Rating Date_Posted Valid_until    Job_Type 
    ## "character" "character"   "numeric" "character" "character" "character" 
    ##      Region 
    ## "character"

It looks like all the data types are correct except for `Date_Posted`
and `Valid_until` which should be dates.

``` r
ds_jobs$Date_Posted <- as.Date(ds_jobs$Date_Posted, format = "%Y-%m-%d")  
ds_jobs$Valid_until <- as.Date(ds_jobs$Valid_until, format = "%Y-%m-%d")  
```

## Incorrect Values

``` r
unique(ds_jobs$State)
```

    ##  [1] "NY"    "NJ"    "CA"    "KY"    "TX"    "Texas" "TN"    "VA"    "MD"   
    ## [10] "DC"    "NC"    NA

There are some values in which the state value is `Texas` instead of
`TX`.

``` r
ds_jobs <- ds_jobs %>% mutate(State = case_when(
                              State == "Texas" ~ "TX",
                              T ~ State
))
```

There are some misspelled values for `City`.

``` r
#cleaning some city names
ds_jobs <- ds_jobs %>% mutate(City = case_when(
                              City == "Mc Lean" ~ "McLean",
                              City == "Crystal City, state=Virginia, Virginia" ~ "Crystal City",
                              T ~ City
))

#made alteration to add state to crystal city that was altered above
ds_jobs <- ds_jobs %>% 
  mutate(State = ifelse(City == 'Crystal City' & is.na(State), 'VA', State))
```

# Data Wrangling

## Metro Area

To make sense of the location of each job, we are grouping specific
cities into metro areas.

``` r
#create data frame of cities and their metro areas
City <- c("College Station", "Washington", "Dallas", "San Antonio","Houston", "Austin","San Francisco","New York", "Brooklyn", "Staten Island", "Bronx", "Maspeth", "Rego Park", "Lynbrook", "Mamaroneck", "Williston Park", "Fort Lee", "Jersey City", "Paramus", "West New York", "West Orange", "Addison", "Allen", "Angleton", "Arlington, TX", "Carrollton", "Denton", "Forney", "Fort Sam Houston", "Fort Worth", "Freeport", "Frisco", "Galveston", "Grapevine", "Harlingen", "Irving", "Kemah", "Kyle", "Leander", "Lewisville", "Pharr", "Plano", "Randolph A F B", "Richardson", "Round Rock", "Spring", "The Woodlands", "University Park", "Webster", "Weslaco", "Westlake", "Alameda", "Belmont","Berkeley", "Brisbane", "Burlingame", "Dublin", "Emeryville", "Fremont", "Hayward", "Hercules", "Menlo Park", "Novato", "Oakland", "Palo Alto", "Redwood City", "San Bruno", "San Carlos", "San Leandro", "San Mateo", "San Rafael", "San Ramon", "Sausalito", "South San Francisco", "Stanford", "Union City", "Woodside", "Annandale", "Arlington, VA", "Burke", "Centreville", "Chantilly", "Crystal City", "Fairfax", "Falls Church", "Fort Belvoir", "Herndon", "McLean", "Reston", "Rosslyn", "Sterling", "Tysons", "Vienna", "Adelphi", "Andrews AFB", "Annapolis Junction", "Beltsville", "Bethesda", "Bowie", "Chevy Chase", "College Park", "Fort Meade", "Fulton", "Gaithersburg", "Germantown", "Greenbelt", "Lanham", "North Bethesda", "Rockville", "Silver Spring", "Suitland", "White Oak", "Foster City", "Concord", "Richmond", "Albany", "Corpus Christi", "Alexandria", "Marshall", "Greenville", "Roanoke", "El Paso", "Lubbock", "Huntsville", "Italy", "Belton", "Comanche Village I", "Bryan", "Burnet", "Germany", "Springfield", "Columbia", "Laurel") 

metro_area <- c("College Station","Washington", "Dallas", "San Antonio", "Houston", "Austin","San Francisco","New York", "New York", "New York","New York", "New York","New York", "New York","New York", "New York","New York", "New York","New York", "New York", "New York", "Dallas", "Dallas", "Houston", "Dallas", "Dallas", "Dallas", "Dallas","San Antonio", "Dallas", "Houston", "Dallas", "Houston", "Dallas", "Brownsville", "Dallas", "Houston", "Austin", "Austin", "Dallas", "Brownsville", "Dallas", "San Antonio", "Dallas", "Austin", "Houston", "Houston", "Dallas", "Houston", "Brownsville", "Dallas", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "San Francisco", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "Washington", "San Francisco", "San Francisco", "San Francisco","San Francisco", "Corpus Christi", "Washington", "Marshall", "Dallas", "Dallas", "El Paso", "Lubbock", "Huntsville", "Dallas", "Killeen", "Killeen", "College Station", "Killeen", "Lufkin", "Washington", "Washington", "Washington")

metro_df <- data.frame(City,metro_area)

#add the metro area to the data set
ds_jobs <- left_join(ds_jobs, metro_df, by = "City")
```

There are two Arlington’s in the data set so we need to clean up these
two metro areas based on state.

``` r
ds_jobs <- ds_jobs  %>% mutate(metro_area = case_when(
                    City == "Arlington" & State == "TX" ~ "Dallas",
                    City == "Arlington" & State == "VA" ~ "Washington",
                    T ~ metro_area
))
```

There are still a few missing values for metro area.

``` r
y <- ds_jobs[is.na(ds_jobs$metro_area),]

y %>% select(City, State, metro_area)
```

    ##          City State metro_area
    ## 1375 Florence    KY       <NA>
    ## 1826     <NA>    TX       <NA>
    ## 1862     <NA>    TX       <NA>
    ## 2029     <NA>    TX       <NA>
    ## 2163  Chennai    TN       <NA>
    ## 2245     <NA>    TX       <NA>
    ## 2246     <NA>    TX       <NA>
    ## 2329    Paris    TX       <NA>
    ## 2375     <NA>    TX       <NA>
    ## 2450  Raleigh    NC       <NA>

The following values remain:

  - Florence, KY
  - Chennai, TN
  - Paris, TX
  - Raleigh, NC
  - 6 NAs

`KY`, `TN` and `NC` are not close to the other regions of the data and
`Paris, TX` was not near any metro areas in the cost of living data set.
We will remove these values from further analysis based on metro areas.

This chunk creates a csv with locations of cities and distance from
state group centers

``` r
#Find unique cities
city_counts <- ds_jobs %>% 
  mutate(Location = paste0(City, ', ', State),
         metro_location = case_when(metro_area == 'Washington' ~ 'Washington, DC',
                                    metro_area == 'New York' ~ 'New York, NY',
                                    T ~ paste0(metro_area, ', ', State))) %>% 
  group_by(City, State, Location, metro_area, metro_location) %>% 
  summarise(count = n()) %>% arrange(-count)

#Remove observations with no city (there are only 6 such postings)
city_counts <- city_counts[!is.na(city_counts$metro_area),]
city_counts <- city_counts[,-4]

#load package to generate locations for each city
library(ggmap)
register_google('YOUR CODE HERE')
locations_city <- mutate_geocode(city_counts, Location)
locations_metro <- mutate_geocode(city_counts, metro_location)

city_locations <- cbind(locations_city, locations_metro[, c(6,7)])

names(city_locations)[6:9] <- c('lon_city', 'lat_city', 'lon_metro', 'lat_metro')

job_concentrations <- city_locations %>% 
  group_by(metro_location) %>% 
  summarise(count, 
            job_concentration = count/sum(count))

city_data <- city_locations %>% left_join(job_concentrations, by = c('metro_location', 'count')) %>% unique()

weighted_locations <- city_data %>% 
  group_by(metro_location) %>% 
  summarise(avg_lon_metro_weighted = sum(job_concentration*lon_city),
            avg_lat_metro_weighted = sum(job_concentration*lat_city))

city_data <- city_data %>% left_join(weighted_locations, by = 'metro_location')

#join job concentration/average location data back into main city df
city_locations <- cbind(city_locations, job_concentrations)

#create new column to determine distance from average for each city
library(geosphere)
city_data <- city_data %>% 
  mutate(reg_dist = distm(c(lon_city, lat_city), c(lon_metro, lat_metro), fun = distHaversine)/1609,
         weight_dist = distm(c(lon_city, lat_city), 
                             c(avg_lon_metro_weighted, avg_lat_metro_weighted), fun = distHaversine)/1609)

city_data <- city_data %>% 
  filter(!metro_location %in% c('Brownsville, TX', 
                                'College Station, TX', 
                                'Corpus Christi, TX',
                                'El Paso, TX',
                                'Huntsville, TX',
                                'Killeen, TX',
                                'Lubbock, TX',
                                'Lufkin, TX',
                                'Marshall, TX'),
         weight_dist <= 50)

ds_jobs <- ds_jobs %>% 
  filter(paste0(City, ', ', State) %in% unique(city_data$Location)) %>% 
  left_join(city_data, by = c('City', 'State')) %>% 
  select(-Region, -metro_area)
```

\#edit From the above, we see that California, DC, and New York all seem
to be reliable metro areas, all with less than 30 miles maximum from the
average location. Texas on the other hand does not seem to be
represented by a single metro area. Further inspection of the cities in
Texas indicates several large concentrated cities that are not close to
each other.

## Cost of Living Index

Now we want to add the cost of living index for each observation based
on metro area.

[Cost of Living Index](https://advisorsmith.com/data/coli/)

``` r
#reading in the COI data
col_index <- read.csv("advisorsmith_cost_of_living_index.csv")

#create a city/state variable to join the tables
col_index$city_state <- paste0(col_index$City, ", ", col_index$State)

#adding the COI to the ds_jobs data
ds_jobs <- left_join(ds_jobs, col_index, by = c("metro_location" = "city_state"))
ds_jobs <- ds_jobs %>% select(-State.y, -City.y) %>% 
  select(COI = Cost.of.Living.Index, State = State.x, City = City.x, everything())
```

# Job Category

From Job\_title

``` r
data <- grep("data", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
engineer <- grep("engineer", data, ignore.case = TRUE, value = TRUE)
analyst <- grep("anal", data, ignore.case = TRUE, value = TRUE)
ds <- grep("scien", data, ignore.case = TRUE, value = TRUE)
ml <- grep("machine", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
stats <- grep("statistic", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
model <- grep("model", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
consult <- grep("consult", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
bio <- grep("bio", ds_jobs$Job_Title, ignore.case = TRUE, value = TRUE)
comp <- grep("computer scie", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
other_analyst <- grep("analy", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
research_scientist <- grep("research sci", ds_jobs$Job_title, ignore.case = TRUE, value = TRUE)
```

From Job\_Desc

``` r
data_desc <- grep("data", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
engineer_desc <- grep("data engineer", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
analyst_desc <- grep("data analy", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
ds_desc <- grep("data scien", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
ml_desc <- grep("machine", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
stats_desc <- grep("statistic", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
bio_desc <- grep("biology", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
comp_desc <- grep("computer science", ds_jobs$Job_Desc, ignore.case = TRUE, value = TRUE)
```

``` r
ds_jobs <- ds_jobs %>%
  mutate(job_category = NA)
  
  
for (i in seq_along(ds_jobs$Job_title)) {
  if ((ds_jobs$Job_title[[i]] %in% engineer) && (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Data Engineer"
  } else if ((ds_jobs$Job_title[[i]] %in% analyst) && (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Data Analyst"
  } else if ((ds_jobs$Job_title[[i]] %in% ds) && (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Data Scientist"
  } else if ((ds_jobs$Job_title[[i]] %in% ml) && (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Machine Learning"
  } else if ((ds_jobs$Job_title[[i]] %in% stats) && (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Statistics"
  } else if((ds_jobs$Job_Desc[[i]] %in% engineer_desc) && 
            (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Data Engineer"
  } else if ((ds_jobs$Job_Desc[[i]] %in% analyst_desc) && 
             (is.na(ds_jobs$job_category[[i]]))) {
            ds_jobs$job_category[[i]] = "Data Analyst"
  } else if ((ds_jobs$Job_Desc[[i]] %in% ds_desc) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Data Scientist"
  } else if ((ds_jobs$Job_Desc[[i]] %in% ml_desc) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Machine Learning"
  } else if ((ds_jobs$Job_Desc[[i]] %in% stats_desc) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Statistics"
  } else if ((ds_jobs$Job_title[[i]] %in% consult) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Consultant"
  } else if ((ds_jobs$Job_title[[i]] %in% bio) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Biology"
  } else if ((ds_jobs$Job_Desc[[i]] %in% bio_desc) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Biology"
  } else if ((ds_jobs$Job_title[[i]] %in% comp) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Computer Scientist"
  } else if ((ds_jobs$Job_Desc[[i]] %in% comp_desc) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Computer Scientist"
  } else if ((ds_jobs$Job_title[[i]] %in% other_analyst) &&
             (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Other Analyst"
  } else if ((ds_jobs$Job_title[[i]] %in% model) && (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Machine Learning"
  } else if ((ds_jobs$Job_title[[i]] %in% research_scientist) &&
             (is.na(ds_jobs$job_category[[i]]))) {
             ds_jobs$job_category[[i]] = "Research Scientist"
  }
}
```

# Master Data set

Here we will save the final cleaned and merged data set to use for our
analysis.

``` r
#clean all the variable names for consistency
ds_jobs <- clean_names(ds_jobs)

write.csv(ds_jobs, "ds_jobs.csv")
```
