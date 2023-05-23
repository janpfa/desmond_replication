---
title: 'Replication Desmond 2015'
author: "Jan Pfänder"
date: "2023-05-08"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


```r
# load required packages
library("tidyverse")
library("readxl") 
library("mice")
```

This is a script to replicate a finding in Desmond 2015. 

> *"Claim: ... renters who experienced a forced move relocate to poorer ... neighborhoods than those who move under less-demanding circumstances. (p. 751)"*

## Data

I rely on two main data sets

- `mars`: The MARS survey, mainly used to compute the independent variable (`forced_move`) and controls. This is a merged data set from several files, all downloaded from [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BLUU3U). 
- `acs`: American Community Survey (ACS), used to compute the outcome variable (`poverty`). 2010 ACS 5-year estimates of the percentage of families in poverty in a certain neighborhood (by Census Block Group in Wisconsin). Data is downloaded from ACS legacy data file download site [at this link](https://www2.census.gov/acs2010_5yr/summaryfile/2006-2010_ACSSF_By_State_By_Sequence_Table_Subset/Wisconsin/Tracts_Block_Groups_Only/20105wi0047000.zip). Geographic identifiers are downloaded from [here](https://www2.census.gov/acs2010_5yr/summaryfile/2006-2010_ACSSF_By_State_By_Sequence_Table_Subset/Wisconsin/Tracts_Block_Groups_Only/g20105wi.csv). 

## Variables

The variables we need to replicate the analysis are:

- `poverty_current`: Neighborhood poverty rate
- `move_reason`: Reason for moving (forced, responsive, voluntary)
- `race`: Race (black, hispanic, other)
- `age` 
- `education_less_than_highschool` : Education (Less Than High School Education)
- `education_highschool` : Education (High School/GED)
- `housing_asstiance`: Housing Assistance in Past Residence
- `single_mom` :  Single-Mother Household in Past Residence
- `criminal_record`: Criminal Record Before Move
- `recent_child`: Had a Child in Previous 2 Years
- `relationship_dissolution` : Relationship Dissolution Before Move in Previous 2 Years 
- `job_loss`: Job Loss Before Move in Previous 2 Years 
- `benefits_loss`: Public Benefits Sanction Before Move in Previous 2 Years 
- `poverty_previous`: Lagged Dependent Variable



### MARS


```r
# read data
mars <- read.csv("data/MARS_data.csv")
```

About the data structure the "User's guide" of Mars provides explanations: 

> *"Accordingly, the data structure entails previous addresses or “spells” clustered within respondents. The variable csid is the unique case identifier, and the variable hhindex_new indicates which “spell” or previous address to which data refer. All cross-sectional data are housed in rows where hhindex_new = 0, referring to a respondent’s address at the time of the survey (her or his “current address”). All retrospective data are housed in rows where hhindex_new = 1, 2, … n, where n refers to the number of addresses at which a respondent has lived in the previous two years. An hhindex_new value of 1 refers to a respondent’s last address; an hhindex_new value of 2 refers to the address the respondent lived in prior to the former address, and so on. Here, then, a “spell” corresponds to addresses, not time points."*

I will first select and re-name key variables from the data frame. When not already available in the data set, I then construct target variables one by one. 

#### Select and rename variables

I start by renaming and selecting variables that we need to compute our target variables. 


```r
# Rename and select variables necessary to compute analysis variables
mars <- mars %>% rename(
  # Identification and other helper variables
  ###
  # Date of last move
  date_last_move_month = A2_m,
  date_last_move_year = A2_y,
  # ID
  id_case = csid,
  id_housing = hhindex_new, 
  # Recent move (to filter the relevant subset of the data)
  recent = recentmove,
  # Current Address Block Group (FIPS)
  current_address = addr1_bg, 
  # Previous Residence: Block Group (FIPS)
  previous_address = past_addr_bg,
  # Survey weights
  weight = weight,
  # reason for moving (forced, responsive, voluntary), 
  ###
  # Were you, or a person you were staying with, evicted? (1)
  forced_1 = H1a,
  # Did you, or a person you were staying with, receive an eviction notice while living at this place? (2)
  forced_2 = H5a,
  # Did you, or a person you were staying with, receive an eviction notice while living at this place? (3)
  forced_3 = H14a,
  # Did you move away from this place because you, or a person you were stay with, missed a rent payment and thought that if you didn’t move you would be evicted? (4)
  forced_4 = H16a, 
  # Did you move away from this place because the city condemned the building? (5)
  forced_5 = H18a,
  # Did you move away because the landlord went into foreclosure? (6d)
  forced_6 = H19a_d, 
  # Did you move away because the landlord raised the rent? (6a)
  responsive_1 = H19a_a, 
  # Did you move away because the neighborhood was too dangerous? (6b)
  responsive_2 = H19a_b, 
  # Did you move away because the landlord wouldn't fix anything? (6c)
  responsive_3 = H19a_c, 
  # race 
  ###
  # What is your race? (several variables, in case of several answers)
  race_1 = J16_a,
  race_2 = J16_b,
  race_3 = J16_c,
  race_4 = J16_d,
  race_5 = J16_e,
  # Are you Hispanic?
  race_6 = J15,
  # age 
  ###
  # How old are you?
  age = J20,
  # education
  ###
  # What is the highest level of schooling you have completed?
  education = J12,
  # housing_assistance
  ###
  # Did the federal, state, or local government help to pay your rent?
  housing_assistance = F9a,
  # single_mom 
  ### 
  # R'S GENDER
  single_mom_gender = J41,
  # When you lived at this place, did any adults over the age of 18 live with you?
  single_mom_alone = G2a, 
  # Number of children < 18 in HH (sum of E8* and E16)
  single_mom_kids = numkidsHH,
  # criminal_record 
  ###
  # Do you have a criminal record?
  criminal_record = J38,
  # Convicted of crime: Month
  criminal_record_month = J39_a,
  # Convicted of crime: Year
  criminal_record_year = J39_b
) %>% 
  rename_with(
    # recent_child
    ###
    # Child A-K:  How old is [fill heshe]? (rename all variables from a to k)
    # YEARS
    ~ str_replace(.x, "E6", "recent_child_year"), starts_with("E6") & ends_with("_a")) %>% 
  rename_with(
    # MONTHS IF LESS THAN 1 YEAR
    ~ str_replace(.x, "E6", "recent_child_months"), starts_with("E6") & ends_with("_b")) %>% 
  rename_with(
    # relationship_dissolution
    ###
    # In what month and year did the most recent serious relationship end?
    # month
    ~ str_replace(.x, "J25", "relationship_dissolution_month"), starts_with("J25") & ends_with("_a")) %>% 
  rename_with(
    # year
    ~ str_replace(.x, "J25", "relationship_dissolution_year"), starts_with("J25") & ends_with("_b")) %>% 
  rename_with(
    # job_loss
    ###
    # In what month and year were you most recently laid off or fired from a job?
    # month
    ~ str_replace(.x, "J28", "job_loss_month"), starts_with("J28") & ends_with("_a")) %>% 
  rename_with(
    # year
    ~ str_replace(.x, "J28", "job_loss_year"), starts_with("J28") & ends_with("_b")) %>% 
  rename_with(
    # benefits_loss
    ###
    # In what month and year were your public assistance benefits most recently stopped or sanctioned?
    # month
    ~ str_replace(.x, "J32", "benefits_loss_month"), starts_with("J32") & ends_with("_a")) %>% 
  rename_with(
    # year
    ~ str_replace(.x, "J32", "benefits_loss_year"), starts_with("J32") & ends_with("_b")) %>% 
  # retain only variables of interest
  select(contains("forced"), contains("responsive"), contains("race"), contains("address"), contains("weight"),
                                  contains("age"), contains("education"), contains("housing_assistance"), 
                                  contains("single_mom"), contains("criminal_record"), contains("recent_child"), 
                                  contains("relationship_dissolution"), contains("job_loss"), 
                                  contains("benefits_loss"), contains("date_last_move"), contains("id_case"), 
                                  contains("id_housing"), contains("recent"))
```


```r
# Make strings more compatible
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(everything(), tolower)
         )
```

#### Re-shape and subset the data

I want to filter out the individuals that moved within two years before the survey. 


```r
# For now, several rows per id_case (because several houses), but only one 
# row (the first) per id_case contains the value of recent. 
# Make sure all rows per id_case contain the value of recent.
mars <- mars %>% 
  group_by(id_case) %>% 
  # fill in all rows within id_case with the first row value
  fill(recent, .direction = "down") %>% 
  # remove grouping
  ungroup %>% 
  filter(recent == 1)
```

The addresses are marked by `id_housing`. The current address has the value `0`, the previous one has the value `1`. The row of the current address contains the values for most variables of interest. The row of the previous address contains information about the reason of the move. 

I suppose there has only been one move within the period of interest, so I consider only the last previous address.

I re-shape the data so that there is only one row per `id_case`. 


```r
# extract info for current address
current_adress <- mars %>% filter(id_housing == 0) %>% 
  # everything BUT moving reasons
  select(-c(
    starts_with(c("forced", "responsive")), 
    housing_assistance, 
    single_mom_alone, previous_address
  )
  )

# extract info for previous address
previous_adress <- mars %>% filter(id_housing == 1) %>% 
  # moving reasons
  select(id_case, starts_with(c("forced", "responsive")), 
         housing_assistance, single_mom_alone, 
         previous_address)

# re-unite data
mars <- left_join(current_adress, previous_adress, by = "id_case")
```

#### Build target variables

##### Moving reason: forced and responsive move

First, we clean all variables. 


```r
# check levels
mars %>% select(starts_with(c("forced", "responsive"))) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count()
```

```
## # A tibble: 55 × 3
## # Groups:   name, value [55]
##    name     value     n
##    <chr>    <chr> <int>
##  1 forced_1 ""       30
##  2 forced_1 "-2"      2
##  3 forced_1 "-4"     10
##  4 forced_1 "no"    497
##  5 forced_1 "yes"    40
##  6 forced_1  <NA>     1
##  7 forced_2 ""       66
##  8 forced_2 "-2"      7
##  9 forced_2 "-4"     14
## 10 forced_2 "no"    458
## # ℹ 45 more rows
```

```r
# mini cleaning function
clean <- function (.x) case_when(.x == "yes"~ 1, 
                      .x == "no" ~ 0, 
                      TRUE ~ NA)

# check cleaning method
mars %>% 
  mutate(
    across(starts_with(c("forced", "responsive")), 
           # apply cleaning function
           clean, 
           .names = "{col}_cleaned"
    )
  ) %>% 
  select(starts_with("forced_1"))
```

```
## # A tibble: 580 × 2
##    forced_1 forced_1_cleaned
##    <chr>               <dbl>
##  1 no                      0
##  2 no                      0
##  3 no                      0
##  4 no                      0
##  5 no                      0
##  6 no                      0
##  7 no                      0
##  8 no                      0
##  9 no                      0
## 10 yes                     1
## # ℹ 570 more rows
```

```r
# apply cleaning
mars <- mars %>% 
  mutate(
    across(starts_with(c("forced", "responsive")), 
           # apply cleaning function
           clean
    )
  )
```

We have several`forced_...` and `responsive_...` variables. For each of these two groups, we want a summary variable that indicates `TRUE` if any of the variables within the group yield `TRUE`. 


```r
# Create target variables
mars <- mars %>%
  rowwise() %>%
  # Whenever there is at least one "yes" (= 1) response, code as 1,
  # otherwise code 0
  mutate(forced = sum(c_across(starts_with("forced")), na.rm = TRUE),
         forced = ifelse(forced > 0, TRUE, FALSE),
         responsive = sum(c_across(starts_with("responsive")), na.rm = TRUE),
         responsive = ifelse(responsive > 0, TRUE, FALSE)
  ) %>%
  ungroup() %>% 
  mutate(
    # so far, overlap is possible between forced and responsive. 
    # I will now prioritize forced
    move_reason = case_when(forced == TRUE ~ "forced", 
                            responsive == TRUE ~ "responsive", 
                            TRUE ~ "voluntary/NA"),
    # set different baseline level
    move_reason = relevel(as.factor(move_reason), ref = "voluntary/NA"))
```

##### Race

Clean and Re-code NA's


```r
# check levels
mars %>% select(starts_with("race")) %>% 
  pivot_longer(everything()) %>% 
  group_by(name, value) %>% 
  count()
```

```
## # A tibble: 30 × 3
## # Groups:   name, value [30]
##    name   value                                           n
##    <chr>  <chr>                                       <int>
##  1 race_1 "american indian or alaska native"              9
##  2 race_1 "asian"                                         5
##  3 race_1 "black or african american"                   285
##  4 race_1 "blank"                                         3
##  5 race_1 "don't know"                                    2
##  6 race_1 "native hawaiian or other pacific islander"     1
##  7 race_1 "refused"                                       2
##  8 race_1 "something else (specify)"                     74
##  9 race_1 "white"                                       199
## 10 race_2 ""                                            283
## # ℹ 20 more rows
```

```r
levels(as.factor(mars$race_6))
```

```
## [1] "blank"      "don't know" "no"         "refused"    "yes"
```


```r
# define NA values
NA_responses <- c("blank", "don't know", "refused", "-3", "")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("race"), recodeNAs)
         )
```

Respondents could indicate several races. In the analysis, 3 groups were compared: Black, Hispanic and Other. 

I assume that an individual can appear in both the Hispanic and the Black Group. For the outcome variable race, I will prioritize black, then hispanic over other.


```r
mars <- mars %>% 
  mutate(
    race_black = ifelse(
    # if all NAs
    if_all(starts_with("race"), is.na),
    # code NA
    NA,
    # else code 1 if black and 0 if otherwise
    if_any(starts_with("race") & !ends_with("6"),
           ~str_detect(., "black")
           )
    ),
    race_hispanic = ifelse(if_all(starts_with("race"), is.na),
                             NA,
                           ifelse(race_6 == "yes", TRUE, FALSE)
                           ), 
    race_other = ifelse(if_all(starts_with("race"), is.na),
                             NA,
                        (ifelse(
                          (race_black == FALSE | is.na(race_black)) & 
                            (race_hispanic == FALSE | is.na(race_hispanic)), 
                          TRUE, FALSE)
                         )
                        )
  ) %>% 
  mutate(
    # so far, overlap is possible between forced and responsive. 
    # I will now prioritize forced
    race = case_when(race_black == TRUE ~ "black", 
                     race_hispanic == TRUE ~ "hispanic", 
                     race_other == TRUE ~ "other",
                     TRUE ~ NA),
    # set different baseline level
    race= relevel(as.factor(race), ref = "other"))

# # check    
# mars %>% filter((race_black == FALSE | is.na(race_black)) & (race_hispanic == FALSE | is.na(race_hispanic))) 
```

##### Age


```r
# check levels
table(mars$age, useNA = "always")
```

```
## 
##              18      19      20      21      22      23      24      25      26 
##     277       7       6       7       9      14      14      12       5      14 
##      27      28      29      30      31      32      33      34      35      36 
##      17      19      10      10       6      13      10       9       3       6 
##      37      38      39      40      41      42      43      44      45      46 
##       4      10       7       8       1       9       5       5       7       2 
##      47      48      49      50      51      52      53      54      55      56 
##       7       1       6       3       3       4       4       6       1       1 
##      57      58      59      61      62      63      67      68      77      91 
##       3       2       3       1       1       1       1       2       2       1 
## refused    <NA> 
##       1       0
```

```r
levels(as.factor(mars$age))
```

```
##  [1] ""        "18"      "19"      "20"      "21"      "22"      "23"     
##  [8] "24"      "25"      "26"      "27"      "28"      "29"      "30"     
## [15] "31"      "32"      "33"      "34"      "35"      "36"      "37"     
## [22] "38"      "39"      "40"      "41"      "42"      "43"      "44"     
## [29] "45"      "46"      "47"      "48"      "49"      "50"      "51"     
## [36] "52"      "53"      "54"      "55"      "56"      "57"      "58"     
## [43] "59"      "61"      "62"      "63"      "67"      "68"      "77"     
## [50] "91"      "refused"
```

```r
# clean
mars <- mars %>% 
  # For now, age is character variable. 
  # This turns all non-numeric values into NAs
  mutate(age = as.numeric(age)) 
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `age = as.numeric(age)`.
## Caused by warning:
## ! NAs introduced by coercion
```

##### Education


```r
# inspect levels
table(mars$education, useNA = "always")
```

```
## 
##          2-year, community college degree 
##                                        34 
##             4-year college degree or more 
##                                        49 
##                                         8 
##                                        39 
##                                     blank 
##                                         2 
##                                don't know 
##                                         2 
## elementary school (1st through 8th grade) 
##                                        21 
##                                       ged 
##                                        36 
##                       high school diploma 
##                                       156 
##                                   refused 
##                                         3 
##                   some college, no degree 
##                                       140 
## some high school (9th through 11th grade) 
##                                        98 
##                                      <NA> 
##                                         0
```

```r
levels(as.factor(mars$education))
```

```
##  [1] "2-year, community college degree"         
##  [2] "4-year college degree or more"            
##  [3] "8"                                        
##  [4] "blank"                                    
##  [5] "don't know"                               
##  [6] "elementary school (1st through 8th grade)"
##  [7] "ged"                                      
##  [8] "high school diploma"                      
##  [9] "refused"                                  
## [10] "some college, no degree"                  
## [11] "some high school (9th through 11th grade)"
```

```r
# define NA values
NA_responses <- c("blank", "don't know", "8", "refused")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(education, recodeNAs)
         )
```

The analysis distinguishes two categories: Less Than High School Education, High School/GED.

I assume the baseline for - each of the two variables respectively - are all other levels. 

```r
less_than_highschool <- c("elementary school (1st through 8th grade)")
highschool <- c("high school diploma", "ged")

mars <- mars %>% 
  mutate(education_less_than_highschool = ifelse(education %in% less_than_highschool, 
                                                 TRUE, FALSE), 
         education_highschool = ifelse(education %in% highschool, 
                                                 TRUE, FALSE)
         ) 
```

##### Housing Assitance


```r
# inspect levels
table(mars$housing_assistance, useNA = "always")
```

```
## 
##        -1   -2   -4   no  yes <NA> 
##   26   12    9   44  460   28    1
```

```r
levels(as.factor(mars$housing_assistance))
```

```
## [1] ""    "-1"  "-2"  "-4"  "no"  "yes"
```


```r
# define valid values
valid_responses <- c("no", "yes")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (!x %in% valid_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(housing_assistance, recodeNAs), 
         housing_assistance = case_when(housing_assistance == "yes" ~ TRUE, 
                                        housing_assistance == "no" ~ FALSE, 
                                        TRUE ~ NA
                                        )
         )
```

##### Recent Child


```r
# check levels
levels(as.factor(mars$recent_child_monthsa_b))
```

```
## [1] ""        "1"       "11"      "2"       "5"       "7"       "8"      
## [8] "blank"   "refused"
```

```r
# # for more extensive check
# mars %>% select(starts_with("recent_child")) %>% 
#   pivot_longer(everything()) %>% 
#   group_by(name, value) %>% 
#   count()
```

```r
# define NA values
NA_responses <- c("", "refused", "blank")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("recent_child"), recodeNAs), 
         # transform to numeric
         across(starts_with("recent_child"), as.numeric)
         )
```

```
## Warning: There were 9 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `across(starts_with("recent_child"), as.numeric)`.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 8 remaining warnings.
```
We calculate the exact age of each child. 

```r
# calculate exact age combining month and year
exact_age <- mars %>% pivot_longer(starts_with("recent_child"), 
                      names_to = "child", 
                      values_to = "age_child") %>% 
  # change the names of the variables slightly
  mutate(child = gsub("(year|months)", "\\1_", child), 
         child = sub("_[^_]*$", "", child)) %>%
  # split at last underscore
  separate(col = child, into = c("date", "pair"), 
           sep = "_(?!.*_)", extra = "merge")  %>% 
  select(id_case, date, pair, age_child) %>% 
  pivot_wider(names_from = date, values_from = age_child) %>% 
  # calculate all in months (multiply years by 12)
  mutate(recent_child_year = recent_child_year*12) %>% 
  rowwise() %>% 
  # calculate exact age
  mutate(recent_child_age = sum(c_across(starts_with("recent_child")), na.rm = TRUE),
         # recode NAs
         recent_child_age = ifelse(recent_child_age == 0, NA, recent_child_age)
         )
```

We retain only the age of the youngest child (which determines the variable, namely whether there was at least one child within the last two years). If that value is smaller than or equal to 24 (months, i.e. two years) I code `TRUE` for the target variable `recent_child`. 


```r
# compute age of youngest child
recent_child <- exact_age %>%  
  group_by(id_case) %>% 
    summarize(youngest_child_age = ifelse(all(is.na(recent_child_age)), 
                                          NA, 
                                          min(recent_child_age, na.rm = TRUE))) %>% 
  # create target variable
  mutate(recent_child = ifelse(youngest_child_age <= 24, TRUE, FALSE)) %>% 
  select(id_case, recent_child)
  

# merge recent_child back into our data frame
mars <- left_join(mars, recent_child)
```

```
## Joining with `by = join_by(id_case)`
```

##### Single mom

We have to check several variables: 

- Is respondent female? (`single_mom_gender == "female"`)
- only adult in household (`single_mom_gender == 0`)
- children under 18 (`single_mom_kids > 0`)


```r
# inspect levels
table(mars$single_mom_gender, useNA = "always")
```

```
## 
##  blank female   male   <NA> 
##      1    358    221      0
```

```r
# inspect levels
table(mars$single_mom_alone, useNA = "always")
```

```
## 
##         0  yes <NA> 
##   48  214  317    1
```

```r
# inspect levels
table(mars$single_mom_kids, useNA = "always")
```

```
## 
##    0    1   13    2    3    4    5    6    7 <NA> 
##  280  106    1   81   60   33   11    5    1    2
```


```r
# define NA values
NA_responses <- c("", "blank")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("single_mom"), recodeNAs), 
         # transform to numeric
         single_mom_kids = as.numeric(single_mom_kids)
         )
```


```r
# create single_mom variable
mars <- mars %>% 
  mutate(single_mom = ifelse(single_mom_gender == "female" & 
                               single_mom_alone == 0 &
                               single_mom_kids > 0, 
                             TRUE, 
                             ifelse(if_any(starts_with("single_mom"), is.na),
                             NA, FALSE)
                               )
         ) 
```

##### Time last move

Many control variables capture a situation *before the last move*. We make an exact measure in years.


```r
# inspect
table(mars$date_last_move_year, useNA = "always")
```

```
## 
## 2007 2008 2009 2010 2011 <NA> 
##   10  104  211  141  114    0
```

```r
table(mars$date_last_move_month, useNA = "always")
```

```
## 
##    1   10   11   12    2    3    4    5    6    7    8    9 <NA> 
##   27   70   56   37   29   35   28   30   77   49   81   61    0
```



```r
# make variable of exact point of moving measured in years
mars <- mars %>% mutate(
  across(starts_with("date_last_move"), as.numeric),
  date_last_move = date_last_move_year + date_last_move_month/12) 
```


##### Criminal Record

We have to check if there was a criminal record *before the last move*. 


```r
# year
levels(as.factor(mars$criminal_record_year))
```

```
##  [1] ""        "1960"    "1974"    "1975"    "1976"    "1978"    "1979"   
##  [8] "1981"    "1983"    "1985"    "1988"    "1989"    "1990"    "1991"   
## [15] "1992"    "1993"    "1994"    "1995"    "1996"    "1997"    "1998"   
## [22] "1999"    "2000"    "2001"    "2002"    "2003"    "2004"    "2005"   
## [29] "2006"    "2007"    "2008"    "2009"    "2010"    "2011"    "blank"  
## [36] "refused"
```

```r
# month
levels(as.factor(mars$criminal_record_month))
```

```
##  [1] ""           "1"          "10"         "11"         "12"        
##  [6] "2"          "3"          "4"          "5"          "6"         
## [11] "7"          "8"          "9"          "blank"      "don't know"
## [16] "refused"
```


```r
# define NA values
NA_responses <- c("blank", "don't know", "refused", "")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("criminal"), recodeNAs)
         )

mars %>% select(starts_with("criminal"), starts_with("interview")) %>% filter(criminal_record == "yes")
```

```
## # A tibble: 107 × 3
##    criminal_record criminal_record_month criminal_record_year
##    <chr>           <chr>                 <chr>               
##  1 yes             <NA>                  2002                
##  2 yes             3                     1992                
##  3 yes             <NA>                  2004                
##  4 yes             <NA>                  1997                
##  5 yes             5                     1994                
##  6 yes             5                     2006                
##  7 yes             1                     1974                
##  8 yes             <NA>                  1985                
##  9 yes             4                     1996                
## 10 yes             <NA>                  2002                
## # ℹ 97 more rows
```


```r
# make variable of exact point of moving measured in years
mars <- mars %>% mutate(
  across(starts_with("criminal_record_"), as.numeric),
  criminal_record_exact = criminal_record_year + criminal_record_month/12,
  # create target variable
  criminal_record = ifelse(date_last_move - criminal_record_exact <= 2 &
                                date_last_move - criminal_record_exact >= 0, 
                              TRUE,
                              FALSE)
  ) 
```

#####  Relationship dissolution


```r
# check levels
levels(as.factor(mars$relationship_dissolution_yeara_b))
```

```
##  [1] ""           "2001"       "2006"       "2007"       "2008"      
##  [6] "2009"       "2010"       "2011"       "blank"      "don't know"
```

```r
# # for more extensive check
# mars %>% select(starts_with("relationship_dissolution")) %>% 
#   pivot_longer(everything()) %>% 
#   group_by(name, value) %>% 
#   count()
```



```r
# define NA values
NA_responses <- c("", "refused", "blank", "don't know")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("relationship_dissolution"), recodeNAs), 
         # transform to numeric
         across(starts_with("relationship_dissolution"), as.numeric)
         )
```

We calculate the exact date for each relationship ending

```r
# calculate exact age combining month and year
exact_relationship <- mars %>% pivot_longer(starts_with("relationship_dissolution"), 
                      names_to = "relationship", 
                      values_to = "date_relationship") %>% 
  # change the names of the variables slightly
  mutate(relationship = gsub("(year|month)", "\\1_", relationship), 
         relationship = sub("_[^_]*$", "", relationship)) %>%
  # split at last underscore
  separate(col = relationship, into = c("date", "pair"), 
           sep = "_(?!.*_)", extra = "merge")  %>% 
  select(id_case, date, pair, date_relationship, date_last_move) %>% 
  pivot_wider(names_from = date, values_from = date_relationship) %>% 
  # calculate all in years (divide months by 12)
  mutate(relationship_dissolution_month = relationship_dissolution_month/12) %>% 
  rowwise() %>% 
  # calculate exact date
  mutate(relationship_dissolution_date = sum(c_across(starts_with("relationship_dissolution")), na.rm = TRUE),
         # recode NAs
         relationship_dissolution_date  = ifelse(relationship_dissolution_date == 0, NA, 
                                                 relationship_dissolution_date)
         )
```

We retain only the date of the most recent relationship dissolution (which determines the variable, namely whether there was at least one dissolution within the last two years from the date of the last move).


```r
# compute age of youngest child
recent_relationship <- exact_relationship %>%  
  group_by(id_case) %>% 
    summarize(relationship_dissolution_date_most_recent = ifelse(all(is.na(relationship_dissolution_date)), 
                                          NA, 
                                          max(relationship_dissolution_date, 
                                              na.rm = TRUE))) %>% 
  select(id_case, relationship_dissolution_date_most_recent)
  

# merge recent_child back into our data frame
mars <- left_join(mars, recent_relationship) %>% 
  # create target variable
  mutate(relationship_dissolution = ifelse(
    date_last_move - relationship_dissolution_date_most_recent <= 2 &
      date_last_move - relationship_dissolution_date_most_recent >= 0,
    TRUE, FALSE))
```

```
## Joining with `by = join_by(id_case)`
```

#####  Job loss


```r
# check levels
levels(as.factor(mars$job_loss_montha_a))
```

```
##  [1] ""           "1"          "10"         "11"         "12"        
##  [6] "2"          "3"          "4"          "5"          "6"         
## [11] "7"          "8"          "9"          "don't know" "refused"
```

```r
# # for more extensive check
# mars %>% select(starts_with("job_loss")) %>% 
#   pivot_longer(everything()) %>% 
#   group_by(name, value) %>% 
#   count()
```



```r
# define NA values
NA_responses <- c("", "refused", "blank", "don't know")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("job_loss"), recodeNAs), 
         # transform to numeric
         across(starts_with("job_loss"), as.numeric)
         )
```

We calculate the exact date for each job loss.

```r
# calculate exact age combining month and year
exact_job_loss <- mars %>% pivot_longer(starts_with("job_loss"), 
                      names_to = "job_loss", 
                      values_to = "date_job_loss") %>% 
  # change the names of the variables slightly
  mutate(job_loss = gsub("(year|month)", "\\1_", job_loss), 
         job_loss = sub("_[^_]*$", "", job_loss)) %>%
  # split at last underscore
  separate(col = job_loss, into = c("date", "pair"), 
           sep = "_(?!.*_)", extra = "merge")  %>% 
  select(id_case, date, pair, date_job_loss, date_last_move) %>% 
  pivot_wider(names_from = date, values_from = date_job_loss) %>% 
  # calculate all in years (divide months by 12)
  mutate(job_loss_month = job_loss_month/12) %>% 
  rowwise() %>% 
  # calculate exact date
  mutate(job_loss_date = sum(c_across(starts_with("job_loss")), na.rm = TRUE),
         # recode NAs
         job_loss_date  = ifelse(job_loss_date == 0, NA, 
                                 job_loss_date)
  )
```

We retain only the date of the most recent job loss (which determines the variable, namely whether there was at least one job loss within the last two years from the date of the last move).


```r
# compute age of youngest child
recent_job_loss <- exact_job_loss %>%  
  group_by(id_case) %>% 
    summarize(job_loss_date_most_recent = ifelse(all(is.na(job_loss_date)), 
                                          NA, 
                                          max(job_loss_date, 
                                              na.rm = TRUE))) %>% 
  select(id_case, job_loss_date_most_recent)
  

# merge recent_child back into our data frame
mars <- left_join(mars, recent_job_loss) %>% 
  # create target variable
  mutate(job_loss = ifelse(
    date_last_move - job_loss_date_most_recent <= 2 &
      date_last_move - job_loss_date_most_recent >= 0,
    TRUE, FALSE))
```

```
## Joining with `by = join_by(id_case)`
```

#####  Benefits loss


```r
# check levels
levels(as.factor(mars$benefits_loss_montha_a))
```

```
##  [1] ""           "1"          "10"         "11"         "12"        
##  [6] "2"          "3"          "4"          "5"          "6"         
## [11] "7"          "8"          "9"          "blank"      "don't know"
```

```r
# # for more extensive check
# mars %>% select(starts_with("benefits_loss")) %>% 
#   pivot_longer(everything()) %>% 
#   group_by(name, value) %>% 
#   count()
```



```r
# define NA values
NA_responses <- c("", "refused", "blank", "don't know")
# mini function to assign NA's to values defined above
recodeNAs <- function(x) (ifelse (x %in% NA_responses, NA, x))

# recode NAs
mars <- mars %>% 
  # make sure answers written to lower case
  mutate(across(starts_with("benefits_loss"), recodeNAs), 
         # transform to numeric
         across(starts_with("benefits_loss"), as.numeric)
         )
```

We calculate the exact date for each benefits loss.

```r
# calculate exact age combining month and year
exact_benefits_loss <- mars %>% pivot_longer(starts_with("benefits_loss"), 
                      names_to = "benefits_loss", 
                      values_to = "date_benefits_loss") %>% 
  # change the names of the variables slightly
  mutate(benefits_loss = gsub("(year|month)", "\\1_", benefits_loss), 
         benefits_loss = sub("_[^_]*$", "", benefits_loss)) %>%
  # split at last underscore
  separate(col = benefits_loss, into = c("date", "pair"), 
           sep = "_(?!.*_)", extra = "merge")  %>% 
  select(id_case, date, pair, date_benefits_loss, date_last_move) %>% 
  pivot_wider(names_from = date, values_from = date_benefits_loss) %>% 
  # calculate all in years (divide months by 12)
  mutate(benefits_loss_month = benefits_loss_month/12) %>% 
  rowwise() %>% 
  # calculate exact date
  mutate(benefits_loss_date = sum(c_across(starts_with("benefits_loss")), na.rm = TRUE),
         # recode NAs
         benefits_loss_date  = ifelse(benefits_loss_date == 0, NA, 
                                 benefits_loss_date)
  )
```

We retain only the date of the most recent benefits loss (which determines the variable, namely whether there was at least one benefits loss within the last two years from the date of the last move).


```r
# compute age of youngest child
recent_benefits_loss <- exact_benefits_loss %>%  
  group_by(id_case) %>% 
    summarize(benefits_loss_date_most_recent = ifelse(all(is.na(benefits_loss_date)), 
                                          NA, 
                                          max(benefits_loss_date, 
                                              na.rm = TRUE))) %>% 
  select(id_case, benefits_loss_date_most_recent)
  

# merge recent_child back into our data frame
mars <- left_join(mars, recent_benefits_loss) %>% 
  # create target variable
  mutate(benefits_loss = ifelse(
    date_last_move - benefits_loss_date_most_recent <= 2 &
      date_last_move - benefits_loss_date_most_recent >= 0,
    TRUE, FALSE))
```

```
## Joining with `by = join_by(id_case)`
```


```r
# export cleaned file
write_csv(mars, "data/cleaned_mars.csv")
```


### American Community Survey (ACS)

#### ACS 5-year Wisconsin block groups by seq table

```r
acs_data <- read.csv("./data/e20105wi0047000.txt", header=F)
```

Add ACS column labels


```r
labels <- read_excel("./data/Seq47.xls")
colnames(acs_data) <- labels %>% colnames()
```

#### ACS 5-year Wisconsin Geographies

```r
acs_geo <- read.csv("./data/g20105wi.csv", header = F) %>%
  rename(LOGRECNO = V5,
         GEOID = V49
         ) %>%
  select(c("LOGRECNO","GEOID")) %>% 
  #convert GEOID to 12-digit FIPS
  mutate(GEOID = substr(GEOID,8,20))
```

##### Clean and subset ACS data & Compute poverty variable


```r
acs_data <- acs_data %>% 
  mutate(across(starts_with("B1"), as.numeric)) %>% 
  pivot_longer(starts_with("B1"), names_to = "name", values_to = "value") %>% 
  separate_wider_delim(name, delim = "_", names = c("year", "measure")) %>% 
  filter(str_detect(measure, "001|002"), !str_detect(year, "[A-Z]+$")) %>% 
  mutate(measure = ifelse(measure == "001", "overall_population", 
                          "population_in_poverty")) %>% 
  pivot_wider(names_from = measure, values_from = value) %>% 
  mutate(poverty = population_in_poverty/overall_population, 
         year = str_extract(year, "\\d{2}$")) %>% 
  select(LOGRECNO, year, poverty) %>% 
  pivot_wider(names_from = year, values_from = poverty, 
              names_prefix = "poverty"
              )
```

```
## Warning: There were 3 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `across(starts_with("B1"), as.numeric)`.
## Caused by warning:
## ! NAs introduced by coercion
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
```


```r
# Unite data and geo info to long format data
acs <- left_join(acs_data, acs_geo, by="LOGRECNO") 
```


```r
# export cleaned file
write_csv(acs, "data/cleaned_acs.csv")
```

### Combined data


```r
#append ACS variables to MARS data
#for current address
data <- left_join(mars, acs, by=c("current_address" = "GEOID"), suffix = c("", "current")) %>% 
  rename_with(~ paste0(.x, "_current"), starts_with("poverty"))

# for past address
data <- left_join(data, acs, by=c("previous_address" = "GEOID")) %>% 
   rename_with(~ paste0(.x, "_previous"), starts_with("poverty") & 
                 !ends_with("current"))

# compute mean values
data <- data %>% 
  rowwise() %>%
  mutate(poverty_current = mean(c_across(starts_with("poverty") & 
                                 ends_with("current")), 
                      na.rm = TRUE),
         poverty_previous = mean(c_across(starts_with("poverty") & 
                                 ends_with("previous")), 
                      na.rm = TRUE)) 

# # check whether different years make a difference
# plot_data <- data %>% 
#   pivot_longer(starts_with("poverty"), 
#                names_to = "year_time", 
#                values_to = "poverty") %>% 
#   separate_wider_delim(year_time, delim = "_", names = c("year", "time")) %>% 
#   mutate(year = str_extract(year, "\\d{2}$")) #%>% 
#   pivot_wider(names_from = time, values_from = value)
# 
# ggplot(plot_data, aes(x = poverty, fill = as.factor(year))) + 
#   geom_density(alpha = 0.5) + 
#   facet_wrap(~time) # appear to only be values for 2010 available


# reduce data to target variables
data <- data %>% 
  select(c(all_of(variables), id_case)) %>% 
  mutate(weight = as.numeric(weight))
```


```r
# export cleaned file
write_csv(data, "data/cleaned_data_combined.csv")
```

### Impute NAs

```r
#impute NAs
library(mice)
# make various samples
data_imputed_NAs <- mice(data,
                m=15,
                seed=12345)
```

```
## 
##  iter imp variable
##   1   1  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   2  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   3  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   4  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   5  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   6  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   7  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   8  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   9  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   10  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   11  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   12  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   13  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   14  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   15  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   1  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   2  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   3  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   4  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   5  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   6  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   7  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   8  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   9  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   10  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   11  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   12  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   13  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   14  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   15  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   1  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   2  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   3  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   4  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   5  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   6  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   7  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   8  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   9  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   10  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   11  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   12  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   13  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   14  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   15  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   1  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   2  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   3  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   4  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   5  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   6  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   7  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   8  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   9  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   10  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   11  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   12  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   13  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   14  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   15  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   1  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   2  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   3  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   4  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   5  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   6  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   7  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   8  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   9  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   10  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   11  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   12  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   13  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   14  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   15  poverty_current  race  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
```

```
## Warning: Number of logged events: 76
```


## Analysis

Without imputed NAs, model cannot run

```r
model <- lm(poverty_current ~ move_reason +
              race +
              age +
              education_less_than_highschool +
              education_highschool +
              housing_assistance +
              single_mom +
              criminal_record +
              recent_child +
              relationship_dissolution +
              job_loss +
              benefits_loss +
              poverty_previous, 
            data = data)
```
Run simple model without controls.

```r
model <- lm(poverty_current ~ move_reason, 
            data = data,
            weights=weight)
summary(model)
```

```
## 
## Call:
## lm(formula = poverty_current ~ move_reason, data = data, weights = weight)
## 
## Weighted Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8088 -0.4788  0.4172  1.6850  6.1089 
## 
## Coefficients:
##                        Estimate Std. Error t value Pr(>|t|)    
## (Intercept)            0.121284   0.007766   15.62   <2e-16 ***
## move_reasonforced      0.032260   0.018124    1.78   0.0756 .  
## move_reasonresponsive -0.013027   0.014309   -0.91   0.3630    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.813 on 569 degrees of freedom
##   (8 observations deleted due to missingness)
## Multiple R-squared:  0.00875,	Adjusted R-squared:  0.005266 
## F-statistic: 2.511 on 2 and 569 DF,  p-value: 0.08206
```

Run final model with imputed values

```r
models <- with(data_imputed_NAs, 
               lm(poverty_current ~ move_reason +
                    race +
                    age +
                    education_less_than_highschool +
                    education_highschool +
                    housing_assistance +
                    single_mom +
                    criminal_record +
                    recent_child +
                    relationship_dissolution +
                    job_loss +
                    benefits_loss +
                    poverty_previous, 
                  weights=weight))

result <- pool(models)
```

### Result table


```r
summary(result)
```

```
##                                  term      estimate  std.error  statistic
## 1                         (Intercept)  0.1106213952 0.08617637  1.2836628
## 2                   move_reasonforced  0.0200851353 0.02325129  0.8638288
## 3               move_reasonresponsive  0.0112604009 0.02129998  0.5286578
## 4                           raceblack  0.0738476472 0.02407087  3.0679259
## 5                        racehispanic  0.0050483218 0.02850295  0.1771158
## 6                                 age  0.0006309446 0.00150124  0.4202824
## 7  education_less_than_highschoolTRUE -0.0103721031 0.03986157 -0.2602031
## 8            education_highschoolTRUE -0.0207929105 0.01776745 -1.1702813
## 9                  housing_assistance  0.0243650046 0.04534425  0.5373339
## 10                         single_mom  0.0262747212 0.03009874  0.8729508
## 11                    criminal_record -0.0686606672 0.03787727 -1.8127145
## 12                       recent_child  0.0270668056 0.03622749  0.7471345
## 13           relationship_dissolution  0.0056486005 0.03202707  0.1763696
## 14                           job_loss -0.0184028846 0.04984513 -0.3692013
## 15                      benefits_loss  0.0074973760 0.06948391  0.1079009
## 16                   poverty_previous  0.0281729099 0.05957607  0.4728897
##          df     p.value
## 1  14.55607 0.219318980
## 2  66.56496 0.390783987
## 3  47.91474 0.599483250
## 4  33.05916 0.004280931
## 5  34.25855 0.860460474
## 6  16.51322 0.679697216
## 7  39.66158 0.796053280
## 8  47.97872 0.247668077
## 9  24.82503 0.595821122
## 10 33.37477 0.388927508
## 11 16.63887 0.087958107
## 12 17.20610 0.465070770
## 13 16.39404 0.862163946
## 14 11.94892 0.718437148
## 15 10.41883 0.916124041
## 16 26.99625 0.640093001
```

### Main result


```r
result %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, digits = 4) %>% 
  filter(term == "move_reasonforced") %>% 
  select(term, estimate, std.error, statistic, p.value)
```

```
##                term estimate std.error statistic p.value
## 1 move_reasonforced   0.0201    0.0233    0.8638  0.3908
```




