---
title: 'Second Analysis: Replication Desmond 2015'
author: "Jan Pfänder"
date: "2023-05-23"
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

The instructions for the second analysis are:

> *"Your analysis should produce a single, main result in terms of statistical families of z-, t-, F-, or χ² tests (or their alternative or non-parametric versions). \n You should control for Black Renter, Hispanic Renter, Other Ethnicity Renter Age, Less Than High School Education,High School/GED, Housing Assistance in Past Residence Single-Mother Household in Past Residence Criminal Record Before Move ,Had a Child in Previous 2 Years"*

Following these instructions, I will proceed as for the first analysis, but this time *leaving out* the following control variables:

- relationship_dissolution 
- job_loss 
- benefits_loss 
- poverty_previous

### Read cleaned data


```r
# export cleaned file
data <- read_csv("data/cleaned_data_combined.csv") %>% 
  mutate(move_reason = relevel(as.factor(move_reason), ref = "voluntary/NA"))
```

### Impute NAs


```r
#impute NAs
# make various samples
data_imputed_NAs <- mice(data,
                m=15,
                seed=12345)
```

```
## 
##  iter imp variable
##   1   1  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   2  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   3  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   4  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   5  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   6  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   7  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   8  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   9  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   10  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   11  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   12  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   13  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   14  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   1   15  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   1  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   2  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   3  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   4  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   5  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   6  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   7  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   8  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   9  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   10  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   11  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   12  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   13  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   14  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   2   15  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   1  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   2  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   3  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   4  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   5  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   6  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   7  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   8  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   9  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   10  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   11  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   12  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   13  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   14  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   3   15  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   1  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   2  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   3  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   4  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   5  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   6  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   7  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   8  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   9  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   10  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   11  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   12  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   13  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   14  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   4   15  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   1  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   2  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   3  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   4  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   5  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   6  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   7  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   8  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   9  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   10  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   11  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   12  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   13  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   14  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
##   5   15  poverty_current  age  housing_assistance  single_mom  criminal_record  recent_child  relationship_dissolution  job_loss  benefits_loss  poverty_previous
```

```
## Warning: Number of logged events: 76
```

### Analyse

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
                    recent_child, 
                  weights=weight))

result <- pool(models) %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, digits = 4)
```

### Results

#### Table


```r
result
```

```
##                                  term estimate std.error statistic p.value
## 1                         (Intercept)   0.1584    0.0440    3.6032  0.0013
## 2                   move_reasonforced   0.0195    0.0251    0.7759  0.4420
## 3               move_reasonresponsive   0.0097    0.0176    0.5527  0.5819
## 4                        racehispanic  -0.0678    0.0200   -3.3979  0.0009
## 5                           raceother  -0.0834    0.0164   -5.0906  0.0000
## 6                                 age   0.0011    0.0009    1.1944  0.2428
## 7  education_less_than_highschoolTRUE  -0.0079    0.0317   -0.2495  0.8035
## 8            education_highschoolTRUE  -0.0233    0.0173   -1.3426  0.1856
## 9                  housing_assistance   0.0250    0.0301    0.8308  0.4082
## 10                         single_mom   0.0240    0.0239    1.0013  0.3200
## 11                    criminal_record  -0.0556    0.0493   -1.1278  0.2817
## 12                       recent_child   0.0337    0.0274    1.2266  0.2325
##         b       df dfcom    fmi lambda  m     riv  ubar
## 1  0.0012  25.4864   567 0.7079 0.6859 15  2.1835 6e-04
## 2  0.0003  43.4456   567 0.5409 0.5202 15  1.0842 3e-04
## 3  0.0001  87.2199   567 0.3643 0.3499 15  0.5381 2e-04
## 4  0.0001 151.9698   567 0.2534 0.2436 15  0.3221 3e-04
## 5  0.0001  87.5542   567 0.3634 0.3491 15  0.5362 2e-04
## 6  0.0000  26.8293   567 0.6908 0.6686 15  2.0176 0e+00
## 7  0.0003 108.0964   567 0.3188 0.3063 15  0.4415 7e-04
## 8  0.0001  49.0050   567 0.5069 0.4872 15  0.9501 2e-04
## 9  0.0003  91.8256   567 0.3530 0.3391 15  0.5130 6e-04
## 10 0.0002  73.4824   567 0.4034 0.3874 15  0.6324 4e-04
## 11 0.0021  11.8816   567 0.9340 0.9238 15 12.1152 2e-04
## 12 0.0005  22.7358   567 0.7464 0.7250 15  2.6367 2e-04
```

#### Main result


```r
result %>% filter(term == "move_reasonforced") %>% 
  select(term, estimate, std.error, statistic, p.value)
```

```
##                term estimate std.error statistic p.value
## 1 move_reasonforced   0.0195    0.0251    0.7759   0.442
```




