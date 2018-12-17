Predicting Cancer Mortality Rate by State
================
Margaret Gacheru (mg3861), Annie Clark (alc2279), Keyanna Davis (kd2640), Tian Li (tl2882)
December 17, 2018

Load the cancer registry data

``` r
cancer_data = read_csv(file = "./data/Cancer_Registry.csv") %>% 
  janitor::clean_names()
```

Data Exploration (County Level)
===============================

Obtain summary statistics for all variables at the county level

``` r
numerical = cancer_data%>%
  dplyr::select(-c(binned_inc, geography))

Minimum = sapply(numerical, min, na.rm = 'TRUE')
Maximum = sapply(numerical, max, na.rm = 'TRUE')
Mean = sapply(numerical, mean, na.rm = 'TRUE')
Median = sapply(numerical, median, na.rm = 'TRUE')
SD = sapply(numerical, sd, na.rm = 'TRUE')
IQR = sapply(numerical, IQR, na.rm='TRUE')
Size = sapply(numerical, length)
Missing = sapply(numerical, function(x) sum(is.na(x)))

rbind(Minimum, Maximum, Mean, SD, Median, IQR, Size, Missing) %>%
  round(digits = 2)%>%t()
```

    ##                             Minimum     Maximum      Mean        SD
    ## avg_ann_count                  6.00    38150.00    606.34   1416.36
    ## avg_deaths_per_year            3.00    14010.00    185.97    504.13
    ## target_death_rate             59.70      362.80    178.66     27.75
    ## incidence_rate               201.30     1206.90    448.27     54.56
    ## med_income                 22640.00   125635.00  47063.28  12040.09
    ## pop_est2015                  827.00 10170292.00 102637.37 329059.22
    ## poverty_percent                3.20       47.40     16.88      6.41
    ## study_per_cap                  0.00     9762.31    155.40    529.63
    ## median_age                    22.30      624.00     45.27     45.30
    ## median_age_male               22.40       64.70     39.57      5.23
    ## median_age_female             22.30       65.70     42.15      5.29
    ## avg_household_size             0.02        3.97      2.48      0.43
    ## percent_married               23.10       72.50     51.77      6.90
    ## pct_no_hs18_24                 0.00       64.10     18.22      8.09
    ## pct_hs18_24                    0.00       72.50     35.00      9.07
    ## pct_some_col18_24              7.10       79.00     40.98     11.12
    ## pct_bach_deg18_24              0.00       51.80      6.16      4.53
    ## pct_hs25_over                  7.50       54.80     34.80      7.03
    ## pct_bach_deg25_over            2.50       42.20     13.28      5.39
    ## pct_employed16_over           17.60       80.10     54.15      8.32
    ## pct_unemployed16_over          0.40       29.40      7.85      3.45
    ## pct_private_coverage          22.30       92.30     64.35     10.65
    ## pct_private_coverage_alone    15.70       78.90     48.45     10.08
    ## pct_emp_priv_coverage         13.50       70.70     41.20      9.45
    ## pct_public_coverage           11.20       65.10     36.25      7.84
    ## pct_public_coverage_alone      2.60       46.60     19.24      6.11
    ## pct_white                     10.20      100.00     83.65     16.38
    ## pct_black                      0.00       85.95      9.11     14.53
    ## pct_asian                      0.00       42.62      1.25      2.61
    ## pct_other_race                 0.00       41.93      1.98      3.52
    ## pct_married_households        22.99       78.08     51.24      6.57
    ## birth_rate                     0.00       21.33      5.64      1.99
    ##                              Median      IQR Size Missing
    ## avg_ann_count                171.00   442.00 3047       0
    ## avg_deaths_per_year           61.00   121.00 3047       0
    ## target_death_rate            178.10    34.00 3047       0
    ## incidence_rate               453.55    60.55 3047       0
    ## med_income                 45207.00 13609.50 3047       0
    ## pop_est2015                26643.00 56987.00 3047       0
    ## poverty_percent               15.90     8.25 3047       0
    ## study_per_cap                  0.00    83.65 3047       0
    ## median_age                    41.00     6.30 3047       0
    ## median_age_male               39.60     6.15 3047       0
    ## median_age_female             42.40     6.20 3047       0
    ## avg_household_size             2.50     0.26 3047       0
    ## percent_married               52.40     8.65 3047       0
    ## pct_no_hs18_24                17.10     9.90 3047       0
    ## pct_hs18_24                   34.70    11.50 3047       0
    ## pct_some_col18_24             40.40    12.40 3047    2285
    ## pct_bach_deg18_24              5.40     5.10 3047       0
    ## pct_hs25_over                 35.30     9.25 3047       0
    ## pct_bach_deg25_over           12.30     6.70 3047       0
    ## pct_employed16_over           54.50    11.70 3047     152
    ## pct_unemployed16_over          7.60     4.20 3047       0
    ## pct_private_coverage          65.10    14.90 3047       0
    ## pct_private_coverage_alone    48.70    14.60 3047     609
    ## pct_emp_priv_coverage         41.10    13.20 3047       0
    ## pct_public_coverage           36.30    10.65 3047       0
    ## pct_public_coverage_alone     18.80     8.25 3047       0
    ## pct_white                     90.06    18.16 3047       0
    ## pct_black                      2.25     9.89 3047       0
    ## pct_asian                      0.55     0.97 3047       0
    ## pct_other_race                 0.83     1.88 3047       0
    ## pct_married_households        51.67     7.63 3047       0
    ## birth_rate                     5.38     1.97 3047       0

Note that pct\_some\_col18\_24 has 2285 out of 3047 missing values (75%), pct\_employed16\_over has 152 out of 3047 missing values (5%), and pct\_private\_coverage\_alone has 609 out of 3047 missing values (20%). Some college has too many missing values so it might be advisable not include it in the list of potential covariates

Since pct\_some\_col18\_24 had a high percentage of missing data, remove the variable from the dataset. Furthermore, remove observations that have missing values for pct\_private\_coverage\_alone and pct\_employed16\_over.

``` r
clean_cancer_data = cancer_data%>%
  dplyr::select(-pct_some_col18_24)%>%
  na.omit()
```

From literature review and preliminary discussions, remove binned\_inc, pct\_some\_col\_18\_24, median\_age, geography, avg\_deaths\_per\_year, and pop\_est\_2015 from the dataset

``` r
reduced_clean = clean_cancer_data%>%
  dplyr::select(-c(binned_inc, geography, median_age, avg_deaths_per_year, pop_est2015))
```

median\_age is removed because of its unrealistic values (extreme outliers)

``` r
clean_cancer_data%>%
  ggplot(aes(x = median_age, y = target_death_rate))+
  geom_point()+
  labs(title  = "Unrealistic Values for Median Age",
      x = "Median Age",
      y = "Target Death Rate")
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-5-1.png)

Next, find out if there are variables that are highly correlated/provide the same information

``` r
par(mfrow=c(1,1))
cor(reduced_clean)%>%
  corrplot(method = "circle", type = "upper", diag=FALSE)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-6-1.png)

The main takeaways from the correlation plot is that there are moderate to strong correlations between variables that provide information on income, insurance, employment, and bachelor degree. This is to be expected since all the mentioned variables can be measures of SES. In addition, there is a strong correlation between black and white as well as between percentage of married and percent of married households

Now, observe the distributions of potential covariates and observe if transformations are needed in the case our variables are skewed

Create a function that plots and performs tranformations on the variables

``` r
distribution = function(variable) {

  reduced_clean = tibble(variable)
  
  reduced_clean%>%
    ggplot(aes(variable))+
    geom_histogram()
}

inspection = function(variable) {

  reduced_clean = tibble(variable)

  #Observe transformations

  transformations = reduced_clean%>%
    dplyr::select(variable)%>%
    mutate(log_variable = log(variable),
          sqrt_variable = sqrt(variable),
          inverse_variable = 1 / variable)%>%
    gather(key = type, value = value)
  
  a = transformations%>%
    filter(type == "variable")%>%
    ggplot(aes(value))+
    geom_density()+
    labs(x = "Original",
        y = "Frequency")

  b = transformations%>%
    filter(type == "sqrt_variable")%>%
    ggplot(aes(value))+
    geom_density()+
    labs(x = "Sqrt(Variable)",
        y = "Frequency")

  c = transformations%>%
    filter(type == "inverse_variable")%>%
    ggplot(aes(value))+
    geom_density()+
    labs(x = "Inverse(Variable)",
        y = "Frequency")

  d = transformations%>%
    filter(type == "log_variable")%>%
    ggplot(aes(value))+
    geom_density()+
    labs(x = "log(Variable)",
        y = "Frequency")

  (a + b) / (c + d)
  
}
```

### Distributions and Transformations

Average annual count has one extreme outlier, making the distribution of the points to be right skewed. Log tranformation most reduces the skewness

``` r
variable = reduced_clean$avg_ann_count

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-8-2.png)

The distribution of incidence rate seems to be appropriately clustered, with a few outliers. Transformations result in similar shapes as the original

``` r
variable = reduced_clean$incidence_rate

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-9-2.png)

Median income is slighly right skewed and the inverse transformation reduces most of the skewness. Transformation also may not be necessary in this case

``` r
variable = reduced_clean$med_income

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-10-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-10-2.png)

Poverty percent is slightly right skewed and the square root transformation reduces most of the skewness. Transformation also may not be necessary in this case

``` r
variable = reduced_clean$poverty_percent

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-11-2.png)

Study per cap is highly right skewed and the log transformation reduces most of the skewness

``` r
variable = reduced_clean$study_per_cap

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-12-2.png)

Median age for males has an appropriate shape and does not need transformation

``` r
variable = reduced_clean$median_age_male

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-13-2.png)

Median age for females has an appropriate shape and does not need transformation

``` r
variable = reduced_clean$median_age_female

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-14-2.png)

Average household size has some unusual values. A majority of the points are clustured around 2.5 and there is a slight skewness to the right. However, there are some outliers to the left that are close to 0. None of the transformations are useful - the log transformation removes the slight right skewness but we still see a another bump to the left

``` r
variable = reduced_clean$avg_household_size

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-15-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-15-2.png)

Percent married is slightly left skewed but none of the transformation perform better, in terms of skewness

``` r
variable = reduced_clean$percent_married

distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-16-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-16-2.png)

pct\_no\_hs18\_24 is slightly right skewed and the square root transformation reduces most of the skewness. However, a transformation might not be necessary in this case

``` r
variable = reduced_clean$pct_no_hs18_24


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-17-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-17-2.png)

pct\_hs18\_24 is appropriately shaped

``` r
variable = reduced_clean$pct_hs18_24


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-18-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-18-2.png)

pct\_bach\_deg18\_24 is right skewed. While the square root transformation reduces most of the skewness, it also creates a second peak close to zero.

``` r
variable = reduced_clean$pct_bach_deg18_24


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-19-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-19-2.png)

pct\_hs25\_over is very slightly left skewed and no transformation is necessary

``` r
variable = reduced_clean$pct_hs25_over


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-20-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-20-2.png)

pct\_bach\_deg25\_over is right skewed and a log transformation best reduces the skewness

``` r
variable = reduced_clean$pct_bach_deg25_over


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-21-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-21-2.png)

pct\_employed16\_over is very slightly left skewed so no transformation is necessary

``` r
variable = reduced_clean$pct_employed16_over


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-22-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-22-2.png)

pct\_unemployed16\_over is right skewed and a square root transformation works best to reduce skewness

``` r
variable = reduced_clean$pct_unemployed16_over


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-23-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-23-2.png)

pct\_private\_coverage is appropriately shaped (very slight skewness) and no transformation is needed

``` r
variable = reduced_clean$pct_private_coverage


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-24-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-24-2.png)

pct\_private\_coverage\_alone is appropriately shaped and no transformation is needed

``` r
variable = reduced_clean$pct_private_coverage_alone


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-25-2.png)

pct\_emp\_priv\_coverage is appropriately shaped

``` r
variable = reduced_clean$pct_emp_priv_coverage


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-26-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-26-2.png)

pct\_public\_coverage is appropriately shaped

``` r
variable = reduced_clean$pct_public_coverage


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-27-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-27-2.png)

pct\_public\_coverage\_alone is slightly skewed to the right and the square root transformation reduces the skewness. But a transformation may not be necessary in this case

``` r
variable = reduced_clean$pct_public_coverage_alone


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-28-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-28-2.png)

pct\_white is left skewed and no transformation improves the distribution

``` r
variable = reduced_clean$pct_white


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-29-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-29-2.png)

pct\_black is right skewed. Although, log transformation best reduces the skewness, it creates an unusual shape where a majority of the points are concentrated towards the upper half of the curve

``` r
variable = reduced_clean$pct_black + 0.01


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-30-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-30-2.png)

pct\_asian is right skewed and a log transformation reduces the skewness

``` r
variable = reduced_clean$pct_asian + 0.01


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-31-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-31-2.png)

pct\_other\_race is right skewed and a log transformation reduces the skewness

``` r
variable = reduced_clean$pct_other_race + 0.01


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-32-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-32-2.png)

pct\_married\_households is appropriately shaped

``` r
variable = reduced_clean$pct_married_households


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-33-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-33-2.png)

birth\_rate is slightly right skewed and square root transformation reduces the skewness. Although, the transformation might be unnecessary

``` r
variable = reduced_clean$birth_rate


distribution(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-34-1.png)

``` r
inspection(variable)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-34-2.png)

Perform the aforementioned transformations

``` r
model_data = 
  cancer_data %>%
  dplyr::select(-pct_some_col18_24) %>%
  na.omit() %>%
  dplyr::select(-c(binned_inc, median_age, avg_deaths_per_year, pop_est2015)) %>% 
  mutate(log_avg_ann_count = log(avg_ann_count),
         inverse_med_income = 1/(med_income),
         sqrt_poverty_percent = sqrt(poverty_percent),
         study_per_cap = study_per_cap + 0.01,
         log_study_per_cap = log(study_per_cap),
         log_avg_household_size = log(avg_household_size),
         sqrt_pct_no_hs18_24 = sqrt(pct_no_hs18_24),
         sqrt_pct_bach_deg18_24 = sqrt(pct_bach_deg18_24),
         log_pct_bach_deg25_over = log(pct_bach_deg25_over),
         sqrt_pct_unemployed16_over = sqrt(pct_unemployed16_over),
         sqrt_pct_public_coverage_alone = sqrt(pct_public_coverage_alone),
         pct_black = pct_black + 0.01,
         log_pct_black = log(pct_black),
         pct_asian = pct_asian + 0.01,
         log_pct_asian = log(pct_asian),
         pct_other_race = pct_other_race + 0.01,
         log_other_race = log(pct_other_race),
         sqrt_birth_rate = sqrt(birth_rate)) 
```

We are interested in creating a predictive model at the state level so we can aggregate the county data to obtain state level estimates (using mean)

``` r
model_data_state = 
  model_data %>% 
  separate(geography, into = c("county", "state"), sep = ",") %>% 
  group_by(state) %>% 
  dplyr::select(-county) %>% 
  summarise(avg_ann_count = mean(avg_ann_count),
            incidence_rate = mean(incidence_rate),
            poverty_percent = mean(poverty_percent),
            pct_no_hs18_24 = mean(pct_no_hs18_24),
            pct_bach_deg18_24 = mean(pct_bach_deg18_24),
            pct_public_coverage_alone = mean(pct_public_coverage_alone),
            pct_asian = mean(pct_asian),
            birth_rate = mean(birth_rate),
            med_income = mean(med_income),
            study_per_cap = mean(study_per_cap),
            median_age_male = mean(median_age_male),
            avg_household_size = mean(avg_household_size),
            pct_hs18_24 = mean(pct_hs18_24),
            pct_hs25_over = mean(pct_hs25_over),
            pct_unemployed16_over = mean(pct_unemployed16_over),
            pct_emp_priv_coverage = mean(pct_emp_priv_coverage),
            pct_white = mean(pct_white),
            pct_other_race = mean(pct_other_race),
            target_death_rate = mean(target_death_rate),
            median_age_female = mean(median_age_female),
            percent_married = mean(percent_married),
            pct_bach_deg25_over = mean(pct_bach_deg25_over),
            pct_private_coverage = mean(pct_private_coverage),
            pct_public_coverage = mean(pct_public_coverage),
            pct_black = mean(pct_black),
            pct_married_households = mean(pct_married_households), 
            pct_employed16_over = mean(pct_employed16_over),
            pct_private_coverage_alone = mean(pct_private_coverage_alone),
            log_avg_ann_count = mean(log_avg_ann_count),
            sqrt_poverty_percent = mean(sqrt_poverty_percent),
            sqrt_pct_no_hs18_24 = mean(sqrt_pct_no_hs18_24),
            sqrt_pct_bach_deg18_24 = mean(sqrt_pct_bach_deg18_24),
            sqrt_pct_public_coverage_alone = mean(sqrt_pct_public_coverage_alone),
            log_pct_asian = mean(log_pct_asian),
            sqrt_birth_rate = mean(sqrt_birth_rate),
            inverse_med_income = mean(inverse_med_income),
            log_study_per_cap = mean(log_study_per_cap),
            log_avg_household_size = mean(log_avg_household_size),
            sqrt_pct_unemployed16_over = mean(sqrt_pct_unemployed16_over),
            log_other_race = mean(log_other_race),
            log_pct_bach_deg25_over = mean(log_pct_bach_deg25_over),
            log_pct_black = mean(log_pct_black)) %>% 
  dplyr::select(-state)
```

Modeling (State level)
======================

Create a subset dataset with only the transformed variables

``` r
transformed_model_data_state = model_data_state%>%
  dplyr::select(target_death_rate, log_avg_ann_count, incidence_rate, inverse_med_income, sqrt_poverty_percent,
                median_age_male, median_age_female, log_avg_household_size, percent_married,
                sqrt_pct_no_hs18_24, pct_hs18_24, sqrt_pct_bach_deg18_24, pct_hs25_over, pct_bach_deg25_over,
                pct_employed16_over, sqrt_pct_unemployed16_over, pct_private_coverage, pct_private_coverage_alone,
                pct_emp_priv_coverage, pct_public_coverage, sqrt_pct_public_coverage_alone, pct_white,
                pct_married_households, sqrt_birth_rate, log_study_per_cap, log_pct_black, log_pct_asian, log_other_race)
```

Our full model has 27 potential predictors - select model based on criterion procedures

``` r
regfit.full = regsubsets(target_death_rate ~ ., data = transformed_model_data_state, nvmax = 27)
reg.summary = summary(regfit.full)

par(mfrow=c(2, 2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")

plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")

plot(reg.summary$cp,xlab="Number of Variables ",ylab="Cp",type="l")

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-38-1.png)

``` r
best <- function(model, ...) 
{
  subsets <- regsubsets(formula(model), model.frame(model), nvmax = 27, ...)
  subsets <- with(summary(subsets),
                  cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
  
  return(subsets)
}  


tranformed_full = lm(target_death_rate ~ . , data = transformed_model_data_state)
round(best(tranformed_full, nbest = 1), 3)
```

    ##     p (Intercept) log_avg_ann_count incidence_rate inverse_med_income
    ## 1   1           1                 0              0                  0
    ## 2   2           1                 0              1                  0
    ## 3   3           1                 0              1                  0
    ## 4   4           1                 0              1                  0
    ## 5   5           1                 0              1                  0
    ## 6   6           1                 0              1                  0
    ## 7   7           1                 0              1                  0
    ## 8   8           1                 0              1                  0
    ## 9   9           1                 0              1                  0
    ## 10 10           1                 0              1                  0
    ## 11 11           1                 0              1                  0
    ## 12 12           1                 0              1                  0
    ## 13 13           1                 0              1                  1
    ## 14 14           1                 0              1                  1
    ## 15 15           1                 0              1                  1
    ## 16 16           1                 0              1                  1
    ## 17 17           1                 0              1                  1
    ## 18 18           1                 0              1                  1
    ## 19 19           1                 0              1                  1
    ## 20 20           1                 0              1                  1
    ## 21 21           1                 0              1                  1
    ## 22 22           1                 0              1                  1
    ## 23 23           1                 1              1                  1
    ## 24 24           1                 0              1                  1
    ## 25 25           1                 1              1                  1
    ## 26 26           1                 1              1                  1
    ## 27 27           1                 1              1                  1
    ##    sqrt_poverty_percent median_age_male median_age_female
    ## 1                     0               0                 0
    ## 2                     0               0                 0
    ## 3                     0               0                 0
    ## 4                     0               0                 0
    ## 5                     0               0                 0
    ## 6                     0               0                 0
    ## 7                     0               0                 0
    ## 8                     0               0                 0
    ## 9                     0               0                 0
    ## 10                    0               0                 0
    ## 11                    1               1                 0
    ## 12                    1               1                 0
    ## 13                    0               1                 0
    ## 14                    0               1                 0
    ## 15                    0               1                 0
    ## 16                    0               1                 1
    ## 17                    0               1                 1
    ## 18                    0               1                 1
    ## 19                    0               1                 1
    ## 20                    0               1                 1
    ## 21                    0               1                 1
    ## 22                    1               1                 1
    ## 23                    1               1                 1
    ## 24                    1               1                 1
    ## 25                    1               1                 1
    ## 26                    1               1                 1
    ## 27                    1               1                 1
    ##    log_avg_household_size percent_married sqrt_pct_no_hs18_24 pct_hs18_24
    ## 1                       0               0                   0           0
    ## 2                       0               0                   0           0
    ## 3                       0               0                   0           0
    ## 4                       0               0                   0           1
    ## 5                       0               0                   0           0
    ## 6                       1               0                   0           0
    ## 7                       0               0                   0           1
    ## 8                       1               0                   0           1
    ## 9                       0               1                   0           1
    ## 10                      1               0                   0           1
    ## 11                      0               1                   0           1
    ## 12                      0               1                   0           1
    ## 13                      0               1                   0           1
    ## 14                      1               1                   0           1
    ## 15                      1               1                   0           1
    ## 16                      1               1                   0           1
    ## 17                      1               1                   0           1
    ## 18                      1               1                   0           1
    ## 19                      1               1                   0           1
    ## 20                      1               1                   0           1
    ## 21                      1               1                   0           1
    ## 22                      1               1                   0           1
    ## 23                      1               1                   0           1
    ## 24                      1               1                   1           1
    ## 25                      1               1                   1           1
    ## 26                      1               1                   1           1
    ## 27                      1               1                   1           1
    ##    sqrt_pct_bach_deg18_24 pct_hs25_over pct_bach_deg25_over
    ## 1                       0             1                   0
    ## 2                       0             0                   1
    ## 3                       0             0                   1
    ## 4                       0             0                   1
    ## 5                       0             1                   0
    ## 6                       0             1                   0
    ## 7                       1             0                   1
    ## 8                       0             0                   1
    ## 9                       0             0                   1
    ## 10                      0             0                   1
    ## 11                      0             0                   1
    ## 12                      1             0                   1
    ## 13                      1             0                   1
    ## 14                      1             0                   1
    ## 15                      1             0                   1
    ## 16                      1             0                   1
    ## 17                      1             0                   1
    ## 18                      1             0                   1
    ## 19                      1             0                   1
    ## 20                      1             0                   1
    ## 21                      1             0                   1
    ## 22                      1             0                   1
    ## 23                      1             0                   1
    ## 24                      1             1                   1
    ## 25                      1             1                   1
    ## 26                      1             1                   1
    ## 27                      1             1                   1
    ##    pct_employed16_over sqrt_pct_unemployed16_over pct_private_coverage
    ## 1                    0                          0                    0
    ## 2                    0                          0                    0
    ## 3                    0                          0                    0
    ## 4                    0                          0                    0
    ## 5                    0                          1                    0
    ## 6                    0                          1                    0
    ## 7                    0                          0                    1
    ## 8                    0                          0                    0
    ## 9                    0                          0                    1
    ## 10                   0                          0                    0
    ## 11                   0                          0                    1
    ## 12                   0                          0                    1
    ## 13                   0                          0                    1
    ## 14                   0                          0                    1
    ## 15                   0                          0                    1
    ## 16                   0                          0                    1
    ## 17                   0                          1                    1
    ## 18                   0                          1                    1
    ## 19                   0                          1                    1
    ## 20                   0                          1                    1
    ## 21                   0                          1                    1
    ## 22                   0                          1                    1
    ## 23                   0                          1                    1
    ## 24                   0                          1                    1
    ## 25                   0                          1                    1
    ## 26                   0                          1                    1
    ## 27                   1                          1                    1
    ##    pct_private_coverage_alone pct_emp_priv_coverage pct_public_coverage
    ## 1                           0                     0                   0
    ## 2                           0                     0                   0
    ## 3                           0                     0                   0
    ## 4                           0                     0                   0
    ## 5                           0                     0                   0
    ## 6                           0                     0                   0
    ## 7                           0                     0                   0
    ## 8                           0                     0                   0
    ## 9                           0                     1                   0
    ## 10                          0                     0                   0
    ## 11                          0                     0                   0
    ## 12                          0                     0                   0
    ## 13                          0                     0                   0
    ## 14                          0                     0                   0
    ## 15                          0                     0                   0
    ## 16                          0                     0                   0
    ## 17                          0                     0                   0
    ## 18                          0                     0                   0
    ## 19                          0                     0                   0
    ## 20                          1                     1                   0
    ## 21                          1                     1                   0
    ## 22                          1                     1                   0
    ## 23                          1                     1                   0
    ## 24                          1                     1                   0
    ## 25                          1                     1                   0
    ## 26                          1                     1                   1
    ## 27                          1                     1                   1
    ##    sqrt_pct_public_coverage_alone pct_white pct_married_households
    ## 1                               0         0                      0
    ## 2                               0         0                      0
    ## 3                               0         0                      1
    ## 4                               0         0                      1
    ## 5                               0         0                      1
    ## 6                               0         0                      1
    ## 7                               0         0                      0
    ## 8                               0         1                      1
    ## 9                               0         0                      1
    ## 10                              0         1                      1
    ## 11                              0         0                      1
    ## 12                              0         0                      1
    ## 13                              1         0                      1
    ## 14                              1         0                      1
    ## 15                              1         1                      1
    ## 16                              1         1                      1
    ## 17                              1         1                      1
    ## 18                              1         1                      1
    ## 19                              1         1                      1
    ## 20                              1         1                      1
    ## 21                              1         1                      1
    ## 22                              1         1                      1
    ## 23                              1         1                      1
    ## 24                              1         1                      1
    ## 25                              1         1                      1
    ## 26                              1         1                      1
    ## 27                              1         1                      1
    ##    sqrt_birth_rate log_study_per_cap log_pct_black log_pct_asian
    ## 1                0                 0             0             0
    ## 2                0                 0             0             0
    ## 3                0                 0             0             0
    ## 4                0                 0             0             0
    ## 5                0                 1             0             0
    ## 6                0                 1             0             0
    ## 7                0                 1             1             0
    ## 8                0                 1             1             0
    ## 9                0                 0             1             0
    ## 10               0                 1             1             1
    ## 11               1                 0             1             0
    ## 12               1                 0             1             0
    ## 13               1                 0             1             0
    ## 14               1                 0             1             0
    ## 15               1                 0             1             0
    ## 16               1                 0             1             0
    ## 17               1                 0             1             0
    ## 18               1                 1             1             0
    ## 19               1                 1             1             1
    ## 20               1                 1             1             0
    ## 21               1                 1             1             1
    ## 22               1                 1             1             1
    ## 23               1                 1             1             1
    ## 24               1                 1             1             1
    ## 25               1                 1             1             1
    ## 26               1                 1             1             1
    ## 27               1                 1             1             1
    ##    log_other_race      rss   rsq adjr2      cp     bic
    ## 1               0 9701.577 0.305 0.291 194.786 -10.671
    ## 2               0 5069.015 0.637 0.622  81.332 -39.845
    ## 3               0 3634.449 0.740 0.723  47.579 -52.880
    ## 4               0 2655.181 0.810 0.793  25.173 -64.960
    ## 5               0 2445.575 0.825 0.805  21.949 -65.222
    ## 6               0 2186.912 0.843 0.822  17.503 -66.991
    ## 7               0 1922.548 0.862 0.840  12.914 -69.630
    ## 8               0 1668.668 0.880 0.858   8.587 -72.921
    ## 9               1 1581.526 0.887 0.862   8.415 -71.725
    ## 10              1 1457.890 0.896 0.869   7.334 -71.944
    ## 11              1 1292.313 0.907 0.881   5.207 -74.161
    ## 12              1 1221.891 0.912 0.885   5.452 -73.087
    ## 13              1 1131.898 0.919 0.890   5.210 -73.057
    ## 14              1 1075.968 0.923 0.893   5.816 -71.709
    ## 15              1 1046.298 0.925 0.893   7.076 -69.204
    ## 16              1 1024.765 0.927 0.892   8.540 -66.332
    ## 17              1 1004.977 0.928 0.891  10.046 -63.395
    ## 18              1  975.670 0.930 0.891  11.316 -60.973
    ## 19              1  962.246 0.931 0.889  12.981 -57.747
    ## 20              1  944.156 0.932 0.887  14.531 -54.783
    ## 21              1  931.921 0.933 0.885  16.226 -51.517
    ## 22              1  928.499 0.933 0.881  18.140 -47.773
    ## 23              1  927.003 0.934 0.877  20.103 -43.923
    ## 24              1  925.432 0.934 0.872  22.064 -40.078
    ## 25              1  923.725 0.934 0.868  24.021 -36.240
    ## 26              1  922.905 0.934 0.862  26.001 -32.353
    ## 27              1  922.866 0.934 0.856  28.000 -28.424

We arrive at a predictor model: incidence\_rate, log\_avg\_household\_size, pct\_hs18\_24, pct\_bach\_deg25\_over, pct\_white, pct\_married\_households, log\_study\_per\_cap, and log\_pct\_black. The 8 predictor model is the smallest model that has a Cp less than the number of parameters. Furthermore, the adjusted Rsquared is not significantly different from the maximum adjusted Rsquared (0.858 vs. 0.893) and the BIC is not substantially different from teh minimum BIC (-72.921 vs. -74.161).

Now, determine if there are collinearity issues

``` r
final_transformed  = lm(target_death_rate ~ incidence_rate + log_avg_household_size + pct_hs18_24 +
                               pct_bach_deg25_over + pct_white + pct_married_households + 
                               log_study_per_cap + log_pct_black, data = transformed_model_data_state)

vif(final_transformed)
```

    ##         incidence_rate log_avg_household_size            pct_hs18_24 
    ##               1.904557               1.422936               2.141975 
    ##    pct_bach_deg25_over              pct_white pct_married_households 
    ##               3.014298               3.260571               2.498583 
    ##      log_study_per_cap          log_pct_black 
    ##               2.985946               3.386269

There are no variables with VIF &gt; 5. So, we arrive at a 8 predictor model with no collinearity issues: incidence\_rate, log\_avg\_household\_size, pct\_hs18\_24, pct\_bach\_deg25\_over, pct\_white, pct\_married\_households, log\_study\_per\_cap, and log\_pct\_black

Model Diagnostics
=================

``` r
par(mfrow=c(2,2))
plot(final_transformed)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-40-1.png)

Check for outliers, leverage, and influential points

``` r
#find outlier in Y
stu_res = rstandard(final_transformed)
outliers_y = stu_res[abs(stu_res)>2.5]

outliers_y
```

    ## named numeric(0)

According to studentized residuals, there are no outliers in y. In the QQ plot, we see that point 37, 26, and 39 are potential outliers.

``` r
distance = cooks.distance(final_transformed)
distance[distance > 0.5]
```

    ## named numeric(0)

There are no points that have a cook's distance that is greater than 0.5. Therefore, we do not have influence points

Comparison Models
=================

1.  Model with untransformed variables

Create the subset dataset with only the untransformed variables

``` r
untransformed_model_data_state = 
  model_data_state %>% 
  dplyr::select(-starts_with("log"), -starts_with("inverse"), -starts_with("sqrt"))
```

Our full model has 27 potential predictors - select model based on criterion procedures

``` r
regfit.full = regsubsets(target_death_rate ~ ., data = untransformed_model_data_state, nvmax = 27)
reg.summary = summary(regfit.full)

par(mfrow=c(2, 2))
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l")

plot(reg.summary$adjr2 ,xlab="Number of Variables ",ylab="Adjusted RSq",type="l")

plot(reg.summary$cp,xlab="Number of Variables ",ylab="Cp",type="l")

plot(reg.summary$bic ,xlab="Number of Variables ",ylab="BIC",type="l")
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-44-1.png)

``` r
best <- function(model, ...) 
{
  subsets <- regsubsets(formula(model), model.frame(model), nvmax=27, ...)
  subsets <- with(summary(subsets),
                  cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
  
  return(subsets)
}  


untranformed_full = lm(target_death_rate ~ . , data = untransformed_model_data_state)
round(best(untranformed_full, nbest = 1), 3)
```

    ##     p (Intercept) avg_ann_count incidence_rate poverty_percent
    ## 1   1           1             0              0               0
    ## 2   2           1             0              1               0
    ## 3   3           1             0              1               0
    ## 4   4           1             0              1               0
    ## 5   5           1             0              1               0
    ## 6   6           1             0              1               0
    ## 7   7           1             1              1               0
    ## 8   8           1             1              1               0
    ## 9   9           1             0              1               0
    ## 10 10           1             0              1               0
    ## 11 11           1             0              1               0
    ## 12 12           1             0              1               0
    ## 13 13           1             0              1               0
    ## 14 14           1             0              1               0
    ## 15 15           1             0              1               0
    ## 16 16           1             0              1               0
    ## 17 17           1             1              1               0
    ## 18 18           1             0              1               0
    ## 19 19           1             1              1               0
    ## 20 20           1             1              1               0
    ## 21 21           1             1              1               0
    ## 22 22           1             1              1               0
    ## 23 23           1             1              1               0
    ## 24 24           1             1              1               0
    ## 25 25           1             1              1               0
    ## 26 26           1             1              1               1
    ## 27 27           1             1              1               1
    ##    pct_no_hs18_24 pct_bach_deg18_24 pct_public_coverage_alone pct_asian
    ## 1               0                 0                         0         0
    ## 2               0                 0                         0         0
    ## 3               0                 0                         0         0
    ## 4               0                 0                         0         0
    ## 5               0                 1                         0         0
    ## 6               0                 1                         0         0
    ## 7               0                 1                         0         0
    ## 8               0                 1                         0         0
    ## 9               0                 1                         0         0
    ## 10              0                 0                         0         1
    ## 11              0                 0                         0         1
    ## 12              1                 0                         0         1
    ## 13              0                 0                         0         1
    ## 14              0                 1                         0         1
    ## 15              0                 1                         1         1
    ## 16              0                 1                         1         1
    ## 17              0                 1                         1         1
    ## 18              0                 1                         1         1
    ## 19              0                 1                         1         1
    ## 20              0                 1                         1         1
    ## 21              0                 1                         1         1
    ## 22              0                 1                         1         1
    ## 23              0                 1                         1         1
    ## 24              0                 1                         1         1
    ## 25              0                 1                         1         1
    ## 26              0                 1                         1         1
    ## 27              1                 1                         1         1
    ##    birth_rate med_income study_per_cap median_age_male avg_household_size
    ## 1           0          0             0               0                  0
    ## 2           0          0             0               0                  0
    ## 3           0          0             0               0                  0
    ## 4           0          0             0               0                  0
    ## 5           0          0             0               0                  0
    ## 6           0          1             0               0                  0
    ## 7           0          1             0               0                  0
    ## 8           0          1             1               0                  0
    ## 9           1          0             0               0                  0
    ## 10          1          1             0               0                  1
    ## 11          1          1             0               0                  1
    ## 12          1          1             0               0                  1
    ## 13          1          1             0               1                  1
    ## 14          1          1             0               1                  1
    ## 15          1          1             0               1                  1
    ## 16          1          1             0               1                  1
    ## 17          1          1             0               1                  1
    ## 18          1          1             0               1                  1
    ## 19          1          1             0               1                  1
    ## 20          1          1             0               1                  1
    ## 21          1          1             0               1                  1
    ## 22          1          1             0               1                  1
    ## 23          1          1             0               1                  1
    ## 24          1          1             0               1                  1
    ## 25          1          1             1               1                  1
    ## 26          1          1             1               1                  1
    ## 27          1          1             1               1                  1
    ##    pct_hs18_24 pct_hs25_over pct_unemployed16_over pct_emp_priv_coverage
    ## 1            0             1                     0                     0
    ## 2            0             0                     0                     0
    ## 3            0             0                     0                     0
    ## 4            1             0                     0                     0
    ## 5            0             0                     0                     0
    ## 6            0             0                     0                     0
    ## 7            0             0                     0                     0
    ## 8            0             0                     0                     0
    ## 9            1             0                     0                     0
    ## 10           0             0                     0                     0
    ## 11           0             0                     0                     0
    ## 12           0             0                     0                     0
    ## 13           1             0                     0                     0
    ## 14           1             0                     0                     0
    ## 15           1             0                     0                     0
    ## 16           1             0                     0                     1
    ## 17           1             0                     0                     1
    ## 18           1             0                     1                     1
    ## 19           1             0                     0                     1
    ## 20           1             0                     1                     1
    ## 21           1             1                     1                     1
    ## 22           1             1                     1                     1
    ## 23           1             1                     1                     1
    ## 24           1             1                     1                     1
    ## 25           1             1                     1                     1
    ## 26           1             1                     1                     1
    ## 27           1             1                     1                     1
    ##    pct_white pct_other_race median_age_female percent_married
    ## 1          0              0                 0               0
    ## 2          0              0                 0               0
    ## 3          0              0                 0               0
    ## 4          0              0                 0               0
    ## 5          0              1                 0               0
    ## 6          0              1                 0               0
    ## 7          0              1                 0               0
    ## 8          0              1                 0               0
    ## 9          0              1                 0               0
    ## 10         0              1                 0               1
    ## 11         0              1                 0               1
    ## 12         0              1                 0               1
    ## 13         0              1                 1               1
    ## 14         0              1                 1               1
    ## 15         0              1                 1               1
    ## 16         0              1                 1               1
    ## 17         0              1                 1               1
    ## 18         0              1                 1               1
    ## 19         0              1                 1               1
    ## 20         0              1                 1               1
    ## 21         0              1                 1               1
    ## 22         1              1                 1               1
    ## 23         1              1                 1               1
    ## 24         1              1                 1               1
    ## 25         1              1                 1               1
    ## 26         1              1                 1               1
    ## 27         1              1                 1               1
    ##    pct_bach_deg25_over pct_private_coverage pct_public_coverage pct_black
    ## 1                    0                    0                   0         0
    ## 2                    1                    0                   0         0
    ## 3                    1                    0                   0         0
    ## 4                    1                    0                   0         0
    ## 5                    1                    1                   0         0
    ## 6                    1                    1                   0         0
    ## 7                    1                    1                   0         0
    ## 8                    1                    1                   0         0
    ## 9                    1                    1                   1         0
    ## 10                   1                    1                   0         0
    ## 11                   1                    1                   1         0
    ## 12                   1                    1                   1         0
    ## 13                   1                    1                   0         0
    ## 14                   1                    1                   0         0
    ## 15                   1                    1                   0         0
    ## 16                   1                    1                   0         0
    ## 17                   1                    1                   0         0
    ## 18                   1                    1                   0         0
    ## 19                   1                    1                   0         1
    ## 20                   1                    1                   0         1
    ## 21                   1                    1                   0         1
    ## 22                   1                    1                   0         1
    ## 23                   1                    1                   1         1
    ## 24                   1                    1                   1         1
    ## 25                   1                    1                   1         1
    ## 26                   1                    1                   1         1
    ## 27                   1                    1                   1         1
    ##    pct_married_households pct_employed16_over pct_private_coverage_alone
    ## 1                       0                   0                          0
    ## 2                       0                   0                          0
    ## 3                       1                   0                          0
    ## 4                       1                   0                          0
    ## 5                       0                   0                          0
    ## 6                       0                   0                          0
    ## 7                       0                   0                          0
    ## 8                       0                   0                          0
    ## 9                       0                   1                          0
    ## 10                      1                   0                          0
    ## 11                      1                   0                          0
    ## 12                      1                   0                          0
    ## 13                      1                   0                          0
    ## 14                      1                   0                          0
    ## 15                      1                   0                          0
    ## 16                      1                   0                          0
    ## 17                      1                   0                          0
    ## 18                      1                   1                          0
    ## 19                      1                   1                          0
    ## 20                      1                   1                          0
    ## 21                      1                   1                          0
    ## 22                      1                   1                          0
    ## 23                      1                   1                          0
    ## 24                      1                   1                          1
    ## 25                      1                   1                          1
    ## 26                      1                   1                          1
    ## 27                      1                   1                          1
    ##         rss   rsq adjr2      cp     bic
    ## 1  9701.577 0.305 0.291 196.312 -10.671
    ## 2  5069.015 0.637 0.622  82.129 -39.845
    ## 3  3634.449 0.740 0.723  48.151 -52.880
    ## 4  2655.181 0.810 0.793  25.591 -64.960
    ## 5  2371.409 0.830 0.811  20.474 -66.792
    ## 6  1957.531 0.860 0.841  12.094 -72.642
    ## 7  1839.593 0.868 0.847  11.136 -71.880
    ## 8  1786.192 0.872 0.848  11.797 -69.450
    ## 9  1692.544 0.879 0.852  11.448 -68.265
    ## 10 1477.274 0.894 0.868   8.050 -71.271
    ## 11 1394.276 0.900 0.872   7.968 -70.288
    ## 12 1330.982 0.905 0.874   8.381 -68.726
    ## 13 1167.787 0.916 0.887   6.288 -71.465
    ## 14 1120.973 0.920 0.888   7.114 -69.620
    ## 15 1067.709 0.923 0.891   7.778 -68.171
    ## 16 1033.604 0.926 0.891   8.922 -65.894
    ## 17 1020.174 0.927 0.889  10.586 -62.630
    ## 18 1003.161 0.928 0.888  12.159 -59.555
    ## 19  978.405 0.930 0.887  13.538 -56.898
    ## 20  962.048 0.931 0.885  15.128 -53.826
    ## 21  946.247 0.932 0.883  16.732 -50.739
    ## 22  932.997 0.933 0.881  18.399 -47.526
    ## 23  930.710 0.933 0.876  20.342 -43.719
    ## 24  919.318 0.934 0.873  22.056 -40.416
    ## 25  917.186 0.934 0.869  24.003 -36.602
    ## 26  917.098 0.934 0.863  26.001 -32.675
    ## 27  917.078 0.934 0.857  28.000 -28.745

We arrive at a 10 predictor model: incidence\_rate, pct\_asian, birth\_rate, med\_income, avg\_household\_size, pct\_other\_race, percent\_married, pct\_bach\_deg25\_over, pct\_private\_coverage, and pct\_married\_households. The 10 predictor model is the smallest model that has a Cp less than the number of predictors. Furthermore, the adjusted Rsquared is not substantially different from the maximum adjusted Rsquared (0.868 vs. 0.891) and the BIC is not substantially difference from the minimum BIC (-72.642 vs. -71.271)

Now, determine if there are collinearity issues

``` r
selected_untransformed  = lm(target_death_rate ~ incidence_rate + pct_asian + birth_rate + 
                               med_income + avg_household_size + pct_other_race + percent_married +
                               pct_bach_deg25_over + pct_private_coverage + pct_married_households,
                             data = untransformed_model_data_state)

vif(selected_untransformed)
```

    ##         incidence_rate              pct_asian             birth_rate 
    ##               2.375061               2.025649               2.821411 
    ##             med_income     avg_household_size         pct_other_race 
    ##               7.258778               5.074402               1.787202 
    ##        percent_married    pct_bach_deg25_over   pct_private_coverage 
    ##              37.200327               4.676865               6.383089 
    ## pct_married_households 
    ##              31.352095

The following variables have a VIF &gt; 5: med\_income, avg\_household\_size, percent\_married, pct\_private\_coverage, and pct\_married\_households. We can find the correlations to determine what variables should remain in the model

``` r
correlation_check = untransformed_model_data_state %>%
  dplyr::select(med_income, avg_household_size, percent_married, pct_private_coverage, 
                pct_married_households)

par(mfrow=c(1, 1))
cor(correlation_check)%>%
  corrplot(method = "square", addCoef.col = "black", tl.col="black", tl.srt=45, insig = "blank", 
         diag=FALSE, number.cex = .7)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-46-1.png)

We find that percent\_married is highly correlated to pct\_married\_households, med\_income is moderately correlated to pct\_private\_coverage, and the rest of the correlations are fairly low. We choose to keep percent\_married (its correlations to other variables are lower than pct\_married\_households' correlations to the same variabes), med\_income (its correlations to other variables are lower than pct\_private\_coverage' correlations to the same variabes), and avg\_household\_size (VIF = 5.07) in the model

``` r
final_untransformed_model  = lm(target_death_rate ~ incidence_rate + pct_asian + birth_rate + 
                               med_income + avg_household_size + pct_other_race + percent_married +
                               pct_bach_deg25_over,
                             data = untransformed_model_data_state)

vif(final_untransformed_model)
```

    ##      incidence_rate           pct_asian          birth_rate 
    ##            2.238731            1.813041            1.917726 
    ##          med_income  avg_household_size      pct_other_race 
    ##            3.848087            2.053039            1.752265 
    ##     percent_married pct_bach_deg25_over 
    ##            1.521627            3.527784

We arrive at a 8 predictor model with no collinearity issues: incidence\_rate, pct\_asian, birth\_rate, med\_income, avg\_household\_size, pct\_other\_race, percent\_married, and pct\_bach\_deg25\_over

1.  Lasso Model

``` r
attach(transformed_model_data_state)

library(glmnet)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     expand

    ## Loading required package: foreach

    ## 
    ## Attaching package: 'foreach'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     accumulate, when

    ## Loaded glmnet 2.0-16

``` r
test.mat = model.matrix(target_death_rate ~ ., data = transformed_model_data_state)[, -1]
lasso.mod = glmnet(test.mat, target_death_rate, alpha = 1)
set.seed(1)
cv.out = cv.glmnet(test.mat,target_death_rate,alpha=1)
par(mfrow=c(1, 1))
plot(cv.out)
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-48-1.png)

``` r
bestlam = cv.out$lambda.min
bestlam
```

    ## [1] 0.04544236

``` r
out = glmnet (test.mat, target_death_rate, alpha=1)
lasso.coef = predict (out, type="coefficients", s= bestlam)
lasso.coef
```

    ## 28 x 1 sparse Matrix of class "dgCMatrix"
    ##                                            1
    ## (Intercept)                     2.291737e+02
    ## log_avg_ann_count               .           
    ## incidence_rate                  1.858463e-01
    ## inverse_med_income             -1.047235e+06
    ## sqrt_poverty_percent           -9.734389e-02
    ## median_age_male                -8.844825e-01
    ## median_age_female               .           
    ## log_avg_household_size          1.452829e+01
    ## percent_married                 2.371732e+00
    ## sqrt_pct_no_hs18_24             .           
    ## pct_hs18_24                     1.058211e+00
    ## sqrt_pct_bach_deg18_24          5.733722e+00
    ## pct_hs25_over                   7.383018e-02
    ## pct_bach_deg25_over            -2.532562e+00
    ## pct_employed16_over             2.464112e-01
    ## sqrt_pct_unemployed16_over      4.241481e+00
    ## pct_private_coverage           -1.087220e+00
    ## pct_private_coverage_alone      .           
    ## pct_emp_priv_coverage           9.083575e-05
    ## pct_public_coverage             .           
    ## sqrt_pct_public_coverage_alone -4.753975e+00
    ## pct_white                       1.973167e-01
    ## pct_married_households         -2.891177e+00
    ## sqrt_birth_rate                -1.562258e+01
    ## log_study_per_cap              -1.308682e+00
    ## log_pct_black                   3.813869e+00
    ## log_pct_asian                   1.242544e+00
    ## log_other_race                 -3.838663e+00

``` r
detach(transformed_model_data_state)
```

Lasso only eliminates five variables: log\_avg\_ann\_count, median\_age\_female, sqrt\_pct\_no\_hs18\_24, pct\_private\_coverage\_alone, and pct\_public\_coverage

Model Validation
================

Compare the three models to determine which has the best predictive ability

Split the data into training and tesing sets

``` r
cv_df =
  crossv_mc(model_data_state, 100) %>% 
  mutate(train = map(train, as_tibble),
         test = map(test, as_tibble))
```

Calculate MSE for the three models (train and test)

``` r
cv_df = 
  cv_df %>% 
  mutate(transformed = map(train, ~lm(target_death_rate ~ incidence_rate + log_avg_household_size + pct_hs18_24 +
                               pct_bach_deg25_over + pct_white + pct_married_households + 
                               log_study_per_cap + log_pct_black,  data = .x)),
         rmse_test_transformed = map2_dbl(transformed, test, ~rmse(model = .x, data = .y)),
         rmse_train_transformed = map2_dbl(transformed, train, ~rmse(model = .x, data = .y)),
         
         
         untransformed = map(train, ~lm(target_death_rate ~ incidence_rate + pct_asian + birth_rate + 
                               med_income + avg_household_size + pct_other_race + percent_married +
                               pct_bach_deg25_over, data = .x)),
         rmse_test_untransformed = map2_dbl(untransformed, test, ~rmse(model = .x, data = .y)),
         rmse_train_untransformed = map2_dbl(untransformed, train, ~rmse(model = .x, data = .y)),
         
         lasso = map(train, ~lm(target_death_rate ~ incidence_rate + inverse_med_income + sqrt_poverty_percent +
                                median_age_male + log_avg_household_size + percent_married + pct_hs18_24 +
                                sqrt_pct_bach_deg18_24 + pct_hs25_over + pct_bach_deg25_over + pct_employed16_over +
                                sqrt_pct_unemployed16_over + pct_private_coverage + pct_emp_priv_coverage +
                                sqrt_pct_public_coverage_alone + pct_white + pct_married_households + sqrt_birth_rate +
                                log_study_per_cap + log_pct_black+log_pct_asian +log_other_race, data = .x)),
         rmse_test_lasso = map2_dbl(lasso, test, ~rmse(model = .x, data = .y)),
         rmse_train_lasso = map2_dbl(lasso, train, ~rmse(model = .x, data = .y))
         )

cv_df %>% 
  dplyr::select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  filter(model %in% c("test_transformed", "test_untransformed", "test_lasso"))%>%
  ggplot(aes(x = model, y = rmse)) + geom_violin()+
  labs(title = "Comparison of MSE using Testing Data",
       y = "RMSE",
       x = "Type of Model")
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-50-1.png)

``` r
cv_df %>% 
  dplyr::select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  filter(model %in% c("train_transformed", "test_transformed"))%>%
  ggplot(aes(x = model, y = rmse)) + geom_violin()+
  labs(title = "Comparison of Train and Test MSE for Transformed Variables",
       y = "RMSE",
       x = "Test vs. Train")
```

![](consolidated_project_files/figure-markdown_github/unnamed-chunk-50-2.png)

``` r
mean(cv_df$rmse_test_transformed)
```

    ## [1] 7.322054

``` r
mean(cv_df$rmse_train_transformed)
```

    ## [1] 5.525591

``` r
mean(cv_df$rmse_test_untransformed)
```

    ## [1] 12.2752

``` r
mean(cv_df$rmse_train_untransformed)
```

    ## [1] 6.821199

``` r
mean(cv_df$rmse_test_lasso)
```

    ## [1] 9.430408

``` r
mean(cv_df$rmse_train_lasso)
```

    ## [1] 3.859607

Our proposed model with transformed variable has the best predictive abilitity (lowest test MSE)
