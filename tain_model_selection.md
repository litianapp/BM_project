tian\_model\_selection
================

initial full model
==================

``` r
data = read_csv("./data/Cancer_Registry.csv") %>% 
  janitor::clean_names()
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   avgDeathsPerYear = col_integer(),
    ##   medIncome = col_integer(),
    ##   popEst2015 = col_integer(),
    ##   binnedInc = col_character(),
    ##   Geography = col_character()
    ## )

    ## See spec(...) for full column specifications.

Transformation decision mainly based on "margaret\_eda.md" Full model variables exclude variables with high-linearity.

``` r
data_trans = data %>% 
  mutate(log_avg_ann_count = log(avg_ann_count)) %>% 
  mutate(study_per_cap = recode(study_per_cap, "0" = 0.001)) %>% #fix "-inf" problem
  mutate(log_study_per_cap = log(study_per_cap)) %>% 
  mutate(sqrt_pct_bach_deg18_24 = sqrt(pct_bach_deg18_24)) %>% 
  mutate(log_pct_bach_deg25_over = log(pct_bach_deg25_over)) %>% 
  mutate(sqrt_pct_unemployed16_over = sqrt(pct_unemployed16_over)) %>% 
  mutate(pct_asian = recode(pct_asian, "0" = 0.001)) %>% #fix "-inf" problem
  mutate(log_pct_asian = log(pct_asian)) %>% 
  mutate(pct_other_race = recode(pct_other_race, "0" = 0.001)) %>% #fix "-inf" problem
  mutate(log_pct_other_race = log(pct_other_race)) %>% 
  dplyr::select(target_death_rate, log_avg_ann_count, incidence_rate, med_income, poverty_percent,  log_study_per_cap, median_age_male, median_age_female, avg_household_size, percent_married, pct_no_hs18_24,  pct_hs18_24, sqrt_pct_bach_deg18_24, pct_hs25_over, log_pct_bach_deg25_over, sqrt_pct_unemployed16_over,  pct_emp_priv_coverage, pct_black, log_pct_asian, log_pct_other_race, pct_married_households, birth_rate)

model_full =
  lm(target_death_rate ~ log_avg_ann_count + incidence_rate + med_income + poverty_percent + log_study_per_cap + median_age_male + median_age_female + avg_household_size + percent_married + pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + pct_hs25_over + log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + pct_emp_priv_coverage + pct_black + log_pct_asian + log_pct_other_race + pct_married_households + birth_rate, data = data_trans) #21 variables
```

Method 1 stepwise
=================

``` r
step(model_full, direction = 'backward')
```

    ## Start:  AIC=18159.67
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + med_income + 
    ##     poverty_percent + log_study_per_cap + median_age_male + median_age_female + 
    ##     avg_household_size + percent_married + pct_no_hs18_24 + pct_hs18_24 + 
    ##     sqrt_pct_bach_deg18_24 + pct_hs25_over + log_pct_bach_deg25_over + 
    ##     sqrt_pct_unemployed16_over + pct_emp_priv_coverage + pct_black + 
    ##     log_pct_asian + log_pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## - median_age_female           1        13 1163958 18158
    ## - log_pct_asian               1        36 1163981 18158
    ## - med_income                  1        42 1163987 18158
    ## - log_study_per_cap           1        86 1164031 18158
    ## - pct_emp_priv_coverage       1       117 1164062 18158
    ## - avg_household_size          1       501 1164446 18159
    ## - pct_black                   1       753 1164698 18160
    ## <none>                                    1163945 18160
    ## - pct_no_hs18_24              1       982 1164927 18160
    ## - log_avg_ann_count           1      1103 1165048 18161
    ## - sqrt_pct_bach_deg18_24      1      1457 1165402 18162
    ## - log_pct_other_race          1      2143 1166089 18163
    ## - median_age_male             1      2561 1166507 18164
    ## - poverty_percent             1      7763 1171708 18178
    ## - birth_rate                  1      9706 1173651 18183
    ## - pct_hs18_24                 1     12303 1176248 18190
    ## - pct_hs25_over               1     12498 1176443 18190
    ## - percent_married             1     13603 1177548 18193
    ## - sqrt_pct_unemployed16_over  1     13836 1177781 18194
    ## - pct_married_households      1     21763 1185708 18214
    ## - log_pct_bach_deg25_over     1     24745 1188690 18222
    ## - incidence_rate              1    276426 1440371 18807
    ## 
    ## Step:  AIC=18157.71
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + med_income + 
    ##     poverty_percent + log_study_per_cap + median_age_male + avg_household_size + 
    ##     percent_married + pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + 
    ##     pct_hs25_over + log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + 
    ##     pct_emp_priv_coverage + pct_black + log_pct_asian + log_pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## - log_pct_asian               1        35 1163993 18156
    ## - med_income                  1        45 1164003 18156
    ## - log_study_per_cap           1        88 1164046 18156
    ## - pct_emp_priv_coverage       1       126 1164084 18156
    ## - avg_household_size          1       532 1164490 18157
    ## - pct_black                   1       741 1164699 18158
    ## <none>                                    1163958 18158
    ## - pct_no_hs18_24              1       996 1164954 18158
    ## - log_avg_ann_count           1      1130 1165088 18159
    ## - sqrt_pct_bach_deg18_24      1      1450 1165408 18160
    ## - log_pct_other_race          1      2142 1166100 18161
    ## - poverty_percent             1      7853 1171811 18176
    ## - birth_rate                  1      9699 1173657 18181
    ## - median_age_male             1     12005 1175963 18187
    ## - pct_hs18_24                 1     12296 1176254 18188
    ## - pct_hs25_over               1     12491 1176449 18188
    ## - percent_married             1     13734 1177692 18191
    ## - sqrt_pct_unemployed16_over  1     14095 1178053 18192
    ## - pct_married_households      1     21900 1185858 18212
    ## - log_pct_bach_deg25_over     1     24732 1188690 18220
    ## - incidence_rate              1    276618 1440577 18805
    ## 
    ## Step:  AIC=18155.8
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + med_income + 
    ##     poverty_percent + log_study_per_cap + median_age_male + avg_household_size + 
    ##     percent_married + pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + 
    ##     pct_hs25_over + log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + 
    ##     pct_emp_priv_coverage + pct_black + log_pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## - med_income                  1        49 1164042 18154
    ## - log_study_per_cap           1        84 1164077 18154
    ## - pct_emp_priv_coverage       1       126 1164119 18154
    ## - avg_household_size          1       522 1164515 18155
    ## - pct_black                   1       715 1164709 18156
    ## <none>                                    1163993 18156
    ## - pct_no_hs18_24              1      1008 1165001 18156
    ## - log_avg_ann_count           1      1097 1165090 18157
    ## - sqrt_pct_bach_deg18_24      1      1429 1165422 18158
    ## - log_pct_other_race          1      2108 1166101 18159
    ## - poverty_percent             1      7826 1171819 18174
    ## - birth_rate                  1      9688 1173681 18179
    ## - median_age_male             1     12174 1176168 18186
    ## - pct_hs18_24                 1     12349 1176342 18186
    ## - pct_hs25_over               1     12465 1176459 18186
    ## - percent_married             1     13783 1177776 18190
    ## - sqrt_pct_unemployed16_over  1     14462 1178455 18191
    ## - pct_married_households      1     21868 1185861 18210
    ## - log_pct_bach_deg25_over     1     24860 1188853 18218
    ## - incidence_rate              1    276868 1440861 18804
    ## 
    ## Step:  AIC=18153.93
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + poverty_percent + 
    ##     log_study_per_cap + median_age_male + avg_household_size + 
    ##     percent_married + pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + 
    ##     pct_hs25_over + log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + 
    ##     pct_emp_priv_coverage + pct_black + log_pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## - log_study_per_cap           1        88 1164130 18152
    ## - pct_emp_priv_coverage       1       229 1164271 18152
    ## - avg_household_size          1       568 1164610 18153
    ## <none>                                    1164042 18154
    ## - pct_black                   1       788 1164830 18154
    ## - pct_no_hs18_24              1       963 1165005 18154
    ## - log_avg_ann_count           1      1103 1165145 18155
    ## - sqrt_pct_bach_deg18_24      1      1382 1165424 18156
    ## - log_pct_other_race          1      2091 1166133 18157
    ## - poverty_percent             1      9489 1173531 18177
    ## - birth_rate                  1      9640 1173682 18177
    ## - median_age_male             1     12137 1176179 18184
    ## - pct_hs18_24                 1     13083 1177125 18186
    ## - pct_hs25_over               1     13637 1177679 18187
    ## - percent_married             1     13955 1177997 18188
    ## - sqrt_pct_unemployed16_over  1     14732 1178774 18190
    ## - pct_married_households      1     22811 1186853 18211
    ## - log_pct_bach_deg25_over     1     24893 1188935 18216
    ## - incidence_rate              1    277300 1441342 18803
    ## 
    ## Step:  AIC=18152.16
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + poverty_percent + 
    ##     median_age_male + avg_household_size + percent_married + 
    ##     pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + pct_hs25_over + 
    ##     log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + pct_emp_priv_coverage + 
    ##     pct_black + log_pct_other_race + pct_married_households + 
    ##     birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## - pct_emp_priv_coverage       1       208 1164338 18151
    ## - avg_household_size          1       570 1164700 18152
    ## <none>                                    1164130 18152
    ## - pct_black                   1       826 1164956 18152
    ## - pct_no_hs18_24              1       944 1165074 18153
    ## - sqrt_pct_bach_deg18_24      1      1405 1165535 18154
    ## - log_avg_ann_count           1      1407 1165538 18154
    ## - log_pct_other_race          1      2098 1166228 18156
    ## - poverty_percent             1      9455 1173585 18175
    ## - birth_rate                  1      9595 1173725 18175
    ## - median_age_male             1     12099 1176229 18182
    ## - pct_hs18_24                 1     13089 1177219 18184
    ## - pct_hs25_over               1     13747 1177877 18186
    ## - percent_married             1     14000 1178131 18187
    ## - sqrt_pct_unemployed16_over  1     14644 1178774 18188
    ## - pct_married_households      1     22730 1186860 18209
    ## - log_pct_bach_deg25_over     1     25482 1189613 18216
    ## - incidence_rate              1    277242 1441372 18801
    ## 
    ## Step:  AIC=18150.7
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + poverty_percent + 
    ##     median_age_male + avg_household_size + percent_married + 
    ##     pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + pct_hs25_over + 
    ##     log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + pct_black + 
    ##     log_pct_other_race + pct_married_households + birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## - avg_household_size          1       537 1164876 18150
    ## <none>                                    1164338 18151
    ## - pct_black                   1       889 1165227 18151
    ## - pct_no_hs18_24              1      1154 1165492 18152
    ## - log_avg_ann_count           1      1272 1165610 18152
    ## - sqrt_pct_bach_deg18_24      1      1300 1165638 18152
    ## - log_pct_other_race          1      2224 1166562 18154
    ## - birth_rate                  1     10066 1174404 18175
    ## - poverty_percent             1     10821 1175159 18177
    ## - pct_hs18_24                 1     12889 1177227 18182
    ## - percent_married             1     13895 1178233 18185
    ## - sqrt_pct_unemployed16_over  1     14437 1178775 18186
    ## - pct_hs25_over               1     14804 1179142 18187
    ## - median_age_male             1     15088 1179426 18188
    ## - pct_married_households      1     22851 1187189 18208
    ## - log_pct_bach_deg25_over     1     25282 1189620 18214
    ## - incidence_rate              1    282957 1447295 18812
    ## 
    ## Step:  AIC=18150.11
    ## target_death_rate ~ log_avg_ann_count + incidence_rate + poverty_percent + 
    ##     median_age_male + percent_married + pct_no_hs18_24 + pct_hs18_24 + 
    ##     sqrt_pct_bach_deg18_24 + pct_hs25_over + log_pct_bach_deg25_over + 
    ##     sqrt_pct_unemployed16_over + pct_black + log_pct_other_race + 
    ##     pct_married_households + birth_rate
    ## 
    ##                              Df Sum of Sq     RSS   AIC
    ## <none>                                    1164876 18150
    ## - pct_black                   1       841 1165717 18150
    ## - pct_no_hs18_24              1      1057 1165932 18151
    ## - sqrt_pct_bach_deg18_24      1      1255 1166131 18151
    ## - log_avg_ann_count           1      1258 1166134 18151
    ## - log_pct_other_race          1      2159 1167035 18154
    ## - birth_rate                  1      9865 1174740 18174
    ## - poverty_percent             1     10966 1175842 18177
    ## - percent_married             1     13373 1178248 18183
    ## - pct_hs18_24                 1     13610 1178485 18184
    ## - pct_hs25_over               1     14516 1179391 18186
    ## - sqrt_pct_unemployed16_over  1     15001 1179876 18187
    ## - median_age_male             1     16927 1181802 18192
    ## - pct_married_households      1     22521 1187397 18206
    ## - log_pct_bach_deg25_over     1     25226 1190102 18213
    ## - incidence_rate              1    282673 1447549 18810

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ log_avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_male + percent_married + pct_no_hs18_24 + 
    ##     pct_hs18_24 + sqrt_pct_bach_deg18_24 + pct_hs25_over + log_pct_bach_deg25_over + 
    ##     sqrt_pct_unemployed16_over + pct_black + log_pct_other_race + 
    ##     pct_married_households + birth_rate, data = data_trans)
    ## 
    ## Coefficients:
    ##                (Intercept)           log_avg_ann_count  
    ##                  118.97526                    -0.58359  
    ##             incidence_rate             poverty_percent  
    ##                    0.19633                     0.58455  
    ##            median_age_male             percent_married  
    ##                   -0.62607                     0.83105  
    ##             pct_no_hs18_24                 pct_hs18_24  
    ##                   -0.08724                     0.28162  
    ##     sqrt_pct_bach_deg18_24               pct_hs25_over  
    ##                   -0.92358                     0.52681  
    ##    log_pct_bach_deg25_over  sqrt_pct_unemployed16_over  
    ##                  -14.91959                     5.22124  
    ##                  pct_black          log_pct_other_race  
    ##                    0.04781                    -0.50749  
    ##     pct_married_households                  birth_rate  
    ##                   -0.97287                    -0.96956

``` r
model_step = lm(formula = target_death_rate ~ log_avg_ann_count + incidence_rate + poverty_percent + median_age_male + percent_married + pct_no_hs18_24 + pct_hs18_24 + sqrt_pct_bach_deg18_24 + pct_hs25_over + log_pct_bach_deg25_over + sqrt_pct_unemployed16_over + pct_black + log_pct_other_race + pct_married_households + birth_rate, data = data_trans)
summary(model_step) #p-value?
```

    ## 
    ## Call:
    ## lm(formula = target_death_rate ~ log_avg_ann_count + incidence_rate + 
    ##     poverty_percent + median_age_male + percent_married + pct_no_hs18_24 + 
    ##     pct_hs18_24 + sqrt_pct_bach_deg18_24 + pct_hs25_over + log_pct_bach_deg25_over + 
    ##     sqrt_pct_unemployed16_over + pct_black + log_pct_other_race + 
    ##     pct_married_households + birth_rate, data = data_trans)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -108.177  -11.107    0.042   10.900  137.170 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                118.975260  11.701380  10.168  < 2e-16 ***
    ## log_avg_ann_count           -0.583587   0.322505  -1.810   0.0705 .  
    ## incidence_rate               0.196329   0.007239  27.120  < 2e-16 ***
    ## poverty_percent              0.584552   0.109432   5.342 9.89e-08 ***
    ## median_age_male             -0.626067   0.094337  -6.637 3.79e-11 ***
    ## percent_married              0.831046   0.140883   5.899 4.06e-09 ***
    ## pct_no_hs18_24              -0.087241   0.052613  -1.658   0.0974 .  
    ## pct_hs18_24                  0.281616   0.047323   5.951 2.97e-09 ***
    ## sqrt_pct_bach_deg18_24      -0.923579   0.511009  -1.807   0.0708 .  
    ## pct_hs25_over                0.526806   0.085719   6.146 9.00e-10 ***
    ## log_pct_bach_deg25_over    -14.919586   1.841532  -8.102 7.77e-16 ***
    ## sqrt_pct_unemployed16_over   5.221240   0.835731   6.248 4.75e-10 ***
    ## pct_black                    0.047811   0.032320   1.479   0.1392    
    ## log_pct_other_race          -0.507487   0.214112  -2.370   0.0178 *  
    ## pct_married_households      -0.972872   0.127088  -7.655 2.58e-14 ***
    ## birth_rate                  -0.969558   0.191369  -5.066 4.30e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19.6 on 3031 degrees of freedom
    ## Multiple R-squared:  0.5034, Adjusted R-squared:  0.501 
    ## F-statistic: 204.9 on 15 and 3031 DF,  p-value: < 2.2e-16

Method 2 criterion-based (&gt;8?)
=================================

``` r
data_trans = as.data.frame(data_trans)
# Leaps function provides all-subsets analysis
# Printing the 2 best models of each size, using the Cp criterion:
leaps(x = data_trans[,2:22], y = data_trans[,1], nbest = 2, method = "Cp")
```

    ## $which
    ##        1     2     3     4     5     6     7     8     9     A     B     C
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 1  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 4  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 4  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 5  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 6  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 8  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 8  FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
    ## 9  FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 9  FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE
    ## 10 FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 10 FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE
    ## 11 FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 11  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 12  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 12 FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
    ## 13  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
    ## 13  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 14  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 14  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
    ## 15  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 15  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 21  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ##        D     E     F     G     H     I     J     K     L
    ## 1  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 4  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 4   TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 5   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7   TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 8   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 8   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 9   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 9   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 10  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 11  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 11  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 13  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 13  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 14  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 14  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 15  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 15  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 21  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## [11] "A"           "B"           "C"           "D"           "E"          
    ## [16] "F"           "G"           "H"           "I"           "J"          
    ## [21] "K"           "L"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 11 12 12 13
    ## [24] 13 14 14 15 15 16 16 17 17 18 18 19 19 20 20 21 21 22
    ## 
    ## $Cp
    ##  [1] 1552.68096 1822.24831  423.22714  706.64711  225.17168  239.42279
    ##  [7]  166.50024  173.33789  129.55997  129.98768   98.59579   98.91516
    ## [13]   74.24591   86.31483   65.88429   66.46511   44.00808   47.32936
    ## [19]   22.26487   27.74093   15.16951   17.70919   13.00256   13.86217
    ## [25]   12.85671   13.35465   12.60373   13.16428   12.41799   13.33222
    ## [31]   13.02120   13.88315   14.48101   14.62754   16.25165   16.34284
    ## [37]   18.12499   18.15080   20.03361   20.09257   22.00000

``` r
# Printing the 2 best models of each size, using the adjusted R^2 criterion:
leaps(x = data_trans[,2:22], y = data_trans[,1], nbest = 2, method = "adjr2")
```

    ## $which
    ##        1     2     3     4     5     6     7     8     9     A     B     C
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 1  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 4  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 4  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 5  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 6  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 8  FALSE  TRUE FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 8  FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE FALSE FALSE  TRUE FALSE
    ## 9  FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 9  FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE
    ## 10 FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 10 FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE
    ## 11 FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 11  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 12  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 12 FALSE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
    ## 13  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
    ## 13  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE FALSE
    ## 14  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 14  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE FALSE  TRUE  TRUE
    ## 15  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 15  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 21  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ##        D     E     F     G     H     I     J     K     L
    ## 1  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 1  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 2  FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 3  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 4  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 4   TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 5  FALSE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 5   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE FALSE
    ## 6   TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 7   TRUE  TRUE FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 8   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 8   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 9   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 9   TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE FALSE
    ## 10  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 10  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 11  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 11  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 12  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 13  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 13  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 14  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 14  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 15  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 15  TRUE  TRUE  TRUE FALSE FALSE FALSE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 16  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 17  TRUE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 18  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 19  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 20  TRUE  TRUE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE
    ## 21  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
    ## 
    ## $label
    ##  [1] "(Intercept)" "1"           "2"           "3"           "4"          
    ##  [6] "5"           "6"           "7"           "8"           "9"          
    ## [11] "A"           "B"           "C"           "D"           "E"          
    ## [16] "F"           "G"           "H"           "I"           "J"          
    ## [21] "K"           "L"          
    ## 
    ## $size
    ##  [1]  2  2  3  3  4  4  5  5  6  6  7  7  8  8  9  9 10 10 11 11 12 12 13
    ## [24] 13 14 14 15 15 16 16 17 17 18 18 19 19 20 20 21 21 22
    ## 
    ## $adjr2
    ##  [1] 0.2459565 0.2017268 0.4314148 0.3848969 0.4640739 0.4617341 0.4738623
    ##  [8] 0.4727393 0.4800869 0.4800166 0.4853334 0.4852809 0.4894960 0.4875119
    ## [15] 0.4910320 0.4909365 0.4947922 0.4942459 0.4985331 0.4976319 0.4998651
    ## [22] 0.4994470 0.5003864 0.5002449 0.5005752 0.5004932 0.5007817 0.5006894
    ## [29] 0.5009773 0.5008266 0.5010429 0.5009008 0.5009673 0.5009431 0.5008403
    ## [36] 0.5008253 0.5006963 0.5006921 0.5005464 0.5005367 0.5003869

``` r
# Summary of models for each size (one model per size)
rs = summary(regsubsets(target_death_rate ~ ., data = data_trans))

par(mfrow = c(1,2))

plot(2:9, rs$cp, xlab = "No of parameters", ylab = "Cp Statistic")
abline(0,1)

plot(2:9, rs$adjr2, xlab = "No of parameters", ylab = "Adj R2")
```

![](tain_model_selection_files/figure-markdown_github/criterion-1.png)
