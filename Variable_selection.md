Variable\_selection
================

``` r
data = read.csv("./Cancer_Registry.csv") %>% 
  janitor::clean_names() 

dim(data)
```

    ## [1] 3047   34

Check for missing value
=======================

``` r
na_output = vector("list", length = 34)

for (i in 1:34) {
  na_output[[i]] = sum(is.na(data[[i]])) / 3047
}

na_output
```

    ## [[1]]
    ## [1] 0
    ## 
    ## [[2]]
    ## [1] 0
    ## 
    ## [[3]]
    ## [1] 0
    ## 
    ## [[4]]
    ## [1] 0
    ## 
    ## [[5]]
    ## [1] 0
    ## 
    ## [[6]]
    ## [1] 0
    ## 
    ## [[7]]
    ## [1] 0
    ## 
    ## [[8]]
    ## [1] 0
    ## 
    ## [[9]]
    ## [1] 0
    ## 
    ## [[10]]
    ## [1] 0
    ## 
    ## [[11]]
    ## [1] 0
    ## 
    ## [[12]]
    ## [1] 0
    ## 
    ## [[13]]
    ## [1] 0
    ## 
    ## [[14]]
    ## [1] 0
    ## 
    ## [[15]]
    ## [1] 0
    ## 
    ## [[16]]
    ## [1] 0
    ## 
    ## [[17]]
    ## [1] 0
    ## 
    ## [[18]]
    ## [1] 0.749918
    ## 
    ## [[19]]
    ## [1] 0
    ## 
    ## [[20]]
    ## [1] 0
    ## 
    ## [[21]]
    ## [1] 0
    ## 
    ## [[22]]
    ## [1] 0.04988513
    ## 
    ## [[23]]
    ## [1] 0
    ## 
    ## [[24]]
    ## [1] 0
    ## 
    ## [[25]]
    ## [1] 0.1998687
    ## 
    ## [[26]]
    ## [1] 0
    ## 
    ## [[27]]
    ## [1] 0
    ## 
    ## [[28]]
    ## [1] 0
    ## 
    ## [[29]]
    ## [1] 0
    ## 
    ## [[30]]
    ## [1] 0
    ## 
    ## [[31]]
    ## [1] 0
    ## 
    ## [[32]]
    ## [1] 0
    ## 
    ## [[33]]
    ## [1] 0
    ## 
    ## [[34]]
    ## [1] 0

"na\_output" shows the percentages of missing values of each variable.

We should exclude variable 18 (pctsomecol18\_24), vairable 25 (pctprivatecoveragealone).

As for variable 22 (pctemployed16\_over), 5% is not very high, but it has high collinearity with pctunemployed16\_over. So, we decide to delete it anyway.

Check for collinearity
======================

We can see from the definitions that some variables have similar definations. We can just use a representative one in each group.

For example, "median income" and "median income per capita binned by decile" are similar. We can delete "\[9\]binnedInc".

For example, "\[1\]avgAnnCount" & "\[2\]avgDeathsPerYear" are similar, their correlation = 0.9394078. We can delete \[2\]avgDeathsPerYear.

Cov(\[6\]pop\_est2015, \[1\]avg\_ann\_count) = 0.926893538, Cov(\[5\]pop\_est2015, \[2\]avg\_deaths\_per\_year) = 0.97763406. We can delete \[6\]pop\_est2015.

Similarly, we can delete "\[24\]pct\_private\_coverage", "\[27\]pct\_public\_coverage", "\[28\]pct\_public\_coverage\_alone", "\[29\]pct\_white"

(all based on the correlation coefficients shown below)

``` r
#pay attention to extrmely high correlation coefficients
cor(data[c(-9, -13, -18, -22, -25)]) #delete [9] and [13] beacause of wrong type.
```

    ##                           avg_ann_count avg_deaths_per_year
    ## avg_ann_count               1.000000000          0.93940778
    ## avg_deaths_per_year         0.939407783          1.00000000
    ## target_death_rate          -0.143531620         -0.09071516
    ## incidence_rate              0.073553175          0.06268986
    ## med_income                  0.269144676          0.22320676
    ## pop_est2015                 0.926893538          0.97763406
    ## poverty_percent            -0.135693914         -0.06691794
    ## study_per_cap               0.082071379          0.06348833
    ## median_age                 -0.024097510         -0.02459872
    ## median_age_male            -0.124968609         -0.14848720
    ## median_age_female          -0.122844098         -0.14406921
    ## avg_household_size          0.064787793          0.08616148
    ## percent_married            -0.106107711         -0.18102911
    ## pct_no_hs18_24             -0.143326877         -0.13679416
    ## pct_hs18_24                -0.182053929         -0.15141783
    ## pct_bach_deg18_24           0.284176205          0.25976080
    ## pct_hs25_over              -0.311375212         -0.29592941
    ## pct_bach_deg25_over         0.321020553          0.29320978
    ## pct_unemployed16_over      -0.009015804          0.06970063
    ## pct_private_coverage        0.132244379          0.05618256
    ## pct_emp_priv_coverage       0.202348916          0.16012370
    ## pct_public_coverage        -0.173548301         -0.13168651
    ## pct_public_coverage_alone  -0.093699079         -0.02733797
    ## pct_white                  -0.136501141         -0.18715902
    ## pct_black                   0.031375602          0.08460710
    ## pct_asian                   0.435071173          0.44307423
    ## pct_other_race              0.209183775          0.21514936
    ## pct_married_households     -0.106220868         -0.16026613
    ## birth_rate                 -0.034507632         -0.07442001
    ##                           target_death_rate incidence_rate   med_income
    ## avg_ann_count                  -0.143531620    0.073553175  0.269144676
    ## avg_deaths_per_year            -0.090715160    0.062689857  0.223206757
    ## target_death_rate               1.000000000    0.449431698 -0.428614927
    ## incidence_rate                  0.449431698    1.000000000 -0.001036186
    ## med_income                     -0.428614927   -0.001036186  1.000000000
    ## pop_est2015                    -0.120073096    0.026912352  0.235522860
    ## poverty_percent                 0.429388980    0.009046252 -0.788965239
    ## study_per_cap                  -0.022285011    0.077282631  0.044002767
    ## median_age                      0.004375077    0.018089172 -0.013287743
    ## median_age_male                -0.021929429   -0.014733235 -0.091662642
    ## median_age_female               0.012048386   -0.009105564 -0.153278401
    ## avg_household_size             -0.036905314   -0.118399973  0.112065314
    ## percent_married                -0.266820464   -0.119524484  0.355122865
    ## pct_no_hs18_24                  0.088462610   -0.170762076 -0.289383120
    ## pct_hs18_24                     0.261975940    0.022643795 -0.190005681
    ## pct_bach_deg18_24              -0.287817410    0.046835423  0.492810246
    ## pct_hs25_over                   0.404589076    0.121724595 -0.471348267
    ## pct_bach_deg25_over            -0.485477318   -0.038177165  0.704928245
    ## pct_unemployed16_over           0.378412442    0.099979455 -0.453107661
    ## pct_private_coverage           -0.386065507    0.105174269  0.724174768
    ## pct_emp_priv_coverage          -0.267399428    0.149824526  0.747293549
    ## pct_public_coverage             0.404571656    0.046108610 -0.754821751
    ## pct_public_coverage_alone       0.449357576    0.040812287 -0.719756152
    ## pct_white                      -0.177399980   -0.014509829  0.167225441
    ## pct_black                       0.257023560    0.113488959 -0.270231619
    ## pct_asian                      -0.186331105   -0.008123427  0.425844240
    ## pct_other_race                 -0.189893571   -0.208748336  0.083634870
    ## pct_married_households         -0.293325341   -0.152176321  0.446082895
    ## birth_rate                     -0.087406970   -0.118181288 -0.010194553
    ##                           pop_est2015 poverty_percent study_per_cap
    ## avg_ann_count              0.92689354    -0.135693914   0.082071379
    ## avg_deaths_per_year        0.97763406    -0.066917939   0.063488331
    ## target_death_rate         -0.12007310     0.429388980  -0.022285011
    ## incidence_rate             0.02691235     0.009046252   0.077282631
    ## med_income                 0.23552286    -0.788965239   0.044002767
    ## pop_est2015                1.00000000    -0.065299150   0.055721518
    ## poverty_percent           -0.06529915     1.000000000  -0.055652350
    ## study_per_cap              0.05572152    -0.055652350   1.000000000
    ## median_age                -0.02521899    -0.029279996  -0.026029802
    ## median_age_male           -0.17660764    -0.214001049  -0.036647292
    ## median_age_female         -0.17793232    -0.148163541  -0.030577044
    ## avg_household_size         0.10994045     0.074307601  -0.004070887
    ## percent_married           -0.16046328    -0.642856868  -0.038143262
    ## pct_no_hs18_24            -0.12658242     0.288106366  -0.090387320
    ## pct_hs18_24               -0.15182121     0.094211082  -0.057035136
    ## pct_bach_deg18_24          0.24837541    -0.387121904   0.063819117
    ## pct_hs25_over             -0.31184921     0.194361157  -0.085127983
    ## pct_bach_deg25_over        0.29746337    -0.531599691   0.108593794
    ## pct_unemployed16_over      0.05076814     0.655148122  -0.031956813
    ## pct_private_coverage       0.05267651    -0.822534292   0.092544651
    ## pct_emp_priv_coverage      0.15864952    -0.683099657   0.100063185
    ## pct_public_coverage       -0.16006562     0.651162060  -0.051496680
    ## pct_public_coverage_alone -0.04146881     0.798642030  -0.055511989
    ## pct_white                 -0.19009450    -0.509432808   0.023291042
    ## pct_black                  0.07304407     0.511529663  -0.019761153
    ## pct_asian                  0.46416779    -0.157288704   0.062543075
    ## pct_other_race             0.24146800     0.047095893  -0.015247481
    ## pct_married_households    -0.12797946    -0.604952784  -0.051735616
    ## birth_rate                -0.05774018    -0.012282511   0.010676193
    ##                             median_age median_age_male median_age_female
    ## avg_ann_count             -0.024097510    -0.124968609      -0.122844098
    ## avg_deaths_per_year       -0.024598722    -0.148487199      -0.144069211
    ## target_death_rate          0.004375077    -0.021929429       0.012048386
    ## incidence_rate             0.018089172    -0.014733235      -0.009105564
    ## med_income                -0.013287743    -0.091662642      -0.153278401
    ## pop_est2015               -0.025218994    -0.176607643      -0.177932323
    ## poverty_percent           -0.029279996    -0.214001049      -0.148163541
    ## study_per_cap             -0.026029802    -0.036647292      -0.030577044
    ## median_age                 1.000000000     0.129119478       0.124678372
    ## median_age_male            0.129119478     1.000000000       0.933696103
    ## median_age_female          0.124678372     0.933696103       1.000000000
    ## avg_household_size        -0.031944148    -0.343188659      -0.367585149
    ## percent_married            0.046371506     0.449986173       0.375207983
    ## pct_no_hs18_24             0.006178084     0.100485523       0.136361328
    ## pct_hs18_24                0.050573668     0.241309928       0.242827279
    ## pct_bach_deg18_24         -0.016909407    -0.034135247      -0.070698993
    ## pct_hs25_over              0.036587378     0.318277051       0.344839719
    ## pct_bach_deg25_over       -0.020352194    -0.131599355      -0.180845331
    ## pct_unemployed16_over      0.018590443    -0.142737472      -0.111161313
    ## pct_private_coverage       0.004665111     0.082231778       0.046909158
    ## pct_emp_priv_coverage     -0.036926459    -0.208663968      -0.252221140
    ## pct_public_coverage        0.049060211     0.398967231       0.455496465
    ## pct_public_coverage_alone -0.003297872     0.002478719       0.047659145
    ## pct_white                  0.035009366     0.398044362       0.339803910
    ## pct_black                 -0.017173240    -0.242748132      -0.156728442
    ## pct_asian                 -0.038423911    -0.238322374      -0.258747912
    ## pct_other_race            -0.030276508    -0.266655447      -0.274119578
    ## pct_married_households     0.014503609     0.222277744       0.161506831
    ## birth_rate                -0.008276233    -0.104105160      -0.098812608
    ##                           avg_household_size percent_married
    ## avg_ann_count                    0.064787793     -0.10610771
    ## avg_deaths_per_year              0.086161477     -0.18102911
    ## target_death_rate               -0.036905314     -0.26682046
    ## incidence_rate                  -0.118399973     -0.11952448
    ## med_income                       0.112065314      0.35512286
    ## pop_est2015                      0.109940447     -0.16046328
    ## poverty_percent                  0.074307601     -0.64285687
    ## study_per_cap                   -0.004070887     -0.03814326
    ## median_age                      -0.031944148      0.04637151
    ## median_age_male                 -0.343188659      0.44998617
    ## median_age_female               -0.367585149      0.37520798
    ## avg_household_size               1.000000000     -0.10051170
    ## percent_married                 -0.100511698      1.00000000
    ## pct_no_hs18_24                   0.064718590     -0.01237458
    ## pct_hs18_24                      0.027228204      0.13279244
    ## pct_bach_deg18_24               -0.060960847      0.05303732
    ## pct_hs25_over                   -0.138728398      0.10243370
    ## pct_bach_deg25_over              0.013917803      0.10358519
    ## pct_unemployed16_over            0.131506325     -0.55148349
    ## pct_private_coverage            -0.144390600      0.44945161
    ## pct_emp_priv_coverage            0.011111227      0.23289907
    ## pct_public_coverage             -0.134812156     -0.24697154
    ## pct_public_coverage_alone        0.061114735     -0.45998992
    ## pct_white                       -0.188445815      0.67741994
    ## pct_black                        0.030277977     -0.62235733
    ## pct_asian                        0.131535433     -0.14869134
    ## pct_other_race                   0.229439641     -0.10466945
    ## pct_married_households           0.091450373      0.87026054
    ## birth_rate                       0.075917596      0.14140393
    ##                           pct_no_hs18_24 pct_hs18_24 pct_bach_deg18_24
    ## avg_ann_count               -0.143326877 -0.18205393      0.2841762046
    ## avg_deaths_per_year         -0.136794157 -0.15141783      0.2597607979
    ## target_death_rate            0.088462610  0.26197594     -0.2878174102
    ## incidence_rate              -0.170762076  0.02264379      0.0468354233
    ## med_income                  -0.289383120 -0.19000568      0.4928102457
    ## pop_est2015                 -0.126582418 -0.15182121      0.2483754141
    ## poverty_percent              0.288106366  0.09421108     -0.3871219044
    ## study_per_cap               -0.090387320 -0.05703514      0.0638191165
    ## median_age                   0.006178084  0.05057367     -0.0169094070
    ## median_age_male              0.100485523  0.24130993     -0.0341352465
    ## median_age_female            0.136361328  0.24282728     -0.0706989935
    ## avg_household_size           0.064718590  0.02722820     -0.0609608467
    ## percent_married             -0.012374580  0.13279244      0.0530373214
    ## pct_no_hs18_24               1.000000000  0.08462928     -0.3814220162
    ## pct_hs18_24                  0.084629285  1.00000000     -0.3893339119
    ## pct_bach_deg18_24           -0.381422016 -0.38933391      1.0000000000
    ## pct_hs25_over                0.217069496  0.43892915     -0.3840487785
    ## pct_bach_deg25_over         -0.396578614 -0.40475397      0.5998141845
    ## pct_unemployed16_over        0.181193218  0.13069406     -0.3089196535
    ## pct_private_coverage        -0.454750805 -0.25385075      0.4877417395
    ## pct_emp_priv_coverage       -0.429994050 -0.24449415      0.4509960515
    ## pct_public_coverage          0.318540309  0.27822049     -0.4224703062
    ## pct_public_coverage_alone    0.327269783  0.23412398     -0.4218045942
    ## pct_white                   -0.157282267  0.04530637      0.0691328203
    ## pct_black                    0.116805155 -0.02486791     -0.0936139964
    ## pct_asian                   -0.217534569 -0.19977046      0.3458827707
    ## pct_other_race               0.126256354 -0.06041485      0.0065469377
    ## pct_married_households       0.005339552  0.12004023     -0.0001044447
    ## birth_rate                   0.125894802  0.05822688     -0.1250734830
    ##                           pct_hs25_over pct_bach_deg25_over
    ## avg_ann_count               -0.31137521          0.32102055
    ## avg_deaths_per_year         -0.29592941          0.29320978
    ## target_death_rate            0.40458908         -0.48547732
    ## incidence_rate               0.12172459         -0.03817717
    ## med_income                  -0.47134827          0.70492824
    ## pop_est2015                 -0.31184921          0.29746337
    ## poverty_percent              0.19436116         -0.53159969
    ## study_per_cap               -0.08512798          0.10859379
    ## median_age                   0.03658738         -0.02035219
    ## median_age_male              0.31827705         -0.13159935
    ## median_age_female            0.34483972         -0.18084533
    ## avg_household_size          -0.13872840          0.01391780
    ## percent_married              0.10243370          0.10358519
    ## pct_no_hs18_24               0.21706950         -0.39657861
    ## pct_hs18_24                  0.43892915         -0.40475397
    ## pct_bach_deg18_24           -0.38404878          0.59981418
    ## pct_hs25_over                1.00000000         -0.74061122
    ## pct_bach_deg25_over         -0.74061122          1.00000000
    ## pct_unemployed16_over        0.08230552         -0.37298005
    ## pct_private_coverage        -0.22193481          0.60324766
    ## pct_emp_priv_coverage       -0.22280299          0.53908363
    ## pct_public_coverage          0.42797377         -0.63609480
    ## pct_public_coverage_alone    0.29714338         -0.60575990
    ## pct_white                    0.18804475          0.04865228
    ## pct_black                   -0.02444526         -0.14640875
    ## pct_asian                   -0.43656094          0.43796288
    ## pct_other_race              -0.28561114          0.03907545
    ## pct_married_households       0.06217592          0.09813386
    ## birth_rate                   0.01660026         -0.08794027
    ##                           pct_unemployed16_over pct_private_coverage
    ## avg_ann_count                      -0.009015804          0.132244379
    ## avg_deaths_per_year                 0.069700627          0.056182557
    ## target_death_rate                   0.378412442         -0.386065507
    ## incidence_rate                      0.099979455          0.105174269
    ## med_income                         -0.453107661          0.724174768
    ## pop_est2015                         0.050768138          0.052676513
    ## poverty_percent                     0.655148122         -0.822534292
    ## study_per_cap                      -0.031956813          0.092544651
    ## median_age                          0.018590443          0.004665111
    ## median_age_male                    -0.142737472          0.082231778
    ## median_age_female                  -0.111161313          0.046909158
    ## avg_household_size                  0.131506325         -0.144390600
    ## percent_married                    -0.551483488          0.449451608
    ## pct_no_hs18_24                      0.181193218         -0.454750805
    ## pct_hs18_24                         0.130694061         -0.253850745
    ## pct_bach_deg18_24                  -0.308919654          0.487741739
    ## pct_hs25_over                       0.082305516         -0.221934807
    ## pct_bach_deg25_over                -0.372980047          0.603247665
    ## pct_unemployed16_over               1.000000000         -0.634317281
    ## pct_private_coverage               -0.634317281          1.000000000
    ## pct_emp_priv_coverage              -0.474745168          0.827458844
    ## pct_public_coverage                 0.529821296         -0.720011521
    ## pct_public_coverage_alone           0.655365736         -0.886233694
    ## pct_white                          -0.501755245          0.429031447
    ## pct_black                           0.469273102         -0.345172126
    ## pct_asian                          -0.022020273          0.189331755
    ## pct_other_race                      0.028463247         -0.176300307
    ## pct_married_households             -0.469609014          0.434640055
    ## birth_rate                         -0.067906273         -0.040436613
    ##                           pct_emp_priv_coverage pct_public_coverage
    ## avg_ann_count                        0.20234892         -0.17354830
    ## avg_deaths_per_year                  0.16012370         -0.13168651
    ## target_death_rate                   -0.26739943          0.40457166
    ## incidence_rate                       0.14982453          0.04610861
    ## med_income                           0.74729355         -0.75482175
    ## pop_est2015                          0.15864952         -0.16006562
    ## poverty_percent                     -0.68309966          0.65116206
    ## study_per_cap                        0.10006319         -0.05149668
    ## median_age                          -0.03692646          0.04906021
    ## median_age_male                     -0.20866397          0.39896723
    ## median_age_female                   -0.25222114          0.45549646
    ## avg_household_size                   0.01111123         -0.13481216
    ## percent_married                      0.23289907         -0.24697154
    ## pct_no_hs18_24                      -0.42999405          0.31854031
    ## pct_hs18_24                         -0.24449415          0.27822049
    ## pct_bach_deg18_24                    0.45099605         -0.42247031
    ## pct_hs25_over                       -0.22280299          0.42797377
    ## pct_bach_deg25_over                  0.53908363         -0.63609480
    ## pct_unemployed16_over               -0.47474517          0.52982130
    ## pct_private_coverage                 0.82745884         -0.72001152
    ## pct_emp_priv_coverage                1.00000000         -0.77831482
    ## pct_public_coverage                 -0.77831482          1.00000000
    ## pct_public_coverage_alone           -0.72882303          0.86583279
    ## pct_white                            0.26981502         -0.13370507
    ## pct_black                           -0.23738803          0.19559747
    ## pct_asian                            0.28248429         -0.30562546
    ## pct_other_race                      -0.06422598         -0.07870778
    ## pct_married_households               0.32256933         -0.36217051
    ## birth_rate                          -0.09387800         -0.03053076
    ##                           pct_public_coverage_alone    pct_white
    ## avg_ann_count                          -0.093699079 -0.136501141
    ## avg_deaths_per_year                    -0.027337969 -0.187159023
    ## target_death_rate                       0.449357576 -0.177399980
    ## incidence_rate                          0.040812287 -0.014509829
    ## med_income                             -0.719756152  0.167225441
    ## pop_est2015                            -0.041468807 -0.190094503
    ## poverty_percent                         0.798642030 -0.509432808
    ## study_per_cap                          -0.055511989  0.023291042
    ## median_age                             -0.003297872  0.035009366
    ## median_age_male                         0.002478719  0.398044362
    ## median_age_female                       0.047659145  0.339803910
    ## avg_household_size                      0.061114735 -0.188445815
    ## percent_married                        -0.459989923  0.677419940
    ## pct_no_hs18_24                          0.327269783 -0.157282267
    ## pct_hs18_24                             0.234123984  0.045306371
    ## pct_bach_deg18_24                      -0.421804594  0.069132820
    ## pct_hs25_over                           0.297143381  0.188044752
    ## pct_bach_deg25_over                    -0.605759903  0.048652281
    ## pct_unemployed16_over                   0.655365736 -0.501755245
    ## pct_private_coverage                   -0.886233694  0.429031447
    ## pct_emp_priv_coverage                  -0.728823026  0.269815023
    ## pct_public_coverage                     0.865832788 -0.133705071
    ## pct_public_coverage_alone               1.000000000 -0.361026352
    ## pct_white                              -0.361026352  1.000000000
    ## pct_black                               0.330110279 -0.828458852
    ## pct_asian                              -0.181380191 -0.265676411
    ## pct_other_race                          0.083755384 -0.233692379
    ## pct_married_households                 -0.473993882  0.596771068
    ## birth_rate                             -0.004752695 -0.008958097
    ##                             pct_black    pct_asian pct_other_race
    ## avg_ann_count              0.03137560  0.435071173    0.209183775
    ## avg_deaths_per_year        0.08460710  0.443074226    0.215149359
    ## target_death_rate          0.25702356 -0.186331105   -0.189893571
    ## incidence_rate             0.11348896 -0.008123427   -0.208748336
    ## med_income                -0.27023162  0.425844240    0.083634870
    ## pop_est2015                0.07304407  0.464167791    0.241468004
    ## poverty_percent            0.51152966 -0.157288704    0.047095893
    ## study_per_cap             -0.01976115  0.062543075   -0.015247481
    ## median_age                -0.01717324 -0.038423911   -0.030276508
    ## median_age_male           -0.24274813 -0.238322374   -0.266655447
    ## median_age_female         -0.15672844 -0.258747912   -0.274119578
    ## avg_household_size         0.03027798  0.131535433    0.229439641
    ## percent_married           -0.62235733 -0.148691337   -0.104669448
    ## pct_no_hs18_24             0.11680516 -0.217534569    0.126256354
    ## pct_hs18_24               -0.02486791 -0.199770462   -0.060414849
    ## pct_bach_deg18_24         -0.09361400  0.345882771    0.006546938
    ## pct_hs25_over             -0.02444526 -0.436560938   -0.285611137
    ## pct_bach_deg25_over       -0.14640875  0.437962881    0.039075451
    ## pct_unemployed16_over      0.46927310 -0.022020273    0.028463247
    ## pct_private_coverage      -0.34517213  0.189331755   -0.176300307
    ## pct_emp_priv_coverage     -0.23738803  0.282484289   -0.064225979
    ## pct_public_coverage        0.19559747 -0.305625464   -0.078707776
    ## pct_public_coverage_alone  0.33011028 -0.181380191    0.083755384
    ## pct_white                 -0.82845885 -0.265676411   -0.233692379
    ## pct_black                  1.00000000  0.016583407   -0.023001308
    ## pct_asian                  0.01658341  1.000000000    0.200781103
    ## pct_other_race            -0.02300131  0.200781103    1.000000000
    ## pct_married_households    -0.57359245 -0.086602036   -0.027352296
    ## birth_rate                -0.06780483 -0.061946987    0.059829476
    ##                           pct_married_households   birth_rate
    ## avg_ann_count                      -0.1062208681 -0.034507632
    ## avg_deaths_per_year                -0.1602661296 -0.074420014
    ## target_death_rate                  -0.2933253405 -0.087406970
    ## incidence_rate                     -0.1521763205 -0.118181288
    ## med_income                          0.4460828953 -0.010194553
    ## pop_est2015                        -0.1279794627 -0.057740178
    ## poverty_percent                    -0.6049527844 -0.012282511
    ## study_per_cap                      -0.0517356157  0.010676193
    ## median_age                          0.0145036093 -0.008276233
    ## median_age_male                     0.2222777445 -0.104105160
    ## median_age_female                   0.1615068308 -0.098812608
    ## avg_household_size                  0.0914503733  0.075917596
    ## percent_married                     0.8702605365  0.141403930
    ## pct_no_hs18_24                      0.0053395517  0.125894802
    ## pct_hs18_24                         0.1200402276  0.058226877
    ## pct_bach_deg18_24                  -0.0001044447 -0.125073483
    ## pct_hs25_over                       0.0621759178  0.016600264
    ## pct_bach_deg25_over                 0.0981338597 -0.087940271
    ## pct_unemployed16_over              -0.4696090139 -0.067906273
    ## pct_private_coverage                0.4346400550 -0.040436613
    ## pct_emp_priv_coverage               0.3225693263 -0.093878001
    ## pct_public_coverage                -0.3621705058 -0.030530759
    ## pct_public_coverage_alone          -0.4739938818 -0.004752695
    ## pct_white                           0.5967710681 -0.008958097
    ## pct_black                          -0.5735924510 -0.067804827
    ## pct_asian                          -0.0866020358 -0.061946987
    ## pct_other_race                     -0.0273522962  0.059829476
    ## pct_married_households              1.0000000000  0.102263300
    ## birth_rate                          0.1022633001  1.000000000
