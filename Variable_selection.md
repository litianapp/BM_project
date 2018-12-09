Variable\_selection
================

``` r
data = read.csv("./Cancer_Registry.csv")
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

We can see from the definitions that some variables are similar. We can just use a representative one in each group.

For example, "median income" and "median income per capita binned by decile" are similar. We can delete "\[9\]binnedInc".

For example, "\[1\]avgAnnCount" & "\[2\]avgDeathsPerYear" are similar, their correlation = 0.9394078. We can delete \[2\]avgDeathsPerYear.

Cov(\[6\]pop\_est2015, \[1\]avg\_ann\_count) = 0.926893538, Cov(\[5\]pop\_est2015, \[2\]avg\_deaths\_per\_year) = 0.97763406. We can delete \[6\]pop\_est2015.

Similarly, we can delete "\[24\]pct\_private\_coverage", "\[27\]pct\_public\_coverage", "\[28\]pct\_public\_coverage\_alone", "\[29\]pct\_white"

(all based on the correlation coefficients shown below)

``` r
#pay attention to extrmely high correlation coefficients
cor(data[c(-9, -13, -18, -22, -25)]) #delete [9] and [13] beacause of wrong type.
```

    ##                         avgAnnCount avgDeathsPerYear TARGET_deathRate
    ## avgAnnCount             1.000000000       0.93940778     -0.143531620
    ## avgDeathsPerYear        0.939407783       1.00000000     -0.090715160
    ## TARGET_deathRate       -0.143531620      -0.09071516      1.000000000
    ## incidenceRate           0.073553175       0.06268986      0.449431698
    ## medIncome               0.269144676       0.22320676     -0.428614927
    ## popEst2015              0.926893538       0.97763406     -0.120073096
    ## povertyPercent         -0.135693914      -0.06691794      0.429388980
    ## studyPerCap             0.082071379       0.06348833     -0.022285011
    ## MedianAge              -0.024097510      -0.02459872      0.004375077
    ## MedianAgeMale          -0.124968609      -0.14848720     -0.021929429
    ## MedianAgeFemale        -0.122844098      -0.14406921      0.012048386
    ## AvgHouseholdSize        0.064787793       0.08616148     -0.036905314
    ## PercentMarried         -0.106107711      -0.18102911     -0.266820464
    ## PctNoHS18_24           -0.143326877      -0.13679416      0.088462610
    ## PctHS18_24             -0.182053929      -0.15141783      0.261975940
    ## PctBachDeg18_24         0.284176205       0.25976080     -0.287817410
    ## PctHS25_Over           -0.311375212      -0.29592941      0.404589076
    ## PctBachDeg25_Over       0.321020553       0.29320978     -0.485477318
    ## PctUnemployed16_Over   -0.009015804       0.06970063      0.378412442
    ## PctPrivateCoverage      0.132244379       0.05618256     -0.386065507
    ## PctEmpPrivCoverage      0.202348916       0.16012370     -0.267399428
    ## PctPublicCoverage      -0.173548301      -0.13168651      0.404571656
    ## PctPublicCoverageAlone -0.093699079      -0.02733797      0.449357576
    ## PctWhite               -0.136501141      -0.18715902     -0.177399980
    ## PctBlack                0.031375602       0.08460710      0.257023560
    ## PctAsian                0.435071173       0.44307423     -0.186331105
    ## PctOtherRace            0.209183775       0.21514936     -0.189893571
    ## PctMarriedHouseholds   -0.106220868      -0.16026613     -0.293325341
    ## BirthRate              -0.034507632      -0.07442001     -0.087406970
    ##                        incidenceRate    medIncome  popEst2015
    ## avgAnnCount              0.073553175  0.269144676  0.92689354
    ## avgDeathsPerYear         0.062689857  0.223206757  0.97763406
    ## TARGET_deathRate         0.449431698 -0.428614927 -0.12007310
    ## incidenceRate            1.000000000 -0.001036186  0.02691235
    ## medIncome               -0.001036186  1.000000000  0.23552286
    ## popEst2015               0.026912352  0.235522860  1.00000000
    ## povertyPercent           0.009046252 -0.788965239 -0.06529915
    ## studyPerCap              0.077282631  0.044002767  0.05572152
    ## MedianAge                0.018089172 -0.013287743 -0.02521899
    ## MedianAgeMale           -0.014733235 -0.091662642 -0.17660764
    ## MedianAgeFemale         -0.009105564 -0.153278401 -0.17793232
    ## AvgHouseholdSize        -0.118399973  0.112065314  0.10994045
    ## PercentMarried          -0.119524484  0.355122865 -0.16046328
    ## PctNoHS18_24            -0.170762076 -0.289383120 -0.12658242
    ## PctHS18_24               0.022643795 -0.190005681 -0.15182121
    ## PctBachDeg18_24          0.046835423  0.492810246  0.24837541
    ## PctHS25_Over             0.121724595 -0.471348267 -0.31184921
    ## PctBachDeg25_Over       -0.038177165  0.704928245  0.29746337
    ## PctUnemployed16_Over     0.099979455 -0.453107661  0.05076814
    ## PctPrivateCoverage       0.105174269  0.724174768  0.05267651
    ## PctEmpPrivCoverage       0.149824526  0.747293549  0.15864952
    ## PctPublicCoverage        0.046108610 -0.754821751 -0.16006562
    ## PctPublicCoverageAlone   0.040812287 -0.719756152 -0.04146881
    ## PctWhite                -0.014509829  0.167225441 -0.19009450
    ## PctBlack                 0.113488959 -0.270231619  0.07304407
    ## PctAsian                -0.008123427  0.425844240  0.46416779
    ## PctOtherRace            -0.208748336  0.083634870  0.24146800
    ## PctMarriedHouseholds    -0.152176321  0.446082895 -0.12797946
    ## BirthRate               -0.118181288 -0.010194553 -0.05774018
    ##                        povertyPercent  studyPerCap    MedianAge
    ## avgAnnCount              -0.135693914  0.082071379 -0.024097510
    ## avgDeathsPerYear         -0.066917939  0.063488331 -0.024598722
    ## TARGET_deathRate          0.429388980 -0.022285011  0.004375077
    ## incidenceRate             0.009046252  0.077282631  0.018089172
    ## medIncome                -0.788965239  0.044002767 -0.013287743
    ## popEst2015               -0.065299150  0.055721518 -0.025218994
    ## povertyPercent            1.000000000 -0.055652350 -0.029279996
    ## studyPerCap              -0.055652350  1.000000000 -0.026029802
    ## MedianAge                -0.029279996 -0.026029802  1.000000000
    ## MedianAgeMale            -0.214001049 -0.036647292  0.129119478
    ## MedianAgeFemale          -0.148163541 -0.030577044  0.124678372
    ## AvgHouseholdSize          0.074307601 -0.004070887 -0.031944148
    ## PercentMarried           -0.642856868 -0.038143262  0.046371506
    ## PctNoHS18_24              0.288106366 -0.090387320  0.006178084
    ## PctHS18_24                0.094211082 -0.057035136  0.050573668
    ## PctBachDeg18_24          -0.387121904  0.063819117 -0.016909407
    ## PctHS25_Over              0.194361157 -0.085127983  0.036587378
    ## PctBachDeg25_Over        -0.531599691  0.108593794 -0.020352194
    ## PctUnemployed16_Over      0.655148122 -0.031956813  0.018590443
    ## PctPrivateCoverage       -0.822534292  0.092544651  0.004665111
    ## PctEmpPrivCoverage       -0.683099657  0.100063185 -0.036926459
    ## PctPublicCoverage         0.651162060 -0.051496680  0.049060211
    ## PctPublicCoverageAlone    0.798642030 -0.055511989 -0.003297872
    ## PctWhite                 -0.509432808  0.023291042  0.035009366
    ## PctBlack                  0.511529663 -0.019761153 -0.017173240
    ## PctAsian                 -0.157288704  0.062543075 -0.038423911
    ## PctOtherRace              0.047095893 -0.015247481 -0.030276508
    ## PctMarriedHouseholds     -0.604952784 -0.051735616  0.014503609
    ## BirthRate                -0.012282511  0.010676193 -0.008276233
    ##                        MedianAgeMale MedianAgeFemale AvgHouseholdSize
    ## avgAnnCount             -0.124968609    -0.122844098      0.064787793
    ## avgDeathsPerYear        -0.148487199    -0.144069211      0.086161477
    ## TARGET_deathRate        -0.021929429     0.012048386     -0.036905314
    ## incidenceRate           -0.014733235    -0.009105564     -0.118399973
    ## medIncome               -0.091662642    -0.153278401      0.112065314
    ## popEst2015              -0.176607643    -0.177932323      0.109940447
    ## povertyPercent          -0.214001049    -0.148163541      0.074307601
    ## studyPerCap             -0.036647292    -0.030577044     -0.004070887
    ## MedianAge                0.129119478     0.124678372     -0.031944148
    ## MedianAgeMale            1.000000000     0.933696103     -0.343188659
    ## MedianAgeFemale          0.933696103     1.000000000     -0.367585149
    ## AvgHouseholdSize        -0.343188659    -0.367585149      1.000000000
    ## PercentMarried           0.449986173     0.375207983     -0.100511698
    ## PctNoHS18_24             0.100485523     0.136361328      0.064718590
    ## PctHS18_24               0.241309928     0.242827279      0.027228204
    ## PctBachDeg18_24         -0.034135247    -0.070698993     -0.060960847
    ## PctHS25_Over             0.318277051     0.344839719     -0.138728398
    ## PctBachDeg25_Over       -0.131599355    -0.180845331      0.013917803
    ## PctUnemployed16_Over    -0.142737472    -0.111161313      0.131506325
    ## PctPrivateCoverage       0.082231778     0.046909158     -0.144390600
    ## PctEmpPrivCoverage      -0.208663968    -0.252221140      0.011111227
    ## PctPublicCoverage        0.398967231     0.455496465     -0.134812156
    ## PctPublicCoverageAlone   0.002478719     0.047659145      0.061114735
    ## PctWhite                 0.398044362     0.339803910     -0.188445815
    ## PctBlack                -0.242748132    -0.156728442      0.030277977
    ## PctAsian                -0.238322374    -0.258747912      0.131535433
    ## PctOtherRace            -0.266655447    -0.274119578      0.229439641
    ## PctMarriedHouseholds     0.222277744     0.161506831      0.091450373
    ## BirthRate               -0.104105160    -0.098812608      0.075917596
    ##                        PercentMarried PctNoHS18_24  PctHS18_24
    ## avgAnnCount               -0.10610771 -0.143326877 -0.18205393
    ## avgDeathsPerYear          -0.18102911 -0.136794157 -0.15141783
    ## TARGET_deathRate          -0.26682046  0.088462610  0.26197594
    ## incidenceRate             -0.11952448 -0.170762076  0.02264379
    ## medIncome                  0.35512286 -0.289383120 -0.19000568
    ## popEst2015                -0.16046328 -0.126582418 -0.15182121
    ## povertyPercent            -0.64285687  0.288106366  0.09421108
    ## studyPerCap               -0.03814326 -0.090387320 -0.05703514
    ## MedianAge                  0.04637151  0.006178084  0.05057367
    ## MedianAgeMale              0.44998617  0.100485523  0.24130993
    ## MedianAgeFemale            0.37520798  0.136361328  0.24282728
    ## AvgHouseholdSize          -0.10051170  0.064718590  0.02722820
    ## PercentMarried             1.00000000 -0.012374580  0.13279244
    ## PctNoHS18_24              -0.01237458  1.000000000  0.08462928
    ## PctHS18_24                 0.13279244  0.084629285  1.00000000
    ## PctBachDeg18_24            0.05303732 -0.381422016 -0.38933391
    ## PctHS25_Over               0.10243370  0.217069496  0.43892915
    ## PctBachDeg25_Over          0.10358519 -0.396578614 -0.40475397
    ## PctUnemployed16_Over      -0.55148349  0.181193218  0.13069406
    ## PctPrivateCoverage         0.44945161 -0.454750805 -0.25385075
    ## PctEmpPrivCoverage         0.23289907 -0.429994050 -0.24449415
    ## PctPublicCoverage         -0.24697154  0.318540309  0.27822049
    ## PctPublicCoverageAlone    -0.45998992  0.327269783  0.23412398
    ## PctWhite                   0.67741994 -0.157282267  0.04530637
    ## PctBlack                  -0.62235733  0.116805155 -0.02486791
    ## PctAsian                  -0.14869134 -0.217534569 -0.19977046
    ## PctOtherRace              -0.10466945  0.126256354 -0.06041485
    ## PctMarriedHouseholds       0.87026054  0.005339552  0.12004023
    ## BirthRate                  0.14140393  0.125894802  0.05822688
    ##                        PctBachDeg18_24 PctHS25_Over PctBachDeg25_Over
    ## avgAnnCount               0.2841762046  -0.31137521        0.32102055
    ## avgDeathsPerYear          0.2597607979  -0.29592941        0.29320978
    ## TARGET_deathRate         -0.2878174102   0.40458908       -0.48547732
    ## incidenceRate             0.0468354233   0.12172459       -0.03817717
    ## medIncome                 0.4928102457  -0.47134827        0.70492824
    ## popEst2015                0.2483754141  -0.31184921        0.29746337
    ## povertyPercent           -0.3871219044   0.19436116       -0.53159969
    ## studyPerCap               0.0638191165  -0.08512798        0.10859379
    ## MedianAge                -0.0169094070   0.03658738       -0.02035219
    ## MedianAgeMale            -0.0341352465   0.31827705       -0.13159935
    ## MedianAgeFemale          -0.0706989935   0.34483972       -0.18084533
    ## AvgHouseholdSize         -0.0609608467  -0.13872840        0.01391780
    ## PercentMarried            0.0530373214   0.10243370        0.10358519
    ## PctNoHS18_24             -0.3814220162   0.21706950       -0.39657861
    ## PctHS18_24               -0.3893339119   0.43892915       -0.40475397
    ## PctBachDeg18_24           1.0000000000  -0.38404878        0.59981418
    ## PctHS25_Over             -0.3840487785   1.00000000       -0.74061122
    ## PctBachDeg25_Over         0.5998141845  -0.74061122        1.00000000
    ## PctUnemployed16_Over     -0.3089196535   0.08230552       -0.37298005
    ## PctPrivateCoverage        0.4877417395  -0.22193481        0.60324766
    ## PctEmpPrivCoverage        0.4509960515  -0.22280299        0.53908363
    ## PctPublicCoverage        -0.4224703062   0.42797377       -0.63609480
    ## PctPublicCoverageAlone   -0.4218045942   0.29714338       -0.60575990
    ## PctWhite                  0.0691328203   0.18804475        0.04865228
    ## PctBlack                 -0.0936139964  -0.02444526       -0.14640875
    ## PctAsian                  0.3458827707  -0.43656094        0.43796288
    ## PctOtherRace              0.0065469377  -0.28561114        0.03907545
    ## PctMarriedHouseholds     -0.0001044447   0.06217592        0.09813386
    ## BirthRate                -0.1250734830   0.01660026       -0.08794027
    ##                        PctUnemployed16_Over PctPrivateCoverage
    ## avgAnnCount                    -0.009015804        0.132244379
    ## avgDeathsPerYear                0.069700627        0.056182557
    ## TARGET_deathRate                0.378412442       -0.386065507
    ## incidenceRate                   0.099979455        0.105174269
    ## medIncome                      -0.453107661        0.724174768
    ## popEst2015                      0.050768138        0.052676513
    ## povertyPercent                  0.655148122       -0.822534292
    ## studyPerCap                    -0.031956813        0.092544651
    ## MedianAge                       0.018590443        0.004665111
    ## MedianAgeMale                  -0.142737472        0.082231778
    ## MedianAgeFemale                -0.111161313        0.046909158
    ## AvgHouseholdSize                0.131506325       -0.144390600
    ## PercentMarried                 -0.551483488        0.449451608
    ## PctNoHS18_24                    0.181193218       -0.454750805
    ## PctHS18_24                      0.130694061       -0.253850745
    ## PctBachDeg18_24                -0.308919654        0.487741739
    ## PctHS25_Over                    0.082305516       -0.221934807
    ## PctBachDeg25_Over              -0.372980047        0.603247665
    ## PctUnemployed16_Over            1.000000000       -0.634317281
    ## PctPrivateCoverage             -0.634317281        1.000000000
    ## PctEmpPrivCoverage             -0.474745168        0.827458844
    ## PctPublicCoverage               0.529821296       -0.720011521
    ## PctPublicCoverageAlone          0.655365736       -0.886233694
    ## PctWhite                       -0.501755245        0.429031447
    ## PctBlack                        0.469273102       -0.345172126
    ## PctAsian                       -0.022020273        0.189331755
    ## PctOtherRace                    0.028463247       -0.176300307
    ## PctMarriedHouseholds           -0.469609014        0.434640055
    ## BirthRate                      -0.067906273       -0.040436613
    ##                        PctEmpPrivCoverage PctPublicCoverage
    ## avgAnnCount                    0.20234892       -0.17354830
    ## avgDeathsPerYear               0.16012370       -0.13168651
    ## TARGET_deathRate              -0.26739943        0.40457166
    ## incidenceRate                  0.14982453        0.04610861
    ## medIncome                      0.74729355       -0.75482175
    ## popEst2015                     0.15864952       -0.16006562
    ## povertyPercent                -0.68309966        0.65116206
    ## studyPerCap                    0.10006319       -0.05149668
    ## MedianAge                     -0.03692646        0.04906021
    ## MedianAgeMale                 -0.20866397        0.39896723
    ## MedianAgeFemale               -0.25222114        0.45549646
    ## AvgHouseholdSize               0.01111123       -0.13481216
    ## PercentMarried                 0.23289907       -0.24697154
    ## PctNoHS18_24                  -0.42999405        0.31854031
    ## PctHS18_24                    -0.24449415        0.27822049
    ## PctBachDeg18_24                0.45099605       -0.42247031
    ## PctHS25_Over                  -0.22280299        0.42797377
    ## PctBachDeg25_Over              0.53908363       -0.63609480
    ## PctUnemployed16_Over          -0.47474517        0.52982130
    ## PctPrivateCoverage             0.82745884       -0.72001152
    ## PctEmpPrivCoverage             1.00000000       -0.77831482
    ## PctPublicCoverage             -0.77831482        1.00000000
    ## PctPublicCoverageAlone        -0.72882303        0.86583279
    ## PctWhite                       0.26981502       -0.13370507
    ## PctBlack                      -0.23738803        0.19559747
    ## PctAsian                       0.28248429       -0.30562546
    ## PctOtherRace                  -0.06422598       -0.07870778
    ## PctMarriedHouseholds           0.32256933       -0.36217051
    ## BirthRate                     -0.09387800       -0.03053076
    ##                        PctPublicCoverageAlone     PctWhite    PctBlack
    ## avgAnnCount                      -0.093699079 -0.136501141  0.03137560
    ## avgDeathsPerYear                 -0.027337969 -0.187159023  0.08460710
    ## TARGET_deathRate                  0.449357576 -0.177399980  0.25702356
    ## incidenceRate                     0.040812287 -0.014509829  0.11348896
    ## medIncome                        -0.719756152  0.167225441 -0.27023162
    ## popEst2015                       -0.041468807 -0.190094503  0.07304407
    ## povertyPercent                    0.798642030 -0.509432808  0.51152966
    ## studyPerCap                      -0.055511989  0.023291042 -0.01976115
    ## MedianAge                        -0.003297872  0.035009366 -0.01717324
    ## MedianAgeMale                     0.002478719  0.398044362 -0.24274813
    ## MedianAgeFemale                   0.047659145  0.339803910 -0.15672844
    ## AvgHouseholdSize                  0.061114735 -0.188445815  0.03027798
    ## PercentMarried                   -0.459989923  0.677419940 -0.62235733
    ## PctNoHS18_24                      0.327269783 -0.157282267  0.11680516
    ## PctHS18_24                        0.234123984  0.045306371 -0.02486791
    ## PctBachDeg18_24                  -0.421804594  0.069132820 -0.09361400
    ## PctHS25_Over                      0.297143381  0.188044752 -0.02444526
    ## PctBachDeg25_Over                -0.605759903  0.048652281 -0.14640875
    ## PctUnemployed16_Over              0.655365736 -0.501755245  0.46927310
    ## PctPrivateCoverage               -0.886233694  0.429031447 -0.34517213
    ## PctEmpPrivCoverage               -0.728823026  0.269815023 -0.23738803
    ## PctPublicCoverage                 0.865832788 -0.133705071  0.19559747
    ## PctPublicCoverageAlone            1.000000000 -0.361026352  0.33011028
    ## PctWhite                         -0.361026352  1.000000000 -0.82845885
    ## PctBlack                          0.330110279 -0.828458852  1.00000000
    ## PctAsian                         -0.181380191 -0.265676411  0.01658341
    ## PctOtherRace                      0.083755384 -0.233692379 -0.02300131
    ## PctMarriedHouseholds             -0.473993882  0.596771068 -0.57359245
    ## BirthRate                        -0.004752695 -0.008958097 -0.06780483
    ##                            PctAsian PctOtherRace PctMarriedHouseholds
    ## avgAnnCount             0.435071173  0.209183775        -0.1062208681
    ## avgDeathsPerYear        0.443074226  0.215149359        -0.1602661296
    ## TARGET_deathRate       -0.186331105 -0.189893571        -0.2933253405
    ## incidenceRate          -0.008123427 -0.208748336        -0.1521763205
    ## medIncome               0.425844240  0.083634870         0.4460828953
    ## popEst2015              0.464167791  0.241468004        -0.1279794627
    ## povertyPercent         -0.157288704  0.047095893        -0.6049527844
    ## studyPerCap             0.062543075 -0.015247481        -0.0517356157
    ## MedianAge              -0.038423911 -0.030276508         0.0145036093
    ## MedianAgeMale          -0.238322374 -0.266655447         0.2222777445
    ## MedianAgeFemale        -0.258747912 -0.274119578         0.1615068308
    ## AvgHouseholdSize        0.131535433  0.229439641         0.0914503733
    ## PercentMarried         -0.148691337 -0.104669448         0.8702605365
    ## PctNoHS18_24           -0.217534569  0.126256354         0.0053395517
    ## PctHS18_24             -0.199770462 -0.060414849         0.1200402276
    ## PctBachDeg18_24         0.345882771  0.006546938        -0.0001044447
    ## PctHS25_Over           -0.436560938 -0.285611137         0.0621759178
    ## PctBachDeg25_Over       0.437962881  0.039075451         0.0981338597
    ## PctUnemployed16_Over   -0.022020273  0.028463247        -0.4696090139
    ## PctPrivateCoverage      0.189331755 -0.176300307         0.4346400550
    ## PctEmpPrivCoverage      0.282484289 -0.064225979         0.3225693263
    ## PctPublicCoverage      -0.305625464 -0.078707776        -0.3621705058
    ## PctPublicCoverageAlone -0.181380191  0.083755384        -0.4739938818
    ## PctWhite               -0.265676411 -0.233692379         0.5967710681
    ## PctBlack                0.016583407 -0.023001308        -0.5735924510
    ## PctAsian                1.000000000  0.200781103        -0.0866020358
    ## PctOtherRace            0.200781103  1.000000000        -0.0273522962
    ## PctMarriedHouseholds   -0.086602036 -0.027352296         1.0000000000
    ## BirthRate              -0.061946987  0.059829476         0.1022633001
    ##                           BirthRate
    ## avgAnnCount            -0.034507632
    ## avgDeathsPerYear       -0.074420014
    ## TARGET_deathRate       -0.087406970
    ## incidenceRate          -0.118181288
    ## medIncome              -0.010194553
    ## popEst2015             -0.057740178
    ## povertyPercent         -0.012282511
    ## studyPerCap             0.010676193
    ## MedianAge              -0.008276233
    ## MedianAgeMale          -0.104105160
    ## MedianAgeFemale        -0.098812608
    ## AvgHouseholdSize        0.075917596
    ## PercentMarried          0.141403930
    ## PctNoHS18_24            0.125894802
    ## PctHS18_24              0.058226877
    ## PctBachDeg18_24        -0.125073483
    ## PctHS25_Over            0.016600264
    ## PctBachDeg25_Over      -0.087940271
    ## PctUnemployed16_Over   -0.067906273
    ## PctPrivateCoverage     -0.040436613
    ## PctEmpPrivCoverage     -0.093878001
    ## PctPublicCoverage      -0.030530759
    ## PctPublicCoverageAlone -0.004752695
    ## PctWhite               -0.008958097
    ## PctBlack               -0.067804827
    ## PctAsian               -0.061946987
    ## PctOtherRace            0.059829476
    ## PctMarriedHouseholds    0.102263300
    ## BirthRate               1.000000000
