---
title: "Salivary Proteomics in Sheep"
author: "Jared Raynes"
date: "27 April 2018"
output: 
  html_document:
    keep_md: true
---

<style>
body {
text-align: justify}
</style>



## Background

R scripts and markdown documents can be found at <https://github.com/jaredraynes/Sheep_Saliva>

Background and data provided by Dr Michelle Colgrave

Dark cutting is a notoriously difficult trait to assess. In this trial we proposed to assess dark cutting by a combination of muscle glycogen content and pH 24 hours post-slaughter. We also performed quantitative proteomics analysis on the saliva taken from sheep under different conditions of nutrition.

### Experimental design

Sixty wethers (castrated males) of around 1 month of age were brought to the Animal House, and allocated into 4 groups (n=15 per group) balanced by weight. Groups 1 and 2 were allocated to High Nutrition treatment, while groups 3 and 4 were allocated to Low Nutrition treatment. The trial had four treatment groups (2 x 2 factorial design):   
1)	ad lib feed; no stress  
2)	ad lib feed: stress*  
3)	restricted feed (sloping 70 – 60 % of maintenance); no stress  
4)	restricted feed; stress* 

Note: the stress event occurred after the saliva was taken and as such should not impact these results.

The sheep were assessed for glycogen content and the pH of the meat was measured 24h after slaughter.

The pH was assessed after 24 h at 4 degress (final pH). A final pH > 5.7 is considered a cause for downgrading in Meat Standards Australia, and seen as indicative of ‘dark cutting’. Note that pH in the leg muscle and in the back muscle are quite different. The leg muscle (M. semitendinosus) is particularly sensitive to glycogen depletion.

### Data provided

The data was provided in the form of two Microsoft Excel documents.  
1. "Copy of 20180413_DataSchool_Glycogen.xlsx"  
2. "Copy of Data Matrix_Peptide Peak Area.xlsx"

#### Initial investigation of Excel 1


```r
excel_sheets("Data/Copy of 20180413_DataSchool_Glycogen.xlsx")
```

```
## [1] "pH"              "Glycogen"        "Glycogen biopsy" "Weight gain"    
## [5] "Cortisol"        "Saliva"
```

####Contains 5 sheets within the Excel document

## pH Data

####Inital look at data

```r
pH <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "pH")
pH
```

```
## # A tibble: 60 x 5
##    `Treatment group` Brand `pH M. semitendinosus` `pH M. longissimu~ X__1 
##                <dbl> <dbl>                  <dbl>              <dbl> <chr>
##  1                1.    1.                   5.34               5.26 <NA> 
##  2                1.    2.                   5.33               5.35 <NA> 
##  3                1.    5.                   5.42               5.31 <NA> 
##  4                1.    6.                   5.46               5.50 <NA> 
##  5                1.    7.                   5.43               5.36 <NA> 
##  6                1.    8.                   5.36               5.24 <NA> 
##  7                1.    9.                   5.36               5.26 <NA> 
##  8                1.   10.                   5.48               5.33 <NA> 
##  9                1.   11.                   5.32               5.22 <NA> 
## 10                1.   12.                   5.39               5.33 <NA> 
## # ... with 50 more rows
```
####Tidying the data


```r
pH$Brand[13] = 64 #replacing data from sheep 15 with 64
pH <- pH[1:4]
names(pH) <- c("Treatment.Group", "ID", "M.semitendinosus", "M.longissimus.dorsi")
pH <- mutate(pH, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
pH_long <- pH %>% 
  gather(key = "Muscle.type", value = "pH", "M.semitendinosus", "M.longissimus.dorsi")
pH_long$Treatment.Group <- factor(pH$Treatment.Group)
pH_long$ID <- factor(pH$ID)
pH_long$Feed <- factor(pH_long$Feed)
pH_long$Stress <- factor(pH_long$Stress)
pH_long
```

```
## # A tibble: 120 x 6
##    Treatment.Group ID    Feed  Stress Muscle.type         pH
##    <fct>           <fct> <fct> <fct>  <chr>            <dbl>
##  1 1               1     TRUE  FALSE  M.semitendinosus  5.34
##  2 1               2     TRUE  FALSE  M.semitendinosus  5.33
##  3 1               5     TRUE  FALSE  M.semitendinosus  5.42
##  4 1               6     TRUE  FALSE  M.semitendinosus  5.46
##  5 1               7     TRUE  FALSE  M.semitendinosus  5.43
##  6 1               8     TRUE  FALSE  M.semitendinosus  5.36
##  7 1               9     TRUE  FALSE  M.semitendinosus  5.36
##  8 1               10    TRUE  FALSE  M.semitendinosus  5.48
##  9 1               11    TRUE  FALSE  M.semitendinosus  5.32
## 10 1               12    TRUE  FALSE  M.semitendinosus  5.39
## # ... with 110 more rows
```

#### Initial visulisation of data

![](sheep_knitr_files/figure-html/pH_plot1-1.png)<!-- -->![](sheep_knitr_files/figure-html/pH_plot1-2.png)<!-- -->

#### Modelling of data


```r
lm.pH1 <- lm(pH ~ Stress*Feed + Muscle.type, data = pH_long)
summary(lm.pH1)
```

```
## 
## Call:
## lm(formula = pH ~ Stress * Feed + Muscle.type, data = pH_long)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.14826 -0.05373 -0.01665  0.03783  0.42841 
## 
## Coefficients:
##                             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)                  5.29764    0.01814 292.056  < 2e-16 ***
## StressTRUE                   0.22589    0.02294   9.845  < 2e-16 ***
## FeedTRUE                    -0.01334    0.02294  -0.581  0.56221    
## Muscle.typeM.semitendinosus  0.12805    0.01622   7.893 1.91e-12 ***
## StressTRUE:FeedTRUE         -0.09278    0.03245  -2.859  0.00504 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.08886 on 115 degrees of freedom
## Multiple R-squared:  0.6422,	Adjusted R-squared:  0.6298 
## F-statistic: 51.61 on 4 and 115 DF,  p-value: < 2.2e-16
```

```r
emmeans(lm.pH1, pairwise~Stress+Feed)
```

```
## $emmeans
##  Stress Feed    emmean         SE  df lower.CL upper.CL
##  FALSE  FALSE 5.361665 0.01622414 115 5.329528 5.393802
##  TRUE   FALSE 5.587560 0.01622414 115 5.555423 5.619697
##  FALSE  TRUE  5.348329 0.01622414 115 5.316192 5.380466
##  TRUE   TRUE  5.481448 0.01622414 115 5.449311 5.513585
## 
## Results are averaged over the levels of: Muscle.type 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast                    estimate        SE  df t.ratio p.value
##  FALSE,FALSE - TRUE,FALSE -0.22589433 0.0229444 115  -9.845  <.0001
##  FALSE,FALSE - FALSE,TRUE  0.01333633 0.0229444 115   0.581  0.9375
##  FALSE,FALSE - TRUE,TRUE  -0.11978233 0.0229444 115  -5.221  <.0001
##  TRUE,FALSE - FALSE,TRUE   0.23923067 0.0229444 115  10.427  <.0001
##  TRUE,FALSE - TRUE,TRUE    0.10611200 0.0229444 115   4.625  0.0001
##  FALSE,TRUE - TRUE,TRUE   -0.13311867 0.0229444 115  -5.802  <.0001
## 
## Results are averaged over the levels of: Muscle.type 
## P value adjustment: tukey method for comparing a family of 4 estimates
```

```r
plot(lm.pH1, which =1)
```

![](sheep_knitr_files/figure-html/pH_modelling-1.png)<!-- -->

#### Modelling Results
1. Feeding regime only had a significant effect (P = 0.005) on the pH of the M.semitendinosus muscle and this only occurred when the animals were stressed.  
2. Stress had a significant (P < 2e-16) impact on muscle pH.  
3. The M.semitendinosus muscle had a significantly (P = 1.91e-12) higher pH compared to the M.longissimus.dorsi muscle. 
4. None of the treatments resulted in 'dark cutting' of meat.

## Glycogen Data (Taken from M.semitendinosus Muscle)

####Inital look at data

```r
Glycogen <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen")
Glycogen
```

```
## # A tibble: 31 x 10
##    `Treatment grou~ Brand `Glycogen mg/g ~ `Lactate mg/g` `Corrected glyc~
##    <chr>            <dbl>            <dbl>          <dbl>            <dbl>
##  1 1                   1.             9.44           2.47            11.9 
##  2 1                   2.             6.91           2.54             9.44
##  3 1                   5.             1.26           3.41             4.67
##  4 1                   6.             4.45           3.03             7.48
##  5 1                   7.             4.81           3.06             7.87
##  6 1                   8.             4.13           4.05             8.18
##  7 1                   9.             5.84           2.98             8.82
##  8 1                  10.             3.81           4.16             7.97
##  9 1                  11.             7.59           5.29            12.9 
## 10 1                  12.             8.14           3.21            11.3 
## # ... with 21 more rows, and 5 more variables: `Treatment group__1` <dbl>,
## #   Brand__1 <dbl>, `Glycogen mg/g tissue__1` <dbl>, `Lactate
## #   mg/g__1` <dbl>, `Corrected glycogen__1` <dbl>
```
####Tidying the data


```r
Glycogen <- Glycogen[-c(31),] ## remove error message
G1 <- Glycogen[1:5] #the treatment groups are in different columns this time
G2 <- Glycogen[6:10] #need to have identical names to use row_bind
names(G2) <- c("Treatment group", "Brand", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
G2$`Treatment group` <- as.character(G2$`Treatment group`) 
Glycogen_comb <- bind_rows(G1, G2) #combining the columns together
Glycogen_comb <- mutate(Glycogen_comb, Feed = ifelse(`Treatment group` <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(`Treatment group` == 2|`Treatment group` == 4, TRUE, FALSE)) #added Stress column
Glycogen_long <- Glycogen_comb %>% 
  gather(key = "Measure", value = "mg/g", "Glycogen mg/g tissue", "Lactate mg/g", "Corrected glycogen")
names(Glycogen_long) <- c("Treatment.Group", "ID", "Feed", "Stress", "Measure", "mg/g")
Glycogen_long$Treatment.Group <- factor(Glycogen_long$Treatment.Group)
Glycogen_long$ID <- factor(Glycogen_long$ID)
Glycogen_sel <- filter(Glycogen_long, Measure == "Corrected glycogen")
Glycogen_long
```

```
## # A tibble: 180 x 6
##    Treatment.Group ID    Feed  Stress Measure              `mg/g`
##    <fct>           <fct> <lgl> <lgl>  <chr>                 <dbl>
##  1 1               1     TRUE  FALSE  Glycogen mg/g tissue   9.44
##  2 1               2     TRUE  FALSE  Glycogen mg/g tissue   6.91
##  3 1               5     TRUE  FALSE  Glycogen mg/g tissue   1.26
##  4 1               6     TRUE  FALSE  Glycogen mg/g tissue   4.45
##  5 1               7     TRUE  FALSE  Glycogen mg/g tissue   4.81
##  6 1               8     TRUE  FALSE  Glycogen mg/g tissue   4.13
##  7 1               9     TRUE  FALSE  Glycogen mg/g tissue   5.84
##  8 1               10    TRUE  FALSE  Glycogen mg/g tissue   3.81
##  9 1               11    TRUE  FALSE  Glycogen mg/g tissue   7.59
## 10 1               12    TRUE  FALSE  Glycogen mg/g tissue   8.14
## # ... with 170 more rows
```

#### Initial visulisation of data

![](sheep_knitr_files/figure-html/Glygogen_plot1-1.png)<!-- -->![](sheep_knitr_files/figure-html/Glygogen_plot1-2.png)<!-- -->

#### Modelling of data (only corrected glycogen)


```r
lm.Gly1 <- lm(`mg/g` ~ Treatment.Group, data = Glycogen_sel)
summary(lm.Gly1)
```

```
## 
## Call:
## lm(formula = `mg/g` ~ Treatment.Group, data = Glycogen_sel)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -4.8238 -1.1499 -0.0021  0.9294  4.0313 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        9.4950     0.4400  21.578  < 2e-16 ***
## Treatment.Group2  -0.7683     0.6223  -1.235   0.2221    
## Treatment.Group3  -1.2669     0.6223  -2.036   0.0465 *  
## Treatment.Group4  -4.0195     0.6223  -6.459  2.7e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.704 on 56 degrees of freedom
## Multiple R-squared:  0.4587,	Adjusted R-squared:  0.4296 
## F-statistic: 15.82 on 3 and 56 DF,  p-value: 1.44e-07
```

```r
emmeans(lm.Gly1, pairwise~Treatment.Group)
```

```
## $emmeans
##  Treatment.Group   emmean        SE df lower.CL  upper.CL
##  1               9.494966 0.4400401 56 8.613460 10.376472
##  2               8.726667 0.4400401 56 7.845161  9.608173
##  3               8.228081 0.4400401 56 7.346575  9.109587
##  4               5.475429 0.4400401 56 4.593923  6.356935
## 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast  estimate        SE df t.ratio p.value
##  1 - 2    0.7682987 0.6223106 56   1.235  0.6078
##  1 - 3    1.2668850 0.6223106 56   2.036  0.1873
##  1 - 4    4.0195371 0.6223106 56   6.459  <.0001
##  2 - 3    0.4985863 0.6223106 56   0.801  0.8536
##  2 - 4    3.2512384 0.6223106 56   5.224  <.0001
##  3 - 4    2.7526521 0.6223106 56   4.423  0.0003
## 
## P value adjustment: tukey method for comparing a family of 4 estimates
```

```r
plot(lm.Gly1, which =1)
```

![](sheep_knitr_files/figure-html/Glycogen_modelling-1.png)<!-- -->

#### Modelling Results
1. Stress had no impact on the level of glycogen when feeding was ad lib.  
2. Stress had a significant (P = 0.003) impact on the level of glycogen when feed was restricted.  
3. The combination of restricted feed and stress had the most significant (P < 0.001) effect on glycogen level, producing the lowest amount of glycogen.

## Glycogen Biopsy Data (Taken from M.semimembranosus Muscle)


```r
Glycogen_biopsy <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Glycogen biopsy")
Glycogen_biopsy
```

```
## # A tibble: 14 x 12
##    `Animal ID`             `Glycogen mg/g` `Lactate mg/g` `Corrected Glyc~
##    <chr>                             <dbl>          <dbl>            <dbl>
##  1 10                                2.56          1.77              4.33 
##  2 11                                1.18          2.36              3.54 
##  3 12                                7.47          2.14              9.61 
##  4 13                                3.66          2.47              6.13 
##  5 14                                5.16          3.42              8.58 
##  6 24                                8.10          1.78              9.88 
##  7 25                                6.20          1.89              8.09 
##  8 26                               11.2           2.55             13.8  
##  9 27                                0.360         0.0677            0.428
## 10 28                                8.50          2.23             10.7  
## 11 29                                9.10          2.03             11.1  
## 12 <NA>                             NA            NA                NA    
## 13 <NA>                             NA            NA                NA    
## 14 Note: biopsies taken f~          NA            NA                NA    
## # ... with 8 more variables: `Animal ID__1` <dbl>, X__1 <dbl>, X__2 <chr>,
## #   X__3 <chr>, X__4 <chr>, X__5 <lgl>, X__6 <dbl>, X__7 <dbl>
```

####Tidying the data


```r
Glycogen_biopsy <- Glycogen_biopsy[1:4]
Glycogen_biopsy <- Glycogen_biopsy[-c(12:14),]
Glycogen_biopsy$Treatment.Group <- c(1,1,1,1,1,2,2,2,2,2,2)
Glycogen_biopsy_long <- Glycogen_biopsy %>% 
  gather(key = "Measure", value = "mg/g", "Glycogen mg/g", "Lactate mg/g", "Corrected Glycogen")
names(Glycogen_biopsy_long) <- c("ID", "Treatment.Group", "Measure", "mg/g")
Glycogen_biopsy_long$Treatment.Group <- factor(Glycogen_biopsy_long$Treatment.Group)
Glycogen_biopsy_long$ID <- factor(Glycogen_biopsy_long$ID)
Glycogen_biopsy_long$Measure <- factor(Glycogen_biopsy_long$Measure)
Glycogen_biopsy_sel <- filter(Glycogen_biopsy_long, Measure == "Corrected Glycogen")
Glycogen_biopsy_long
```

```
## # A tibble: 33 x 4
##    ID    Treatment.Group Measure       `mg/g`
##    <fct> <fct>           <fct>          <dbl>
##  1 10    1               Glycogen mg/g  2.56 
##  2 11    1               Glycogen mg/g  1.18 
##  3 12    1               Glycogen mg/g  7.47 
##  4 13    1               Glycogen mg/g  3.66 
##  5 14    1               Glycogen mg/g  5.16 
##  6 24    2               Glycogen mg/g  8.10 
##  7 25    2               Glycogen mg/g  6.20 
##  8 26    2               Glycogen mg/g 11.2  
##  9 27    2               Glycogen mg/g  0.360
## 10 28    2               Glycogen mg/g  8.50 
## # ... with 23 more rows
```

#### Initial visulisation of data

![](sheep_knitr_files/figure-html/Glygogen_Biops_plot1-1.png)<!-- -->

#### Modelling of data (only corrected glycogen)


```r
lm.Glybiops1 <- lm(`mg/g` ~ Treatment.Group, data = Glycogen_biopsy_sel)
summary(lm.Glybiops1)
```

```
## 
## Call:
## lm(formula = `mg/g` ~ Treatment.Group, data = Glycogen_biopsy_sel)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -8.5767 -1.5144  0.8786  2.1357  4.7661 
## 
## Coefficients:
##                  Estimate Std. Error t value Pr(>|t|)   
## (Intercept)         6.437      1.719   3.744   0.0046 **
## Treatment.Group2    2.567      2.328   1.103   0.2987   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 3.845 on 9 degrees of freedom
## Multiple R-squared:  0.1191,	Adjusted R-squared:  0.02117 
## F-statistic: 1.216 on 1 and 9 DF,  p-value: 0.2987
```

```r
plot(lm.Glybiops1, which =1)
```

![](sheep_knitr_files/figure-html/Glycogen_Biops_modelling-1.png)<!-- -->

#### Modelling Results
1. Stress caused a significant (P = 0.0046) increase in M.semimembranosus muscle glycogen levels.

## Weight Gain Data

####Inital look at data

```r
Weight_gain <- read_xlsx("Data/Copy of 20180413_DataSchool_Glycogen.xlsx", sheet = "Weight gain")
Weight_gain
```

```
## # A tibble: 61 x 9
##    `Treatment group` Brand `Week -1` `Week 0` `Week 1` `Week 2` `Week 3`
##                <dbl> <dbl>     <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
##  1                1.    1.      45.5     46.0     45.5     46.0     48.4
##  2                1.    2.      42.0     40.5     42.5     41.0     44.4
##  3                1.    5.      44.5     45.5     47.5     43.5     47.4
##  4                1.    6.      46.0     46.0     42.0     39.0     37.6
##  5                1.    7.      42.0     42.5     44.0     41.5     46.4
##  6                1.    8.      39.0     41.0     41.0     38.5     41.2
##  7                1.    9.      40.0     41.5     39.0     38.0     43.6
##  8                1.   10.      42.5     43.0     40.5     39.5     44.2
##  9                1.   11.      44.0     41.5     42.5     40.0     45.6
## 10                1.   12.      41.0     42.0     42.5     40.0     43.8
## # ... with 51 more rows, and 2 more variables: `Week 4` <dbl>, X__1 <dbl>
```

####Tidying the data


```r
Weight_gain <- Weight_gain[1:8]
Weight_gain <- Weight_gain[-c(61),]
names(Weight_gain) <- c("Treatment.Group", "ID", "-1", "0", "1", "2", "3", "4")
Weight_gain <- mutate(Weight_gain, Feed = ifelse(Treatment.Group <= 2, TRUE, FALSE)) %>%  #added feed column
  mutate(Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
Weight_gain_long <- Weight_gain %>% 
  gather(key = "Week", value = "Weight.kg", "-1", "0", "1", "2", "3", "4")
Weight_gain_long$Treatment.Group <- factor(Weight_gain_long$Treatment.Group)
Weight_gain_long$ID <- factor(Weight_gain_long$ID)
Weight_gain_long$Week <- factor(Weight_gain_long$Week)
Weight_gain_long$Feed <- factor(Weight_gain_long$Feed)
Weight_gain_long$Stress <- factor(Weight_gain_long$Stress)
Weight_gain_long$Feed <- relevel(Weight_gain_long$Feed, ref= 'TRUE')
Weight_gain_long
```

```
## # A tibble: 360 x 6
##    Treatment.Group ID    Feed  Stress Week  Weight.kg
##    <fct>           <fct> <fct> <fct>  <fct>     <dbl>
##  1 1               1     TRUE  FALSE  -1         45.5
##  2 1               2     TRUE  FALSE  -1         42.0
##  3 1               5     TRUE  FALSE  -1         44.5
##  4 1               6     TRUE  FALSE  -1         46.0
##  5 1               7     TRUE  FALSE  -1         42.0
##  6 1               8     TRUE  FALSE  -1         39.0
##  7 1               9     TRUE  FALSE  -1         40.0
##  8 1               10    TRUE  FALSE  -1         42.5
##  9 1               11    TRUE  FALSE  -1         44.0
## 10 1               12    TRUE  FALSE  -1         41.0
## # ... with 350 more rows
```

#### Initial visulisation of data

![](sheep_knitr_files/figure-html/Weight_plot1-1.png)<!-- -->

#### Modelling of data


```r
lm.Weight1 <- lm(Weight.kg ~ Week + Stress*Feed, data = Weight_gain_long)
summary(lm.Weight1)
```

```
## 
## Call:
## lm(formula = Weight.kg ~ Week + Stress * Feed, data = Weight_gain_long)
## 
## Residuals:
##    Min     1Q Median     3Q    Max 
## -6.549 -2.036  0.035  1.950  7.470 
## 
## Coefficients:
##                      Estimate Std. Error t value Pr(>|t|)    
## (Intercept)           44.2608     0.4413 100.293  < 2e-16 ***
## Week0                  0.4167     0.5096   0.818   0.4141    
## Week1                 -1.0167     0.5096  -1.995   0.0468 *  
## Week2                 -3.2083     0.5096  -6.296 9.13e-10 ***
## Week3                 -1.4117     0.5096  -2.770   0.0059 ** 
## Week4                 -0.7917     0.5096  -1.554   0.1212    
## StressTRUE            -0.4389     0.4161  -1.055   0.2922    
## FeedFALSE             -2.3000     0.4161  -5.528 6.33e-08 ***
## StressTRUE:FeedFALSE  -0.4656     0.5884  -0.791   0.4294    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 2.791 on 351 degrees of freedom
## Multiple R-squared:  0.2885,	Adjusted R-squared:  0.2723 
## F-statistic: 17.79 on 8 and 351 DF,  p-value: < 2.2e-16
```

```r
#emmeans(lm.Weight1, pairwise ~ Week + Stress*Feed)
plot(lm.Weight1, which =1)
```

![](sheep_knitr_files/figure-html/Weight_modelling-1.png)<!-- -->

#### Modelling Results
1. Before birth (-1 week) and at birth (0 week) there is no statistical difference in the weights of the sheep between treatment groups, as is expected because the treatments have not started.  
2. By the week 1 measurement, the treatment 4 group has a statistically significantly (P = 0.0236) lower (Est 3.033 +- 1.33 kg) weight than the other 3 groups. Meaning that the restricted feeding regime as well as stress is causing a lower sheep weight.  
3. By week 2 all treatments have lost a statistically significant (P = 0.0412) amount of weight compared to the previous weeks. Again, Treatment group 4 has lost statistically significantly (P = 0.0130) more weight in week 2 than the other groups.  
4. By week 3 treatment groups 1 and 2 have gained a statistically significant amount of weight compared to treatment groups 3 and 4, but between groups 1 and 2, and 3 and 4 there is no significant difference.  
5. By week 4 the trends are the same as week 3.

## Peptide Matrix Data

####Data are the peak areas of identified peptides


```r
peptide <- read_xlsx("Data/Copy of Data Matrix_Peptide Peak Area.xlsx")
peptide
```

```
## # A tibble: 2,116 x 49
##    `Sample ID` `1`   `5`   `6`   `9`   `11`  `12`  `13`  `14`  `15`  `16` 
##    <chr>       <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
##  1 Group       1     1     1     1     1     1     1     1     1     1    
##  2 Nutrition:~ HN    HN    HN    HN    HN    HN    HN    HN    HN    HN   
##  3 tr|W5PFP1|~ 1887~ 8163~ 5242~ 2767~ 2077~ 9258~ 6390~ 1596~ 1373~ 2690~
##  4 tr|W5PFP1|~ 1409~ 6142~ 4180~ 1979~ 1422~ 7159~ 5216~ 1634~ 1162~ 2627~
##  5 tr|W5PFP1|~ 9676~ 4652~ 2383~ 1342~ 9833~ 5243~ 3804~ 1107~ 7213~ 1552~
##  6 tr|W5PFP1|~ 4624~ 2348~ 1215~ 7070~ 5093~ 2554~ 2000~ 5470~ 3708~ 77193
##  7 tr|W5PFP1|~ 6281~ 3683~ 2023~ 1061~ 7110~ 3819~ 2127~ 9957~ 5638~ 23527
##  8 tr|W5PFP1|~ 5230~ 2835~ 1546~ 8600~ 6105~ 3363~ 2567~ 7267~ 4698~ 1095~
##  9 tr|W5PFP1|~ 4234~ 13699 293.~ 1277~ 3179  3207~ 2783~ 518.~ 248.~ 1277~
## 10 tr|W5PFP1|~ 1969~ 2309~ 3979~ 1984~ 1471~ 1337~ 6507~ 1581~ 1296~ 1877~
## # ... with 2,106 more rows, and 38 more variables: `17` <chr>, `3` <chr>,
## #   `4` <chr>, `18` <chr>, `19` <chr>, `20` <chr>, `21` <chr>, `22` <chr>,
## #   `23` <chr>, `25` <chr>, `26` <chr>, `27` <chr>, `28` <chr>,
## #   `29` <chr>, `30` <chr>, `32` <chr>, `34` <chr>, `35` <chr>,
## #   `36` <chr>, `40` <chr>, `41` <chr>, `42` <chr>, `43` <chr>,
## #   `44` <chr>, `46` <chr>, `47` <chr>, `48` <chr>, `49` <chr>,
## #   `51` <chr>, `52` <chr>, `53` <chr>, `54` <chr>, `55` <chr>,
## #   `56` <chr>, `57` <chr>, `59` <chr>, `60` <chr>, `62` <chr>
```

####Tidy the data


```r
peptide_T <- t(peptide) #transpose
peptide_T <- rownames_to_column(data.frame(peptide_T), 'ID')
peptide_T$ID[1] <- "ID"
peptide_T[] <- lapply(peptide_T, as.character)
peptide_T$X2[1] <- "Nutrition"
peptide_T$X1[1] <- "Treatment.Group"
colnames(peptide_T) <- peptide_T[1, ]
peptide_T <- peptide_T[-1 ,]
peptide_T[4:2117] <- lapply(peptide_T[4:2117], as.numeric)
peptide_T[1:3] <- lapply(peptide_T[1:3], as.factor)
peptide_T <- mutate(peptide_T, Stress = ifelse(Treatment.Group == 2|Treatment.Group == 4, TRUE, FALSE))
peptide_T <- select(peptide_T, Stress, everything())
peptide_T$Stress <- factor(peptide_T$Stress)
```
 
####Principle Component Analysis of the Data


```r
pca_pep <- prcomp(peptide_T[5:2114], scale=TRUE)
```
###Plotting the PCA and Colouring by Qualitative Variables

![](sheep_knitr_files/figure-html/PCA_plots-1.png)<!-- -->![](sheep_knitr_files/figure-html/PCA_plots-2.png)<!-- -->![](sheep_knitr_files/figure-html/PCA_plots-3.png)<!-- -->

####From the PCA plots, stress does not influence the abundance of individual peptides from the saliva, as expected, because the saliva was taken before the stress event.  Nutrition is having an effect on saliva peptide abundance though.  

###Plotting with the Nutrition Groups Coloured and with a 95% Confidence Ellipse

![](sheep_knitr_files/figure-html/PCA_plot2-1.png)<!-- -->![](sheep_knitr_files/figure-html/PCA_plot2-2.png)<!-- -->

###The nutrition groups separate relatively well.  
###The top 5 peptides contributing the most influence on the PCA are:   
GTLDPVEK +2  
DAGTIAGLNVLR +2  
NSLESYAFNMK +2  
FIIPNIVK +2  
GNPTVEDLFTAK +2

