---
title: "Multivariate Analysis of Variance in R"
---



For a detailed description of MANOVA including assumptions see [Renesh Bedre](https://www.reneshbedre.com/blog/manova.html?utm_content=cmp-true)

**Example 39.6 Multivariate Analysis of Variance** from [SAS MANOVA User Guide](https://support.sas.com/documentation/cdl/en/statug/63033/HTML/default/viewer.htm#statug_glm_sect051.htm)

This example employs multivariate analysis of variance (MANOVA) to measure differences in the chemical characteristics of ancient pottery found at four kiln sites in Great Britain. The data are from Tubb, Parker, and Nickless (1980), as reported in Hand et al. (1994).

For each of 26 samples of pottery, the percentages of oxides of five metals are measured. The following statements create the data set and perform a one-way MANOVA. Additionally, it is of interest to know whether the pottery from one site in Wales (Llanederyn) differs from the samples from other sites.



::: {.cell}

```{.r .cell-code}
library(tidyverse)
```

::: {.cell-output .cell-output-stderr}

```
── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
✔ dplyr     1.1.4     ✔ readr     2.1.5
✔ forcats   1.0.0     ✔ stringr   1.5.1
✔ ggplot2   3.5.1     ✔ tibble    3.2.1
✔ lubridate 1.9.3     ✔ tidyr     1.3.1
✔ purrr     1.0.2     
── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
✖ dplyr::filter() masks stats::filter()
✖ dplyr::lag()    masks stats::lag()
ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```


:::

```{.r .cell-code}
library(knitr)
library(emmeans)
```

::: {.cell-output .cell-output-stderr}

```
Welcome to emmeans.
Caution: You lose important information if you filter this package's results.
See '? untidy'
```


:::

```{.r .cell-code}
knitr::opts_chunk$set(echo = TRUE)
pottery <- read.csv("../data/manova1.csv")
pottery
```

::: {.cell-output .cell-output-stdout}

```
           site   al   fe   mg   ca   na
1    Llanederyn 14.4 7.00 4.30 0.15 0.51
2    Llanederyn 13.8 7.08 3.43 0.12 0.17
3    Llanederyn 14.6 7.09 3.88 0.13 0.20
4    Llanederyn 11.5 6.37 5.64 0.16 0.14
5    Llanederyn 13.8 7.06 5.34 0.20 0.20
6    Llanederyn 10.9 6.26 3.47 0.17 0.22
7    Llanederyn 10.1 4.26 4.26 0.20 0.18
8    Llanederyn 11.6 5.78 5.91 0.18 0.16
9    Llanederyn 11.1 5.49 4.52 0.29 0.30
10   Llanederyn 13.4 6.92 7.23 0.28 0.20
11   Llanederyn 12.4 6.13 5.69 0.22 0.54
12   Llanederyn 13.1 6.64 5.51 0.31 0.24
13   Llanederyn 12.7 6.69 4.45 0.20 0.22
14   Llanederyn 12.5 6.44 3.94 0.22 0.23
15     Caldicot 11.8 5.44 3.94 0.30 0.04
16     Caldicot 11.6 5.39 3.77 0.29 0.06
17 IslandThorns 18.3 1.28 0.67 0.03 0.03
18 IslandThorns 15.8 2.39 0.63 0.01 0.04
19 IslandThorns 18.0 1.50 0.67 0.01 0.06
20 IslandThorns 18.0 1.88 0.68 0.01 0.04
21 IslandThorns 20.8 1.51 0.72 0.07 0.10
22  AshleyRails 17.7 1.12 0.56 0.06 0.06
23  AshleyRails 18.3 1.14 0.67 0.06 0.05
24  AshleyRails 16.7 0.92 0.53 0.01 0.05
25  AshleyRails 14.8 2.74 0.67 0.03 0.05
26  AshleyRails 19.1 1.64 0.60 0.10 0.03
```


:::
:::



**1 Perform one way MANOVA**

Response ID for ANOVA is order of 1=al, 2=fe, 3=mg, ca, na.

We are testing H0: group mean vectors are the same for all groups or they dont differ significantly vs

H1: At least one of the group mean vectors is different from the rest.



::: {.cell}

```{.r .cell-code}
dep_vars <- cbind(pottery$al,pottery$fe,pottery$mg, pottery$ca, pottery$na)
fit <-manova(dep_vars ~ pottery$site)
summary.aov(fit)
```

::: {.cell-output .cell-output-stdout}

```
 Response 1 :
             Df  Sum Sq Mean Sq F value    Pr(>F)    
pottery$site  3 175.610  58.537  26.669 1.627e-07 ***
Residuals    22  48.288   2.195                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

 Response 2 :
             Df  Sum Sq Mean Sq F value    Pr(>F)    
pottery$site  3 134.222  44.741  89.883 1.679e-12 ***
Residuals    22  10.951   0.498                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

 Response 3 :
             Df Sum Sq Mean Sq F value    Pr(>F)    
pottery$site  3 103.35  34.450   49.12 6.452e-10 ***
Residuals    22  15.43   0.701                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

 Response 4 :
             Df   Sum Sq  Mean Sq F value    Pr(>F)    
pottery$site  3 0.204703 0.068234  29.157 7.546e-08 ***
Residuals    22 0.051486 0.002340                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

 Response 5 :
             Df  Sum Sq  Mean Sq F value    Pr(>F)    
pottery$site  3 0.25825 0.086082  9.5026 0.0003209 ***
Residuals    22 0.19929 0.009059                      
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::



'summary(fit)' outputs the MANOVA testing of an overall site effect.

P\<0.001 suggests there is an overall difference between the chemical composition of samples from different sites.



::: {.cell}

```{.r .cell-code}
summary(fit)
```

::: {.cell-output .cell-output-stdout}

```
             Df Pillai approx F num Df den Df    Pr(>F)    
pottery$site  3 1.5539   4.2984     15     60 2.413e-05 ***
Residuals    22                                            
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```


:::
:::



**2 Now we test to see if the Llanaderyn site is different to the other sites**

NOTE: interest may now lie in using pre-planned contrast statements to investigate if one site differs when compared to the average of the others. You would imagine this could be done using the 'contrast' function something like the code below, however this result does not match the SAS user guide and so looks to be doing a different analysis. **SUGGEST THIS IS NOT USED UNTIL MORE RESEARCH INTO THIS METHOD CAN BE PERFORMED.** One alternative suggestion is to perform a linear descriminent analysis (LDA).



::: {.cell}

```{.r .cell-code}
manova(dep_vars ~ pottery$site) %>% 
          emmeans("site") %>% 
     contrast(method=list(
          "Llanederyn vs other sites"= c("Llanederyn"=-3, "Caldicot"=1, "IslandThorns"=1, "AshleyRails"=1)))
```

::: {.cell-output .cell-output-stdout}

```
 contrast                  estimate    SE df t.ratio p.value
 Llanederyn vs other sites     1.51 0.661 22   2.288  0.0321

Results are averaged over the levels of: rep.meas 
```


:::
:::



NOTE: if you feel you can help with the above discrepancy please contribute to the CAMIS repo by following the instructions on the [contributions page](../contribution/contribution.qmd).

