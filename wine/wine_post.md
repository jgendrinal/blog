``` r
suppressPackageStartupMessages({
  library(tidyverse)
  library(knitr)
})
read_csv("data/wine.csv") %>% 
  rename(alcohol = X1, 
         mall.Ac = X2, 
         ash = X3, 
         alk.Ash = X4, 
         magnes = X5, 
         tot.Phen = X6, 
         flavan = X7, 
         nonFl.ph = X8, 
         proanth = X9, 
         col.Int = X10, 
         hue = X11, 
         dil = X12, 
         proline = X13) -> wine
```
