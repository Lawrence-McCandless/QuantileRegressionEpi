rm(list=ls(all=TRUE)); options(tibble.print_max = Inf); options(max.print = 10000)
setwd("C:/Users/lawre/Dropbox/Research/2025/ocr_book"); source('libraries.r') 
library(MASS); library(dplyr); library(quantreg);
load('C:/Users/lawre/Dropbox/Research/2025/ocr_book/pms.RData')
pms <- pms[pms$sb==0,] # have to remove the stillbirths from the dataset

pms.arXiv <- pms[,c("ga", "album", "female", "smk", "single")]  # retain only variables needed for arXiv paper


## Synthetic PMS data:
## 1. Perturb the 24 strata using multinomial resampling
## 2. Generate ga by safe index sampling within strata

## Synthetic PMS data with:
## 1. Multinomial perturbation of the 24 strata
## 2. Nonparametric resampling of ga within strata

set.seed(123)

vars <- c("album", "female", "smk", "single")

## Original contingency table
tab <- as.data.frame(table(pms.arXiv[vars]))
names(tab) <- c(vars, "Freq")

## Remove empty cells
tab <- tab[tab$Freq > 0, ]

## Total sample size
N <- sum(tab$Freq)

## Original cell probabilities
p <- tab$Freq / N

## Perturb counts slightly using multinomial resampling
tab$Freq.synthetic <- as.vector(
  rmultinom(
    n = 1,
    size = N,
    prob = p
  )
)

## Keep nonempty synthetic strata
tab.syn <- tab[tab$Freq.synthetic > 0, ]

## Generate synthetic data
out <- vector("list", nrow(tab.syn))

for(i in seq_len(nrow(tab.syn))) {
  
  rowi <- tab.syn[i, ]
  
  n <- rowi$Freq.synthetic
  
  ## Observed ga values in this stratum
  pool <- pms.arXiv[
    pms.arXiv$album  == rowi$album &
      pms.arXiv$female == rowi$female &
      pms.arXiv$smk    == rowi$smk &
      pms.arXiv$single == rowi$single,
    "ga",
    drop = TRUE
  ]
  
  ## Safe index sampling
  ga.syn <- pool[
    sample(
      seq_along(pool),
      size = n,
      replace = TRUE
    )
  ]
  
  out[[i]] <- data.frame(
    ga      = ga.syn,
    album   = rowi$album,
    female  = rowi$female,
    smk     = rowi$smk,
    single  = rowi$single
  )
}

## Combine all strata
pms.synthetic <- do.call(rbind, out)

## Reset row names
rownames(pms.synthetic) <- NULL

## Compare original and synthetic counts
compare.tab <- tab[, c(vars, "Freq", "Freq.synthetic")]

compare.tab

## Compare ga summaries
summary(pms.arXiv$ga)
summary(pms.synthetic$ga)

## Save to CSV
write.csv(
  pms.synthetic,
  file = "C:/Users/lawre/Dropbox/Research/2025/ocr_book/fqr-arXiv-web-codes/pms.synthetic.csv",
  row.names = FALSE
)


> compare.tab
album female smk single Freq Freq.synthetic
1      0      0   0      0  723            705
2      1      0   0      0   69             68
3      0      1   0      0  655            702
4      1      1   0      0   45             57
5      0      0   1      0   90             75
6      1      0   1      0    5              3
7      0      1   1      0   81             75
8      1      1   1      0    3              6
9      0      0   2      0  370            360
10     1      0   2      0   27             28
11     0      1   2      0  374            377
12     1      1   2      0   24             16
13     0      0   0      1   10             14
14     1      0   0      1    2              3
15     0      1   0      1   14             13
16     1      1   0      1    2              3
17     0      0   1      1    1              0
18     1      0   1      1    1              1
19     0      1   1      1    5              5
21     0      0   2      1   13              8
22     1      0   2      1    1              0
23     0      1   2      1   22             20
24     1      1   2      1    3              1
> 
  > ## Compare ga summaries
  > summary(pms.arXiv$ga)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
169.0   275.0   282.0   280.9   289.0   320.0 
> summary(pms.synthetic$ga)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
183     275     282     281     289     320 