# Workflow:
## setup
## For each data-set (sdat), e.g COW:
### A. Load data-set
### B. Clean data and reshape to long
### C. determine countryX year key  adn *make corrections*
### D. Merge with codelist_panel from countrycode package (not necc. balanced / consecutive)
### E. Save merged dataset as `.RData` and `.dta`
## F. record basics (types of vars, coverage) in the 02dataDoc_ArmedPease.Rmd 
## G. master merge -> balanced and consecutive (03masterMerge_ArmedPeace.R) and write out
## H. *Double check*

###########################################################################
##### Preliminaries

rm(list = ls())
gc()

## libraries
library(here)
library(tidyverse)
library(magrittr)
library(gtools)

# install packages
library(purrr)
library(foreign)
# install.packages('readstata13')
library('readstata13')
#install.packages("panelView")
library(panelView)
library(plm)
library (dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(countrycode)
library(haven)
library(janitor)

###########################################################################
##### Source code

## the `countrycode` package is our friend:  https://github.com/vincentarelbundock/countrycode/blob/main/README.md

source(paste0(here::here("Data/Raw/countrycode-main/dictionary/"), "/build_v3.R"))  # makes 'our' version of: `codelist_panel2.dta` and `codelist_panel2_ConBal.dta` = consecutive and balanced.
# Also saves  `codelist2, codelist_panel2, codelist_panel2_ConBal,  ccs_year, ccs_noYear, iso3cRet_cowCustom` in CCdatOrig.RData



write.csv(sort(unique(codelist_panel$country.name.en))
          , file = paste0(here("Data/Processed/","country_name_en.csv") )) ## creates `country_name_en.csv` =  English country names used by countrycode package.



