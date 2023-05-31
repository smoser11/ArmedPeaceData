# Imputation attempts
## Look at ImputationAttempt_20230220.R


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

library(mice)
library(Amelia)



ds <- dir(paste0(here("Data/Processed/") ))[str_detect(dir(paste0(here("Data/Processed/") )), "^mm.*RData$")]
ds
tokens <- str_replace_all(ds, '.RData', '')

load(file =paste0(here("Data/Processed/", "mmHSS50.RData") ))
sort(unique(mmHSS50$country.name.en))

load(file =paste0(here("Data/Processed/", "mmZFS50.RData") ))


load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

