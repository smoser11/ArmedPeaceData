rm(list =ls())

## For each data-set (sdat), e.g COW:
### 1. record basics (types of vars, coverage) in the Rmd
### 2. determine countryX year key 
### 3. merge it with 'master' codelist_panel 
### 3a. make sure the merged panel is balanced and consecutive (this will make lots of NAs)
### 4. write out this merged data.frame both as `mmSDAT.Rdata`, e.g. mmCOW50.Rdata and as a Stata (`.dta`) file



####################################################
library(purrr)
library(foreign)
# install.packages('readstata13')
library('readstata13')

#install.packages("panelView")
library(panelView)
library(plm)
library (dplyr)
library(tidyr)



###############################################################
####			MASTER MERGE		###########################
###############################################################
####			MASTER MERGE		###########################
###############################################################
####			MASTER MERGE		###########################
###############################################################
####			MASTER MERGE		###########################
###############################################################
####			MASTER MERGE		###########################

rm(list=ls())

library(here)
## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

library(stringr)

getwd()
load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

str_starts(dir(paste0(here("Data/Processed/") )), "mm")

ds <- dir(paste0(here("Data/Processed/") ))[str_detect(dir(paste0(here("Data/Processed/") )), "^mm[A-Z].*RData$")]
ds

tokens <- str_replace_all(ds, '.RData', '')

i <- 1
dat <- list()
dat[[1]] <- ccpConBal50
for (i in seq_along(ds)) {  # 
  load(paste0(here("Data/Processed/"),'/',ds[i]) )
  tokens[i]
  eval(parse(text=paste0("dat[[(i+1 )]] <- ", tokens[i])))
}
names(dat[[2]])
str(dat)
getwd()
save(dat, file = paste0(here("Data/Processed/"),"/datMerge.RData") )

#pan <- dat

# cou <- sapply(dat, function(x) x$country.name.en.regex) %>% unlist %>% unique
# cou
# names(dat[[1]])
# yea <- sapply(dat, function(x) x$year) %>% unlist %>% unique
# yea
# yea <- min(yea):max(yea)
# yea
# rec <- expand_grid(country.name.en.regex = cou,
#                    year = yea)
# View(rec)
# names(ccpConBal50)
# #pan <- c(list(rec), dat) %>%
  
mmmALL50 <- dat %>%  purrr::reduce(left_join, by = c(names(ccpConBal50)))
names(dat[[2]])

# mmALL50cy <- dat %>%  purrr::reduce(left_join, by = c("year","country.name.en" ) )

names(mmmALL50)

### Light cleaning

names(mmmALL50)
mmmALL50 <- mmmALL50 %>% select(-"NA." )
mmmALL50 <- mmmALL50 %>% select(-"NA._PJM"    )
mmmALL50 <- mmmALL50 %>% select(-  c( "NA._BB","...1_COW", "x_YE_COW"  ) )

mmmALL50 %>% select(contains("PJM")) %>% View()

## hack to add 'double zeros' to Pat's PJM data - TODO!
mmmALL50 <- mmmALL50 %>%  mutate_at(vars(USally_PJM,Rusdefense_PJM), ~replace_na(., 0))


mmmALL50 %>% select(contains("PJM")) %>% is.na() %>% any()



getwd()
library(haven)
library(janitor)
write_dta(clean_names(mmmALL50), path =  paste0(here("Data/Processed/", "mmmALL50.dta") ))

save(mmmALL50, file =  paste0(here("Data/Processed/", "mmmALL50.RData")))

write_csv(mmmALL50, file =  paste0(here("Data/Processed/", "mmmALL50.csv") ))





# 


# 


# 

# 


