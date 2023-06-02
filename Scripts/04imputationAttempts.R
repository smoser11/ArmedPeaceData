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


load(file =paste0(here("Data/Processed/", "mmCOW50.RData") ))
sort(unique(mmCOW50$country.name.en))

load(file =paste0(here("Data/Processed/", "mmALL50.RData") ))
sort(unique(mmHSS50$country.name.en))
sort(unique(mmZFS50$country.name.en))

library(sampleSelection)

mmALL50$country <- mmALL50$country.name.en
mmALL50 <- mmALL50 %>% select(-country.name.en)
glimpse(mmALL50)
pd <- mmALL50 %>% select(contains("HSS"), country, year)
### compare:
missmap(pd, csvar = "ccode_HSS", tsvar = "year_HSS")

names(mmHSS50)
missmap(mmHSS50, csvar = "ccode_HSS", tsvar = "year_HSS")
names(mmZFS50)
missmap(mmZFS50, csvar = "STATE_ZFS", tsvar = "YEAR_ZFS")
ss <- pd %>% group_by(country) %>%
	summarise_all(~sum(is.na(.))/n())



load(file =paste0(here("Data/Processed/", "mmZFS50.RData") ))


load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))


##########

## imputation attempts



load(file =paste0(here("Data/Processed/", "mmALL50.RData") ))
names(mmALL50)
getwd()
library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)

mmALL50$country <- mmALL50$country.name.en
mmALL50 <- mmALL50 %>% select(-country.name.en)
glimpse(mmALL50)
pd <- mmALL50

#install.packages("Amelia")

missmap(pd, csvar = "country", tsvar = "year")

ss <- pd %>% group_by(country) %>%
	summarise_all(~sum(is.na(.))/n())

sort(unique(pd$country) )
names(ss)
sss <- ss %>% filter(country_PWT >0.5) %>% select(country, contains("PWT"), contains("BB"), everything() ) 

sss %>% View()

# PWT only starts in 1970

names(mmALL50)

pd <- pd %>% filter(year>1969)
n <- ave(pd$LMILEX_BB, pd$country, FUN=function(x)sum(!is.na(x)))
n
pd2 <- pd[n > 15, ]  #only keep if we have 15 years of this variable
names(pd2)
unique(pd2$country)
unique(mmALL50$country.name.en)

pd2$country <- droplevels(pd2$country)
unique(pd2$country)
pd2 <- pd2 %>% select(-country.name.en)

## Drop _variables_ with too few observations

### Now we define a function that can calculate the longest run of non-NA values in a vector
### https://stackoverflow.com/questions/24600716/drop-variable-in-panel-data-in-r-conditional-based-on-a-defined-number-of-consec
consecnonNA <- function(x) {
	rr<-rle(is.na(x))
	max(rr$lengths[rr$values==FALSE])
}

atleast <- function(i) {function(x) x>=i}
hasatleast8 <- names(Filter(atleast(15), sapply(pd2, consecnonNA)))
sort(hasatleast8 )

pd3 <- pd2[,hasatleast8]

n <- ave(pd3$LMILEX_BB, pd3$country, FUN=function(x)sum(!is.na(x)))
n
# pd3 <- pd3[n > 30, ]  #only keep if we have 15 years of this variable
# names(pd3)
# unique(pd3$country)
# unique(mmALL50$country.name.en)

pd3$country <- droplevels(pd3$country)
unique(pd3$country)

sort(names(pd3) )
missmap(pd3, csvar = "country", tsvar = "year")



mmALL50 %>% group_by(country_PWT) %>% summarise(cs = colSums(is.na(.))) %>% print(n=200)

library(mice)
md.pattern(mmALL50)

# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# install.packages(c("UpSetR ", "naniar")
library(naniar)
# https://naniar.njtierney.com/articles/getting-started-w-naniar.html



# https://stackoverflow.com/questions/71881921/r-calculate-percentage-of-missing-values-na-per-day-for-a-column-in-a-data-fr


set.seed(111111111)
library(Amelia)

## check logical bounds of vars.
names(mmALL50)

# nominal
majPow_COW      
"USally_PJM"                
[84] "Rusdefense_PJM" 

# check these
[60] "LMILEX_BB"                 
[62] "PEACEYRS_BB"               
[63] "DEMOC_BB"                  
[64] "TRADE_GDP_BB"              
[65] "CONTIG_BB"                 
[66] "ALLIES_BB"  
"LNRGDP_BB"                 
[70] "LNFOES_BB"                 
[71] "LNFRIENDS_BB"  
"DEMOC_NORD"  
"theil_UTIP"                
[148] "gini_UTIP"  





## and set k X 3 matrix for `bounds` argument in `amelia()`

library(psych)

describe(mmALL50$gini_UTIP)

describe(mmALL50$gini_UTIP)


## subset for imputation
pd4 <- mmALL50 %>% select(-contains("PWT"), -contains("SWIID"))
names(mmALL50)
names(pd4)


names(pd4)
# add ridge priors (5%)
a.out.time1950_noPWTnoSWIID <- amelia(pd4, ts = "year", cs = "country", parallel = "multicore", ncpus = 4, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(pd4), idvars = c("iso3n", "cown", "un", "gwn", "fao" ,"gaul" ,"imf" ,"iso4217n", "un.region.code", "un.regionintermediate.code" ,"un.regionsub.code" ,"unpd", "vdem" ,"country.name.de", "country.name.de.regex", "country.name.en.regex", "country.name.fr", "country.name.fr.regex", "country.name.it", "country.name.it.regex", "iso3c", "cowc", "wb", "ar5", "cctld", "continent", "currency", "dhs", "ecb", "eu28", "eurocontrol_pru", "eurocontrol_statfor", "eurostat", "fips", "genc2c", "genc3c", "genc3n", "gwc", "icao.region", "ioc", "iso2c", "iso4217c", "p4c", "p5c", "region", "region23", "unhcr", "unicode.symbol", "wb_api2c", "wb_api3c", "NAMENEW_BB", "obs_NORD", "NAMENEW_NORD", "ccode2_PJM", "version_COW", "statenme_COW", "stateabb_COW", "wvs", "code_UTIP", "country_UTIP", "countryname_UTIP", "cown_UTIP"   ))  # "cown_SWIID"
save.image( file = "aOutTime1950_noPWTnoSWIID_empi05-polytime3.RData")



# add ridge priors (1% -> less 'shrinkage')
a.out.time1950_noPWTnoSWIID <- amelia(pd4, ts = "year", cs = "country", parallel = "multicore", ncpus = 4, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .01 * nrow(pd4), idvars = c("iso3n", "cown", "un", "gwn", "fao" ,"gaul" ,"imf" ,"iso4217n", "un.region.code", "un.regionintermediate.code" ,"un.regionsub.code" ,"unpd", "vdem" ,"country.name.de", "country.name.de.regex", "country.name.en.regex", "country.name.fr", "country.name.fr.regex", "country.name.it", "country.name.it.regex", "iso3c", "cowc", "wb", "ar5", "cctld", "continent", "currency", "dhs", "ecb", "eu28", "eurocontrol_pru", "eurocontrol_statfor", "eurostat", "fips", "genc2c", "genc3c", "genc3n", "gwc", "icao.region", "ioc", "iso2c", "iso4217c", "p4c", "p5c", "region", "region23", "unhcr", "unicode.symbol", "wb_api2c", "wb_api3c", "NAMENEW_BB", "obs_NORD", "NAMENEW_NORD", "ccode2_PJM", "version_COW", "statenme_COW", "stateabb_COW", "wvs", "code_UTIP", "country_UTIP", "countryname_UTIP", "cown_UTIP"   ))  # "cown_SWIID"
save.image( file = "aOutTime1950_noPWTnoSWIID_empi01-polytime3.RData")




# https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17



pd5 <- mmALL50 %>% select(-contains("PWT") )
names(mmALL50)
names(pd5)

# add ridge priors (5% -> less 'shrinkage')
a.out.time1950_all_polytime1 <- amelia(mmALL50, ts = "year", cs = "country", parallel = "multicore", ncpus = 4, polytime =1, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(mmALL50), idvars = c("iso3n", "cown", "un", "gwn", "fao" ,"gaul" ,"imf" ,"iso4217n", "un.region.code", "un.regionintermediate.code" ,"un.regionsub.code" ,"unpd", "vdem" ,"country.name.de", "country.name.de.regex", "country.name.en.regex", "country.name.fr", "country.name.fr.regex", "country.name.it", "country.name.it.regex", "iso3c", "cowc", "wb", "ar5", "cctld", "continent", "currency", "dhs", "ecb", "eu28", "eurocontrol_pru", "eurocontrol_statfor", "eurostat", "fips", "genc2c", "genc3c", "genc3n", "gwc", "icao.region", "ioc", "iso2c", "iso4217c", "p4c", "p5c", "region", "region23", "unhcr", "unicode.symbol", "wb_api2c", "wb_api3c", "NAMENEW_BB", "obs_NORD", "NAMENEW_NORD", "ccode2_PJM", "version_COW", "statenme_COW", "stateabb_COW", "wvs", "code_UTIP", "country_UTIP", "countryname_UTIP", "cown_UTIP" , "cown_SWIID", "country_PWT" , "currency_PWT",
																																																   "i_cig_PWT", "i_xm_PWT", "i_xr_PWT", "i_outlier_PWT", "i_irr_PWT"))  # "cown_SWIID"
save.image( file = "aOutTime1950_all_empi05_polytime1.RData")




#####################################################
################################################
#### IMPUTATION CHECKS (from Amelia)




#####################################################
################################################
#### CLEAN UP LAGS (USE `purr` to function over a list)

sort(names(imp1))

# lmilex1_nord   lmilex1_bb

glimpse(imp1)


