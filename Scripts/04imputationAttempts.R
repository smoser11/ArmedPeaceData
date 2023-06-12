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

load(file =paste0(here("Data/Processed/", "mmmALL50.RData") ))
names(mmmALL50)

library(sampleSelection)

mmmALL50$country <- mmmALL50$country.name.en
mmmALL50 <- mmmALL50 %>% select(-country.name.en)
glimpse(mmmALL50)
pd <- mmmALL50 %>% select(contains("HSS"), country, year)
### compare:
missmap(pd, csvar = "ccode_HSS", tsvar = "year_HSS")

names(mmHSS50)
missmap(mmHSS50, csvar = "ccode_HSS", tsvar = "year_HSS")
names(mmZFS50)
missmap(mmZFS50, csvar = "STATE_ZFS", tsvar = "YEAR_ZFS")
ss <- pd %>% group_by(country) %>%
	summarise_all(~sum(is.na(.))/n())

########################
rm(list=ls())

### PJM + UTIP + ZFS

load(file =paste0(here("Data/Processed/", "mmZFS50.RData") ))
sort(names(mmZFS50))
names(mmZFS50)
mmZFS50 <- mmZFS50 %>% filter(year>= 1950)

load(file =paste0(here("Data/Processed/", "mmPJM50.RData") ))
names(mmPJM50)
mmPJM50 <- mmPJM50 %>% filter(year>= 1950)
names(mmPJM50)

load(file =paste0(here("Data/Processed/", "mmUTIP50.RData") ))
mmUTIP50 <- mmUTIP50 %>% filter(year>= 1950)

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)
names(ccpConBal50)

sort(unique(mmZFS50$country.name.en))
sort(unique(mmPJM50$country.name.en))

names(mmZFS50)
dat <- list(mmZFS50,mmPJM50,mmUTIP50)
names(dat[[3]])
impZFS1 <- dat %>%  purrr::reduce(left_join, by = c(names(ccpConBal50)))
names(impZFS1)
head(impZFS1)

pd <- impZFS1
missmap(pd, csvar = "country.name.en", tsvar = "year")

which(is.na(impZFS1$Rusdefense_PJM))


save(impZFS1,file =paste0(here("Data/Processed/", "impZFS1.RData") ))



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

pd3$country <- droplevels(pd3$country)
unique(pd3$country)

sort(names(pd3) )
missmap(pd3, csvar = "country", tsvar = "year")



mmmALL50 %>% group_by(country) %>% summarise(cs = colSums(is.na(.)))

library(mice)
md.pattern(mmmALL50)



## Drop countries

ss <- pd %>% group_by(country.name.en) %>%
	summarise_all(~sum(is.na(.))/n())

toDrop <- ss$country[which(ss$country_name_HSS >= 0.5)]

impZFS1 <- impZFS1 %>% filter(!(country %in% toDrop) )
missmap(mmmCLEAN50, csvar = "country", tsvar = "year")

mmmCLEAN50 <- mmmCLEAN50 %>% select(!contains("_YE_"))
names(mmmCLEAN50)






### PJM + UTIP + HSS

load(file =paste0(here("Data/Processed/", "mmHSS50.RData") ))
mmHSS50 <- mmHSS50 %>% filter(year>= 1950)
load(file =paste0(here("Data/Processed/", "mmPJM50.RData") ))
mmPJM50 <- mmPJM50 %>% filter(year>= 1950)
load(file =paste0(here("Data/Processed/", "mmUTIP50.RData") ))
mmUTIP50 <- mmUTIP50 %>% filter(year>= 1950)
load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))
ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

names(mmZFS50)
dat <- list(mmHSS50,mmPJM50,mmUTIP50)
names(dat[[3]])
impHSS1 <- dat %>%  purrr::reduce(left_join, by = c(names(ccpConBal50)))
names(impHSS1)
head(impHSS1)

save(impHSS1,file =paste0(here("Data/Processed/", "impHSS1.RData") ))

sort(unique(mmHSS50$country.name.en))
sort(unique(mmZFS50$country.name.en))

#####################################################################################

## imputation attempts


library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)

### 

load(file =paste0(here("Data/Processed/Imputations", "impHSS1.RData") ))

getwd()
#install.packages("Amelia")



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

ccnames <- c(paste(sQuote(names(ccpConBal50)), collapse = "," ))

names_paste_quotesf <- function(my_data) {
	paste0('"',
		   paste0(names(my_data), collapse = '" , "'),
		   '"') %>% cat
}
ccnames <- names_paste_quotesf(ccpConBal50) 

ccnames <- c("country.name.de" , "country.name.de.regex" , "country.name.en.regex" , "country.name.fr" , "country.name.fr.regex" , "country.name.it" , "country.name.it.regex" , "iso3n" , "iso3c" , "cown" , "cowc" , "un" , "wb" , "gwn" , "ar5" , "cctld" , "continent" , "currency" , "dhs" , "ecb" , "eu28" , "eurocontrol_pru" , "eurocontrol_statfor" , "eurostat" , "fao" , "fips" , "gaul" , "genc2c" , "genc3c" , "genc3n" , "gwc" , "icao.region" , "imf" , "ioc" , "iso2c" , "iso4217c" , "iso4217n" , "p4c" , "p4n" , "p5c" , "p5n" , "region" , "region23" , "un.region.code" , "un.regionintermediate.code" , "un.regionsub.code" , "unhcr" , "unicode.symbol" , "unpd" , "vdem" , "wb_api2c" , "wb_api3c" , "wvs")

ccnames

cccnames <- c("iso3n", "cown", "un", "gwn", "fao" ,"gaul" ,"imf" ,"iso4217n", "un.region.code", "un.regionintermediate.code" ,"un.regionsub.code" ,"unpd", "vdem" ,"country.name.de", "country.name.de.regex", "country.name.en.regex", "country.name.fr", "country.name.fr.regex", "country.name.it", "country.name.it.regex", "iso3c", "cowc", "wb", "ar5", "cctld", "continent", "currency", "dhs", "ecb", "eu28", "eurocontrol_pru", "eurocontrol_statfor", "eurostat", "fips", "genc2c", "genc3c", "genc3n", "gwc", "icao.region", "ioc", "iso2c", "iso4217c", "p4c", "p5c", "p5n","region", "region23", "unhcr", "unicode.symbol", "wb_api2c", "wb_api3c")

c(ccnames, "asdf")
names(impHSS1)

impHSS1 %>% select(! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ) %>% names()

seed<-111111111
set.seed(seed)
library(Amelia)


##### BENCHMARKING

## 1.

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)

impFun <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
}

# Start the clock!
ptm <- proc.time()


# Loop through the vector, adding one
tt <- foreach(i=1:2) %dopar% impFun(i)

# Stop the clock
time1<- proc.time() - ptm


# 2
set.seed(seed*2)
# Start the clock!
ptm2 <- proc.time()

# Use amelia multi-core
a.out.time1950_HSS1_empi05 <- amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "multicore", ncpus = 5, polytime = 3, intercs = TRUE, p2s = 2, m=2, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"

# Stop the clock
time2<- proc.time() - ptm2

time1
time2

# ------------


seed<-111111111
set.seed(seed)
library(Amelia)

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)


# add ridge priors (5%)
a.out.time1950_HSS1_empi05 <- amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "multicore", ncpus = 5, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"

save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi05-polytime3.RData")))



#
#
#
#
#
#
#
#
#


# impFun <- function(x) {
# 	set.seed( (seed+x))
# 	library(Amelia)
# 	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
# }


seed<-111111111
set.seed(seed)
library(Amelia)

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)


a.out.time1950_HSS1_emp03 <- list()

# add ridge priors (2% -> less 'shrinkage')
impFun_empi03 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .03 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
}
# Loop through the vector, adding one
a.out.time1950_HSS1_emp03 <- foreach(i=1:5) %dopar% impFun(i)

save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi03_polytime3.RData")))



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


