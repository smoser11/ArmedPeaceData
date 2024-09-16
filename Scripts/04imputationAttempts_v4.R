# Imputation attempts

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
names(mmmALL50)
mmmALL50 <- mmmALL50 %>% select(-country.name.en)
glimpse(mmmALL50)
pd <- mmmALL50 %>% select(contains("HSS"), country, year)



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

library(plm)
tt <- pdata.frame(impZFS1, index = c("STATE_ZFS","YEAR_ZFS"))

duplicates <- impZFS1 %>%
	group_by(STATE_ZFS, YEAR_ZFS) %>%
	filter(n() > 1)

# View the duplicates
print(duplicates)  

## SSOMETHING WRONG WITH 255 merge
ZFSimp3 <- impZFS1 %>% select(STATE_ZFS, YEAR_ZFS, morethreat_ZFS, moreathreat_ZFS, everything())
# Load necessary libraries
library(dplyr)
library(plm)

ZFSimp3 <- impZFS1 %>% filter(STATE_ZFS!=255)
	
# Assuming your data frame is named ZFSimp3
# Load your data (example)
# ZFSimp3 <- read.csv("your_data_file.csv")

# Identify and print duplicates
duplicates <- ZFSimp3 %>%
	group_by(STATE_ZFS, YEAR_ZFS) %>%
	filter(n() > 1)

print(duplicates)

# Convert factors to characters to avoid type mismatch
ZFSimp3 <- ZFSimp3 %>%
	mutate(across(where(is.factor), as.character))

# Custom function to handle duplicates
combine_duplicates <- function(df) {
	df_combined <- df[1,] # Start with the first row of the group
	
	for (col in colnames(df)) {
		values <- df[[col]]
		unique_values <- unique(values[!is.na(values)])
		
		if (length(unique_values) == 1) {
			# All non-missing values are the same
			df_combined[[col]] <- unique_values
		} else if (any(!is.na(values))) {
			# There are non-missing values, but they differ
			non_missing_values <- values[!is.na(values)]
			if (length(unique(non_missing_values)) == 1) {
				df_combined[[col]] <- non_missing_values[1]
			} else {
				stop(paste("Inconsistent non-missing values in column", col))
			}
		} else {
			# All values are missing
			df_combined[[col]] <- NA
		}
	}
	
	return(df_combined)
}

### DOUBLE CHECK THIS IS A PANEL!
library(plm)
pdata.frame(ZFSimp3, index=c("STATE_ZFS","YEAR_ZFS"))  ## TROUBLE HERE

# ## Make sure this is a panel!
# library(plm)
# names(ZFSimp3)
# 
# #pdata.frame(mmPWT50, index = c("country_PWT","year_PWT")) %>% pdim()
# pd <- pdata.frame(ZFSimp3, index = c("STATE_ZFS","YEAR_ZFS")) %>% pdim()
# table(index(pd), useNA = "always")
# table(index(pd), useNA = "ifany")
# # Identify rows with NAs in index columns
# na_rows <- which(is.na(ZFSimp3$STATE_ZFS) | is.na(ZFSimp3$YEAR_ZFS))
# print((na_rows))
# 
# # ZFSimp3 <- ZFSimp3[-na_rows,]
# 
# # Apply the custom function to each group
# ZFSimp3_combined <- ZFSimp3 %>%
# 	group_by(STATE_ZFS, YEAR_ZFS) %>%
# 	do(combine_duplicates(.)) %>%
# 	ungroup()
# 
# Create the panel data frame
ZFSimp3_pdata <- pdata.frame(ZFSimp3, index = c("STATE_ZFS", "YEAR_ZFS"))

# Verify the panel data structure
print(head(ZFSimp3_pdata))


impZFS1 <- ZFSimp3_pdata
save(impZFS1,file =paste0(here("Data/Processed/", "impZFSimp3.RData") ))



# 
# ## Drop countries
# 
# ss <- pd %>% group_by(country.name.en) %>%
# 	summarise_all(~sum(is.na(.))/n())
# 
# toDrop <- ss$country[which(ss$country_name_HSS >= 0.5)]
# 
# impZFS1 <- impZFS1 %>% filter(!(country %in% toDrop) )
# missmap(mmmCLEAN50, csvar = "country", tsvar = "year")
# 
# mmmCLEAN50 <- mmmCLEAN50 %>% select(!contains("_YE_"))
# names(mmmCLEAN50)
# 





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

names(mmHSS50)
dat <- list(mmHSS50,mmPJM50,mmUTIP50)
names(dat[[3]])
impHSS1 <- dat %>%  purrr::reduce(left_join, by = c(names(ccpConBal50)))
names(impHSS1)
head(impHSS1)


## Make sure this is a panel!
library(plm)
sort(names(impHSS1) )

#pdata.frame(mmPWT50, index = c("country_PWT","year_PWT")) %>% pdim()
pd <- pdata.frame(impHSS1, index = c("country.name.en","year")) %>% pdim()
table(index(pd), useNA = "always")
table(index(pd), useNA = "ifany")
# # Identify rows with NAs in index columns
# na_rows <- which(is.na(ZFSimp3$STATE_ZFS) | is.na(ZFSimp3$YEAR_ZFS))
# print((na_rows))
pd
class(pd)
pdim(pd)

save(impHSS1,file =paste0(here("Data/Processed/", "impHSS1.RData") ))

sort(unique(mmHSS50$country.name.en))
sort(unique(mmZFS50$country.name.en))

#####################################################################################
############################################################################
#####################################################################
## imputation attempts


load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)
names(ccpConBal50)


library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)

### 

load(file =paste0(here("./Data/Processed/", "impHSS1.RData") ))

getwd()
#install.packages("Amelia")



# library(mice)
# md.pattern(mmALL50)

# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# install.packages(c("UpSetR ", "naniar")
library(naniar)
# https://naniar.njtierney.com/articles/getting-started-w-naniar.html



# https://stackoverflow.com/questions/71881921/r-calculate-percentage-of-missing-values-na-per-day-for-a-column-in-a-data-fr


library(Amelia)


# # nominal
# majPow_COW      
# "USally_PJM"                
# [84] "Rusdefense_PJM" 
# 
# # check these
# [60] "LMILEX_BB"                 
# [62] "PEACEYRS_BB"               
# [63] "DEMOC_BB"                  
# [64] "TRADE_GDP_BB"              
# [65] "CONTIG_BB"                 
# [66] "ALLIES_BB"  
# "LNRGDP_BB"                 
# [70] "LNFOES_BB"                 
# [71] "LNFRIENDS_BB"  
# "DEMOC_NORD"  
# "theil_UTIP"                
# [148] "gini_UTIP"  




library(psych)

#ccnames <- c(paste(sQuote(names(ccpConBal50)), collapse = "," ))

names_paste_quotesf <- function(my_data) {
	paste0('"',
		   paste0(names(my_data), collapse = '" , "'),
		   '"') %>% cat
}
ccnames <- names_paste_quotesf(ccpConBal50) 
ccnames

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
	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
}

# Start the clock!
ptm <- proc.time()


# Loop through the vector, adding one
tt <- foreach(i=1:1) %dopar% impFun(i)

# Stop the clock
time1<- proc.time() - ptm


# 2
set.seed(seed*2)
# Start the clock!
ptm2 <- proc.time()

# Use amelia multi-core
a.out.time1950_HSS1_empi05 <- amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "multicore", ncpus = 5, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"

# Stop the clock
time2<- proc.time() - ptm2

time1
time2

# ------------
#########################################################################
#########################################################################
### FULL IMPUTATION RUNS
#########################################################################
#########################################################################


names_paste_quotesf <- function(my_data) {
	paste0('"',
		   paste0(names(my_data), collapse = '" , "'),
		   '"') %>% cat
}
ccnames <- names_paste_quotesf(ccpConBal50) 
ccnames

ccnames <- c("country.name.de" , "country.name.de.regex" , "country.name.en.regex" , "country.name.fr" , "country.name.fr.regex" , "country.name.it" , "country.name.it.regex" , "iso3n" , "iso3c" , "cown" , "cowc" , "un" , "wb" , "gwn" , "ar5" , "cctld" , "continent" , "currency" , "dhs" , "ecb" , "eu28" , "eurocontrol_pru" , "eurocontrol_statfor" , "eurostat" , "fao" , "fips" , "gaul" , "genc2c" , "genc3c" , "genc3n" , "gwc" , "icao.region" , "imf" , "ioc" , "iso2c" , "iso4217c" , "iso4217n" , "p4c" , "p4n" , "p5c" , "p5n" , "region" , "region23" , "un.region.code" , "un.regionintermediate.code" , "un.regionsub.code" , "unhcr" , "unicode.symbol" , "unpd" , "vdem" , "wb_api2c" , "wb_api3c" , "wvs")

ccnames

cccnames <- c("iso3n", "cown", "un", "gwn", "fao" ,"gaul" ,"imf" ,"iso4217n", "un.region.code", "un.regionintermediate.code" ,"un.regionsub.code" ,"unpd", "vdem" ,"country.name.de", "country.name.de.regex", "country.name.en.regex", "country.name.fr", "country.name.fr.regex", "country.name.it", "country.name.it.regex", "iso3c", "cowc", "wb", "ar5", "cctld", "continent", "currency", "dhs", "ecb", "eu28", "eurocontrol_pru", "eurocontrol_statfor", "eurostat", "fips", "genc2c", "genc3c", "genc3n", "gwc", "icao.region", "ioc", "iso2c", "iso4217c", "p4c", "p5c", "p5n","region", "region23", "unhcr", "unicode.symbol", "wb_api2c", "wb_api3c")


seed<-111111111
set.seed(seed)
library(Amelia)

library(parallel)                          
detectCores() 

library(doParallel)
library(foreach)
cl <- makeCluster(10)           
registerDoParallel(cl)

load(file =paste0(here("./Data/Processed/", "impHSS1.RData") ))

sort(names(impHSS1))

library(tidyverse)
impHSS1 <- dplyr::select(impHSS1, ! c(defense_dem_HSS, defense_nodem_HSS, LMILGDP_HSS, icowterr_HSS, ht66_HSS, Rusdefense_PJM) )
sort(names(impHSS1))

class(impHSS1)
names(impHSS1)
pdata.frame(impHSS1, index = c("ccode_HSS", "year_HSS"))

a.out.time1950_HSS1_empi05 <- list()

# add ridge priors (2% -> less 'shrinkage')
impFun_empi05 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
}
# Loop through the vector, adding one
a.out.time1950_HSS1_empi05 <- foreach(i=1:5) %dopar% impFun_empi05(i)


save(a.out.time1950_HSS1_empi05, file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi05_polytime3.RData")))


# a.out.time1950_HSS1_emp05 <- list()
# # add ridge priors (5%)
# a.out.time1950_HSS1_empi05 <- amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "multicore", ncpus = 5, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
# 
# save(a.out.time1950_HSS1_empi05,file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi05-polytime3.RData")) )
# 
# #save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi05-polytime3.RData")))
# 


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


##################################################
# Less shrinkage

gc()
seed<-111111111
set.seed(seed)
library(Amelia)

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)


a.out.time1950_HSS1_empi03 <- list()

# add ridge priors (2% -> less 'shrinkage')
impFun_empi03 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .03 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
}
# Loop through the vector, adding one
a.out.time1950_HSS1_empi03 <- foreach(i=1:5) %dopar% impFun_empi03(i)

save(a.out.time1950_HSS1_empi03, file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi03_polytime3.RData")))




# https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17


                                      
#############################################
### Even less shrinkage

gc()
seed<-11111112
set.seed(seed)
library(Amelia)

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)


a.out.time1950_HSS1_empi01 <- list()

# add ridge priors (1% -> less 'shrinkage')
impFun_empi01 <- function(x) {
	set.seed( (seed*x))
	library(Amelia)
	amelia( dplyr::select(impHSS1,! c(version_HSS, icowterrA_HSS, icowterrB_HSS, pn6_50_HSS, pn6_66_HSS, pn6_33_HSS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .01 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_HSS", "year_HSS" ,"country_name_HSS", "stateabbA_HSS", "stateabb_HSS"  ))  # "cown_SWIID"
}
# Loop through the vector, adding one
a.out.time1950_HSS1_empi01 <- foreach(i=1:5) %dopar% impFun_empi01(i)
#save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi01_polytime3.RData")))
save(a.out.time1950_HSS1_empi01, file = paste0(here("Data/Processed/Imputations", "aOutTime1950_HSS1_empi01_polytime3.RData")))





#################################################################################
######   ZFS data

rm(list=ls())
gc()

library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)
library(here)

### 

#load(file =paste0(here("Data/Processed/", "impZFS1.RData") ))
load(file =paste0(here("Data/Processed", "impZFSimp3.RData") ))

#save(impZFS1,file =paste0(here("Data/Processed/", "impZFSimp3.RData") ))
pdata.frame(impZFS1, index = c("STATE_ZFS","YEAR_ZFS"))
pdata.frame(impZFS1, index = c("country.name.en","year"))


getwd()
#install.packages("Amelia")


load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)
names(ccpConBal50)


# library(mice)
# md.pattern(mmALL50)

# https://cran.r-project.org/web/packages/naniar/vignettes/naniar-visualisation.html
# install.packages(c("UpSetR ", "naniar")
library(naniar)
# https://naniar.njtierney.com/articles/getting-started-w-naniar.html



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
names(impZFS1)

impZFS1 %>% select(! c(year_PJM,ccode_PJM,code_UTIP, year_UTIP, country.name.en_UTIP  ) )  %>% names()



## MAKE SURE THIS IS AS PANNEL
## Make sure this is a panel!
library(plm)
sort(names(impZFS1) )

#pdata.frame(mmPWT50, index = c("country_PWT","year_PWT")) %>% pdim()
pd <- pdata.frame(impZFS1, index = c("country.name.en","year")) %>% pdim()
table(index(pd), useNA = "always")
table(index(pd), useNA = "ifany")
# # Identify rows with NAs in index columns
# na_rows <- which(is.na(ZFSimp3$STATE_ZFS) | is.na(ZFSimp3$YEAR_ZFS))
# print((na_rows))
pd
class(pd)


seed<-111111111
set.seed(seed)
library(Amelia)

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)



# load(file =paste0(here("./Data/Processed/", "impZFS1.RData") ))

sort(names(impZFS1))

library(tidyverse)
# impZFS1 <- dplyr::select(impZFS1, ! c(defense_dem_ZFS, defense_nodem_ZFS, LMILGDP_ZFS, icowterr_ZFS, ht66_ZFS, Rusdefense_PJM) )  # not needed
sort(names(impZFS1))

class(impZFS1)
names(impZFS1)
pdata.frame(impZFS1, index = c("country.name.en", "year"))
pdata.frame(impZFS1, index = c("STATE_ZFS", "YEAR_ZFS"))



a.out.time1950_ZFS1_emp05 <- list()

# add ridge priors (2% -> less 'shrinkage')
impFun_empi05 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia( dplyr::select(impZFS1,! c(version_ZFS, icowterrA_ZFS, icowterrB_ZFS, pn6_50_ZFS, pn6_66_ZFS, pn6_33_ZFS ) ), ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_ZFS", "year_ZFS" ,"country_name_ZFS", "stateabbA_ZFS", "stateabb_ZFS"  ))  # "cown_SWIID"
}

# add ridge priors (2% -> less 'shrinkage')
impFun_empi05 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia( impZFS1, ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .05 * nrow(impHSS1), idvars = c(   "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_ZFS", "year_ZFS" ,"country_name_ZFS", "stateabbA_ZFS", "stateabb_ZFS"  ))  # "cown_SWIID"
}
sort(names(impZFS1))

# Loop through the vector, adding one
a.out.time1950_ZFS1_emp05 <- foreach(i=1:5) %dopar% impFun_empi05(i)

save(a.out.time1950_ZFS1_emp05, file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi05_polytime3.RData")))


sort(names(impZFS1))
a.out.time1950_ZFS1_emp05 <- list()
# # add ridge priors (5%)
# a.out.time1950_ZFS1_empi05 <- amelia( dplyr::select(impZFS1,! c( icowterrA_ZFS, icowterrB_ZFS, pn6_50_ZFS, pn6_66_ZFS, pn6_33_ZFS ) ), ts = "year", cs = "country.name.en", parallel = "multicore", ncpus = 5, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(impHSS1), idvars = c( ccnames,  "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "ccode_ZFS", "year_ZFS" ,"country_name_ZFS", "stateabbA_ZFS", "stateabb_ZFS"  ))  # "cown_SWIID"

sort(names(impZFS1))
impZFS1$country <- impZFS1$country.name.en

impZFS1 <- dplyr::select(impZFS1, ! c( uncertain_ZFS, smilincr_ZFS, milincr_ZFS, rgdp1_ZFS, threat_ZFS, athreat1_ZFS, rgdpincr_2_ZFS, rgdpincr_3_ZFS ) )

sort(names(impZFS1))
ccnames

# add ridge priors (5%)
a.out.time1950_ZFS1_empi05 <- amelia( impZFS1 , ts = "year", cs = "country.name.en", parallel = "multicore", ncpus = 5, polytime = 3, intercs = TRUE, p2s = 2, m=5, empri = .05 * nrow(impZFS1), idvars = c( ccnames, "country", "country.name.en_UTIP", "year_UTIP", "code_UTIP", "country_UTIP", "countryname_UTIP", "year_PJM", "ccode_PJM", "YEAR_ZFS" , "STATE_ZFS"))  # "cown_SWIID"

save(a.out.time1950_ZFS1_empi05,file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi05-polytime3.RData")) )




a.out.time1950_ZFS1_empi05 <- list()

impFun_empi05 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia(dplyr:::select(impZFS1, !c("ccode_PJM","year_PJM", "code_UTIP"    ,"year_UTIP","country_UTIP", "countryname_UTIP" ,  "country.name.en_UTIP"  ) ),
		   ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .05 * nrow( impZFS1), idvars = c( ccnames  ))  # "cown_SWIID"
}

># Loop through the vector, adding one
a.out.time1950_ZFS1_empi05 <- foreach(i=1:5) %dopar% impFun_empi05(i)

save(a.out.time1950_ZFS1_empi05, file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi05_polytime3.RData")))

#save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi05_polytime3.RData")))



## Less shrinkage
names(impZFS1)

# amelia(dplyr:::select(impZFS1, !c("ccode_PJM","year_PJM", "code_UTIP"    ,"year_UTIP","country_UTIP", "countryname_UTIP" ,  "country.name.en_UTIP"  ) ),
# ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .03 * nrow( impZFS1), idvars = c( ccnames  ))  # "cown_SWIID"


# add ridge priors (3% -> less 'shrinkage')
a.out.time1950_ZFS1_emp03 <- list()

impFun_empi03 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia(dplyr:::select(impZFS1, !c("ccode_PJM","year_PJM", "code_UTIP"    ,"year_UTIP","country_UTIP", "countryname_UTIP" ,  "country.name.en_UTIP"  ) ),
		   ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .03 * nrow( impZFS1), idvars = c( ccnames  ))  # "cown_SWIID"
}
# Loop through the vector, adding one
a.out.time1950_ZFS1_emp03 <- foreach(i=1:5) %dopar% impFun_empi03(i)

save(a.out.time1950_ZFS1_emp03,file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi03_polytime3.RData")))


# save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi03_polytime3.RData")))

str(a.out.time1950_ZFS1_emp03[[1]]$imputations$imp1)

missmap(a.out.time1950_HSS1_emp03[[1]]$imputations$imp1, csvar = "country.name.en", tsvar = "year")


# https://medium.com/coinmonks/dealing-with-missing-data-using-r-3ae428da2d17



#############################################
### Even less shrinkage

gc()
seed<-11111112
set.seed(seed)
library(Amelia)

library(parallel)
detectCores()

library(doParallel)
library(foreach)
cl <- makeCluster(5)
registerDoParallel(cl)


a.out.time1950_ZFS1_emp01 <- list()

# add ridge priors (1% -> less 'shrinkage')

impFun_empi01 <- function(x) {
	set.seed( (seed+x))
	library(Amelia)
	amelia(dplyr:::select(impZFS1, !c("ccode_PJM","year_PJM", "code_UTIP"    ,"year_UTIP","country_UTIP", "countryname_UTIP" ,  "country.name.en_UTIP"  ) ),
		   ts = "year", cs = "country.name.en", parallel = "no", ncpus = 1, polytime = 3, intercs = TRUE, p2s = 2, m=1, empri = .01 * nrow( impZFS1), idvars = c( ccnames  ))  # "cown_SWIID"
}
# Loop through the vector, adding one
a.out.time1950_ZFS1_emp01 <- foreach(i=1:5) %dopar% impFun_empi01(i)

save(a.out.time1950_ZFS1_emp01, file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi01_polytime3.RData")))

#save.image( file = paste0(here("Data/Processed/Imputations", "aOutTime1950_ZFS1_empi01_polytime3.RData")))
























#####################################################
################################################
#### IMPUTATION CHECKS (from Amelia)




#####################################################
################################################
#### CLEAN UP LAGS (USE `purr` to function over a list)

sort(names(imp1))

# lmilex1_nord   lmilex1_bb

glimpse(imp1)


