## Inspecting and Cleaning merged data

rm(list=ls())


getwd()
library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)
library(here)
library(plm)

load(file =  paste0(here("Data/Processed/", "mmALL50.RData") ))

mmALL50$country <- mmALL50$country.name.en
mmALL50 <- mmALL50 %>% select(-country.name.en)
glimpse(mmALL50)
pd <- mmALL50


missmap(pd, csvar = "country", tsvar = "year")


mmCLEAN50 <- mmALL50 %>% select(-"NA." )
mmCLEAN50 <- mmCLEAN50 %>% select(-"NA._PJM"    )
mmCLEAN50 <- mmCLEAN50 %>% select(-  c("NA._HSS" ,"...1_COW"  ) )
names(mmCLEAN50)

sort(unique(mmCLEAN50$country))

pd <- mmCLEAN50
missmap(pd, csvar = "country.name.en", tsvar = "year")

ss <- pd %>% group_by(country.name.en) %>%
	summarise_all(~sum(is.na(.))/n())

toDrop <- ss$country[which(ss$country_name_HSS >= 0.5)]

c("Andorra", "Antigua & Barbuda", "Armenia","")

mmCLEAN50 <- mmCLEAN50 %>% filter(!(country %in% toDrop) )
missmap(mmCLEAN50, csvar = "country", tsvar = "year")

mmCLEAN50 <- mmCLEAN50 %>% select(!contains("_YE_"))
names(mmCLEAN50)


######################################################
######################################################
######################################################
## INSPECT MERGED DATA
######################################################


getwd()
library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)
library(here)

load(file =  paste0(here("Data/Processed/", "mmALL50.RData") ))

mmALL50$country <- mmALL50$country.name.en
mmALL50 <- mmALL50 %>% select(-country.name.en)
glimpse(mmALL50)
pd <- mmALL50


missmap(pd, csvar = "country", tsvar = "year")

ss <- pd %>% group_by(country) %>%
	summarise_all(~sum(is.na(.))/n())
sort(unique(pd$country) )

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



mmALL50 %>% group_by(country) %>% summarise(cs = colSums(is.na(.)))

library(mice)
md.pattern(mmALL50)

sort(unique(mmALL50$country))


names(mmALL50)







