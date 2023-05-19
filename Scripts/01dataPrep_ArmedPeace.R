rm(list =ls())

## In this file:
## For each data-set (sdat), e.g COW:
### A. Load data-set
### B. Clean data and reshape to long
### C. determine countryX year key  and *make corrections*
### D. Merge with codelist_panel from countrycode package (not necc. balanced / consecutive)
### E. Save merged dataset as `.RData` and `.dta`


####################################################

library(here)
library(tidyverse)


#################################################################

##################################################################
##################################################################
##################################################################
##################################################################
# Penn world tables (general) v10 https://www.rug.nl/ggdc/productivity/pwt/
##  relative levels of income, output, input, and productivity for 183 countries between 1950 and 2019.
## https://cran.r-project.org/web/packages/pwt10/pwt10.pdf
##################################################################

rm(list=ls())
###### countrycode and codelist_panel 
library(tidyverse)
library(janitor)
library(countrycode)



# install.packages("pwt10")
library(pwt10)
data("pwt10.0")
pwt10 <- pwt10.0

summary(pwt10$year)
##################################################################


library(stringr)

##################################################################
######## Merge with codelist_panel

library(tidyverse)

getwd()


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed","codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

sort(unique(ccp50$country.name.en))


names(pwt10)

##############################################
### FIND KEY

rrn <- pwt10$year
rrc <- pwt10$isocode

guess_field(rrc)   # so pwt10$isocode is iso3c !!!!!


##################################################################
######## Merge with codelist_panel

library(tidyverse)

getwd()


# add suffix to vars
## Add suffix
colnames(pwt10) <- paste(colnames(pwt10), "PWT", sep = "_")
colnames(pwt10)
names(pwt10)

mmPWT50 <- left_join(pwt10, ccpConBal50, by = c( "isocode_PWT" = "iso3c", "year_PWT"="year") )
names(mmPWT50)


library(janitor)
library(haven)
write_dta(clean_names(mmPWT50), path = paste0(here("Data/Processed","mmPWT50.dta") ))

getwd()
save(mmPWT50, file = paste0(here("Data/Processed","mmPWT50.RData") ))



##################################################################
##################################################################
##################################################################
##################################################################
# COW State Membership data:
## https://correlatesofwar.org/data-sets/state-system-membership/
##################################################################

getwd()

cow_states <- read.csv(paste0(here("Data/Raw/COW/StateSystemMembership", "states2016.csv") ))
cow_majors <- read.csv(paste0(here("Data/Raw/COW/StateSystemMembership", "majors2016.csv") ))
cow_systems <- read.csv(paste0(here("Data/Raw/COW/StateSystemMembership", "system2016.csv") ))

library(tidyverse)

class(cow_states$styear)
library(lubridate)

cow_states$startDate <- as.Date( paste(cow_states$styear, cow_states$stmonth, cow_states$stday, sep = "-") )

cow_states$endDate <- as.Date( paste(cow_states$endyear, cow_states$endmonth, cow_states$endday, sep = "-") )

### make long panel

## https://rforpoliticalscience.com/2022/04/22/convert-event-level-data-to-panel-level-data-with-tidyr-in-r/
library(tidyverse)
library(magrittr)
library(lubridate)
library(tidyr)
library(rvest)
library(janitor)


names(cow_states)

cc <- cow_states %>% select( !contains("month") & !contains("day"))

names(cc)

cclong <- cc %>% 
  pivot_longer( c(startDate, endDate), names_to = "event", values_to = "date") 

# cclong <- cc %>% 
#   pivot_longer( c(styear, endyear), names_to = "event", values_to = "year") 

ccfill <- cclong %>%
  mutate(date = as.Date(as.character(date), format = "%Y")) %>% 
  group_by(ccode, statenme) %>%
  complete(stateabb , date = seq.Date(min(date), max(date), by = "year"))

names(cclong)

ccfill$version <-2016  

ccfill <- ccfill %>% select(1:4,7)

cccfill <- ccfill%>% mutate(year = lubridate::year(date) )

cccfill <- cccfill%>% select(year, everything())

cccfill <- cccfill%>% arrange(stateabb, year)  %>% select(-date)

## same for major powers data

### make long panel

## https://rforpoliticalscience.com/2022/04/22/convert-event-level-data-to-panel-level-data-with-tidyr-in-r/
library(tidyverse)
library(magrittr)
library(lubridate)
library(tidyr)
library(rvest)
library(janitor)


cow_majors$startDate <- as.Date( paste(cow_majors$styear, cow_majors$stmonth, cow_majors$stday, sep = "-") )

cow_majors$endDate <- as.Date( paste(cow_majors$endyear, cow_majors$endmonth, cow_majors$endday, sep = "-") )

### make long panel


names(cow_majors)

ccM <- cow_majors %>% select( !contains("month") & !contains("day"))

names(ccM)

cclongM <- ccM %>% 
  pivot_longer( c(startDate, endDate), names_to = "event", values_to = "date") 

# cclong <- cc %>% 
#   pivot_longer( c(styear, endyear), names_to = "event", values_to = "year") 

cclongM$majPow <- 1

ccfillM <- cclongM %>%
  mutate(date = as.Date(as.character(date), format = "%Y")) %>% 
  group_by(ccode, styear) %>%
  complete(majPow , date = seq.Date(min(date), max(date), by = "year"))

names(cclongM)

ccfillM$version <-2016  

ccfillM <- ccfillM %>% ungroup() %>% select(1,3,4,6)

cccfillM <- ccfillM %>% mutate(year = lubridate::year(date) )

cccfillM <- cccfillM %>% select(year, everything())

cccfillM <- cccfillM %>% arrange(ccode, year)  %>% select(-date)

## merge and replace NA in majPow with zeros (= not in ccfillM => not a major power in that countnryX year)
names(cccfill)
names(cccfillM)
mmf <- left_join(cccfill, cccfillM, by= c("ccode", "year") )

mmf <- mmf %>%  mutate_at(vars(majPow), ~replace_na(., 0))

mmf <- mmf %>% select(-endyear)

glimpse(mmf)


getwd()
###################################
## save
names(mmf)
cow_statesMajor16long <- mmf
save(cow_statesMajor16long, file = paste0(here("Data/Processed/", "COW_statesMajor16long.RData") ))
getwd()
write.csv(cow_statesMajor16long, file = paste0(here("Data/Processed/", "COW_statesMajor16long.csv" )), row.names = F)

length(unique(cow_statesMajor16long$ccode))

##################################################################
######## Merge with codelist_panel

library(tidyverse)

getwd()


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

sort(unique(ccp50$country.name.en))


##############################################
### FIND KEY

names(mmf)
library(countrycode)
guess_field(mmf$ccode)
guess_field(mmf$stateabb)
guess_field(mmf$statenme)

sort(unique(mmf$statenme))
# https://cran.r-project.org/web/packages/demcon/vignettes/ccode-considerations.html#:~:text=Germany%20(CoW%20255)%3A%20This,%2DWWII%20%E2%80%9Cunified%E2%80%9D%20Germany.&text=West%20Germany%20(CoW%20260)%3A,or%20the%20German%20Federal%20Republic.&text=East%20Germany%20(CoW%20265)%3A,or%20the%20German%20Democratic%20Republic.

mmf50 <-  mmf %>% filter(year > 1949)

countrycode(mmf50$ccode, origin = "cown", destination = "cown")
# mmg50$cown <-   countrycode({mmg50$ccode}, origin = "cown", destination = "cown")

# so mmg50$ccode is \cown\

# add \_COW\ suffix to vars
## Add suffix
colnames(mmf50) <- paste(colnames(mmf50), "COW", sep = "_")
colnames(mmf50)


mmCOW50 <- left_join(mmf50, ccpConBal50,  by = c( "ccode_COW" = "cown", "year_COW" = "year") )
names(mmCOW50)

## double check merge
mmCOW50 %>% select(year_COW,ccode_COW,  statenme_COW, country.name.en, contains("_COW") ) %>% View()


library(janitor)
library(haven)
write_dta(clean_names(mmCOW50), path = paste0(here("Data/Processed", "mmCOW50.dta") ))

getwd()
save(mmCOW50, file =paste0(here("Data/Processed/", "mmCOW50.RData") ))




##################################################################
##################################################################
##################################################################
# UT Inequality Project [UT Poverty Institution] (Thiel and Gini, 151 cos. 1963-2015)
## https://utip.gov.utexas.edu/
##################################################################

library(readxl)
ehii <- read_xlsx(paste0(here("Data/Raw/UTIP", "/UtipUnidoEhiiV2017_v.1.xlsx") ), sheet = 1)
## pivot to long format
library(tidyverse)
ginilong <- pivot_longer(ehii, cols = starts_with("y"),
                         names_to = "year",
                         names_prefix = "y",
                         values_to = "gini"
)

getwd()
ehii <- read_xlsx(paste0(here("Data/Raw/UTIP", "/UtipUnidoEhiiV2017_v.1.xlsx") ), sheet = 2)

theillong  <- pivot_longer(ehii, cols = starts_with("y"),
                           names_to = "year",
                           names_prefix = "y",
                           values_to = "theil"
)

theillong2 <- read_xlsx(paste0(here("Data/Raw/UTIP", "/utipunidov2017.xlsx") ), sheet = 1)

names(theillong)
ineqlong <- merge(theillong, ginilong, by = c("code", "year", "country", "countryname"))
ineqlong$year <- as.integer(ineqlong$year)


##################################################################
## Clean by removing NAs on important vars = ineqlong$theil  and ginilong$gini

ineqlong2 <- ineqlong %>% filter(!is.na(theil) )
ginilong2 <- ginilong %>% filter(!is.na(gini) )
ginilong2$year <- as.numeric(ginilong2$year)
ineqlong2$year <- as.numeric(ineqlong2$year)



## merge ineqlong into the 'master' codelist_panel
#### BUT: to do this we have to know the UTIP codes countries!
library(ISOcodes)
data("ISO_3166_1")

names(ineqlong2)

rrn <- ineqlong2$country
rrc <- ineqlong2$code
library(countrycode)
rrcn <- ineqlong2$countryname

guess_field(rrn)  
guess_field(ineqlong2$code) # iso3c promising
guess_field(ineqlong2$countryname)

countrycode(rrc, origin = 'iso3c', destination = 'country.name.en' )

guess_field(rrc)    # iso3c promising
guess_field(rrcn)
data("codelist_panel")

codelist_panel %>% select(country.name.en, year, iso3c, cown) %>% View()

iso3cRet_country.name.enCustom <- c( "CSK" = "Czechoslovakia", "SCG" = "Serbia and Montenegro", "DDR" = "German Democratic Republic", "YUG" = "Yugoslavia", "GER" = "Germany")

cownT <-  countrycode(rrc, origin = 'iso3c', destination = 'country.name.en', custom_match = iso3cRet_country.name.enCustom)


# iso3cRet_cowCustom <- c("BUR" = 775,"BYS" = 370, "CSK" = 315, "SCG" = 345, "DDR" = 265, "GER" = 255, "DHY" = 434, "FXX" = 220, "HVO" = 439, "RHO" = 552, "SUN" = 365, "VDR" = 816, "YMD" = 680, "YUG" = 345, "ZAR" = 490)
# 
# cownT <-  countrycode(rrc, origin = 'iso3c', destination = 'cown', custom_match = iso3cRet_cowCustom)
names(cownT)
length(cownT)

any(is.na(cownT))
rrc[which(is.na(cownT))] ## not needed in this instance

ineqlong2$country.name.en <- countrycode(rrc, origin = 'iso3c', destination = 'country.name.en', custom_match = iso3cRet_country.name.enCustom)



colnames(ineqlong2) <- paste(colnames(ineqlong2), "UTIP", sep = "_")




##################################################################
######## Merge with codelist_panel

library(tidyverse)

getwd()


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/","codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)
library(plm)
pp <- pdata.frame(ccpConBal50)
pdim(pp)

sort(unique(ccp50$country.name.en))



###############################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## ineqlong$code is almost \iso3c\ and \countryname\ is county.name.en

glimpse(ineqlong2)
glimpse(ccpConBal50)


names(ineqlong2)
mmineqlong50 <- left_join(ineqlong2, ccpConBal50, by = c("country.name.en_UTIP" ="country.name.en", "year_UTIP" = "year") )
names(mmineqlong50)

dim(ccpConBal50)
dim(ineqlong2)
dim(mmineqlong50)  

## below not needed in this instance, keep for future checking of merges?
# ## checking
# names(mmineqlong50)
# smm <- mmineqlong50 %>% select(!contains("UTIP"))
# aa <- anti_join(smm, ccpConBal50)
# dsmm <- distinct(smm)
# dim(dsmm)
# glimpse(smm)
# glimpse(ccpConBal50)
# 
# dim(mmineqlong50)
# mmineqlong50 <- distinct(mmineqlong50)
# 
# df <- smm
# which(duplicated(df) | duplicated(df, fromLast = TRUE) )
# # Yugoslavia is messing up in the merge, somehow.
# 
# 
# mmineqlong50[which( (duplicated(df) | duplicated(df, fromLast = TRUE) ) & (mmineqlong50$countryname_UTIP == "Serbia and Montenegro") ),] %>% View()

## a total hack.  Change Serbia and Montenegro to Yugoslavia only for 1994-2001

ineqlong3 <- ineqlong2

library(car)

recode(ineqlong3$country.name.en_UTIP, "'Serbia and Montenegro' = 'Yugoslavia'")

ineqlong3$country.name.en_UTIP <- recode(ineqlong3$country.name.en_UTIP, "'Serbia and Montenegro' = 'Yugoslavia'")

mmUTIP50 <- left_join(ineqlong3, ccpConBal50, by = c("country.name.en_UTIP" ="country.name.en", "year_UTIP" = "year") )
names(mmUTIP50)

save(mmUTIP50, file = paste0(here("Data/Processed","mmUTIP50.RData") ))

getwd()

library(haven)
write_dta(clean_names(mmUTIP50), path = paste0(here("Data/Processed","mmUTIP50.dta" ) ))




##################################################################
##################################################################
##################################################################
##################################################################
# SIPRI for military spending
################################################################## MARKER

rm(list=ls())

getwd()

library(here)
library(stringr)
library(tidyverse)
library(janitor)
library(xlsx)

sipri_2020USD <- read.xlsx(paste0(here("Data/Raw/SIPRI","SIPRI-Milex-data-1949-2021.xlsx")), sheetIndex = 5, startRow = 6) 

sipri_currentUSD <- read.xlsx(paste0(here("Data/Raw/SIPRI","SIPRI-Milex-data-1949-2021.xlsx")), sheetIndex = 6, startRow = 6) 

sipri_GDPshare <- read.xlsx(paste0(here("Data/Raw/SIPRI","SIPRI-Milex-data-1949-2021.xlsx")), sheetIndex = 7, startRow = 6) 

sipri_perCapita <- read.xlsx(paste0(here("Data/Raw/SIPRI","SIPRI-Milex-data-1949-2021.xlsx")), sheetIndex = 8, startRow = 7) 

sipri_spendingShare <- read.xlsx(paste0(here("Data/Raw/SIPRI","SIPRI-Milex-data-1949-2021.xlsx")), sheetIndex = 9, startRow = 8) 

## Clean
cleann <- function(df) {
  df <- df %>% select(-2)
  df[df=="xxx"] <- NA
  df[df=="..."] <- NA
  return(df)
}

### ... repeat

#Make list with all data frames
lst <- mget(ls(pattern="^sipri_"))

cc <-lapply(lst, cleann)


## pivot form wide to long

deparse(substitute(sipri_2020USD))
# 
piv <- function(df, vt){
      cat(vt)
      df <- pivot_longer(df, cols = starts_with("X"),
                             names_to = "year",
                             names_prefix = "X",
                             values_to = vt
  )
      return(df)

}

# piv(sipri_2020USD)

### https://stackoverflow.com/questions/65876600/getting-dataframe-name-inside-a-lapply-function-on-a-list-ggplot2
names(lapply(cc, names) )

names(cc) <- names(lapply(cc, names) )

library(purrr)

sipriLongL  <- imap(cc, ~piv(.x,.y) )
length(sipriLongL)

sipriLong <- sipriLongL %>% reduce(left_join,  c("Country","year") )
  
  

## merge SIPRI(s) into the 'master' codelist_panel
#### BUT: to do this we have to know the SIPRI codes countries!
## guess-field from countrycode
library(countrycode)
names(sipriLong)
guess_field(sipriLong$Country)

rrc <- sipriLong$Country

###############################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## sipriLong$Country is basically country.name.en (no region/ continent code, which is fine -- we don't care about these)
## get rid of non-country names in sipriLong

vcc <- unique(codelist_panel$country.name.en)
vcc

sipriLongCo <- sipriLong %>% filter(Country %in% vcc)
sipriLongCo$year <- as.numeric(sipriLongCo$year)

guess_field(sipriLongCo$Country)  #####!!!!!!!!!! Country = country.name.en

###############################
## 

glimpse(sipriLongCo)


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed","codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

colnames(sipriLongCo) <- paste(colnames(sipriLongCo), "SIPRI", sep = "_")


mmSIPRI50 <- left_join(sipriLongCo, ccpConBal50,   by = c( "Country_SIPRI" = "country.name.en"  , "year_SIPRI" = "year") )
names(mmSIPRI50)

dim(mmSIPRI50)
## Slight cleaning: remove sipri_currentUSD_SIPRI = NA
mmSIPRI50 <- mmSIPRI50 %>% filter(!is.na(sipri_currentUSD_SIPRI))
dim(mmSIPRI50)

getwd()
save(mmSIPRI50, file = paste0(here("Data/Processed", "mmSIPRI50.RData") ))



############################################################################################################################################################################################################
#################################################################
# Nordhaus et al. (2012) 'threat' data + freinds and foes spending
## (1950-2001)
##################################################################
rm(list=ls())
getwd()




## from original:
library(here)
library(xlsx)
library(countrycode)

nord <- read.xlsx(paste0(here("Data/Raw/",  "The Effects of the International Security Environment on National Military Expenditures/S0020818312000173sup001.xlsx" )), sheetIndex = 1)

### Changed 260 to 255 from 1990-2000
library(xlsx)
nord <- read.xlsx(paste0(here("Data/Processed/", "Nord3.xlsx")), sheetIndex = 1)


nnm <- names(nord)
nnm[5] <- "LMILEX1"
names(nord) <- nnm



guess_field(nord$STATE)

countrycode(nord$STATE, origin = "cown", destination = "cown")


# as cleaned by: Hauenstein, M., Smith, M., & Souva, M. (2021). Democracy, external threat, and military spending. Research and Politics, October-December, 13.



getwd()

## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


# add suffix to vars
## Add suffix
colnames(nord) <- paste(colnames(nord), "NORD", sep = "_")
colnames(nord)
names(nord)

library(tidyverse)

mmNORD50 <- left_join(nord, ccpConBal50, by = c("STATE_NORD" = "cown", "YEAR_NORD"="year") )
names(mmNORD50)
names(nord)
names(ccpConBal50)

library(janitor)
library(haven)
write_dta(clean_names(mmNORD50), path = paste0(here("Data/Processed/", "mmNORD50.dta") ))

getwd()
save(mmNORD50, file = paste0(here("Data/Processed/", "mmNORD50.RData") ))




############################################################################################################################################################################################################
#################################################################
# REPLICATION MATERIAL for Forcasting Military Expenditure    by Tobias Boehmelt & Vincenzo Bove
## Contains IVs used in creating PN6
##################################################################
rm(list=ls())
getwd()



library(readstata13)

forcasting <- read.dta13(paste0(here("Data/Raw/", "Forecasting Military Expenditure/Final Data.dta") ))
names(forcasting)


guess_field(forcasting$STATE) 

countrycode(forcasting$STATE, origin = "cown", destination = "country.name.en")

# so forcasting$STATE - 1990 - 260 should be 255

# TODO: fix STATE 
## Export to excel
library(xlsx)
write.xlsx(forcasting, "Data/Processed/forcasting.xlsx")
## Manually changed 1990 260 to 255
forcasting <- read.xlsx(paste0(here("Data/Processed/", "forcasting2.xlsx")), sheetIndex = 1)


guess_field(forcasting$STATE) 

countrycode(forcasting$STATE, origin = "cown", destination = "cown")


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


# add suffix to vars
## Add suffix
colnames(forcasting) <- paste(colnames(forcasting), "BB", sep = "_")
colnames(forcasting)
names(forcasting)

mmFORCASTING50 <- left_join(forcasting, ccpConBal50, by = c("STATE_BB" = "cown", "YEAR_BB"="year") )
names(mmFORCASTING50)

library(janitor)
library(haven)
write_dta(clean_names(mmFORCASTING50), path = paste0(here("Data/Processed/", "mmFORCASTING50.dta") ))

getwd()
save(mmFORCASTING50, file = paste0(here("Data/Raw/", "mmFORCASTING50.RData") ))



########################################################################################################################################################################################################
#################################################################
# Pat McDonald's 'alliances' (US/ Russia) data
##################################################################
rm(list=ls())
getwd()


library(readstata13)
rus <- read.dta13(paste0(here("Data/Raw/hierarchydata","Russiaally4_1 Aug22.dta") ))
names(rus)

usa <- read.dta13(paste0(here("Data/Raw/hierarchydata", "USally4_1 Aug22.dta") ))
names(usa)
usa$ccode <- usa$ccode2
library(tidyverse)
library(countrycode)

glimpse(usa)
guess_field(usa$ccode)
guess_field(rus$ccode)

countrycode(usa$ccode, origin = "cown", destination = "country.name.en")

# TODO: fix 260 
## Export to excel
library(xlsx)
write.xlsx(usa, "Data/Processed/usa.xlsx")
## Manually changed 260 in 1990 to 255
usa <- read.xlsx(paste0(here("Data/Processed/", "usa2.xlsx")), sheetIndex = 1)


guess_field(usa$ccode) 

countrycode(usa$ccode, origin = "cown", destination = "country.name.en")



sort(unique(usa$ccode)) %in% unique(codelist_panel$cown)
names(usa)
## so rus$ccode and usa$ccode are both cown

PJM <- full_join(usa, rus, by = c("year", "ccode"))
PJManti <- anti_join(usa, rus, by = c("year", "ccode"))
PJM3 <- anti_join(rus, usa, by = c("year", "ccode"))


library(plm)
pp <- pdata.frame(PJM, index = c('ccode', 'year'))
pdim(pp)

# add suffix to vars
## Add suffix
colnames(PJM) <- paste(colnames(PJM), "PJM", sep = "_")
colnames(PJM)
names(PJM)

## make ccp50 = standardized, balanced and consecutive
load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

mmPJM50 <- left_join(PJM, ccpConBal50, by = c("ccode_PJM" = "cown", "year_PJM"="year") )
names(mmPJM50)

mmPJM50 %>%
	filter(is.na(country.name.en)) %>%
	View()

mmPJM50 <- mmPJM50 %>%  mutate_at(vars(USally_PJM,Rusdefense_PJM), ~replace_na(., 0))

# mmPJM50 <- mmPJM50 %>% mutate(USally_PJM = replace_na(USally_PJM,0))
# 
# mmPJM50 <- mmPJM50 %>% mutate(Rusdefense_PJM = replace_na(Rusdefense_PJM,0))


library(janitor)
library(haven)
write_dta(clean_names(mmPJM50), path = paste0(here("Data/Processed/", "mmPJM50.dta") ))

getwd()
save(mmPJM50, file = paste0(here("Data/Processed/", "mmPJM50.RData") ))


##################################################################

##################################################################
##################################################################
## SWIID data -- see 'R_swiid.pdf'
##################################################################
##################################################################
rm(list=ls())
getwd()



## this is the raw MI SWIID data, with ccodes added based on Country Name

library(countrycode)
library(haven)
library(readstata13)
# Get GDP per capita data from the Penn World
# Tables, Version 10.0 (Feenstra et al. 2015)
getwd()



swiid_summary <- read.csv("https://raw.githubusercontent.com/fsolt/swiid/master/data/swiid_summary.csv")

names(swiid_summary)
guess_field(swiid_summary$country)

countrycode(swiid_summary$country, origin = "country.name.en", destination = "country.name.en")

### FIX Micronesia ??
### Micronesia is confusing me can I just remove it??

## is country X year a unique key in SWIID?
dim(swiid_summary)
dim(unique(swiid_summary[c("year", "country")]))  ##yes

library(plm)
pp <- pdata.frame(swiid_summary, index = c('country', 'year'))
pdim(pp)

swiid_summary <- swiid_summary %>%
  mutate(cown = countrycode(country,
                            origin = "country.name",
                            destination = "cown"))


# TODO: inspect these countries and fix. - all don't have cown 
## Anguilla - Sovereign state = UK
## Greenland - part of Denmark
## Hong Kong - China
## Micronesia - ???
## Palestinian Territories - occupied by Israel since 1967
## Puerto Rico - Unincorporated territory of the US
## Serbia - officially Republic of Serbia ?? - formally Yugoslavia
## Turks and Caicos Islands - British overseas territory = UK

colnames(swiid_summary) 
# add suffix to vars
## Add suffix
colnames(swiid_summary) <- paste(colnames(swiid_summary), "SWIID", sep = "_")
colnames(swiid_summary)
names(swiid_summary)

## make ccp50 = standardized, balanced and consecutive
load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

## TODO: fix the below

mmSWIIDcow <- left_join(ccpConBal50, swiid_summary, by = c("cown" = "cown_SWIID", "year"="year_SWIID") )
names(mmSWIIDcow)
distinct(mmSWIIDcow)

mmSWIID50 <- left_join(ccpConBal50, swiid_summary, by = c("country.name.en" = "country_SWIID", "year"="year_SWIID") )
names(mmSWIID50)
distinct(mmSWIID50)

anti_join(mmSWIID50, mmSWIIDcow) %>% dim()  # there are 4666 rows in mmSWIID *not* in swiidCow

anti_join(mmSWIIDcow, mmSWIID50) %>% dim()  # there are 12027 rows in mmSWIIDcow *not* in SWIID

aa_c <- anti_join(mmSWIID50, mmSWIIDcow)  # there are 4666 rows in mmSWIID *not* in swiidCow

aa <- anti_join(mmSWIIDcow, mmSWIID50)   # there are 12027 rows in mmSWIIDcow *not* in SWIID

stt <- as.data.frame(table(mmSWIIDcow$country_SWIID, mmSWIIDcow$cown))
##conclusion: using cown to merge doesn't work for SWIID.  using country names instead.




library(janitor)
library(haven)
write_dta(clean_names(mmSWIID50), path = paste0(here("Data/Processed/", "mmSWIID50.dta") ))

getwd()
save(mmSWIID50, file = paste0(here("Data/Processed/", "mmSWIID50.RData") ))





######################
load(url("https://github.com/fsolt/swiid/blob/master/data/swiid9_3.rda?raw=true") )
names(swiid_summary)

runs <- swiid_summary %>% group_by(country) %>% filter(min(year) <= 1970)
unique(runs$country)


swiid_summary %>% panelview(gini_disp ~ 1, 
                            index = c("country","year"), 
                            axis.lab="both", type = "miss")





# correlates of war naming, historic:  https://www.paulhensel.org/icownames.html
## note: COW doesn't have terretories (e.g. Anguilla, Cayman Islands, Hong Kong, Puerto Rico, Turks and Caicos)
## note:  Serbia, Serbia and Montenegro get the Yougoslavia ccode, 345  see  https://www.paulhensel.org/icownames.html

swiid1_lac[str_detect(swiid1_lac$country, "^Serb"),'cown'] <- 345 













