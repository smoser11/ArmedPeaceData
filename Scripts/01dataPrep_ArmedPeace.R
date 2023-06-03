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

mmPWT50 <- left_join(pwt10, ccpConBal50, by = c( "isocode_PWT" = "iso3c", "year_PWT"="year"), keep=TRUE )
names(mmPWT50)

gt <- mmPWT50 %>% filter(country_PWT == "Germany") %>% select(year_PWT, country_PWT,  country.name.en, cown, cowc, isocode_PWT)


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
rm(list =ls())
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
mmf <- left_join(cccfill, cccfillM, by= c("ccode", "year"), keep=TRUE )

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

## Manually changed mmf 1990 265 German Democratic Republic to 255 Germany
library(xlsx)
write.csv(mmf, "Data/Processed/mmf.csv")
library(readxl)
mmf <- read_xlsx(paste0(here("Data/Processed", "mmf2.xlsx") ), sheet = 1)




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


mmCOW50 <- left_join(mmf50, ccpConBal50,  by = c( "ccode_COW" = "cown", "year_COW" = "year"),keep=TRUE )
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
rm(list =ls())
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

### Manually changed german federal republic to germany for 1990
write.xlsx(ineqlong2, "Data/Processed/ineqlong2.xlsx")

ineqlong2 <- read.xlsx(paste0(here("Data/Processed/", "ineqlong2.xlsx")), sheetIndex = 1)



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
mmineqlong50 <- left_join(ineqlong2, ccpConBal50, by = c("country.name.en_UTIP" ="country.name.en", "year_UTIP" = "year"), keep=TRUE )
names(mmineqlong50)

gt <- mmineqlong50 %>% filter(country.name.en_UTIP == "Germany") %>% select(year_UTIP, country_UTIP, countryname_UTIP, country.name.en_UTIP, cown, cowc)


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

mmUTIP50 <- left_join(ineqlong3, ccpConBal50, by = c("country.name.en_UTIP" ="country.name.en", "year_UTIP" = "year"), keep=TRUE )
names(mmUTIP50)

mmUTIP50 <- mmUTIP50 %>% select(!"NA.")
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


mmSIPRI50 <- left_join(sipriLongCo, ccpConBal50,   by = c( "Country_SIPRI" = "country.name.en"  , "year_SIPRI" = "year"), keep=TRUE )
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

mmNORD50 <- left_join(nord, ccpConBal50, by = c("STATE_NORD" = "cown", "YEAR_NORD"="year"), keep=TRUE )
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

# TODO: fix STATE #(SM)I think we did this?
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

mmFORCASTING50 <- left_join(forcasting, ccpConBal50, by = c("STATE_BB" = "cown", "YEAR_BB"="year"), keep=TRUE )
names(mmFORCASTING50)

library(janitor)
library(haven)
write_dta(clean_names(mmFORCASTING50), path = paste0(here("Data/Processed/", "mmFORCASTING50.dta") ))

getwd()
save(mmFORCASTING50, file = paste0(here("Data/Processed/", "mmFORCASTING50.RData") ))



########################################################################################################################################################################################################
#################################################################
# Pat McDonald's 'alliances' (US/ Russia) data
##################################################################
rm(list=ls())
getwd()

library(countrycode)
library(here)
library(tidyverse)
library(readstata13)
rus <- read.dta13(paste0(here("Data/Raw/hierarchydata","Russiaally4_1 Aug22.dta") ))
names(rus)

usa <- read.dta13(paste0(here("Data/Raw/hierarchydata", "USally4_1 Aug22.dta") ))
names(usa)
usa$ccode <- usa$ccode2

unique(rus$Rusdefense)
unique(usa$USally)

glimpse(usa)
guess_field(usa$ccode)
guess_field(rus$ccode)

countrycode(usa$ccode, origin = "cown", destination = "country.name.en")

# fix 260
## Export to excel
library(xlsx)
write.xlsx(usa, "Data/Processed/usa.xlsx")
write.xlsx(rus, "Data/Processed/rus.xlsx")

## Manually changed 260 in 1990 to 255  and removed Germany for 1954, as it has no cown code)
library(xlsx)
usa <- read.xlsx(paste0(here("Data/Processed/", "usa2.xlsx")), sheetIndex = 1)
usa <- usa %>% arrange(ccode, year)

## Manually add USA is an ally of itself
usa <- read.xlsx(paste0(here("Data/Processed/", "usa3.xlsx")), sheetIndex = 1)
usa <- usa %>% arrange(ccode, year)

## Manually add RUS is an ally of itself
rus <- read.xlsx(paste0(here("Data/Processed/", "rus2.xlsx")), sheetIndex = 1)
rus <- rus %>% arrange(ccode, year)

guess_field(usa$ccode)
countrycode(usa$ccode, origin = "cown", destination = "country.name.en")
countrycode(usa$ccode, origin = "cown", destination = "cown")
countrycode(usa$ccode, origin = "cown", destination = c("cown", "country.name.en") )



sort(unique(usa$ccode)) %in% unique(codelist_panel$cown)
names(usa)
## so rus$ccode and usa$ccode are both cown

PJM <- full_join(usa, rus, by = c("year", "ccode"))

unique(PJM$USally)
unique(PJM$Rusdefense)
names(PJM)
PJM <- PJM %>% select(!"NA..x")
PJM <- PJM %>% select(!"NA..y")

plot(PJM$ccode, PJM$ccode2)
PJM <- PJM %>% select(!"ccode2")

### 
# find duplicates?
PJM <- PJM %>% arrange(ccode, year)
names(PJM)
ppp <- PJM %>% select(ccode, year)
which(duplicated(ppp))

which(duplicated(usa))  # so the duplication comes after the full_merge b/t `usa` and `rus` somehow...

# SUPER HACK: remove 2171 from PJM
PJM[2170:2171,]
PJM <- PJM[-2171,]



## Zero out the NAs _after_ the join -- done in 03masterMerge file TODO!
any(is.na(PJM))

# add suffix to vars
## Add suffix
colnames(PJM) <- paste(colnames(PJM), "PJM", sep = "_")
colnames(PJM)
names(PJM)

## make ccp50 = standardized, balanced and consecutive
load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

mmPJM50 <- left_join(PJM, ccpConBal50, by = c("ccode_PJM" = "cown", "year_PJM"="year"), keep=TRUE )
names(mmPJM50)

mmPJM50 <- mmPJM50 %>% filter(year>= 1950)

mmPJM50$USally_PJM[is.na(mmPJM50$USally_PJM)] <- 0
mmPJM50$Rusdefense_PJM[is.na(mmPJM50$Rusdefense_PJM)] <- 0

names(PJM)
guess_field(PJM$ccode)

PJManti <- anti_join(usa, rus, by = c("year", "ccode"))


names(PJM)

library(plm)
pp <- pdata.frame(PJM, index = c('ccode', 'year'))
table(index(pp), useNA = "ifany")

any(is.na(mmPJM50))
mmPJM50 %>%
	filter(is.na(country.name.en)) %>%
	View()


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
countrycode(swiid_summary$country_SWIID, origin = "country.name.en", destination = "country.name.en")

sort(unique(swiid_summary$country_SWIID))

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
library(tidyverse)

# TODO: inspect these countries and fix. 

##### Changes made in excel SWIID:
## Remove micronesia manually
## Changed Serbia to Yugoslavia for the years 1997-2005
## Dropped Anguilla, Greenland, Puerto Rico, Turks & Caicos because there is no data for it in codelist panel
## Changed Hong Kong to Hong Kong SAR China
## Added isocode3c in SWIID for Palestinian Territories?
swiid_summary <- read.csv(paste0(here("Data/Processed/", "swiid_summary2.csv")))

countrycode(swiid_summary$country, origin = "country.name.en", destination = "country.name.en")


colnames(swiid_summary)
# add suffix to vars
## Add suffix
colnames(swiid_summary) <- paste(colnames(swiid_summary), "SWIID", sep = "_")
colnames(swiid_summary)
names(swiid_summary)

## make ccp50 = standardized, balanced and consecutive
library(here)
load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


## join by country.name.en

mmSWIIDcc <- left_join(swiid_summary, ccpConBal50, by = c("country_SWIID" = "country.name.en", "year_SWIID" = "year"), keep=TRUE )
names(mmSWIIDcc)
distinct(mmSWIIDcc)

#### check for faults 
mmSWIIDanti <- anti_join(swiid_summary, ccpConBal50, by = c("country_SWIID" = "country.name.en", "year_SWIID" = "year") )

mmSWIID50 <- left_join(swiid_summary, ccpConBal50, by = c("country_SWIID" = "country.name.en", "year_SWIID"="year"), keep=TRUE )
names(mmSWIID50)
distinct(mmSWIID50)

## Join with ISO3c for Palestinian Territories
mmSWIIDiso <- left_join(swiid_summary, ccpConBal50, by = c("iso3c_SWIID" = "iso3c", "year_SWIID" = "year"), keep=TRUE)
names(mmSWIIDiso)
distinct(mmSWIIDiso)



library(janitor)
library(haven)
write_dta(clean_names(mmSWIID50), path = paste0(here("Data/Processed/", "mmSWIID50.dta") ))

getwd()
save(mmSWIID50, file = paste0(here("Data/Processed/", "mmSWIID50.RData") ))





library(panelView)
swiid_summary %>% panelview(gini_disp ~ 1,
                            index = c("country","year"),
                            axis.lab="both", type = "miss")





# correlates of war naming, historic:  https://www.paulhensel.org/icownames.html
## note: COW doesn't have territories (e.g. Anguilla, Cayman Islands, Hong Kong, Puerto Rico, Turks and Caicos)
## note:  Serbia, Serbia and Montenegro get the Yougoslavia ccode, 345  see  https://www.paulhensel.org/icownames.html



########## Democracy External Threat and Military Spending (Hauenstein et al. 2021) https://journals-sagepub-com.nottingham.idm.oclc.org/doi/pdf/10.1177/20531680211049660 ######## "dp_Nord_clean.dta" ###### (HSS) ###
rm(list=ls())
getwd()
library(here)
library(readstata13)

HSS <- read.dta13(paste0(here("Data/Raw/Democracy external threat and military spending/", "DP_Nord_clean.dta") ))

### Problem with guess_field
guess_field(HSS$ccode)

countrycode(HSS$ccode, origin = "cown", destination = "country.name.en")
## 260 problem

## Export to excel
library(xlsx)
write.xlsx(HSS, "Data/Processed/HSS.xlsx")

## Manually change 260 to 255 for 1990-2000
HSS <- read.xlsx(paste0(here("Data/Processed/", "HSS2.xlsx")), sheetIndex = 1)


guess_field(HSS$ccode)

countrycode(HSS$ccode, origin = "cown", destination = "country.name.en")


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


# add suffix to vars
## Add suffix
colnames(HSS) <- paste(colnames(HSS), "HSS", sep = "_")
colnames(HSS)
names(HSS)

mmHSS50 <- left_join(HSS, ccpConBal50, by = c("ccode_HSS" = "cown", "year_HSS" = "year"), keep=TRUE )
names(mmHSS50)

mmHSS50 <- mmHSS50 %>% select(!"NA._HSS"  )
	
library(janitor)
library(haven)
write_dta(clean_names(mmHSS50), path = paste0(here("Data/Processed/", "mmHSS50.dta") ))

getwd()
save(mmHSS50, file = paste0(here("Data/Processed/", "mmHSS50.RData") ))




##### JPR - What Goes UP - Replication (Zielinski et al. 2017) https://www-jstor-org.nottingham.idm.oclc.org/stable/48590474 ###### "replication.dta" ###### (ZFS)
rm(list=ls())
getwd()
library(here)
library(readstata13)

ZFS <- read.dta13(paste0(here("Data/Raw/JPR - What Goes Up - Replication/", "replication.dta") ))


guess_field(ZFS$STATE)

countrycode(ZFS$STATE, origin = "cown", destination = "country.name.en")

# so ZFS 260, 340, 396, 711, 971, 972, 973 were matched unambiguously

# fix 260, 340, 296, 397, 711, 971, 972, 973
## Export to excel
library(xlsx)
write.xlsx(ZFS, "Data/Processed/ZFS.xlsx")

## Manually changed 260 to 255 for 1990 to 2015, dropped 340, 396, 397, 711, 971, 972, 973
gc()
ZFS <- read.xlsx(paste0(here("Data/Processed/", "ZFS2.xlsx")), sheetIndex = 1)


guess_field(ZFS$STATE)

countrycode(ZFS$STATE, origin = "cown", destination = "cown")


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))
library(tidyverse)
ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


# add suffix to vars
## Add suffix
colnames(ZFS) <- paste(colnames(ZFS), "ZFS", sep = "_")
colnames(ZFS)
names(ZFS)

mmZFS50 <- left_join(ZFS, ccpConBal50, by = c("STATE_ZFS" = "cown", "YEAR_ZFS" = "year"), keep=TRUE )
names(mmZFS50)

mmZFS50 <- mmZFS50 %>% select(!"NA._ZFS"  )

library(janitor)
library(haven)
write_dta(clean_names(mmZFS50), path = paste0(here("Data/Processed/", "mmZFS50.dta") ))

getwd()
save(mmZFS50, file = paste0(here("Data/Processed/", "mmZFS50.RData") ))




######## Impacts of neighbouring countries on military expenditures (M Ensar Yesilyurt & Paul Elhorst. 2017) https://journals-sagepub-com.nottingham.idm.oclc.org/doi/pdf/10.1177/0022343317707569 ##### "datasetcow2015.xlsx" and "datasetwb2015.xlsx" #### ("YE_COW" and "YE_WB" )
rm(list=ls())
getwd()

##### 1. "datasetcow2015.xlsx"
library(xlsx)
library(here)
library(countrycode)

YE_COW <- read.xlsx(paste0(here("Data/Raw/M Ensar Yesilyurt & J Paul Elhorst/", "datasetcow2015.xlsx")), sheetIndex = 1)
	
guess_field(YE_COW$X.)

countrycode(YE_COW$X., origin = "country.name.en", destination = "country.name.en")



#### 2. "datasetwb2015.xlsx"
library(xlsx)

YE_WB <- read.xlsx(paste0(here("Data/Raw/M Ensar Yesilyurt & J Paul Elhorst/", "datasetwb2015.xlsx")), sheetIndex = 1)

guess_field(YE_WB$Country.Name)

countrycode(YE_WB$Country.Name, origin = "country.name.en", destination = "country.name.en")


## make ccp50 = standardized, balanced and consecutive

load(file =paste0(here("Data/Processed/", "codelist_panel2.RData") ))

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

library(janitor)
# add suffix to vars (1)
## Add suffix
colnames(YE_COW) <- paste(make_clean_names(colnames(YE_COW)), "YE_COW", sep = "_")
colnames(YE_COW)
names(YE_COW)

mmYE_COW50 <- left_join(YE_COW, ccpConBal50, by = c("x_YE_COW" = "country.name.en", "year_YE_COW" = "year"), keep=TRUE )
names(mmYE_COW50)

library(janitor)
library(haven)
write_dta(mmYE_COW50, path = paste0(here("Data/Processed/", "mmYE_COW50.dta") ))

getwd()
save(mmYE_COW50, file = paste0(here("Data/Processed/", "mmYE_COW50.RData") ))


# add suffix to vars (2)
## Add suffix
colnames(YE_WB) <- paste(make_clean_names(colnames(YE_WB)), "YE_WB", sep = "_")
colnames(YE_WB)
names(YE_WB)

mmYE_WB50 <- left_join(YE_WB, ccpConBal50, by = c("country_name_YE_WB" = "country.name.en", "year_YE_WB" = "year"), keep=TRUE )
names(mmYE_WB50)

library(janitor)
library(haven)
write_dta(mmYE_WB50, path = paste0(here("Data/Processed/", "mmYE_WB50.dta") ))

getwd()
save(mmYE_WB50, file = paste0(here("Data/Processed/", "mmYE_WB50.RData") ))



