rm(list =ls())


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

##################################################################
##################################################################
##################################################################
##################################################################
# COW State Membership data:
## https://correlatesofwar.org/data-sets/state-system-membership/
##################################################################

getwd()

cow_states <- read.csv("./data_ArmedPeace/COW/StateSystemMembership/states2016.csv")
cow_majors <- read.csv("./data_ArmedPeace/COW/StateSystemMembership/majors2016.csv")
cow_systems <- read.csv("./data_ArmedPeace/COW/StateSystemMembership/system2016.csv")

library(tidyverse)

class(cow_states$styear)
library(lubridate)

  
library(tidyverse)
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



###################################
## save

cow_statesMajor16long <- mmf
save(cow_statesMajor16long, file = "COW_statesMajor16long.RData")
getwd()
write.csv(cow_statesMajor16long, file = "COW_statesMajor16long.csv", row.names = F)

length(unique(cow_statesMajor16long$ccode))

##################################################################
######## Merge with codelist_panel

library(tidyverse)

getwd()

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

sort(unique(ccp50$country.name.en))


##############################################
### FIND KEY

names(mmf)
library(countrycode)
guess_field(mmf$ccode)
guess_field(mmf$stateabb)

sort(unique(mmf$statenme))
# https://cran.r-project.org/web/packages/demcon/vignettes/ccode-considerations.html#:~:text=Germany%20(CoW%20255)%3A%20This,%2DWWII%20%E2%80%9Cunified%E2%80%9D%20Germany.&text=West%20Germany%20(CoW%20260)%3A,or%20the%20German%20Federal%20Republic.&text=East%20Germany%20(CoW%20265)%3A,or%20the%20German%20Democratic%20Republic.

mmf50 <-  mmf %>% filter(year > 1949)
countrycode({mmf50$ccode}, origin = "cown", destination = "cown")
# mmg50$cown <-   countrycode({mmg50$ccode}, origin = "cown", destination = "cown")

# so mmg50$ccode is \cown\

# add \_COW\ suffix to vars
## Add suffix
colnames(mmf50) <- paste(colnames(mmf50), "COW", sep = "_")
colnames(mmf50)


mmCOW50 <- right_join(ccpConBal50, mmf50, by = c("cown" = "ccode_COW", "year"="year_COW") )
names(mmCOW50)

## double check merge
mmCOW50 %>% select(year,cown,  statenme_COW, country.name.en, contains("_COW") ) %>% View()


library(janitor)
library(haven)
write_dta(clean_names(mmCOW50), path = "mmCOW50.dta")

getwd()
save(mmCOW50, file = "mmCOW50.RData")


#################################################################
##################################################################
##################################################################
##################################################################
# UT Inequality Project [UT Poverty Institution] (Thiel and Gini, 151 cos. 1963-2015)
## https://utip.gov.utexas.edu/
##################################################################


# library(gdata)
# ehii <- read_xlsx("./data_ArmedPeace/UtipUnidoEhiiV2017_v.1.xlsx")

getwd()
library(readxl)
ehii <- read_xlsx("./UtipUnidoEhiiV2017_v.1.xlsx", sheet = 1)
## pivot to long format
library(tidyverse)
ginilong <- pivot_longer(ehii, cols = starts_with("y"),
                       names_to = "year",
                       names_prefix = "y",
                       values_to = "gini"
                       )

getwd()
ehii <- read_xlsx("./UtipUnidoEhiiV2017_v.1.xlsx", sheet = 2)

theillong  <- pivot_longer(ehii, cols = starts_with("y"),
                           names_to = "year",
                           names_prefix = "y",
                           values_to = "theil"
)

theillong2 <- read_xlsx("./utipunidov2017.xlsx", sheet = 1)
names(theillong)
ineqlong <- merge(theillong, ginilong, by = c("code", "year", "country", "countryname"))
ineqlong$year <- as.integer(ineqlong$year)
names(ineqlong)

colnames(ineqlong) <- c( names(ineqlong)[1:4],"theil_UTIP", "gini_UTIP")


##################################################################


library(stringr)


## merge ineqlong into the 'master' codelist_panel
#### BUT: to do this we have to know the UTIP codes countries!
library(ISOcodes)
data("ISO_3166_1")

names(ineqlong)

rrn <- ineqlong$country
rrc <- ineqlong$code
rrcn <- ineqlong$countryname

guess_field(rrn)  
guess_field(ineqlong$code) # iso3c promising
guess_field(ineqlong$countryname)

countrycode(rrc, origin = 'iso3c', destination = 'country.name.en' )

guess_field(rrc)    # iso3c promising
guess_field(rrcn)
data("codelist_panel")

codelist_panel %>% select(country.name.en, year, iso3c, cown) %>% View()

iso3cRet_cowCustom <- c("BUR" = 775,"BYS" = 370, "CSK" = 315, "SCG" = 345, "DDR" = 265, "DHY" = 434, "FXX" = 220, "HVO" = 439, "RHO" = 552, "SUN" = 365, "VDR" = 816, "YMD" = 680,
                        "YUG" = 345,
                        "ZAR" = 490)

cownT <-  countrycode(rrc, origin = 'iso3c', destination = 'cown', custom_match = iso3cRet_cowCustom)
# so \code\ is iso3c + iso3c_ret = called `iso3c_offRet` (official plus retired)

countrycode(rrc, origin = 'ioc', destination = 'country.name.en' )
countrycode(rrc, origin = 'iso3c', destination = 'country.name.en' )
## idea: replace non iso3c codes in rrc with Pat's crosswalk.
## e.g. CSK -> 315

countrycode(rrn, origin = 'iso3n', destination = 'cown' )

countrycode(rrcn, origin = 'country.name.en', destination = 'country.name.en' )

badco <- sort(unique(rrcn) ) %in% sort(unique(codelist_panel$country.name.en) )
sort(unique(rrcn) )[!badco]

sort(unique(rrcn) )


iso3cRet_namesCustom <- c("DDR" = "German Democratic Republic", "DEU" = "Germany",    "GER" = "Germany", 
                          "CSK" = "Czechoslovakia", "SCG" = "Yugoslavia")

iso3cRet_cowCustom
# 
# ", "DHY" = 434, "FXX" = "France", "HVO" = 439, "RHO" = 552, "SUN" = 365, "VDR" = 816, "YMD" = 680,
#                         "YUG" = 345,
#                         "ZAR" = 490)
sort(unique(rrc) )

countrycode("DEU", origin = 'iso3c', destination = 'country.name.en' )

countrycode(rrc, origin = 'ioc', destination = 'country.name.en' )

countrycode(rrc, origin = 'iso3c', destination = 'cown', custom_match = iso3cRet_cowCustom)

iso3cRet_cowCustom

countrycode(rrc, origin = 'iso3c', destination = 'cown')
            

utip3cown <- c( "CSK" = "315", "DDR" = "265" , "DEU" = "260" , "GER" = "255" , "SCG" = "345" , "YUG" = "345" )
## try 0 map 3character code to cown
ineqlong$cown <- as.numeric( countrycode(ineqlong$code, origin = 'iso3c', destination = 'cown', custom_match = utip3cown ) )

ineqlong$c.n.en0 <- countrycode(ineqlong$cown, origin = 'cown', destination = 'country.name.en')


write.csv(sort(unique(codelist_panel$country.name.en))
, file = "country_name_en.csv")

##################################################################
######## Merge with codelist_panel

library(tidyverse)

getwd()

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

sort(unique(ccp50$country.name.en))



###############################!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## ineqlong$code is almost \iso3c\ and \countryname\ is county.name.en

glimpse(ineqlong)
glimpse(ccpConBal50)
## Add suffix
colnames(ineqlong) <- paste(colnames(ineqlong), "UTIP", sep = "_")
colnames(ineqlong)

mmineqlong50 <- left_join(ccpConBal50, ineqlong,  by = c("cown" ="cown_UTIP", "year" = "year_UTIP") )
names(mmineqlong50)

save(mmineqlong50, file = "mmUTIP_50.Rdata")

getwd()

library(haven)
library(janitor)
write_dta(clean_names(mmineqlong50), path = "mmUTIP_50.dta" )

##################################################################
##################################################################
##################################################################
##################################################################
# Penn world tables (general) v10 https://www.rug.nl/ggdc/productivity/pwt/
##  relative levels of income, output, input, and productivity for 183 countries between 1950 and 2019.
## https://cran.r-project.org/web/packages/pwt10/pwt10.pdf
##################################################################

rm(list=ls())
###### countrycode and codelist_panel MARKER
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

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

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

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)



# add suffix to vars
## Add suffix
colnames(pwt10) <- paste(colnames(pwt10), "PWT", sep = "_")
colnames(pwt10)
names(pwt10)

mmPWT50 <- right_join(ccpConBal50, pwt10, by = c("iso3c" = "isocode_PWT", "year"="year_PWT") )
names(mmPWT50)


library(janitor)
library(haven)
write_dta(clean_names(mmPWT50), path = "mmPWT50.dta")

getwd()
save(mmPWT50, file = "mmPWT50.RData")




##################################################################
##################################################################
##################################################################
##################################################################
# SIPRI for military spending
################################################################## MARKER

rm(list=ls())

getwd()

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

library(stringr)
library(tidyverse)
library(janitor)
library(xlsx)

sipri_2020USD <- read.xlsx("./SIPRI-Milex-data-1949-2021.xlsx", sheetIndex = 5, startRow = 6)

sipri_currentUSD <- read.xlsx("./SIPRI-Milex-data-1949-2021.xlsx", sheetIndex = 6, startRow = 6)

sipri_GDPshare <- read.xlsx("./SIPRI-Milex-data-1949-2021.xlsx", sheetIndex = 7, startRow = 6)

sipri_perCapita <- read.xlsx("./SIPRI-Milex-data-1949-2021.xlsx", sheetIndex = 8, startRow = 7)

sipri_spendingShare <- read.xlsx("./SIPRI-Milex-data-1949-2021.xlsx", sheetIndex = 9, startRow = 8)

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

piv <- function(df, vt){
      cat(vt)
      df <- pivot_longer(df, cols = starts_with("X"),
                             names_to = "year",
                             names_prefix = "X",
                             values_to = vt
  )
      return(df)
  
}

piv(sipri_2020USD)

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

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

colnames(sipriLongCo) <- paste(colnames(sipriLongCo), "SIPRI", sep = "_")


mmSIPRI50 <- left_join(ccpConBal50, sipriLongCo,  by = c("country.name.en" ="Country_SIPRI" , "year" = "year_SIPRI") )
names(mmSIPRI50)


getwd()
save(mmSIPRI50, file = "mmSIPRI50.Rdata")



############################################################################################################################################################################################################
#################################################################
# Nordhaus et al. (2012) 'threat' data + freinds and foes spending
## (1950-2001)
##################################################################
rm(list=ls())
getwd()

setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## from origional:

library(xlsx)

nord <- read.xlsx("../The Effects of the International Security Environment on National Military Expenditures/S0020818312000173sup001.xlsx", sheetIndex = 1)

nnm <- names(nord)
nnm[5] <- "LMILEX1"
names(nord) <- nnm



guess_field(nord$STATE)

countrycode(nord$STATE, origin = "cown", destination = "country.name.en")

# so nord$STATE is exactly cown




# as cleaned by ____ Democracy external threat...

library(readstata13)
dp <- read.dta13("../Democracy external threat and military spending/DP_Nord_clean.dta")
names(dp)
guess_field(dp$ccode)

guess_field(dp$stateabbA)

guess_field(dp$stateabb)

countrycode(dp$ccode, origin = 'cown', destination = 'country.name.en')

## so dp$ccode is exactly cown


setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


# add suffix to vars
## Add suffix
colnames(nord) <- paste(colnames(nord), "NORD", sep = "_")
colnames(nord)
names(nord)

mmNORD50 <- left_join(ccpConBal50, nord, by = c("cown" = "STATE_NORD", "year"="YEAR_NORD") )
names(mmNORD50)

library(janitor)
library(haven)
write_dta(clean_names(mmNORD50), path = "mmNORD50.dta")

getwd()
save(mmNORD50, file = "mmNORD50.RData")




############################################################################################################################################################################################################
#################################################################
# REPLICATION MATERIAL for Forcasting Military Expenditure    by Tobias Boehmelt & Vincenzo Bove
## Contains IVs used in creating PN6
##################################################################
rm(list=ls())
getwd()

setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")


library(readstata13)

forcasting <- read.dta13("../Forecasting Military Expenditure/Final Data.dta")
names(forcasting)


guess_field(forcasting$STATE)

countrycode(forcasting$STATE, origin = "cown", destination = "country.name.en")

# so forcasting$STATE is exactly cown


setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)


# add suffix to vars
## Add suffix
colnames(forcasting) <- paste(colnames(forcasting), "BB", sep = "_")
colnames(forcasting)
names(forcasting)

mmFORCASTING50 <- left_join(ccpConBal50, forcasting, by = c("cown" = "STATE_BB", "year"="YEAR_BB") )
names(mmFORCASTING50)

library(janitor)
library(haven)
write_dta(clean_names(mmFORCASTING50), path = "mmFORCASTING50.dta")

getwd()
save(mmFORCASTING50, file = "mmFORCASTING50.RData")



########################################################################################################################################################################################################
#################################################################
# Pat McDonald's 'alliances' (US/ Russia) data
##################################################################

getwd()

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

library(readstata13)
rus <- read.dta13("./hierarchydata/Russiaally4_1 Aug22.dta")
names(rus)

usa <- read.dta13("./hierarchydata/USally4_1 Aug22.dta")
names(usa)
usa$ccode <- usa$ccode2
library(tidyverse)
library(countrycode)

glimpse(usa)
guess_field(usa$ccode)
guess_field(rus$ccode)

countrycode(usa$ccode, origin = "cown", destination = "country.name.en")

sort(unique(usa$ccode)) %in% unique(codelist_panel$cown)
names(usa)
## so rus$ccode and usa$ccode are both cown

PJM <- full_join(usa, rus, by = c("year", "ccode"))
library(plm)
pp <- pdata.frame(PJM, index = c('ccode', 'year'))
pdim(pp)

# add suffix to vars
## Add suffix
colnames(PJM) <- paste(colnames(PJM), "PJM", sep = "_")
colnames(PJM)
names(PJM)

## make ccp50 = standardized, balanced and consecutive
load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

mmPJM50 <- left_join(ccpConBal50, PJM, by = c("cown" = "ccode_PJM", "year"="year_PJM") )
names(mmPJM50)

mmPJM50 <- mmPJM50 %>%  mutate_at(vars(USally_PJM,Rusdefense_PJM), ~replace_na(., 0))

# mmPJM50 <- mmPJM50 %>% mutate(USally_PJM = replace_na(USally_PJM,0))
# 
# mmPJM50 <- mmPJM50 %>% mutate(Rusdefense_PJM = replace_na(Rusdefense_PJM,0))


library(janitor)
library(haven)
write_dta(clean_names(mmPJM50), path = "mmPJM50.dta")

getwd()
save(mmPJM50, file = "mmPJM50.RData")


##################################################################




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

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")

## make ccp50 = standardized, balanced and consecutive

load(file ="codelist_panel2.RData")

ccp50 <- codelist_panel2 %>% filter(year>= 1950)
ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)

library(stringr)

str_starts(dir(), "mm")
ds <- dir()[str_detect(dir(), "^mm.*RData$")]
ds
tokens <- str_replace_all(ds, '.RData', '')

i <- 1
dat <- list()
for (i in seq_along(ds)) {  # 
  load(ds[i])
  tokens[i]
  eval(parse(text=paste0("dat[[tokens[i] ]] <- ", tokens[i])))
}

str(dat)
getwd()
save(dat, file = "datMerge.RData")

#pan <- dat

cou <- sapply(dat, function(x) x$country.name.en.regex) %>% unlist %>% unique
cou
yea <- sapply(dat, function(x) x$year) %>% unlist %>% unique
yea
yea <- min(yea):max(yea)
yea

rec <- expand_grid(country.name.en.regex = cou,
                   year = yea)
View(rec)
names(ccpConBal50)
#pan <- c(list(rec), dat) %>%
  
mmALL50 <- dat %>%  purrr::reduce(left_join, by = c(names(ccpConBal50)))

getwd()
library(haven)
library(janitor)
write_dta(clean_names(mmALL50), path = "mmALL50.dta")

save(mmALL50, file = "mmALL50.RData")



















# 


# 


# 

# 




rr<- read.dta13("ArPeacemasterJul2017ave-noGaps.dta")    
names(rr)
rrr <- read.dta13("ArPeacemasterDec2016ave.dta")

names(rrr)
glimpse(rrr)
library(zoo)
library(lubridate)
rrr$yyear <- ymd(rrr$year)
rrr$yyear <- year(rrr$yyear)
library(panelView)
rrr %>% panelview(gini_market ~ 1, 
                  index = c("ccode","yyear"), 
                  axis.lab="both", type = "miss", cex.axis = 8)

names(rr)
glimpse(rr)
library(zoo)
library(lubridate)
rr$yyear <- ymd(rr$year)
rr$yyear <- year(rr$yyear)
# rr$ccode <- as.factor(rr$ccode)

glimpse(rr)
library(panelView)
rr %>% panelview(gini_net ~ 1, 
                 index = c("ccode","yyear"), 
                 axis.lab="both", type = "miss", cex.axis = 8)


## https://github.com/fsolt/swiid/blob/master/data/swiid9_3.rda


##################################################################
##################################################################
## SWIID data -- see 'R_swiid.pdf'
##################################################################
##################################################################

## this is the raw MI SWIID data, with ccodes added based on Country Name



library(countrycode)
library(haven)
library(readstata13)
# Get GDP per capita data from the Penn World
# Tables, Version 10.0 (Feenstra et al. 2015)
getwd()

# install.packages("RCurl")
library(RCurl)
URL <- "https://www.rug.nl/ggdc/docs/pwt100.dta"
x <- getURL(URL)

download.file("https://www.rug.nl/ggdc/docs/pwt100.dta","pwt100.dta", method = 'libcurl')
pwt100 <- read_dta("pwt100.dta")
pwt100 <- pwt100 %>% select(-currency_unit)
pwt100_gdppc <- pwt100 %>%
  transmute(country = countrycode(country,
                                  origin = "country.name",
                                  destination = "country.name"),
            year = year,
            gdppc = rgdpe/pop/1000) %>%
  filter(!is.na(gdppc))



runs <- swiid_summary %>% group_by(country) %>% filter(min(year) <= 1970)
unique(runs$country)

load(url("https://github.com/fsolt/swiid/blob/master/data/swiid9_3.rda?raw=true") )
names(swiid_summary)
library(panelView)
swiid_summary %>% panelview(gini_disp ~ 1, 
                            index = c("country","year"), 
                            axis.lab="both", type = "miss")


SWIID.ccode <- swiid # 100 imputed datasets from SWIID in wide format a list of 100 imputed datasets
str(swiid[1])
swiid1 <- swiid[[1]]
names(swiid[[1]])
# gini_net: Estimate of Gini index of inequality in equivalized (square root scale) household disposable (post-tax, post-transfer) income, using Luxembourg Income Study data as the standard.
# gini_market: Estimate of Gini index of inequality in equivalized (square root scale) household market (pre-tax, pre-transfer) income, using Luxembourg Income Study data as the standard.
# abs_red: Estimated absolute redistribution, the number of Gini-index points market-income inequality is reduced due to taxes and transfers: the dierence between the gini_market and gini_net.
# rel_red: Estimated relative redistribution, the percentage reduction in market-income inequality due to taxes and transfers: the dierence between the gini_market and gini_net, divided by gini_market, multiplied by 100.


any(grep(x= swiid_summary$country, pattern =  "Argentina"))

swiid_summary[grep(x= swiid_summary$country, pattern =  "Argentina"),] %>% as.data.frame()


table(SWIID.ccode$year)

## is country X year a unique key in SWIID?
dim(swiid_summary)
dim(unique(swiid_summary[c("year", "country")]))  ##yes

names(swiid_summary)
## is ccode X year a unique key in SWIID?
dim(unique(swiid_summary[c("year", "ccode")]))  ##no
##get 'problem' ccode X years
SWIID.ccode[duplicated(SWIID.ccode[c("year", "ccode")]),c("year", "ccode", "country")]

#2 Adding Variables and Subsetting
library(purrr)
library(countrycode)

swiid1_lac <- swiid_summary %>%
  mutate(cown = countrycode(country,
                            origin = "country.name",
                            destination = "cown"))

swiid1_lac <- swiid1_lac %>%
  mutate(p4.name = countrycode(country,
                               origin = "country.name",
                               destination = "p4.name"))   # p4.name cown  cowc  cow.name

swiid1_lac <- swiid1_lac %>%
  mutate(wvs = countrycode(country,
                           origin = "country.name",
                           destination = "wvs"))   # p4.name cown  cowc  cow.name

swiid1_lac <- swiid1_lac %>%
  mutate(wb = countrycode(country,
                          origin = "country.name",
                          destination = "wb"))   # p4.name cown  cowc  cow.name

swiid1_lac <- swiid1_lac %>%
  mutate(iso3n = countrycode(country,
                             origin = "country.name",
                             destination = "iso3n"))   # p4.name cown  cowc  cow.name

# swiid1_lac <- swiid1 %>%
#   mutate(p4.name = countrycode(country,
#                                origin = "country.name",
#                                destination = "p4.name"))   # p4.name cown  cowc  cow.name

countrycode("Kyrgyzstan", origin = "country.name", destination = "cown")


# correlates of war naming, historic:  https://www.paulhensel.org/icownames.html
## note: COW doesn't have terretories (e.g. Anguilla, Cayman Islands, Hong Kong, Puerto Rico, Turks and Caicos)
## note:  Serbia, Serbia and Montenegro get the Yougoslavia ccode, 345  see  https://www.paulhensel.org/icownames.html

swiid1_lac[str_detect(swiid1_lac$country, "^Serb"),'cown'] <- 345 




# Generate region variable and subset LAC data
swiid_lac <- swiid %>%
  map(. %>% mutate(region = countrycode(country,
                                        origin = "country.name",
                                        destination = "region"))) %>%
  map(. %>% filter(region == "Caribbean" |
                     region == "Central America" | # WB regions; includes MEX
                     region == "South America"))


names(rr)
# https://yiqingxu.org/packages/panelview/articles/tutorial.html#plot-missingness-only
rr %>% panelview(gini_net ~ 1, 
                 index = c("ccode","year"), 
                 axis.lab="both", type = "miss")



##################################################################
##################################################################
## Merge COW data with SWIID data
##################################################################
##################################################################

library(foreign)
library(readstata13)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggthemes)
library(tidyr)

old <-read.dta("~/Dropbox/research/terryPat/masterAPSA2013.dta")
names(old)


table(old[old$ccode==703,]$country)

table(old[old$ccode==703,]$year)

any(old$country=="Kyrgyz")



################################
###

ap <- read.dta13("~/Box Sync/Armed Peace/data/Master10.5.2016.dta")
names(ap)

## is country X year a unique key in our ArmedPeace merged data??
dim(ap)
dim(unique(ap[c("year", "country")]))  ##no
##get 'problem' country X years
ap[duplicated(ap[c("year", "country")]),c("year", "ccode", "country", "NAMENEW")]

## is ccode X year a unique key in our ArmedPeace merged data??
dim(ap)
dim(unique(ap[c("year", "ccode")]))  ##yes

##################################################################
### clean up or ArmedPeace data:

ap[ap$NAMENEW=="",c("NAMENEW","country","ccode")]

names(ap)[1:20]
ap[!ap$NAMENEW=="",1:6]

ap.c1<- ap[!is.na(ap$LMILEX),]
ap.c2<- ap.c1[!is.na(ap.c1$LNRGDP),]
ap.clean <- ap.c2[!ap.c2$NAMENEW=="",]

ap.clean<- ap.clean[ap.clean$year>=1960,]

## is country X year a unique key in our ArmedPeace merged data??
dim(ap.clean)
dim(unique(ap.clean[c("year", "country")]))  ##no
##get 'problem' country X years
ap.clean[duplicated(ap.clean[c("year", "country")]),c("year", "ccode", "country", "NAMENEW")]

## is ccode X year a unique key in our ArmedPeace merged data??
dim(ap.clean)
dim(unique(ap.clean[c("year", "ccode")]))  ##yes


##################################################################
##################################################################
###############
### TRY 2: merge our old ArmP data into SWIID using ArmP$NAMESNEW and SWIID $country
library(stringr)

ap.clean[tolower(ap.clean$NAMENEW)=="german democratic republic",c("NAMENEW","country","ccode","year")]

tolower(ap.clean$NAMENEW)
tolower(SWIID.ccode$country)






#####################################################################################
#############################################
#############################################
#############################################



##################################################################


library(stringr)


## merge ineqlong into the 'master' codelist_panel
#### BUT: to do this we have to know the UTIP codes countries!


names(ineqlong)

rrn <- ineqlong$country
rrc <- ineqlong$code

# all the country codes ith NUMBERS that \countrycode\ supports
ccnum <- codelist_panel %>% select_if(is.numeric) %>% names()
errs <- list()
warns <- list()

ittsNumsOrg <- data.frame(itter=(1:length(ccnum)), originTry=ccnum, warnMsg=NA, warnLength=NA)
for (i in 1:length(ccnum)) {
  tryCatch({  
    eval(parse(text=paste0("countrycode(rrn, origin = '",ccnum[i], "' , destination = 'country.name.en')"
    )))
    ittsNumsOrg$itter[i] <- i
  },   error=function(e){cat("ERROR : ",i,"\t",conditionMessage(e), "\n")
    ittsNumsOrg$warnMsg[i] <<- paste("ERROR: ", e$message)
    ittsNumsOrg$itter[i] <<- i
  }, #end error function
  warning=function(w){
    cat("Itteration = ",i,"\t origin = ",ccnum[i],"\t  WARNING:",conditionMessage(w), "\n");
    ittsNumsOrg$itter[i] <<- i;
    ittsNumsOrg$originTry[i] <<- ccnum[i]
    ittsNumsOrg$warnMsg[i] <<- w$message
    ittsNumsOrg$warnLength[i] <<- length(str_split(w$message, pattern = ",", simplify = T))
  }
  )
  
} # end for loop  


#### Same, but for 'origins' that are characters
rrc <- ineqlong$countryname
# all the country codes ith NUMBERS that \countrycode\ supports
ccchar <- codelist_panel %>% select_if(is.character) %>% names()
errs <- list()
warns <- list()
i <- 1

ittsCharOrg <- data.frame(itter=(1:length(ccchar)), originTry=ccchar, warnMsg=NA, warnLength=NA)
withCallingHandlers({
  for (i in 1:length(ccchar)) {
    tryCatch({  
      eval(parse(text=paste0("countrycode(rrc, origin = '",ccchar[i], "' , destination = 'country.name.en')"
      )))
      ittsCharOrg$itter[i] <- i
    }, 
    error=function(e){cat("ERROR : ",i,"\t",conditionMessage(e), "\n")
      ittsCharOrg$warnMsg[i] <<- paste("ERROR: ", e$message)
      ittsCharOrg$itter[i] <<- i
    }, #end error function
    
    warning=function(w){
      cat("Itteration = ",i,"\t origin = ",ccchar[i],"\t  WARNING:",conditionMessage(w), "\n");
      ittsCharOrg$itter[i] <<- i;
      ittsCharOrg$originTry[i] <<- ccchar[i]
      ittsCharOrg$warnMsg[i] <<- w$message
      ittsCharOrg$warnLength[i] <<- length(str_split(w$message, pattern = ",", simplify = T))
    }
    )
  } # end for loop 
})

