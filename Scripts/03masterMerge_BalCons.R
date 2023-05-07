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

write_csv(mmALL50, file = "mmALL50.csv")



######################################################
######################################################
######################################################
## INSPECT MERGED DATA
######################################################


rm(list=ls())

setwd("~/Dropbox/research/terryPat/data_ArmedPeace")
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/data_ArmedPeace")


getwd()
library(haven)
library(janitor)
library(tidyverse)
library(Amelia)
library(sampleSelection)

load(file = "mmALL50.RData")
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



mmALL50 %>% group_by(country_PWT) %>% summarise(cs = colSums(is.na(.)))

library(mice)
md.pattern(mmALL50)














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

