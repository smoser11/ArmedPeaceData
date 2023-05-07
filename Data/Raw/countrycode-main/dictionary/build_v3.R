
rm(list = ls() )
library(here)
setwd(here::here())
getwd()

source(paste0(here("Data/Raw/countrycode-main/dictionary", "utilities_v2.R")))

getwd()

##################
#  availability  #
##################

scrapers <- Sys.glob(paste0(here("Data/Raw/countrycode-main/dictionary", 'get_*.R') ))
scrapers
datasets <- Sys.glob(paste0(here("Data/Raw/countrycode-main/dictionary", 'data_*.csv') ))

datasets
datasets <- datasets[datasets != paste0(here('Data/Raw/countrycode-main/dictionary', 'data_regex.csv'))]
datasets <- datasets[datasets != paste0(here('Data/Raw/countrycode-main/dictionary','data_iso_ret.csv'))]
datasets <- datasets[datasets != paste0(here('Data/Raw/countrycode-main/dictionary','data_iso_ret2.csv'))]

datasets

ds <- datasets


# missing scrapers and datasets
tokens_datasets <- str_replace_all(datasets, '.*data_|.csv', '')
tokens_datasets
tokens_scrapers <- str_replace_all(scrapers, '.*get_|.R', '')

tokens_scrapers <- setdiff(tokens_scrapers, "countryname_dict")
tokens_scrapers

if (length(setdiff(tokens_scrapers, tokens_datasets)) > 0) {
    msg <- paste(setdiff(tokens_scrapers, tokens_datasets), collapse = ', ')
    msg <- paste('Missing datasets:', msg)
    stop(msg)
}
#msg

if (length(setdiff(tokens_datasets, tokens_scrapers)) > 0) {
    msg <- paste(setdiff(tokens_datasets, tokens_scrapers), collapse = ', ')
    msg <- paste('Missing scrapers:', msg)
    warning(msg)
}


###############
#  load data  #
###############

dat <- list()
dat$regex <- read_csv(paste0(here("Data/Raw/countrycode-main/dictionary/", 'data_regex.csv') ), col_types = cols(), progress = FALSE)

message("Load:")

for (i in seq_along(datasets)) {  # 
    message("  ", tokens_datasets[i])
    tmp <- read_csv(datasets[i], col_types = cols(), na = "", progress = FALSE) %>%
        mutate(
            country = utf8::utf8_encode(country),
            country.name.en.regex = CountryToRegex(country)) %>%
        select(-country)
    SanityCheck(tmp)
    dat[[tokens_datasets[i]]] <- tmp
}

# Namibia iso2c is not missing
dat$iso$iso2c[dat$iso$iso.name.en == 'Namibia'] <- 'NA'

str(dat)

save(dat, file = paste0(here("Data/Processed/", "CCdatOrig.RData") ))

#############################################################
#############################################################
#############################################################
#############################################################
###################
#  cross-section  #
###################

rm(list=ls())
getwd()
source('./Data/Raw/countrycode-main/dictionary/utilities_v2.R')
load(file =  paste0(here("Data/Processed/", "CCdatOrig.RData") ) )

idx <- sapply(dat, function(x) !'year' %in% names(x))
ccs_noYear <- idx

#View(dat[idx])
cs <- dat[idx] %>% 
      reduce(left_join, by = 'country.name.en.regex')

# sanity check
SanityCheck(cs)
checkmate::assert_true(nrow(cs) > 250)
checkmate::assert_true(ncol(cs) > 50)


###########
#  panel  #
###########

idx <- sapply(dat, function(x) 'year' %in% names(x))
ccs_year <- idx
ccs_year
pan <- dat[idx]
#pan <- lapply(pan, ExtendCoverage, last_year = 2020)

cou <- sapply(pan, function(x) x$country.name.en.regex) %>% unlist %>% unique
cou
yea <- sapply(pan, function(x) x$year) %>% unlist %>% unique
yea
yea <- min(yea):max(yea)
yea
rec <- expand_grid(country.name.en.regex = cou,
                   year = yea)
pan <- c(list(rec), pan) %>%
       purrr::reduce(left_join, by = c('country.name.en.regex', 'year'))

idx <- (pan[, 3:ncol(pan)] %>% is.na %>% rowSums) != (ncol(pan) - 2)  # rows of pan with not all NAs in all CCs
idx
pan2 <- pan[idx,]

dd <- setdiff(pan, pan2)
dd

pan <- pan2

###########
#  merge  #
###########

# merge last panel observation into cs
names(pan)

tmp <- pan %>%
  arrange(country.name.en.regex, year) %>%
  group_by(country.name.en.regex) %>%
  mutate_at(vars(-group_cols()), na.locf, na.rm = FALSE) %>% arrange(country.name.en.regex, year)
#identical(tmp,pan)
setdiff(pan,tmp)  # so tmp at this point fills in 'gap's in the panel data.  But it shouldn't e.g. COW and Cuba 1907 and 1908 of Haiti 1916-1920

# head(sort(unique(tmp$vdem.name)), 55)

tmp <- pan %>%
       arrange(country.name.en.regex, year) %>%
       group_by(country.name.en.regex) %>%
 
        mutate_at(vars(-group_cols()), na.locf, na.rm = FALSE) %>% 
       filter(year %in% max(year)) %>%
       # arbitrary choices
       mutate(p5n = ifelse(p4.name %in% 'Prussia', NA, p5n),
              p5c = ifelse(p4.name %in% 'Prussia', NA, p5c),
              p5n = ifelse(p4.name %in% 'Serbia and Montenegro', NA, p5n),
              p5c = ifelse(p4.name %in% 'Serbia and Montenegro', NA, p5c),
              p4n = ifelse(p4.name %in% 'Prussia', NA, p4n),
              p4c = ifelse(p4.name %in% 'Prussia', NA, p4c),
              p4n = ifelse(p4.name %in% 'Serbia and Montenegro', NA, p4n),
              p4c = ifelse(p4.name %in% 'Serbia and Montenegro', NA, p4c),
              vdem = ifelse(vdem.name %in% 'Czechoslovakia', NA, vdem)) ## ????????

cs <- cs %>% 
      left_join(tmp, by = 'country.name.en.regex') %>%
      select(-year)

# english names with priority
priority <- c('cldr.name.en', 'iso.name.en', 'un.name.en', 'cow.name',
              'p4.name', 'vdem.name', 'country.name.en') ## this is weird.  this prioritized english name of country _in order of \priority\ vector (?)
cs$country.name <-  NA
i <- priority[1]
for (i in priority) {
    cs$country.name <- ifelse(is.na(cs$country.name), cs[[i]], cs$country.name)
}
cs$country.name.en <- cs$country.name
cs$country.name <- NULL

# merge cs into pan
idx <- c('country.name.en.regex', setdiff(colnames(cs), colnames(pan)))
idx
pan <- pan %>% 
       left_join(cs[, idx], by = 'country.name.en.regex') %>%
       select(-matches('name$|cldr'))

## so now all CCs in ccs_noYear have values for the entire series.
## NO!  a country has a (the) CC in nns_noYear _for all the years it has a row in \pan\_

sort(unique(pan$country.name.en))
library(panelView)
ccs_noYear
aaa <- pan %>% filter( country.name.en %in% sort(unique(pan$country.name.en))[1:3] )

pan %>% filter( country.name.en %in% sort(unique(pan$country.name.en))[1:3] ) %>%
                  panelview(eurostat  ~ 1, index = c("country.name.en","year"), type = 'miss' )

pan %>%  filter( country.name.en %in% sort(unique(pan$country.name.en))[3] ) %>%
{unique(.$year)}
  


###########
#  clean  #
###########

idx <- c('country.name.en.regex', setdiff(colnames(cs), colnames(pan)))

pan <- pan %>%
       arrange(country.name.en, year) %>%
       select(country.name.en, year, order(names(.))) %>%
       select(-matches('cldr|name$|iso.name|un.name'))

idx1 <- sort(grep('cldr', colnames(cs), value = TRUE))
idx2 <- sort(setdiff(colnames(cs), idx1))

cs <- cs[, c(idx2, idx1)] %>%
      arrange(country.name.en)

library(panelView)
ccs_noYear
#panelview(eurostat ~ 1, index = c("country.name.en","year"), type = 'miss', data =pan )


##########
#  utf8  #
##########

for (col in colnames(cs)[sapply(cs, class) == 'character']) {
    if (!all(na.omit(stringi::stri_enc_mark(cs[[col]])) == 'ASCII')) {
        cs[[col]] <- enc2utf8(cs[[col]])
    }
}

for (col in colnames(pan)[sapply(pan, class) == 'character']) {
    if (!all(na.omit(stringi::stri_enc_mark(pan[[col]])) == 'ASCII')) {
        pan[[col]] <- enc2utf8(pan[[col]])
    }
}



###########
##  save  #
###########

codelist2 <- cs
codelist_panel2 <- pan


# save(codelist, file = 'data/codelist.rda', compress = 'xz', version = 2)
# save(codelist_panel, file = 'data/codelist_panel.rda', compress = 'xz', version = 2)
# 
# # These uncompressed files are better for seeing diffs in version control
# codelist_without_cldr <- codelist %>% select(-starts_with('cldr'))
# codelist_panel_without_cldr <- codelist_panel %>% select(-starts_with('cldr'))
# 
# write_csv(codelist_without_cldr, 'dictionary/codelist_without_cldr.csv', na = '')
# write_csv(codelist_panel_without_cldr, 'dictionary/codelist_panel_without_cldr.csv', na = '')
# 


##################################################################
##################################################################
##################################################################
##################################################################
###### countrycode and codelist_panel  -- MOVED TO \build_v2.r\
##################################################################

getwd()
library(tidyverse)
names(codelist_panel2)
codelist_panel2 <- codelist_panel2 %>% 
  select(year, starts_with("count"), iso3n, iso3c, cown, cowc, un, wb, gwn, everything()) 

codelist_panel2 <- codelist_panel2 %>% arrange(country.name.en, year)

#--------------------------------------
library(plm)
pcc <- pdata.frame(codelist_panel2, index = c("country.name.en", "year"))
is.pconsecutive(pcc)

codelist_panel2_ConBal <- make.pconsecutive(pcc, balanced = TRUE)
codelist_panel2_Con <- make.pconsecutive(pcc, balanced = FALSE)

codelist_panel2_ConBal <- as.data.frame(lapply(codelist_panel2_ConBal, 
                                               function(x){attr(x, "index") <- NULL; x}) )
codelist_panel2_ConBal$year <- as.integer(as.character(codelist_panel2_ConBal$year) )

codelist_panel2_Con <- as.data.frame(lapply(codelist_panel2_Con, 
                                               function(x){attr(x, "index") <- NULL; x}) )
codelist_panel2_Con$year <- as.integer(as.character(codelist_panel2_Con$year) )

library(janitor)
cc_dta <- clean_names(codelist_panel2)
glimpse(cc_dta)
names(cc_dta)

cc_ConBal_dta <- clean_names(codelist_panel2_ConBal)

library(haven)
write_dta(cc_dta, path = paste0(here("Data/Processed/", "codelist_panel2.dta") ))
write_dta(cc_ConBal_dta, path = paste0(here("Data/Processed/","/codelist_panel2_ConBal.dta") ))


###  make custom dictionary to use Formerly Used ISO codes:  https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3
## from:https://github.com/vincentarelbundock/countrycode/issues/291  
### to use: countrycode(countries, "country.name", "iso2c", custom_match = custom_codes)
library(ISOcodes)
data("ISO_3166_3")

iso3cRet_cowCustom <- c("BUR" = 775,"BYS" = 370, "CSK" = 315, "SCG" = 345, "DDR" = 265, "DHY" = 434, "FXX" = 220, "HVO" = 439, "RHO" = 552, "SUN" = 365, "VDR" = 816, "YMD" = 680,
                        "YUG" = 345,
                        "ZAR" = 490 )

# countrycode( , origin = "iso3c", destination = "cown", custom_dict = iso3cRet_cowCustom)

getwd()
save(codelist2, codelist_panel2, codelist_panel2_ConBal, 
     ccs_year, ccs_noYear, iso3cRet_cowCustom,  file =paste0(here("Data/Processed/","codelist_panel2.RData" ) ))




##----------------------------------------------------

library(panelView)


codelist_panel %>% filter(year>1900 & !is.na(eu28))%>% panelview( cown ~1, index = c("country.name.en", "year"), type = 'miss', axis.adjust = TRUE,  main = 'Missing values of cown, EU countries')


