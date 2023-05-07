
rm(list = ls() )
   library(here)
setwd(here::here())
getwd()
setwd("C:/Users/ldzsm2/OneDrive - The University of Nottingham/research_terryPat/")
setwd("~/Dropbox/research/terryPat/")

source('./countrycode-main/dictionary/utilities.R')


##################
#  availability  #
##################

scrapers <- Sys.glob('./countrycode-main/dictionary/get_*.R')
scrapers
datasets <- Sys.glob('./countrycode-main/dictionary/data_*.csv')
datasets
datasets <- datasets[datasets != './countrycode-main/dictionary/data_regex.csv']
datasets

# ds <- datasets
# datasets <- c("./countrycode-main/dictionary/data_cow.csv","./countrycode-main/dictionary/data_vdem.csv","./countrycode-main/dictionary/data_iso.csv")

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
msg

if (length(setdiff(tokens_datasets, tokens_scrapers)) > 0) {
    msg <- paste(setdiff(tokens_datasets, tokens_scrapers), collapse = ', ')
    msg <- paste('Missing scrapers:', msg)
    warning(msg)
}


###############
#  load data  #
###############

dat <- list()
dat$regex <- read_csv('./countrycode-main/dictionary/data_regex.csv', col_types = cols(), progress = FALSE)

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

###################
#  cross-section  #
###################

idx <- sapply(dat, function(x) !'year' %in% names(x))
ccs_noYear <- idx

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
# pan <- lapply(pan, ExtendCoverage, last_year = 2020)

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
names(tmp)

tmp <- pan %>%
  arrange(country.name.en.regex, year) %>%
  group_by(country.name.en.regex) %>%
  mutate_at(vars(-group_cols()), na.locf, na.rm = FALSE) %>% arrange(country.name.en.regex, year)
identical(tmp,pan)
setdiff(pan,tmp)  # so tmp at this point fills in 'gap's in the panel data.  But it shouldn't e.g. COW and Cuba 1907 and 1908 of Haiti 1916-1920

head(sort(unique(tmp$vdem.name)), 55)

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
              'p4.name', 'vdem.name', 'country.name.en') ## this is weird.  thiss prioritized english name of country _in order of \priority\ vector (?)
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
panelview(eurostat ~ 1, index = c("country.name.en","year"), type = 'miss', data =pan )


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


library(panelView)
ccs_noYear
panelview(eurostat ~ 1, index = c("country.name.en","year"), type = 'miss', data =pan )

###########
##  save  #
###########

codelist <- cs
codelist_panel <- pan

save(codelist, file = 'data/codelist.rda', compress = 'xz', version = 2)
save(codelist_panel, file = 'data/codelist_panel.rda', compress = 'xz', version = 2)

#### write countrycode panel for stata (MOVED to `build202209XX.R`)
library(janitor)
cc <- clean_names(codelist_panel2)
write_dta(cc, path = "codelist_panel2.dta")




# These uncompressed files are better for seeing diffs in version control
codelist_without_cldr <- codelist %>% select(-starts_with('cldr'))
codelist_panel_without_cldr <- codelist_panel %>% select(-starts_with('cldr'))

write_csv(codelist_without_cldr, 'dictionary/codelist_without_cldr.csv', na = '')
write_csv(codelist_panel_without_cldr, 'dictionary/codelist_panel_without_cldr.csv', na = '')

