---
title: "Data Documentation, Armed Peace Project"
author: "Arabella Cue and Scott Moser"
date: "Today"
citation_package: biblatex
biblatex: true
biblio-style: "apalike"
link-citations: true
link-bibliography: true
output:
  bookdown::html_document2:
#    css: ["~/Dropbox/LaTeX/Stationary/Rmakrdown/seminar_v5.css"]
    number_sections: false  #true
    toc: true
    toc_float: true
    toc_depth: 2
    df_print: paged
    pandoc_args: [
      "--citeproc",
      "--bibliography", "~/Dropbox/Bibliog/scottAll5-BBL-URLdoi2023.bib",
#      "--bibliography", "~/Dropbox/Bibliog/KFcurrent.bib"
#      "--bibliography", "C:/Users/ldzsm2/OneDrive - The University of Nottingham/Bibliog/scottAll5-BBL-URLdoi2023.bib"
      ]
    keep_md: true
    autosize: true
fontfamily: mathpazo
fontsize: 12pt
geometry: margin=1in
header-includes:
  - \linespread{1.05}
  - \usepackage{graphicx}
  - \usepackage{float}

---

<!-- # ```{r setup, include=FALSE} -->
<!-- # library(knitr) -->
<!-- # knitr::opts_chunk$set(echo = TRUE) -->
<!-- # #opts_knit$set(root.dir=normalizePath('~')) -->
<!-- # opts_knit$set(root.dir=paste0(here("Data/Processed/", "forcasting2.xlsx")  "C:/Users/ldzsm2/OneDrive - The University of Nottingham/research/terryPat/") -->
<!-- # ``` -->


# Constuction of the Data-set

Idea:

1. Create a standard panel of multiple country code systems (CCS). Call this the CC Panel, `ccp`.
2. For each _source data-set_ (hereafter `sdat`, e.g. UTIP, SIPRI, PWT, COW, @nord12-effects and @bohm14-forecasting
 threat data, Pat McDonald's US/Russia alliance data), identify the key (e.g. `ccode` and `year`) and merge against standard panel, `ccp`.  At this stage there is a possibility of observations from a sdat to be missing, for example if an `sdat` contains a country not included in the `ccp` (e.g. Hong Kong is included in the UTIP data, but is not recognized in `ccp` and hence will be lost in the merged data-set).  We have multiple data-sets.
3. At this stage there are multiple data-sets, each for one `sdat` such that all data-sets contain the CCS from the standard panel.  Pick a key (e.g. year X cown) and perform a 'master merge' each of these 'attached' data-sets.  This will require choosing a 'master country code' (e.g. `countrycodes`, `cown` ).  Observations will be lost at this stage.


## Standard CC Panel

There are innumerable difficulties when attempting to create 'cross-walks' across different country coding systems (CCS).  Some of these difficulties stem from the conceptualization and measurement of the 'unit':  country, state, territory, political regime, etc. (@sark10-chapter).  Some are static, in that there is a single set of country codes (CC).  Some CCs are already panels, e.g. PolityIV (`countrycodes` calls such CCSes panels).  Some are just a static list of country codes, such as the World Bank CCs (`countrycodes` calls these 'cross-sections').  Some CCS are ‘dynamic’ in that at any point in time the country codes are static, but this list changes overtime (like ISO codes that can be ‘transitioned’ out of use, or IMF codes that are listed monthly and hence have the potential to change).


Construction of the standard `ccp` is based on `countrycode` R: package ()[https://cran.r-project.org/web/packages/countrycode/index.html]. Github [here](https://github.com/vincentarelbundock/countrycode)

DETAILS of original here:

- number of countries, years, codes included.

-- TODO: write on universe of country names and regexes.

- how time dealt with

- when a year X country row is present $\leadsto$ unbalanced, non-consecutive panel (w/ examples)

unbalanced, with `year` being the temporal dimension, running from 1789 to 2021.  There are `r `length(unique(codelist_panel$country.name.en))` countries. `year` X `country.name.en` uniquely identifies each row in this panel.

```
library(plm)
ccp <- pdata.frame(codelist_panel2, index = c("country.name.en", "year"))
pdim(pp)
```

TEMPORAL CCs in `ccs_year` and CCs with no year variable in `ccs_noYear`.

- Earliest year in panel for each country is the earliest year that not all CCs in `ccs_year` are missing (`NA`).  That is, first entry in `pp` is the first year any of the CCses in `ccs_year` have a value for that country.

- 'gaps' in the panel for year in which no CCs has a value for that country-year. `is.pconsecutive(pp)`


---


We differ from this in two ways, producing two datasets, `codelist_panel2` and `codelist_panel2_ConBal` each a possible standard panel.  Specifically...

 1. We do not 'pad out' CCs (using `ExtendCoverage()`) past the year of the most recent version of the CCS (as of the date of this document).  For example, `cown` is the numeric COW code for states.  The most recent version only goes up to 2016, so our panel only has `cown` codes until 2016.  This is the only difference between `codelist_panel` and `codelist_panel2`.  `country.name.en` and `year` jointly identify distinct rows (as in `codelist_panel`).

2. We provide a balanced, consecutive version of the standard panel, in which county-year rows missing in code-list_panel2 are filled in with `NA`s.  Called `codelist_panel2_ConBal`   `country.name.en` and `year` jointly identify distinct rows (as in `codelist_panel`).



This is done in the file `build_v2.R`, based on `build.R` in the `countrycodes` package, and requires the sub-folder structure in their `dictionary` folder <https://github.com/vincentarelbundock/countrycode/tree/main/dictionary>.   The `codelist_panel2_ConBal` and `codelist_panel2` data-sets are saved in both `.RData` and STATA format.  For Stata's `.dta` files, `clean_names()` from the package `janior` is used to change variable names to STATA-friendly variables names (e.g. `country.name.en` is renamed to `country_name_en` in the stata files).

These two panel are saved in `codelist_panel2.RData`.  This `RData` file also contains the following objects.

- `ccs_noYear` and `ccs_year`: two logical vector indicating if a given CCS has a temporal dimension or not.

- `iso3cRet_cowCustom`: a vector of (some) retired ISO3 character (`iso3c`) codes to COW numeric CCs (`cown`). These -- and the justifications therefore -- are in the file `iso3c_ret-pjm.csv`.







--------------------------------------------

## Data Sources and Merging to the Standard Panel


```r
library(tidyverse)
library(ISOcodes)
library(countrycode)
library(plm)
```


First, since the main variables of interest begin in 1950 (at the earliest, 1963 really), we first limit the standard panel to run from 1950-2020.  I'll use the consecutive and balanced version, but either are fine.


```r
load(file =".\data_ArmedPeaceData\codelist_panel2.RData")

ccpConBal50 <- codelist_panel2_ConBal %>% filter(year>= 1950)
pcc50 <- pdata.frame(ccpConBal50, index = c("country.name.en", "year"))
```





### Correlates of War, State System Membership (v2016)

<https://correlatesofwar.org/data-sets/state-system-membership/>

Coverage: 1816-2016, 216 countries, unbalanced and non-consecutive

Vars: Major Power (`MajPow_COW`)

Merge key:  `c("cown" = "ccode_COW", "year"="year_COW")`

Dropped observations in merge? Yes.

Edits: Manually changed mmf 1990 265 German Democratic Republic to 255 Germany.



### University of Texas Inequality Project (UTIP)

<https://utip.gov.utexas.edu/datasets.html>
Estimated Household Income Inequality Data Set (EHII)


Coverage: 151 cos. 1963-2015

Vars: `gini_UTIP` and `theil_UTIP`  (inequality)

Merge key: I have no clue how counties are identified. There are three possible country IDs: `country` (numeric); `code` (three character code, somewhat akin to iso3c, but with some retired iso3c codes); and `countryname` (uses both 'Serbia and Montenegro' and 'Yugoslavia').  The variable `code` is somewhat similar to `iso3c`, but with the following exceptions: "CSK, DDR, GER, HKG, MAC, PRI, PSE, SCG, YUG". The codes "HKG, MAC, PRI, PSE" are Hong Kong, Macao, Puerto Rico, and Palestine, respectively.  These are not recognized states in the COW framework.  The remaining countries are mapped to the following COW numeric codes.

```
utip3cown <- c( "CSK" = "315", "DDR" = "265" , "DEU" = "260" , "GER" = "255" , "SCG" = "345" , "YUG" = "345" )
## try 0 map 3character code to cown
ineqlong$cown <- countrycode(ineqlong$code, origin = 'iso3c', destination = 'cown', custom_match = utip3cown )
```

So the key is `cown` X `year`.
Dropped observations in merge? HKG, MAC, PRI, PSE, SCG

Edits: Manually changed german federal republic to germany for 1990. Changed Serbia and Montenegro to Yugoslavia only for 1994-2001



### Penn world tables (general) v10 (PWT)


<https://www.rug.nl/ggdc/productivity/pwt/>

Coverage: 183 countries between 1950 and 2019.

Vars:  relative levels of income, output, input, and productivity



Merge key: `pwt10$isocode` is `iso3c`!!

Dropped observations in merge? No.




### SIPRI

<>

Coverage: 191 countries between 1949 and 2022.

Vars: Military spending of countries

Merge key:  `by = c("country.name.en" ="Country" , "year" = "year") `


Dropped observations in merge? Region values (Africa, etc.)


### NORD

<>

Coverage: 165 countries between 1950 and 2000.

Vars: Influence of countries' security environments on military spending

Merge key: Merged with cown and year.

Dropped observations in merge? No. 

Edits: Changed 260 to 255 from 1990-2000.


### FORECASTING

<>

Coverage: 141 countries between 1952 and 2000. 

Vars: Frequently cited determinants of military spending and how they help to predict future expenditure

Merge key: Merged with cown and year.

Dropped observations in merge? No. 

EDits: Manually changed 1990 260 to 255. 



### P. MACDONALD 'alliances'

<>

Coverage:

Vars:

Merge key: Merged with cown and year.

Dropped observations in merge? Yes: Removed 2107 from PJM.

Edits: Manually changed 260 in 1990 to 255  and removed Germany for 1954, as it has no cown code). Remove 2107 from PJM.



### SWIID

<>

Coverage: 198 countries (?) between 1960 and present.

Vars: Income inequality

Merge key: c("country.name.en" = "country_SWIID", "year" = "year_SWIID") / c("country_SWIID" = "country.name.en" 

Dropped observations in merge? Yes.

Edits: Removed Micronesia manually. Changed Serbia to Yugoslavia for the years 1997-2005. Dropped Anguilla, Greenland, Puerto Rico, Turks & Caicos because there is no data for it in codelist panel. Changed Hong Kong to Hong Kong SAR China. Added isocode3c in SWIID for Palestinian Territories?



### HSS
 Democracy External Threat and Military Spending (Hauenstein et al. 2021)  @haue21-democracy
 
 https://journals-sagepub-com.nottingham.idm.oclc.org/doi/pdf/10.1177/20531680211049660 ######## "dp_Nord_clean.dta" ###### (HSS) ###

<>

Coverage:

Vars:

Merge key: Merged with cown and year?

Dropped observations in merge? No. 

Edits: Manually changed 260 to 255 for the years 1990-2000.



### ZFS

@hall15-what

JPR - What Goes UP - Replication (Zielinski et al. 2017) ###### "replication.dta" ###### 
<https://www-jstor-org.nottingham.idm.oclc.org/stable/48590474>


Coverage: 1949- 2015

Vars: "athreat", "UDel_temp_popweight", "UDel_precip_popweight", "smilex", "uncertain", "sgov", "rivalry", "smilex1", "smilincr", "milex", "milex1", .....

Merge key: Merged with cown and year.

Dropped observations in merge? Yes.

Edits: Fixed 260 and dropped 340, 296, 397, 711, 971, 972, 973. 



### YE_COW

<>

Coverage: 

Vars: 

Names cleaned with `clean_names()` from `janitor`.

Merge key: Merged with country.name.en and year.

Dropped observations in merge? No.



### YE_WB

<>

Coverage: 1993- 2007?

Names cleaned with `clean_names()` from `janitor`.

Vars: "Inration", "InGDP", "Inpop", "inttot", "civtot", "polity2", "militaryexpenditure..", "realgdp", "population", "continent", "missing", "citizensmissing"

Merge key: Merged with country.name.en and year.

Dropped observations in merge? No. 



## Replication Data 

From:

@nord12-effects (and @nord11-online)

@capp17-what

@bisc21-military




# Data Documentation for Armed Peace Project


## Overview

This document describes the process of constructing the final merged dataset used in the Armed Peace project. The workflow involves:

1. Setting up the R environment with required packages
2. For each source dataset:
   - Load the data 
   - Clean and reshape data to long format
   - Determine country-year key and make corrections
   - Merge with the standard country code panel `codelist_panel` 
   - Save merged dataset in both .RData and .dta formats
3. Document each source dataset (types of variables, coverage, merge keys, edits made)
4. Perform a master merge of all cleaned datasets into a single balanced and consecutive panel
5. Inspect and clean the final merged data
6. Conduct multiple imputation on the merged data to handle missing values

## Setup 

The setup script `00setup.R` loads the required R packages and sources the `build_v3.R` script to generate the standard country code panel datasets:

- `codelist_panel2.RData`: Country code panel from 1950-2021 
- `codelist_panel2_ConBal.RData`: Balanced and consecutive version of the panel

It also saves a list of unique English country names.

## Source Datasets

The following source datasets are processed and merged with the standard panel:

1. Correlates of War (COW) State System Membership v2016 
2. University of Texas Inequality Project (UTIP) 
3. Penn World Tables (PWT) v10
4. Stockholm International Peace Research Institute (SIPRI) Military Expenditure Database
5. Threat and military expenditure data from @nord12-effects and @bohm14-forecasting
6. US/Russia alliance data from McDonald 
7. Standardized World Income Inequality Database (SWIID)
8. Democracy, threat and military expenditure data from @haue21-democracy
9. Replication data from @hall15-what

For each dataset, the script:

- Loads the raw data
- Cleans and reshapes to long format 
- Determines merge key (usually country code and year)
- Makes manual corrections for country codes if needed
- Merges with the standard country code panel
- Saves the merged data in .RData and .dta formats

Detailed information on each dataset's coverage, variables, merge keys, and edits made are documented in `02dataDoc_ArmedPeace.Rmd`.

## Master Merge 

The script `03masterMerge_BalCons.R` performs the master merge of all the processed datasets into a single panel `mmmALL50.RData`. 

- Datasets are joined using the variables from the balanced consecutive panel `ccpConBal50`
- Light cleaning like removing unneeded columns is done
- The merged panel is saved in .RData, .dta and .csv formats

## Inspecting and Cleaning Merged Data

`03aInspectMerged-clean.R` summarizes missingness patterns in the merged data.

- Countries and variables with high missingness are identified 
- Some countries and variables are dropped based on thresholds
- The cleaned merged data is saved

## Multiple Imputation

Multiple imputation using Amelia II is conducted on two datasets in `04imputationAttempts_v2.R`:

1. `impHSS1` containing variables from HSS, PJM and UTIP
2. `impZFS1` with variables from ZFS, PJM and UTIP 

- Ridge priors of varying strengths are used
- Parallel processing with 5 cores 
- Imputed datasets are saved for different prior strengths

The imputation is validated by examining missingness in the imputed data.

## Summary

In summary, the Armed Peace project merges multiple country-year datasets on economic, political and military variables. Extensive cleaning and standardization of country codes is done before merging. The final merged dataset still contains significant missing data, which is handled via multiple imputation. The scripts provide a reproducible workflow from raw data to analysis-ready imputed datasets.




---
# References
