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
#      "--bibliography", "~/Dropbox/Bibliog/scottAll5_BBL-URLdoi.bib",
#      "--bibliography", "~/Dropbox/Bibliog/KFcurrent.bib"
      "--bibliography", "C:/Users/ldzsm2/OneDrive - The University of Nottingham/Bibliog/scottAll5_BBL-URLdoi.bib"
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



### Penn world tables (general) v10 (PWT)


<https://www.rug.nl/ggdc/productivity/pwt/>

Coverage: 183 countries between 1950 and 2019.

Vars:  relative levels of income, output, input, and productivity



Merge key: `pwt10$isocode` is `iso3c`!!

Dropped observations in merge? No.




### SIPRI

<>

Coverage:

Vars:

Merge key:  `by = c("country.name.en" ="Country" , "year" = "year") `


Dropped observations in merge? Region values (Africa, etc.)

###

<>

Coverage:

Vars:

Merge key:

Dropped observations in merge?


###

<>

Coverage:

Vars:

Merge key:

Dropped observations in merge?



###

<>

Coverage:

Vars:

Merge key:

Dropped observations in merge?


## Master Merge






# Variables

From `archModels-ave_2017july30-TS.do`:

```
gen milexpRate = milex/L.milex
gen milper1 = L.milper
gen milperD1 = D.milper
gen lmilper = log(milper)
gen lmilper1 = L.lmilper
gen milperRate = milper/milper1
gen lmilperRate = log(milper/milper1)
gen lmilperD1 = D.lmilper
```


`lnmilex`
`lexport82per  gini_net  warparticip polity2 lnrgdpe USdef Russiadef`

```
ylist lmilper /* lnmilex milperRate */
global xlist lexport82per  gini_net  warparticip polity2 lnrgdpe USdef Russiadef  /* lnmilex1 lnexport82 */
```

`ArPeacemasterDec2016.dta`
`ArPeacemasterDec2016ave.dta`


## Replication Data 

From:

@nord12-effects (and @nord11-online)

@capp17-what

@bisc21-military



---
# References
