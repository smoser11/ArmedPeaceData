### Dataset creation for Armed Peace

# Loading Package ---------------------------------------------------------
rm(list = ls())
library(peacesciencer)
library(tidyverse)
library(readxl)
library(haven)
library(countrycode)
library(zoo)

# Loading data ------------------------------------------------------------

BFMM <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Research/Armed Peace with Scott, Pat, and Terry/R/Domestic Bargaining and Military Balance/data/ArmedPeaceData-main/Data/Raw/Barnum et al JPR 2024/data/estimates_milex_cur_20231205.rds") 
BFMM <- BFMM |> filter(indicator == "milex_cur_nmc") |> # the indiator is the COW NMC.
	select(-indicator,  -og,  -con, - group) |> 
	rename(milex_log = mean_log,
		   milex = mean,
		   milex_log10 = mean_log10,
		   milex_sd_log = sd_log,
		   milex_sd = sd,
		   milex_sd_log10 = sd_log10)

GDP <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Research/Armed Peace with Scott, Pat, and Terry/R/Domestic Bargaining and Military Balance/data/ArmedPeaceData-main/Data/Raw/GDP-etc/estimates_gdp_model_combined_normal_noslope_gamma_lambda_additive_test_20240416.rds")
GDP <- GDP |>  filter(indicator == "WorldBank_gdp_con_bc_2010") |>  #the indicator is World Bank GDP 2010
	select(-indicator,  -og, -og_log, -og_log10, -group) |> 
	rename(gdp_mean_log = mean_log,
		   gdp = mean,
		   gdp_mean_log10 = mean_log10,
		   gdp_sd_log = sd_log,
		   gdp_sd = sd,
		   gdp_sd_log10 = sd_log10)

GDPpc <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Research/Armed Peace with Scott, Pat, and Terry/R/Domestic Bargaining and Military Balance/data/ArmedPeaceData-main/Data/Raw/GDP-etc/estimates_gdppc_model_combined_normal_noslope_gamma_lambda_additive_test_20240416.rds")
GDPpc <- GDPpc |>  filter(indicator == "WorldBank_gdppc_con_bc_2010") |>  #the indicator is World Bank GDP 2010
	select(-indicator,  -og, -og_log, -og_log10, -group) |> 
	rename(gdppc_mean_log = mean_log,
		   gdppc = mean,
		   gdppc_mean_log10 = mean_log10,
		   gdppc_sd_log = sd_log,
		   gdppc_sd = sd,
		   gdppc_sd_log10 = sd_log10)

POP <- readRDS("~/Library/Mobile Documents/com~apple~CloudDocs/Research/Armed Peace with Scott, Pat, and Terry/R/Domestic Bargaining and Military Balance/data/ArmedPeaceData-main/Data/Raw/GDP-etc/estimates_pop_model_combined_normal_noslope_gamma_lambda_additive_test_20240416.rds")
POP <- POP |> filter(indicator == "WorldBank_pop")|>  #the indicator is World Bank
	select(-indicator,  -og, -og_log, -og_log10, -group) |> 
	rename(pop_mean_log = mean_log,
		   pop = mean,
		   pop_mean_log10 = mean_log10,
		   pop_sd_log = sd_log,
		   pop_sd = sd,
		   pop_sd_log10 = sd_log10)

political_data <- create_stateyears(system = "gw", mry = TRUE, subset_years=  c(1816:2022)) |> 
	add_ccode_to_gw() |> 
	add_cow_wars(type = "intra") |> mutate(civilwar = if_else(cowintraonset == 1 | cowintraongoing == 1, 1, 0)) |> 
	add_democracy() |> 
	select(gwcode, statename, year, ccode,  civilwar, v2x_polyarchy, polity2)

polity <- read_excel("Data/Raw/polity.xls") |> select(ccode, year, polity) #peacesciencer does not have  -66, -77,  and -88 in the polity Data, so I created Polity Data from scratch.
political_data <- political_data |> left_join(polity, by = c("ccode", "year")) |> mutate(polity_int = if_else(polity == -66 |polity == -77 | polity == -88, 1, 0 ))

#trade variables
pwt <- read_dta("Data/Raw/pwt1001.dta") |> select(countrycode, year, csh_x) |> mutate(gwno = countrycode(countrycode, origin="iso3c", destination = "gwn"))
# some countries are not matched one-to-one. (ABW, AIA, ATG, BMU, CUW, CYM, DMA, GRD, HKG, KNA, LCA, MAC, MSR, PSE, STP, SXM, SYC, TCA, VCT, VGB, YEM )

# international war 
intlwar <- create_dyadyears(system = "gw", mry = TRUE, subset_years=  c(1816:2022)) |>
	add_ccode_to_gw() |> 
	add_cow_wars(type = "inter") |> 
	mutate(interwar_el = if_else(cowinterongoing == 1 | cowinteronset ==1, 1, 0)) |> 
	group_by(ccode1, year) |> mutate(interwar = if_else(sum(interwar_el, na.rm = T)>= 1 , 1, 0)) |> 
	ungroup() |> distinct(ccode1, year, .keep_all = T) |> 
	rename(ccode = ccode1) |> select(ccode, year, interwar)


alliance <- create_dyadyears(system = "cow", mry = TRUE, directed = TRUE, subset_years = c(1816:2016)) |> add_democracy() |> add_atop_alliance() |>  
	mutate(dem_ally_el = if_else(atop_defense == 1 & polity22 >= 7, 1, 0), nondem_ally_el = if_else(atop_defense == 1 & polity22 <= 6, 1, 0)) |> # Following  Digiuseppe and Poast (2018)
	group_by(ccode1, year) |> mutate(dem_ally = if_else(sum(dem_ally_el, na.rm = T)>=1, 1, 0),  nondem_ally = if_else(sum(nondem_ally_el, na.rm = T)>=1, 1, 0)) |> ungroup() |> distinct(ccode1, year, .keep_all = T) |> 
	rename(ccode = ccode1) |> select(ccode, year, dem_ally, nondem_ally) 

#NATO or not 
atop <- read_csv("Data/Raw/ATOP 5.1 (.csv)/atop5_1sy_NNA.csv")
atop <- atop |> mutate(nato = if_else(as.logical(rowSums(select(atop, starts_with("atopid")) == 3180, na.rm = T)), 1, 0)) |> select(state, year, defense, nato)
# Atop id. 3180 is NATO
alliance <- alliance |> left_join(atop, by = c( "ccode"="state" ,"year")) |> mutate(defense = replace_na(defense, 0),
																					nato = replace_na(nato, 0), 
																					defense_wo_nato = if_else(defense == 1 & nato ==0, 1, 0),
																					dem_ally_wo_nato = if_else(dem_ally == 1 & nato ==0, 1, 0))
# since atop data is merged to a state-year dataset, when defensive alliance does not exist, it has NA in the data. The code here fixes this problem


#income inequality data
load("Data/Raw/swiid9_6/swiid9_6.rda")
swiid_summary # data coverage is limitted



# Merging Data ------------------------------------------------------------
data <- BFMM |> left_join(political_data, by =c("year", "gwno" = "gwcode")) |> 
	left_join(intlwar, by =c("year", "ccode")) |> 
	left_join(GDP, by = c("year", "gwno")) |> 
	left_join(GDPpc ,  by = c("year","gwno")) |> 
	left_join(POP,  by = c("year", "gwno")) |> 
	left_join(pwt, by = c("year", "gwno"))

data <- data |> left_join(alliance, by = c("ccode", "year"))


#finally, since alliance data exist until 2016, I use data before 2016
data <- data |> filter(year<=2016)


# Creating the fluctuation of arming (as a DV)-------------------------------------------------------


data <- data |> mutate(milex_ratio = milex/gdp) |> 
	group_by(ccode) |>  
	mutate(milex_ma10 = rollapply(milex_ratio, 10, mean, na.rm = TRUE, align = "center", partial = T, fill = NA),
		   milex_ma5 = rollapply(milex_ratio, 5, mean, na.rm = TRUE, align = "center", partial = T, fill = NA),
		   
		   milex_ma10_fluc = (milex_ratio - milex_ma10)^2,
		   milex_ma5_fluc = (milex_ratio - milex_ma5)^2, 
		   
		   milex_ma10_fluc_log = log(milex_ma10_fluc),
		   milex_ma5_fluc_log = log(milex_ma5_fluc))	


# Creating RHS Variables (Lagged Variables) -------------------------------

#GDP growth
data <- data |> mutate(gdp_growth = 100 * (gdp/lag(gdp, n = 1, order_by = year) - 1))  #growth percentage

# lag RHS
data <- data |> mutate(civilwar_lag2 = lag(civilwar, n=2, order_by = year),
					   civilwar_lag4 = lag(civilwar, n=4, order_by = year),
					   
					   polity2_lag2 = lag(polity2, n = 2, order_by = year),
					   polity2_lag4 = lag(polity2, n = 4, order_by = year),
					   
					   polity_int_lag2 = lag(polity_int, n = 2, order_by = year),
					   polity_int_lag4 = lag(polity_int, n = 4, order_by = year),
					   
					   polyarchy_lag2 = lag(v2x_polyarchy, n = 2, order_by = year),
					   polyarchy_lag4 = lag(v2x_polyarchy, n = 4, order_by = year),
					   
					   interwar_lag2 = lag(interwar, n = 2, order_by = year),
					   interwar_lag4 = lag(interwar, n = 4, order_by = year),
					   
					   dem_ally_lag2 = lag(dem_ally, n = 2, order_by = year),
					   dem_ally_lag4 = lag(dem_ally, n = 4, order_by = year),
					   
					   nondem_ally_lag2 = lag(nondem_ally, n = 2, order_by = year),
					   nondem_ally_lag4 = lag(nondem_ally, n = 4, order_by = year),
					   
					   defense_lag2 = lag(defense, n = 2, order_by = year),
					   defense_lag4 = lag(defense, n = 4, order_by = year),
					   
					   defense_wo_nato_lag2 = lag(defense_wo_nato, n = 2, order_by = year),
					   defense_wo_nato_lag4 = lag(defense_wo_nato, n = 4, order_by = year),
					   
					   dem_ally_wo_nato_lag2 = lag(dem_ally_wo_nato, n = 2, order_by = year),
					   dem_ally_wo_nato_lag4 = lag(dem_ally_wo_nato, n = 4, order_by = year),
					   
					   gdp_growth_lag2 = lag(gdp_growth, n = 2, order_by = year),
					   gdp_growth_lag4 = lag(gdp_growth, n = 4, order_by = year), 
					   
					   pop_lag2 = lag(pop, n=2, order_by = year),
					   pop_lag4 = lag(pop, n=4, order_by = year),
					   
					   gdppc_lag2 = lag(gdppc, n=2, order_by = year),
					   gdppc_lag4 = lag(gdppc, n=4, order_by = year),
					   
					   gdp_lag2 = lag(gdp, n = 2, order_by = year),
					   gdp_lag4 = lag(gdp, n = 4, order_by = year),
					   
					   csh_x_lag2 = lag(csh_x, n = 2, order_by = year),
					   csh_x_lag4 = lag(csh_x, n = 4, order_by = year))


save(data, file="data.rda")
