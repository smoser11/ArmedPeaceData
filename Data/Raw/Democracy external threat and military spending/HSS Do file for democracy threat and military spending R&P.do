
/* The core data for this research comes from Digiuseppe and Poast (DP) 2018 BJPS replication data. 
DP use the Nordhaus, Oneal, and Russett (NOR) military spending and threat data. 
To the DP data we add two regime variables from VDEM 11.1: v2x_polyarchy and v2x_regime 

Digiuseppe, Matthew, and Paul Poast. "Arms versus democratic allies." British Journal of Political Science 48, no. 4 (2018): 981-1003.

Nordhaus, William, John R. Oneal, and Bruce Russett. "The effects of the international security environment on national military expenditures: A multicountry study." International Organization (2012): 491-513.

* The DP data has a lot of extra variables. We only keep the variables we use. After loading their data, execute this command line. 

keep ccode year defense_dem defense_nodem  atwar civilwar LMILEX LMILEX1 PN6 LNRGDP  LNFOES LNFRIENDS DEMOC CWPRBCOW country_name v2x_polyarchy v2x_regime

*/ 


/* There are some minor errors in the DP data. 
I think DP created their variables and then merged them into the NOR data. 
However, NOR changed the COW codes for a few countries. */ 

* Germany/West Germany
replace defense_dem = 1 if year >= 1990 & year <= 2001 & ccode == 260
replace defense_nodem = 1 if year >= 1990 & year <= 2001 & ccode == 260
replace civilwar = 0 if year >= 1990 & year <= 2001 & ccode == 260
replace atwar =0 if year >= 1990 & year <= 1998 & ccode == 260
replace atwar = 1 if year == 1999 & ccode == 260
replace atwar = 0 if year == 2000 & ccode == 260
replace atwar = 1 if year == 2001 & ccode == 260

replace v2x_regime = 3 if year >= 1990 & year <= 2001 & ccode == 260 

list v2x_polyarchy year if year >= 1990 & year <= 2001 

replace v2x_polyarchy = .909 if year >= 1990 & year <= 1993 & ccode == 260
replace v2x_polyarchy = .905 if year == 1994 & ccode == 260
replace v2x_polyarchy = .889 if year >= 1995 & year <= 1997 & ccode == 260
replace v2x_polyarchy = .895 if year == 1998 & ccode == 260
replace v2x_polyarchy = .911 if year >= 1999 & year <= 2001 & ccode == 260

* Yemen
replace defense_dem = 0 if year >= 1991 & year <= 2001 & ccode == 678
replace defense_nodem = 1 if year >= 1991 & year <= 2001 & ccode == 678
replace atwar = 0 if year >= 1991 & year <= 2001 & ccode == 678
replace civilwar = 0 if year >= 1991 & year <= 2001 & ccode == 678
replace civilwar = 1 if year == 1994 & ccode == 678

replace v2x_regime = 1 if year >= 1990 & year <= 2001 & ccode == 678 

replace v2x_polyarchy = .204 if year == 1990 & ccode == 678

replace v2x_polyarchy = .299 if year == 1991 & ccode == 678
replace v2x_polyarchy = .295 if year == 1992 & ccode == 678
replace v2x_polyarchy = .326 if year == 1993 & ccode == 678
replace v2x_polyarchy = .332 if year == 1994 & ccode == 678
replace v2x_polyarchy = .335 if year == 1995 & ccode == 678
replace v2x_polyarchy = .335 if year == 1996 & ccode == 678
replace v2x_polyarchy = .297 if year == 1997 & ccode == 678
replace v2x_polyarchy = .290 if year == 1998 & ccode == 678
replace v2x_polyarchy = .281 if year == 1999 & ccode == 678
replace v2x_polyarchy = .302 if year == 2000 & ccode == 678

* Create binary Democracy indicator
gen vdem_dem = 0
replace vdem_dem = 1 if v2x_regime == 3 | v2x_regime == 2
replace vdem_dem = . if v2x_regime == .

* Create trichotomous regime indicator 
gen vdemtri = .
replace vdemtri = 2 if v2x_regime ==3
replace vdemtri = 2 if v2x_regime ==2
replace vdemtri = 1 if v2x_regime ==1
replace vdemtri = 0 if v2x_regime ==0

* Create binary external threat indicator based on the median
* External threat = 1 if a state's value on the NOR continuous threat measure is >= the median, 0 otherwise
egen pn6_50 = pctile(PN6), p(50)
gen ht50 = 0
replace ht50 = 1 if PN6 >= pn6_50
replace ht50 = . if PN6 == . 
label var ht50 "=1 if PN6 >= 50th percentile

* Create a binary threat indicator based on the 66th percentile
egen pn6_66 = pctile(PN6), p(66)
gen ht66 = 0
replace ht66 = 1 if PN6 >= pn6_66
replace ht66 = . if PN6 == . 
label var ht66 "=1 if PN6 >= 66th percentile

* Create a binary threat indicator based on the 33rd percentile 
egen pn6_33 = pctile(PN6), p(33) 
gen ht33 = 0
replace ht33 = 1 if PN6 >= pn6_33
replace ht33 = . if PN6 == . 
label var ht33 "=1 if PN6 >= 33rd percentile


/* Identify four country types. This is used for Table 1. 
Autocracies with high external threat
Autocracies with low external threat
Democracies with high external threat
Democracies with low external threat
*/ 

gen reg_threat = 0
replace reg_threat = 1 if vdem_dem == 0 & ht50 == 1
replace reg_threat = 2 if vdem_dem == 0 & ht50 == 0
replace reg_threat = 3 if vdem_dem == 1 & ht50 == 1
replace reg_threat = 4 if vdem_dem == 1 & ht50 == 0
label var reg_threat "1:aut_ht,2=aut_no_ht,3=dem_ht,4=dem_no_ht

* Create a variable for the annual change in the natural log of military expenditures
gen lmilexdelta1 = LMILEX - LMILEX1

label define threat_label 0 "Low Threat" 1 "High Threat"
label define regime_label 0 "Autocracy" 1 "Democracy"
label values ht50 threat_label
label values vdem_dem regime_label 

****************************************
* Table 1: Mean annual change in the natural log of military expenditures by Regime Type and Threat Level
bysort reg_threat: sum lmilexdelta1
****************************************

* Percentage of democracies and autocracies with high/low External threat
* Discussed in introduction 
tab ht50 vdem_dem, col

* Create the interaction manually. For some operations this helps.  
gen demXht50 = vdem_dem*ht50 

xtset ccode year

* Model 1: Lagged DV, AR1, PCSE
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m1

* Model 2: Lagged DV, AR1, FE
xtregar LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, fe
estimates store m2

* Model 3: Instrumental variable, robust SE
sort ccode year
ivregress 2sls  LMILEX i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP (LMILEX1 = L(1/2).(LNRGDP ht50)), robust first
estimates store m3


esttab m1 m2 m3, style(fixed) cells(b(star fmt(3)) se(par fmt(3))) ///
   legend varlabels(_cons constant) ///
   mtitles("Model 1" "Model 2" "Model 3") ///
	rename(LMILEX1 Lag_DV 1.vdem_dem Democracy 1.ht50 Threat 1.vdem_dem#1.ht50 Dem*Threat defense_dem Dem_Ally defense_nodem Aut_Ally atwar Int_War civilwar Civil_War LNRGDP Ln(GDP)) ///   
   order(Lag_DV Democracy Threat Dem*Threat Dem_OT Dem_CS Dem_OT*Threat Dem_CS*Threat Dem_Ally Aut_Ally Int_War Civil_War Ln(GDP)) ///
   varwidth(15) ///
   title("Table 1: Regression Estimates of Military Spending, 1952-2000") ///
   star(* 0.10 ** 0.05) /// 
   stats(N r2, fmt(0 3) label(N R^2)) ///
   addnotes("Notes:" ///
   "Note 1: Model 1: AR(1) process with panel corrected standard errors." ///
   "Note 2: Model 2: Fixed Effects, AR(1), and panel corrected standard errors" ///
   "Note 3: Model 3: Instrumental variable and robust standard errors") ///
   noomitted nobase wrap nonumbers

* Re-estimate Models 1-3 and calculate conditional average marginal effects and place them in a figure

* Model 1: Lagged DV, AR1, PCSE
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
margins, dydx(vdem_dem) at(ht50=(0 1)) post
eststo m1x

* Model 2: Lagged DV, AR1, FE
xtregar LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, fe
margins, dydx(vdem_dem) at(ht50=(0 1))  post
eststo m2x


* Model 3: Instrumental variable, robust SE
sort ccode year
ivregress 2sls  LMILEX i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP (LMILEX1 = L(1/2).(LNRGDP ht50)), robust first
margins, dydx(vdem_dem) at(ht50=(0 1))  post
eststo m3x

set scheme plottig
coefplot (m1x,label(Model 1) pstyle(p1) msymbol(O)) ///
(m2x, label(Model 2) pstyle(p5) msymbol(T)) ///
(m3x, label(Model 3) pstyle(p7) msymbol(D)), ///
xline(0) nolabel ///
title("Models 1-3, 1952-2000" "Average Marginal Effect of Democracy on Ln(Military Spending)") ///
ylabel(1 "Democracy at Low Threat" 2 "Democracy at High Threat") ///
note("Model 1: PCSE." "Model 2: Fixed Effects and AR(1) errors." "Model 3: Instrumental Variable." "Full results in Appendix") 

   
/* 
Here's another conditional average marginal effect figure. This one shows that the coefficient on the interaction term is statistically different from zero. 

* Re-estimate Model 1 and create a figure for the conditional marginal effect of the focal variables  
* Model 1
eststo m: xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)

margins, dydx(vdem_dem) at(ht50=(0(1)1) defense_dem=0 defense_nodem=1 atwar=0 civilwar=0) atmeans post

matrix b=e(b)
matrix b=b[1,3] \ b[1,4] \ b[1,4]-b[1,3]
matrix v=e(V)
matrix v=v[3,3] \ v[4,4] \ v[3,3]+v[4,4]-2*v[3,4]

svmat b
svmat v

replace v1=sqrt(v1)

g lb = b1-1.96*v1
g ub = b1+1.96*v1

*Verify difference is calculated correctly* 
est restore m
margins, dydx(vdem_dem) at(ht50=(0(1)1) defense_dem=0 defense_nodem=1 atwar=0 civilwar=0) atmeans pwcompare(ci)

list b1 lb ub in 3

g point = _n in 1/3
label define plab 1 "Low Threat" ///
2 "High Threat" ///
3 "Difference" 
label values point plab

twoway (scatter b1 point, color(black)) ///
( rcap lb ub point, lcolor(black)), ///
xlabel(1(1)3, valuelabel labsize(small) angle(horizontal)) ///
ytitle("Marginal Effect of Democracy" " ", size(medium)) ///
xtitle("External Threat") xscale(range(.85, 3.15)) ///
 legend(off) yline(0, lcolor(black) lpattern(solid)) ///
title("Conditional Marginal Effect of Democracy" "on Ln(Military Spending), by External Threat") ///
subtitle("Model 1, 1952-2000") scheme(plottigblind)

drop b1 v1 lb ub point
label drop plab

*/ 

*********************************
* Long run effects
*********************************

*Model 1
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)

*Long run effect of democracy at low threat
nlcom _b[1.vdem_dem]/(1-_b[LMILEX1])
matrix b=r(b)
scalar lr_dl = b[1,1]
matrix V=r(V)
scalar lr_dl_sd = sqrt(V[1,1])

*Long run effect of democracy at high threat
nlcom (_b[1.vdem_dem]+_b[1.vdem_dem#1.ht50])/(1-_b[LMILEX1])
matrix b=r(b)
scalar lr_dh = b[1,1]
matrix V=r(V)
scalar lr_dh_sd = sqrt(V[1,1])

*Long run interaction effect
nlcom _b[1.vdem_dem#1.ht50]/(1-_b[LMILEX1])
matrix b=r(b)
scalar lr_int = b[1,1]
matrix V=r(V)
scalar lr_int_sd = sqrt(V[1,1])

*Matrix
matrix b = lr_dl \ lr_dh \ lr_int
matrix sd = lr_dl_sd \ lr_dh_sd \ lr_int_sd

svmat b
svmat sd

g lb = b1-1.96*sd1
g ub = b1+1.96*sd1

g point = _n in 1/3
label define plab 1 "Effect of Democracy | Low Threat" ///
2 "Effect of Democracy | High Threat" ///
3 "Change in Effect of Democracy as Threat Increases" 
label values point plab

twoway (scatter point b1, color(black)) ///
( rcap lb ub point, hor lcolor(black)), ///
ylabel(1(1)3, valuelabel labsize(small) angle(horizontal)) ///
ytitle("") legend(off) xline(0, lcolor(black) lpattern(solid)) ///
title("Long Run Effect of Democracy on Military Spending," "Conditional on Threat") ///
scheme(plottigblind)

clear matrix
drop b1 sd1 lb ub point


preserve 
set matsize 10000

xtpcse LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)

matrix b=e(b)
matrix v=e(V)

set seed 1015

matrix mean_dem_low = ( 6.680771 \ 1 \ 0 \ 0 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)
matrix mean_dem_hi = ( 6.680771 \ 1 \ 1 \ 1 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)
matrix mean_aut_low = ( 6.680771 \ 0 \ 0 \ 0 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)
matrix mean_aut_hi = ( 6.680771 \ 0 \ 1 \ 0 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)

matrix hold_low_dem=J(21,3,.)
matrix hold_hi_dem=J(21,3,.)
matrix hold_low_aut=J(21,3,.)
matrix hold_hi_aut=J(21,3,.)

forval i = 1(1)21{

drawnorm s1-s10, cov(v) means(b) n(1000) clear
mkmat s1-s10, mat(m)

matrix point_dem_low = m*mean_dem_low
matrix point_dem_hi=m*mean_dem_hi
matrix point_aut_low=m*mean_aut_low
matrix point_aut_hi=m*mean_aut_hi

mata : st_matrix("mean_point_low_dem", mean(st_matrix("point_dem_low"))) 
matrix list mean_point_low_dem
scalar mean_point_low_dem=mean_point_low_dem[1,1]
mata : st_matrix("sort_low_dem", sort(st_matrix("point_dem_low"), 1))
scalar dem_low_lb=sort_low_dem[25,1]
scalar dem_low_ub=sort_low_dem[976,1]
matrix hold_low_dem[`i',1] =(dem_low_lb, mean_point_low_dem,  dem_low_ub)

mata : st_matrix("mean_point_hi_dem", mean(st_matrix("point_dem_hi"))) 
matrix list mean_point_hi_dem
scalar mean_point_hi_dem=mean_point_hi_dem[1,1]
mata : st_matrix("sort_hi_dem", sort(st_matrix("point_dem_hi"), 1))
scalar dem_hi_lb=sort_hi_dem[25,1]
scalar dem_hi_ub=sort_hi_dem[976,1]
matrix hold_hi_dem[`i',1] = (dem_hi_lb, mean_point_hi_dem , dem_hi_ub)

mata : st_matrix("mean_point_low_aut", mean(st_matrix("point_aut_low"))) 
matrix list mean_point_low_aut
scalar mean_point_low_aut=mean_point_low_aut[1,1]
mata : st_matrix("sort_low_aut", sort(st_matrix("point_aut_low"), 1))
scalar aut_low_lb=sort_low_aut[25,1]
scalar aut_low_ub=sort_low_aut[976,1]
matrix hold_low_aut[`i',1] =(aut_low_lb, mean_point_low_aut,  aut_low_ub)

mata : st_matrix("mean_point_hi_aut", mean(st_matrix("point_aut_hi"))) 
matrix list mean_point_hi_aut
scalar mean_hi_aut=mean_point_hi_aut[1,1]
mata : st_matrix("sort_hi_aut", sort(st_matrix("point_aut_hi"), 1))
scalar aut_hi_lb=sort_hi_aut[25,1]
scalar aut_hi_ub=sort_hi_aut[976,1]
matrix hold_hi_aut[`i',1] = (aut_hi_lb, mean_point_hi_aut, aut_hi_ub)


matrix mean_dem_low = ( mean_point_low_dem \ 1 \ 0 \ 0 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)
matrix mean_dem_hi = ( mean_point_hi_dem \ 1 \ 1 \ 1 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)
matrix mean_aut_low = ( mean_point_low_aut\ 0 \ 0 \ 0 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)
matrix mean_aut_hi = ( mean_point_hi_aut \ 0 \ 1 \ 0 \ 0 \ 1 \ 0 \ 0 \  10.36337 \ 1)

}


matrix all = (hold_low_dem \ hold_hi_dem \ hold_low_aut \ hold_hi_aut)

svmat all

g point = _n-1 in 1/21
replace point = _n-22 in 22/42
replace point = _n-43 in 43/63
replace point = _n-64 in 64/84

g est= 1 in 1/21
replace est =2 in 22/42
replace est=3 in 43/63
replace est=4 in 64/84

g dem=1 in 1/42
replace dem=0 in 43/84

label values dem regime_label 

g threat=0 in 1/84
replace threat=1 in 22/42
replace threat=1 in 64/84


twoway (scatter all2 point if threat==0, color(black)) ///
( rcap all1 all3 point if threat==0, lcolor(black)) ///
(scatter all2 point if threat==1, color(blue)) ///
( rcap all1 all3 point if threat==1, lcolor(blue)), ///
by(dem, title("Simulated Military Spending Conditional on Threat")) legend(order(1 "Low Threat" 3 "High Threat")) ///
xtitle("Years") ytitle("Ln(Mil Expenditure)") ///
scheme(plottigblind)

restore 


* Calculate summary statistics based on Model 1

xtpcse LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estpost sum LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem atwar civilwar LNRGDP if e(sample)

label var LMILEX "Ln(milex)
label var LMILEX1 "lag Ln(milex)
label var vdem_dem "Democracy
label var ht50 "External Threat
label var demXht50 "Dem*External Threat
label var defense_dem "Ally_Democracy
label var defense_nodem "Ally_Autocracy
label var atwar "Interstate War
label var civilwar "Civil War
label var LNRGDP "Ln(GDP)

esttab, label cell((mean(fmt(%9.2f)) sd(fmt(%9.2f)) min(fmt(%9.2f)) max(fmt(%9.2f)))) nonumber nomtitle


************************
* Robustness checks 
************************

* Random Effects Between Within Model
* Create all group means & deviations
gen z = vdem_dem*ht50
foreach var of varlist LMILEX1 vdem_dem ht50 defense_dem defense_nodem atwar civilwar LNRGDP z {
egen as`var' = mean(`var'), by(ccode)
gen ot`var'=`var'-as`var'
}

* Estimate Random Effects Between Within Model 
xtregar LMILEX asLMILEX otLMILEX otvdem_dem otht50 otz asvdem_dem asht50 asz asdefense_dem otdefense_dem otdefense_nodem asdefense_nodem  asatwar otatwar ascivilwar otcivilwar asLNRGDP otLNRGDP, re
estimates store m4 

set scheme plottig

coefplot (m4,label(REBW) pstyle(p1) msymbol(O)), ///
keep (asLMILEX1 otLMILEX1 otvdem_dem otht50 otz asvdem_dem asht50 asz asdefense_dem otdefense_dem otdefense_nodem asdefense_nodem asatwar otatwar ascivilwar otcivilwar asLNRGDP otLNRGDP) ///
xline(0) nolabel ///
rename(otvdem_dem=otDem otht50=otThreat otz=otDem*Threat asvdem_dem=asDEM asht50=asThreat asz=asDem*Threat ) ///   
title("Model 4: Random Effects Between Within Model Estimates") ///
caption("Each variable has an ot (over-time) fixed effect" "and an as (across-space) effect (see Bell and Jones (2015))")


* OLS with clustered SE
*reg LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, cluster(ccode)

* Are the results robust to dropping high threat democracies? 

* Drop Israel and United States
gen ius = 1 if ccode == 666 | ccode == 2
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP if ius !=1, p corr(psar)
estimates store m5

* Drop Democratic members of the UN Security Council 
gen dem_sec_council = 1 if ccode == 2 | ccode == 200 | ccode == 220
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP if dem_sec_council !=1, p corr(psar)
estimates store m6

* Drop top five democracies with the most high threat years: Israel, India, and Italy, the United States, and Denmark
gen high_threat_dems = 1 if ccode == 666 | ccode == 750 | ccode == 325 | ccode == 2 | ccode == 390 
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP if high_threat_dems !=1, p corr(psar)
estimates store m7

   esttab m5 m6 m7, style(fixed) cells(b(star fmt(3)) se(par fmt(3))) ///
   legend varlabels(_cons constant) ///
   mtitles("Model 5" "Model 6" "Model 7") ///
	rename(LMILEX1 Lag_DV 1.vdem_dem Democracy 1.ht50 Threat 1.vdem_dem#1.ht50 Dem*Threat defense_dem Dem_Ally defense_nodem Aut_Ally atwar Int_War civilwar Civil_War LNRGDP Ln(GDP)) ///   
   order(Lag_DV Democracy Threat Dem*Threat Dem_Ally Aut_Ally Int_War Civil_War Ln(GDP)) ///
   varwidth(15) ///
   title("Regression Estimates of Military Spending, 1952-2000") ///
   star(* 0.10 ** 0.05) /// 
   stats(N r2, fmt(0 3) label(N R^2)) ///
   addnotes("Notes:" ///
   "Note 1: Model 5: AR(1) and panel corrected standard errors (PCSE), dropping Israel and USA" ///
   "Note 2: Model 6: AR(1) and PCSE, dropping Security Council Democracies" ///
   "Note 3: Model 7: AR(1) and PCSE, dropping top five high threat democracies: Israel, India, Italy, USA, Denmark") ///
   noomitted nobase wrap nonumbers

   
   
* Continuous democracy/polyarchy measure from VDEM: v2x_polyarchy
xtpcse LMILEX LMILEX1 c.v2x_polyarchy##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m8

gen d6 = DEMOC >= 6
replace d6 = . if DEMOC == . 
tab d6

* Binary democracy based on Polity indicator, 6+ equals 1, 0 otherwise
xtpcse LMILEX LMILEX1 i.d6##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m9

* All values of Polity democracy-autocracy index indicator
xtpcse LMILEX LMILEX1 c.DEMOC##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m10

   esttab m8 m9 m10, style(fixed) cells(b(star fmt(3)) se(par fmt(3))) ///
   legend varlabels(_cons constant) ///
   mtitles("Model 8" "Model 9" "Model 10") ///
	rename(LMILEX1 Lag_DV v2x_polyarchy Polyarchy 1.ht50 Threat 1.ht50#c.v2x_polyarchy Polyarchy*Threat 1.vdem_dem Democracy 1.d6 Polity_Dem 1.d6#1.ht50 Polity_Dem*Threat DEMOC Polity_Index 1.ht50#c.DEMOC Polity_Index*Threat defense_dem Dem_Ally defense_nodem Aut_Ally atwar Int_War civilwar Civil_War LNRGDP Ln(GDP)) ///   
   varwidth(20) ///
   title("Regression Estimates of Military Spending, 1952-2000") ///
   star(* 0.10 ** 0.05) /// 
   stats(N r2, fmt(0 3) label(N R^2)) ///
   addnotes("Notes:" "Note 1: Each model estimated as AR(1) process with panel corrected standard errors." ///
   "Note 2: Model 8 uses VDEM's continuous democracy/polyarchy measure." ///
   "Note 3: Model 9 uses the Polity democracy-autocracy index coded as a binary indicator with 6 and higher equal to 1, 0 otherwise." ///
   "Note 4: Model 10 uses the full Polity democracy-autocracy index.") ///
   noomitted nobase wrap nonumbers



* NOR continuous threat measure 
xtpcse LMILEX LMILEX1 i.vdem_dem##c.PN6 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m11


* Change the threshold for determining the presence of a significant external threat
* High external threat =1 if probability of fatal MID > 66th percentile 
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht66 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m12

* High external threat =1 if probability of fatal MID > 33rd percentile 
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht33 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m13

* Threat measured as territorial claim 

* First, correct some missing values due to COW code differences
recode icowterr .=0 if ccode == 260 & year >= 1991 & year <= 2001
recode icowterr .=0 if ccode == 678 & year >= 1991 & year <= 2001 


* Model 1: Threat measured as territorial claim 
xtpcse LMILEX LMILEX1 i.vdem_dem##i.icowterr defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m14


   esttab m11 m12 m13 m14, style(fixed) cells(b(star fmt(3)) se(par fmt(3))) ///
   legend varlabels(_cons constant) ///
   mtitles("Model 11" "Model 12" "Model 13" "Model 14") ///
	rename(LMILEX1 Lag_DV v2x_polyarchy Polyarchy 1.ht50 Threat 1.ht50#c.v2x_polyarchy Polyarchy*Threat 1.vdem_dem Democracy PN6 Threat_Continuous 1.vdem_dem#c.PN6 Dem*Threat_Cont 1.ht66 Threat_66 1.vdem_dem#1.ht66 Dem*Threat66 1.ht33 Threat_33 1.vdem_dem#1.ht33 Dem*Threat33 1.icowterr Terr_Dispute 1.vdem_dem#1.icowterr Dem*Terr_Dispute defense_dem Dem_Ally defense_nodem Aut_Ally atwar Int_War civilwar Civil_War LNRGDP Ln(GDP)) ///   
   varwidth(20) ///
   title("Regression Estimates of Military Spending, 1952-2000") ///
   star(* 0.10 ** 0.05) /// 
   stats(N r2, fmt(0 3) label(N R^2)) ///
   addnotes("Notes:" "Note 1: Each model estimated as AR(1) process with panel corrected standard errors." ///
   "Note 2: Model 11 uses the continuous threat measure from Nordhaus, Oneal, and Russett (NOR) (2012)." ///
   "Note 3: Model 12 uses binary threat measure, with threat equal to 1 if NOR continuous threat measure greater than 66th percentile." ///
   "Note 4: Model 13 uses binary threat measure, with threat equal to 1 if NOR continuous threat measure greater than 33rd percentile." ///
   "Note 5: Model 14: Threat measured as presence of Territorial Dispute (Frederick et al (2017)") ///
   noomitted nobase wrap nonumbers


* AIC Model Comparison

* Compare AIC for a model with the interactive term and for one without the term
quietly reg LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, cluster(ccode)
estat ic

quietly reg LMILEX LMILEX1 vdem_dem ht50 defense_dem defense_nodem atwar civilwar LNRGDP, cluster(ccode)
estat ic


* Change the DV to Defense Burden

* Model 1 with ln(defense burden)
xtpcse LMILGDP l1.LMILGDP i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, p corr(psar)
estimates store m15
* Model 2 with Defense Burden
xtregar LMILGDP l1.LMILGDP i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP, fe
estimates store m16
* Model 3 with Defense Burden 
sort ccode year
ivregress 2sls  LMILGDP i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP (l1.LMILGDP = L(1/2).(LNRGDP ht50)), robust first
estimates store m17


esttab m15 m16 m17, style(fixed) cells(b(star fmt(3)) se(par fmt(3))) ///
   legend varlabels(_cons constant) ///
   mtitles("Model 15" "Model 16" "Model 17") ///
	rename(l1.LMILGDP Lag_DV 1.vdem_dem Democracy 1.ht50 Threat 1.vdem_dem#1.ht50 Dem*Threat defense_dem Dem_Ally defense_nodem Aut_Ally atwar Int_War civilwar Civil_War LNRGDP Ln(GDP)) ///   
   order(Lag_DV Democracy Threat Dem*Threat Dem_Ally Aut_Ally Int_War Civil_War Ln(GDP)) ///
   varwidth(15) ///
   title("Regression Estimates of Defense Burden, 1952-2000") ///
   star(* 0.10 ** 0.05) /// 
   stats(N r2, fmt(0 3) label(N R^2)) ///
   addnotes("Notes:" ///
   "Note 1: Dependent variable is LN(defense burden) (Military Spending/GDP)" ///
   "Note 2: Model 15: AR(1) process with PCSE." ///
   "Note 3: Model 16: Fixed Effects and AR(1)" ///
   "Note 4: Model 17: Instrumental Variable Model, robust standard errors") ///
   noomitted nobase wrap nonumbers

   
* Only countries with at least one autocratic neighbor

* Model 1: Autocratic neighbors: countries with at least one autocratic neighbor 
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP if autNdummy==1, p corr(psar)
estimates store m18
* Model 2, only Autocratic neighbors
xtregar LMILEX LMILEX1  i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP if autNdummy==1, fe
estimates store m19
* Model 3, only Autocratic neighbors
sort ccode year
ivregress 2sls  LMILEX i.vdem_dem##i.ht50 defense_dem defense_nodem atwar civilwar LNRGDP (LMILEX1 = L(1/2).(LNRGDP ht50)) if autNdummy==1, robust first
estimates store m20



esttab m18 m19 m20, style(fixed) cells(b(star fmt(3)) se(par fmt(3))) ///
   legend varlabels(_cons constant) ///
   mtitles("Model 18" "Model 19" "Model 20" "Model 21") ///
	rename(LMILEX1 Lag_DV 1.vdem_dem Democracy 1.ht50 Threat 1.vdem_dem#1.ht50 Dem*Threat defense_dem Dem_Ally defense_nodem Aut_Ally atwar Int_War civilwar Civil_War LNRGDP Ln(GDP)) ///   
   order(Lag_DV Democracy Threat Dem*Threat Dem_Ally Aut_Ally Int_War Civil_War Ln(GDP)) ///
   varwidth(15) ///
   title("Regression Estimates of Military Spending, 1952-2000" ///
   "Only Countries With At Least One Autocratic Neighbor") ///
   star(* 0.10 ** 0.05) /// 
   stats(N r2, fmt(0 3) label(N R^2)) ///
   addnotes("Notes:" ///
   "Note 1: Dependent variable is LN(Military Spending)" ///
   "Note 2: Model 18: AR(1) process with PCSE." ///
   "Note 3: Model 19: Fixed Effects and AR(1)" ///
   "Note 4: Model 20: Instrumental Variable and robust standard errors") ///
   noomitted nobase wrap nonumbers


* Model: Lagged DV, AR1, PCSE and interactions of democracy and war
xtpcse LMILEX LMILEX1 i.vdem_dem##i.ht50 defense_dem defense_nodem i.vdem_dem##i.atwar i.vdem_dem##i.civilwar LNRGDP, p corr(psar)
estimates store m21

set scheme plottig

coefplot (m21,label(extra_interactions) pstyle(p1) msymbol(O)), ///
xline(0) nolabel ///
rename(LMILEX1=Lag_DV 1.vdem_dem=Democracy 1.ht50=Threat 1.vdem_dem#1.ht50=Dem*Threat defense_dem=Ally_Dem defense_nodem=Ally_Aut 1.atwar=War 1.vdem_dem#1.atwar=Dem*War 1.civilwar=Civil_War 1.vdem_dem#1.civilwar=Dem*Civil_War LNRGDP=LNRGDP _cons=Constant) ///   
title("Model 1 with Additional Democracy and War Interactions")


***********************************************
* Out of sample predictions: interactive model 
***********************************************

* Out of sample predictions: interactive model 

set matsize 500 

* Make a place to store the results
matrix MSPE_base = J(500,1,.)

* execute each of the four groups below ten times
forvalues h=1/500 {
preserve
* obtain estimation sample and assign each observation to one of four groups
quietly reg LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem atwar civilwar LNRGDP, cluster(ccode)
xtile group=uniform() if e(sample), nq(4)
drop if group==.
gen yhat=.


forvalues i=1/4 {
* estimate model and obtain predictions. Do this for each of the four groups. 
quietly reg LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem atwar civilwar LNRGDP if group~=`i', cluster(ccode)
quietly predict yhat_i

quietly replace yhat=yhat_i if group==`i'
quietly drop yhat_i
}

* calculate MSPE 
gen sqerr_model=(LMILEX-yhat)^2
quietly sum sqerr_model
matrix MSPE_base[`h',1]=r(mean)

capture drop group yhat

restore
}

*svmat the matrix then take the mean
svmat MSPE_base
quietly sum MSPE_base

*Drop when done
clear matrix


* Out of sample predictions: model dropping interaction term

* Make a place to store the results
matrix MSPE_noint = J(500,1,.)

* execute each of the four groups below ten times
forvalues h=1/500 {
preserve
* obtain estimation sample and assign each observation to one of four groups
quietly reg LMILEX LMILEX1 vdem_dem ht50 defense_dem defense_nodem atwar civilwar LNRGDP, cluster(ccode)
xtile group=uniform() if e(sample), nq(4)
drop if group==.
gen yhat=.


forvalues i=1/4 {
* estimate model and obtain predictions. Do this for each of the four groups. 
quietly reg LMILEX LMILEX1 vdem_dem ht50 defense_dem defense_nodem atwar civilwar LNRGDP if group~=`i', cluster(ccode)
quietly predict yhat_i

quietly replace yhat=yhat_i if group==`i'
quietly drop yhat_i
}

* calculate MSPE 
gen sqerr_model=(LMILEX-yhat)^2
quietly sum sqerr_model
matrix MSPE_noint[`h',1]=r(mean)

capture drop group yhat

restore
}

*svmat the matrix then take the mean
svmat MSPE_noint
quietly sum MSPE_noint

*Drop when done
clear matrix


* Drop both war vars 

* Make a place to store the results
matrix MSPE_nowar = J(500,1,.)

* execute each of the four groups below ten times
forvalues h=1/500 {
preserve
* obtain estimation sample and assign each observation to one of four groups
quietly reg LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem LNRGDP, cluster(ccode)
xtile group=uniform() if e(sample), nq(4)
drop if group==.
gen yhat=.


forvalues i=1/4 {
* estimate model and obtain predictions. Do this for each of the four groups. 
quietly reg LMILEX LMILEX1 vdem_dem ht50 demXht50 defense_dem defense_nodem LNRGDP if group~=`i', cluster(ccode)
quietly predict yhat_i

quietly replace yhat=yhat_i if group==`i'
quietly drop yhat_i
}

* calculate MSPE 
gen sqerr_model=(LMILEX-yhat)^2
quietly sum sqerr_model
matrix MSPE_nowar[`h',1]=r(mean)

capture drop group yhat

restore
}

*svmat the matrix then take the mean
svmat MSPE_nowar
quietly sum MSPE_nowar

*Drop when done
clear matrix

sum MSPE_base MSPE_noint MSPE_nowar
drop MSPE_base MSPE_noint MSPE_nowar 


* Interflex

interflex LMILEX vdem_dem PN6 LMILEX1 defense_dem defense_nodem atwar civilwar LNRGDP, nbins(3) cluster (ccode)


* Mediation Analysis

medeff (probit ht50 vdem_dem LMILEX1 defense_dem defense_nodem atwar civilwar LNRGDP) (regress LMILEX vdem_dem ht50 LMILEX1 defense_dem defense_nodem atwar civilwar LNRGDP) , treat(vdem_dem) mediate(ht50) vce(cluster ccode) sims(1000)

medeff (probit icowterr vdem_dem LMILEX1 defense_dem defense_nodem atwar civilwar LNRGDP) (regress LMILEX vdem_dem icowterr LMILEX1 defense_dem defense_nodem atwar civilwar LNRGDP) , treat(vdem_dem) mediate(icowterr) vce(cluster ccode) sims(1000)

