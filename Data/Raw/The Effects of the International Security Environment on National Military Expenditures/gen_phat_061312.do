/*produce phat used in Nordhaus, Oneal, and Russett IO July 2012; modified test_boot_baseeqn_061312: 06-13-12*/


/*no peaceyrs, interpolations, pop>500 in eqn 1: 4-13-2011*/

set mem 200000
set matsize 500
 

use "\\\asfs.asnet.ua-net.ua.edu\users$\home\joneal\My Documents\DEFBURD\stanford\or070809.dta"

desc 
summ

keep if year>1949
set seed 02061961

gen dependa=trade/cgdp_a
gen dependb=trade/cgdp_b
drop trade 

gen smldep=dependa if dependa<=dependb & dependb~=.  
replace smldep=dependb if dependb<dependa & dependa~=.  

replace smldep=. if dependa>1 | dependb>1
drop depend*

gen logdstab=ln(distance)

/*set non-contiguous as baseline*/
tab contig, gen(contig)
drop contig6 

gen contig13=1 if contig1==1 | contig2==1 | contig3==1
replace contig13=0 if contig13==.

gen dircont=1 if contig<6
replace dircont=0 if contig==6

gen dyadid=(statea*1000)+stateb

/*calculate real GDPs in millions constant 2000 $*/
gen rgdpa=rgdp2000pca*pop_a/1000
gen rgdpb=rgdp2000pcb*pop_b/1000

/*calculate probability of winning based on GDPs*/
gen lrggdp=rgdpb if rgdpa<=rgdpb & rgdpb~=.  
replace lrggdp=rgdpa if rgdpb<rgdpa & rgdpa~=.  

gen pwin_gdp=lrggdp/(rgdpa+rgdpb)

/*calc ratio of larger gdp to world gdp*/
gen nlrggdp=lrggdp/worldgdp

gen smldem=polity_a if polity_a<=polity_b & polity_b~=.
replace smldem=polity_b if polity_b<polity_a & polity_a~=.
gen lrgdem=polity_b if polity_a<=polity_b & polity_b~=.
replace lrgdem=polity_a if polity_b<polity_a & polity_a~=.

/*Hegre's code for revised systsize: lnNt*/
sort year statea
capture drop Nt
by year statea: egen Nt= count(stateb) if statea==2 
replace Nt = Nt + 1 
replace Nt = Nt[_n-1] if statea!=2 & year==year[_n-1] 
gen lnNt = ln(Nt) 
summ lnNt 
replace lnNt = lnNt - 4.32    
/*NB: 4.32 is minimum value of lnNt from previous summarize command.*/
replace lnNt = 0 if dircont == 1
sort statea stateb year

#del ;
/*fill missing RHS w/ interpolated values*/
#del cr
quietly by statea stateb: ipolate smldem year, gen(smldem_ip)
quietly by statea stateb: ipolate lrgdem year, gen(lrgdem_ip)
summ smldem* lrgdem* 

#del ;
/*NOR specification, Table 1, col. 3, from Xi's boot_baseeqn_050511.do*/
logit fatinv1 smldem_ip lrgdem_ip smldep contig13 logdstab pwin_gdp allies nlrggdp lnNt if  pop_a>500 & pop_a~=. & pop_b>500 & pop_b~=.;
predict p_fatal_ab;
compress;

#del cr
keep if p_fatal_ab~=.

#del ;
stack statea stateb year p_fatal_ab stateb statea year p_fatal_ab, into(state target year p_fatal_ab) clear;
#del cr

/*calculate 1-[(1-p1)(1-p2)...(1-pn)]*/
gen lnp_pce_ab=ln(1-p_fatal_ab)

sort state year target
quietly by state year: egen lnp_peace=sum(lnp_pce_ab)
drop _s target p_fatal_ab
sort state year
quietly by state year: keep if _n==_N

*rename p_fatal to indicate no peaceyrs in eq1
gen p_fatal_nopy=1-exp(lnp_peace)
drop lnp_peace lnp_pce_ab

/*p_fatal_nopy is for year t+1 so change year to be consistent w/ lnrmilex; 1951 is
first year of valid esimates of p_fatal_nopy*/
replace year=year+1 
keep if year>1950
sort state year

ren p_fatal_nopy phat_061312

summ

sort state year
lab data "p-hat w/ interpolated values, no py; test_boot_baseeqn.do"
lab var phat_061312 "1-[(1-p_fatal1)(1-p_fatal2)...] w/o peaceyrs & w/ ipolated RHS values"
lab var year "p_fatal_nopy in year t, not t+1"

save phat_061312, replace



drop _all
insheet using "\\\asfs.asnet.ua-net.ua.edu\users$\home\joneal\My Documents\defburd\io_final\bill's final files\variables_macro_041111.txt"

keep state year phat

sort state year

desc 
summ

merge state year using "\\\asfs.asnet.ua-net.ua.edu\users$\home\joneal\My Documents\defburd\io_final\bill's final files\phat_061312"
tab _m
drop _m

reg phat_061312 phat, nocons





exit















use "\\\asfs.asnet.ua-net.ua.edu\users$\home\joneal\My Documents\DEFBURD\io_final\p_fatal_nopy.dta"

merge state year using "\\\asfs.asnet.ua-net.ua.edu\users$\home\joneal\My Documents\DEFBURD\nw2\nor042409.dta"

set seed 02061961

estsimp reg lnrmx_ppp p_fatal_nopy lnrgdp, cluster(state)
predict mx

summ b*

*hypothetical cases with p_fatal at 10% and 90%
summ lnrgdp
summ p_fatal_nopy, det

setx p_fatal_nopy .085 lnrgdp 10.25
simqi, listx
disp exp(6.23)
disp exp(.10)

setx p_fatal_nopy .390
simqi, listx
disp exp(7.00)
disp exp(.09)

gen lnfoes=ln(foes)
gen lnfriends=ln(friends)
drop b*

estsimp reg lnrmx_ppp p_fatal_nopy lnrgdp nfatalities actual_pfat lnfriends lnfoes democ, cluster(state)
summ b*


*twoway line mx lnrmx_ppp year if state==2

exit

gen n=_n
li n if state==2 & year==1953
li n if state==2 & year==2000

li n if state==920 & year==1953

*est for US in 1953
setx [3]
simqi, listx
disp exp(12.84)

*actual for US in 1953
li lnrmx_ppp in 3
disp exp(12.29)

*est for NewZealand in 1953
setx [6472]
simqi, listx
disp exp(5.93)

*est for US in 2000
setx [50]
simqi, listx
disp exp(13.03)

*actual for US in 2000
li lnrmx_ppp in 50
disp exp(12.62)

*est for US in 2000 with p_fatalr set at 1953 level
setx p_fatal_nopy .97
simqi, listx
disp exp(14.48)





