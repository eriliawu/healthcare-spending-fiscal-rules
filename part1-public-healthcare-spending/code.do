* do file

clear all
capture log close
set more off

***jan 29***new model*****healthcare spending trend*****************************
import excel "health exp by rule_trend.xlsx", sheet("trend") firstrow clear
rename D trend
rename gdp pct_spending
keep country year trend pct
replace trend=trend*100 // the original trend is expressed [0,1]
label var pct_spend "gov healthcare exp as % of GDP"

*ssc install unique
drop if missing(country)
merge 1:1 country year using "master_data.dta"
drop _mer

global control ln_gdp ln_pop ln_debt bailout // minus the gate keeper effect
global y ln_govexp
global fe i.country i.year

* sensitivity check
* use health spending trend
areg $y fr##c.trend $control i.year, absorb(country)
est table, keep(fr fr##c.trend $control) b se p

areg $y fr##c.pct $control i.year, absorb(country) // doesnt make sense to use this var

* interact with pre-/post-recession
eststo clear
eststo: quietly areg $y fr fr##c.trend $control i.year, absorb(country)
eststo: quietly areg $y er er##c.trend $control i.year, absorb(country)
eststo: quietly areg $y bbr bbr##c.trend $control i.year, absorb(country)
eststo: quietly areg $y i.bbr_d i.bbr_d##c.trend $control i.year, absorb(country)
esttab using "sensitivity_FR_trend.rtf", replace b(2) se(2) nogap ar2

* table 1 fiscal rules and healthcare cost
* findit eststo
eststo clear
foreach var in fr er i.bbr_d bbr {
	eststo: xi: reg $y `var' $control $fe
}
.
foreach var in fr er bbr {
	eststo: xi: reg $y `var' $control $fe if mand==0
}
.
foreach var in fr er bbr {
	eststo: xi: reg $y `var' $control $fe if mand_us==0
}
.
esttab using "table1_fr_.rtf", ar2 b(2) se(2) replace

* table 2 fiscal rules 1 & 2 years lag and healthcare cost
* lag year = 1, 2 years
drop bbr1 bbr2
drop fr1 fr2
drop er1 er2

foreach var in fr er bbr {
	bysort nr: gen `var'1 = `var'[_n-1]
	bysort nr: gen `var'2 = `var'[_n-2]
}
.

eststo clear
foreach var in fr1 fr2 er1 er2 bbr1 bbr2 {
	eststo: xi: reg $y `var' $control1 $fe
}
.
esttab using "table2_lag_with_gk.rtf", ar2 b(2) se(2) replace

eststo clear
foreach var in fr1 fr2 er1 er2 bbr1 bbr2 {
	eststo: xi: reg $y `var' $control2 $fe
}
.
esttab using "table2_lag_without_gk.rtf", ar2 b(2) se(2) replace

* table 3 supporting procedures
* ER*ER legal basis
* regroup ER national legal basis
/* gen er_nat=2
replace er_nat=1 if er_nat_basis==1 | er_nat_basis==2
replace er_nat=0 if er_nat_basis==0
* drop er_nat
gen  er_legal=er*er_nat */

gen er_nat1=(er_nat_basis==1 | er_nat_basis==2)
gen er_nat2=(er_nat_basis==3 | er_nat_basis==4 | er_nat_basis==5)
gen er_legal1=er*er_nat1
gen er_legal2=er*er_nat2
xi: reg $y er er_legal1 er_legal2 er_nat1 er_nat2 $control2 $fe
test er er_legal1 er_legal2 // F test shows joint significance

tab2 er er_nat

* xi: reg ln_govexp er i.er_legal gk ln_debt ln_gdp ln_pop bailout i.country i.year
* test er i.er_legal // er_legal not found?????

* BBR*BBR legal basis
/* gen bbr_nat=2
replace bbr_nat=1 if bbr_nat_basis==1 |  bbr_nat_basis==2
replace bbr_nat=0 if bbr_nat_basis==0
gen bbr_legal=bbr*bbr_nat */

gen bbr_nat1=(bbr_nat_basis==1 | bbr_nat_basis==2)
gen bbr_nat2=(bbr_nat_basis==3 | bbr_nat_basis==4 | bbr_nat_basis==5)
gen bbr_legal1=bbr*bbr_nat1
gen bbr_legal2=bbr*bbr_nat2
xi: reg $y bbr bbr_legal1 bbr_legal2 bbr_nat1 bbr_nat2 $control2 $fe
test bbr bbr_legal1 bbr_legal2 bbr_nat1 bbr_nat2 // F test shows joint significance

tab2 bbr bbr_nat

* bugdet ceiling
gen fr_ceil=fr*ceil
xi: reg $y fr fr_ceil ceil $control2 $fe
test fr fr_ceil // fr and fe_ceil jointly significant

tab2 fr ceil

* independent body setting
gen fr_ind=fr*ind
xi: reg $y fr fr_ind ind $control2 $fe
test fr fr_ind

tab2 fr ind

* independent body monitoring
gen fr_mn=fr*ind_mn
xi: reg $y fr fr_mn ind_mn $control2 $fe
test fr fr_mn

tab2 fr ind_mn

* integrate into table 4
eststo clear
eststo: xi: reg $y er er##i.er_nat i.er_nat $control1 $fe
eststo: xi: reg $y bbr bbr##i.bbr_nat i.bbr_nat $control1 $fe
eststo: xi: reg $y fr fr_ceil ceil $control1 $fe
test fr fr_ceil ceil //F test shows joint significance
eststo: xi: reg $y fr fr_ind ind $control1 $fe
test fr fr_ind ind //F test shows joint significance
eststo: xi: reg $y fr fr_mn ind_mn $control1 $fe
test fr fr_mn ind_mn //F test shows joint significance
esttab using "table3_supporting_with_gk1.rtf", ar2 b(2) se(2) replace

eststo clear
eststo: xi: reg $y er er##i.er_nat i.er_nat $control2 $fe
eststo: xi: reg $y bbr bbr##i.bbr_nat i.bbr_nat $control2 $fe
eststo: xi: reg $y fr fr_ceil ceil $control2 $fe
test fr fr_ceil ceil //F test shows joint significance
eststo: xi: reg $y fr fr_ind ind $control2 $fe
test fr fr_ind ind //F test shows joint significance
eststo: xi: reg $y fr fr_mn ind_mn $control2 $fe
test fr fr_mn ind_mn //F test shows joint significance
esttab using "table3_supporting_without_gk1.rtf", ar2 b(2) se(2) replace

log close

******************** Sep 8 *****************************************************
************* address BMC review ***********************************************
********************************************************************************

* create 65+ pop var
gen pop65 = ratio65/100*pop/1000000
gen pop_mil = pop/1000000
sum pop65 pop_m
label var pop65 "population age 65+ (mil)"
label var pop_m "population in million"
gen ln_pop65 = ln(pop65)
gen ln_pop_mil = ln(pop_m)
sum ln_pop6 ln_pop_m

* change pop to 65+
foreach var in fr er i.bbr_d bbr {
	areg $y `var' $control3 i.year, absorb(country)
}
.

* associate FR rules with long term growth
twoway (scatter ln_gdp year if er==0, msize(0.5) m(Oh) mc(black) mlw(vthin)) ///
		(scatter ln_gdp year if er==1, msize(0.5) m(Dh) mc(black) mlw(vthin)) ///
		(lfit ln_gdp year if er==0, lpattern(solide) lcolor(black) lwidth(vthin)) ///
		(lfit ln_gdp year if er==1, lpattern(dash) lcolor(black) lwidth(vthin)), ///
		legend(label(1 "Log GDP Without ER") label(2 "Log GDP With ER") label(3 "Linear Fit Without ER") label(4 "Linear Fit With ER")) ///
		legend(order (1 2 3 4)) title("Distribution of GDP, By Expenditure Rule") ///
		ytitle("Log GDP")
graph export "gdp distribution.png", as(png) replace
		
*twoway (scatter gdp_growth year, msize(0.5) m(Oh) mc(black)), by(er)

****sep 14*** reg with additional healthcare utilization vars*******************
***essentially recreate table 1 with healthcare utilization vars as y***********
*vars: consultation inpatient_discharge length occupancy_rate
sum cons inpatient_discharge length occu

areg cons fr $control2 i.year, absorb(country) //nothing promising from this set
areg cons er $control2 i.year, absorb(country)
areg cons bbr $control2 i.year, absorb(country)

areg length fr $control2 i.year, absorb(country) //more promising results
areg leng er $control2 i.year, absorb(country)
areg leng bbr $control2 i.year, absorb(country)

areg occu fr $control2 i.year, absorb(country) //p-value is mixed, but +/- on beta is negative
areg occu er $control2 i.year, absorb(country)
areg occu bbr $control2 i.year, absorb(country)

areg cons fr $control2 i.year, absorb(country)
areg cons er $control2 i.year, absorb(country)
areg cons bbr $control2 i.year, absorb(country)

gen ln_inpatient=ln(inpatient_discharge)
areg ln_inpatient fr $control2 i.year, absorb(country)
areg ln_inp er $control2 i.year, absorb(country)
areg ln_inp bbr $control2 i.year, absorb(country)

*make a table
eststo clear
foreach y in cons length occu ln_inpati {
	foreach x in fr er bbr {
		eststo: areg `y' `x' $control2 i.year, absorb(country)
		}
}
.
esttab using "table1_healtcare_utilization.rtf", ar2 b(2) se(2) replace
erase "additional_var.dta"
