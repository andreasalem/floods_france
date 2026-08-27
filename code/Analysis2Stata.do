********************************************************************************
* Set-Up
********************************************************************************
/* Dependencies:

// LP-DiD
ssc install lpdid, replace
ssc install reghdfe, replace
ssc install boottest, replace
ssc install egenmore, replace


// Matching
ssc install cem, replace 
ssc install kmatch

// Other
ssc install winsor // removes outliers 

*/

/*

0: Set globals
1: Load data
2: Descriptive stats
3: Build ATT weights
4: LP-DiD (absorbing)
   - Specification 1
   - ...
5: LP-DiD (non-absorbing)
*/

********************************************************************************
* 0 - Set file directories as globals
********************************************************************************

// Define main folder
if "`c(username)'" == "F1CHOCS_A_SALEM00"{
	global repo "C:/Users/Public/Documents/Fontaine/Floods_Shock"
}


// Define folder globals
global clean_data 		 ${repo}/1 - Data processing/Clean
global EAIP_data 		 ${repo}/1 - Data processing/Import/EAIP
global output_figures    ${repo}/2 - Data analysis/Figures
global output_tables     ${repo}/2 - Data analysis/Tables




// Define file globals
global data_sub 			   "${clean_data}/subsamples/sample_2010_2019.dta"
global ape_crosswalk 		   "${clean_data}/crosswalk_APEs.dta"
global communes_dt_2010_2019   "${clean_data}/communes_dt_2010_2019.dta"
global EAIP_risk			   "${clean_data}/flooding/commune_EAIP_risk_profile.dta"
global coastal_communes 	   "${clean_data}/flooding/coastal_communes.dta"
global urban_communes 	   	   "${clean_data}/flooding/urban_rural_communes.dta"
global income_communes 	   	   "${clean_data}/flooding/poverty_communes.dta"



********************************************************************************
* 1 - Data import (2010-2019 subsample)
********************************************************************************

use "$data_sub", replace

destring SIRET, replace force
format SIRET %15.0fc
sort SIRET ANNEE

// specify variables identifying units of observation + flood statistics + flood risk index
local vars_init SIREN SIRET ANNEE INSEE_COM ZEMPT /// 
				emp_share mono_etab ape_diff ///
				flood_dummy duration_category total_floods floods_last_10y floods_last_5y floods_last_3y 				floods_last_2y floods_last_year_dummy years_since_last_flood ///
				flood_risk_index_RP100

// specify outcome variables
local outcomes ETP EFF_ALL EFF_3112 EFF_MOY_ET HOURS HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 CDI CDD interim othercontract redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET

// keep selected variables
keep `vars_init' `outcomes'

// replace ape_diff with updated sector of activity
merge m:1 ape_diff using "$ape_crosswalk", keep(match master) nogenerate 
drop ape_diff Division libelle intitule  // Keep rev2 Section and Final_Sector

// add coastal dummy (categories are: Mer, Lac, Estuaire.
merge m:1 INSEE_COM using "$coastal_communes", keep(match master) nogenerate
foreach var of varlist litorale litorale_estuary litorale_lake litorale_sea{
	replace `var' = 0 if missing(`var')
}



// add urban dummy (categories are: urbain dense, urbain densite intermediaire, rural autonome peu dense, rural autonome tres peu dense, rural sous faible influence d'un pole, rural sous forte influence d'un pole. I include a dummy "urbain" and variable for rural or urban type.
merge m:1 INSEE_COM using "$urban_communes", keep(match master) nogenerate

// add Niveau de Vie (disposale income of a household, from FILOSOFI data and aggregated at commune level: I select categories low-, medium-, high- income commune based on the average disposable income in that commune
merge m:1 INSEE_COM using "$income_communes", keep(match master) nogenerate
gen income_category_temp = .
replace income_category_temp = 1 if income_category == "Low income (bottom 30%)"
replace income_category_temp = 2 if income_category == "Middle income (30-90%)"
replace income_category_temp = 3 if income_category == "High income (top 10%)"
drop income_category
rename income_category_temp income_category

label define income_label ///
	1 "Low income (bottom 30%)" ///
	2 "Middle income (30-90%)" ///
	3 "High income (top 10%)"
label values income_category income_label

// encode variables
encode Section, gen(Section_destr)
drop Section
rename Section_destr Section

encode INSEE_COM, gen(INSEE_COM_destr)
drop   INSEE_COM
rename INSEE_COM_destr INSEE_COM


// remove outliers (winsorization replaces values outside 1th-99th percentile with first inward value) 
local financial 	redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET 
local non_financial ETP EFF_3112 HWAGE AVGWAGE_3112 CDI CDD interim othercontract

foreach var of local non_financial {
	winsor `var', p(0.01) gen(`var'_winsor)
	replace `var' = `var'_winsor
	drop `var'_winsor
} // I keep financials as they are, since they display heavy tails

// compute share of CDI, CDD, interim and othercontract per ANNEE-SIRET
egen total_contracts = rowtotal(CDI CDD interim othercontract)

local contract_type CDI CDD interim othercontract
foreach var of local contract_type {
  gen share_`var' = `var' / total_contracts
}
drop CDI CDD interim othercontract

// encode duration_category
gen flood_intensity = .
replace flood_intensity = 1 if duration_category == "less than 1 day"
replace flood_intensity = 2 if duration_category == "1 to 22 days"
replace flood_intensity = 3 if duration_category == "more than 22 days"
drop duration_category

// Assign value labels
label define flood_intensity 1 "less than 1 day" 2 "1 to 22 days" 3 "more than 22 days", replace

// Assign variable labels
label variable redi_r310_SIRET 	"Total turnover (CATOTAL)"
label variable r004_SIRET 		"Value added (VAHT - IMPOTAX + SUBVEXP)"
label variable redi_r216_SIRET 	"Wages and salaries (SALTRAI)"
label variable b330_SIRET  		"Loans and similar debts (EMPDETT)"
label variable b319_SIRET 		"Earnings before income tax (PBCAI)"
label variable rev2				"NAF code rev.2"

// Add EAIP data
merge m:1 INSEE_COM using "$EAIP_risk", keep(match master) nogenerate 

replace Etabl_nr_EAIP = trim(Etabl_nr_EAIP) // clean variables
destring Etabl_nr_EAIP, replace force
destring pop_CE_EAIP, replace force
destring pop_SM_EAIP, replace force
destring batim_SM_EAIP, replace force

// Define firm size
gen firm_size = .

replace firm_size = 0 if EFF_3112 == 0 // 680'000 SIRETS-YEARS!, 580'000 unique SIRETS!
replace firm_size = 1 if inrange(EFF_3112, 1, 4)
replace firm_size = 2 if inrange(EFF_3112, 5, 9)
replace firm_size = 3 if inrange(EFF_3112, 10, 49)
replace firm_size = 4 if inrange(EFF_3112, 50, 149)
replace firm_size = 5 if inrange(EFF_3112, 150, 199)


// Generate useful variables

//// - Ever treated indicators
gen ever_trated_since2000 = (total_floods>0)                
bysort SIRET: egen ever_trated_since2010 = max(flood_dummy)

//// - Flooded in 2000-2009
gen flooded_2000_2009 = 0
replace flooded_2000_2009 = 1 if ever_trated_since2000 == 1 & ever_trated_since2010 == 0

//// - flag SIRETS whose flood risk index is within 1 sd of treated medians' flood risk index
egen risk_median_treated = median(flood_risk_index_RP100)     if ever_trated_since2000 == 1
egen risk_sd_treated     = sd(flood_risk_index_RP100) 		  if ever_trated_since2000 == 1
gen similar_risk         = (abs(flood_risk_index_RP100 - risk_median_treated) <= risk_sd_treated) 

//// -  Coarsen flood risk index
xtile flood_risk_group = flood_risk_index_RP100, nq(5)

//// -  Flood_dummy excluding low_intensity events
gen flood_dummy_extreme = (flood_dummy == 1 & flood_intensity > 1)
label variable flood_dummy_extreme 	"floods with intensity > 1"

//// -  "year of first flood"
bysort SIRET (ANNEE): egen Ei = min(cond(flood_dummy_extreme == 1, ANNEE, .))
label variable Ei 	"year of first extreme flood (intensity > 1)"

//// -  K "time to treatment" (time relative to Ei)
gen K = ANNEE - Ei
replace K = 0 if missing(Ei)
label variable K 	"time to treatment Ei"

//// - generate D (treatment indicator (=. if Control or not yet treated))
gen D = (K >= 0) if Ei < .
label variable D "indicates Ei switching on and staying on"

//// -  "ever treated" (in 2010-2019 period)
gen ever_treat = !missing(Ei)

//// -  always present throughout 2010-2019 (to build balaned panel)
//local start_y min(ANNEE)
//local end_y max(ANNEE)
//local required = `end_y' - `start_y' + 1
//bysort SIRET = gen n_years = _N

// sort dataset
sort SIRET ANNEE

// Dataset is now ready

********************************************************************************
* 2 - Summary stats on this dataset
********************************************************************************

display "Total obs.: " _N
unique SIRET
display "Unique SIRETs: " r(unique)
unique INSEE_COM
display "Unique communes: " r(unique)
tab ANNEE

summ ANNEE
local min_year = r(min)
local max_year = r(max)
local total_years = r(max) - r(min) + 1

bysort SIRET: gen n_years_firms = _N
tab n_years_firms // around 13% appear all years

bysort INSEE_COM ANNEE: gen firms_per_commune = _N
sum firms_per_commune, detail
histogram firms_per_commune, title("Distribution of firms per INSEE_COM YEAR")

// Graph of proportion of SIRETS flooded by year
preserve
collapse (sum)   tot_floods = flood_dummy (count) n_firms=SIRET, by(ANNEE)
gen flood_rate = tot_floods/n_firms * 100

twoway line flood_rate ANNEE
graph save "${output_figures}/flood_rate_siret.gph", replace
restore

// Graph correlation between risk index and flood events TO DO

// count SIRETs per year
preserve
bysort ANNEE SIRET: gen tag = _n == 1
collapse (sum) tag, by(ANNEE)
rename tag num_sirets
list ANNEE num_sirets, sep(0)
restore


// Summary statistics table
summ mono_etab b319_SIRET b330_SIRET redi_r310_SIRET redi_r216_SIRET r004_SIRET 
summ EFF_3112 EFF_MOY_ET HWAGE AVGWAGE AVGWAGE_3112 share_CDI share_CDD share_interim share_othercontract
summ litorale urban niveau_vie flood_risk_index_RP100 Etabl_share_EAIP Etabl_nr_EAIP
preserve
collapse (sum) ETP, by(Final_Sector)
egen total_emp = total(ETP)
gen sector_share_pct = 100*(ETP/total_emp)
list Final_Sector ETP sector_share_pct
restore

// Summary statistics of flooding
summ mono_etab b319_SIRET b330_SIRET redi_r310_SIRET redi_r216_SIRET r004_SIRET 
summ EFF_3112 EFF_MOY_ET HWAGE AVGWAGE AVGWAGE_3112 share_CDI share_CDD share_interim share_othercontract
summ litorale urban niveau_vie flood_risk_index_RP100 Etabl_share_EAIP Etabl_nr_EAIP
preserve
collapse (sum) ETP, by(Final_Sector)
egen total_emp = total(ETP)
gen sector_share_pct = 100*(ETP/total_emp)
list Final_Sector ETP sector_share_pct
restore


********************************************************************************
* 3 - Generate ATT weights
********************************************************************************

// I USE THIS ONE
logit flood_dummy_extreme flood_risk_group Etabl_share_EAIP income_category urban litorale_sea floods_1982_1999, vce(cluster INSEE_COM)
//  Etabl_nr_EAIP pop_SM_EAIP  batim_SM_EAIP pop_CE_EAIP batim_CE_EAIP floods_1982_1999 TRI
// litorale_sea verw good predictor

// Alternative 1
teffects ipw (Ei) ///
	(flood_dummy_extreme i.litorale_sea i.flood_risk_group i.income_category i.ANNEE), ///
	atet vce(cluster INSEE_COM)


// Alternative 2 specification (takes longer time)
xtset SIRET ANNEE
xtlogit flood_dummy_extreme i.flood_risk_group c.Etabl_share_EAIP i.income_category i.urban i.litorale i.floods_1982_1999, re vce(cluster SIRET)

// Estimate PS
predict pscore, pr

// compute IPW (ATT weights)
gen w_att = flood_dummy_extreme + (1-flood_dummy_extreme)*pscore/(1-pscore)

// visualize weights
tabstat w_att, by(flood_dummy_extreme) // all T should be 1 then C is more dispersed

// plot by status (inspect common support)
tabstat pscore if flood_dummy_extreme == 1, s(n min p1 p50 p99 max)
tabstat pscore if flood_dummy_extreme == 0, s(n min p1 p50 p99 max)

gen in_support = pscore >= 0.0131377 & pscore <= 0.2419747

twoway ///
	(kdensity pscore if flood_dummy_extreme == 1, legend(label(1 "Flooded"))) ///
	(kdensity pscore if flood_dummy_extreme == 0, legend(label(2 "Not Flooded"))) ///
	, ytitle("Density") xtitle("Propensity score") ///
	title("Common support") ///
	name(ps_before, replace)

// plot again, with weights!
twoway ///
	(kdensity pscore if flood_dummy_extreme == 1 [aw =  w_att], legend(label(1 "Flooded"))) ///
	(kdensity pscore if flood_dummy_extreme == 0 [aw =  w_att], legend(label(2 "Not Flooded"))) ///
	, ytitle("Density") xtitle("Propensity score") ///
	title("Common support")  ///
	name(ps_after, replace)
	
// combine
graph combine ps_before ps_after, ///
	cols(2) ycommon imargin(0 1 0 1) ///
	xsize(14) ysize(4) ///
	title("PScores distributions; raw vs ATT-weighted") ///
	name(ps_both, replace)

// Save data to plot in R
kdensity pscore if flood_dummy_extreme == 1, gen(x1 d1)
kdensity pscore if flood_dummy_extreme == 0, gen(x2 d2)
kdensity pscore if flood_dummy_extreme == 1 [aw = w_att], gen(x3 d3)
kdensity pscore if flood_dummy_extreme == 0 [aw = w_att], gen(x4 d4)

list x1 d1 x2 d2 x3 d3 x4 d4 if !missing(x1), clean noobs // plot in R
	

	
// can check also balance of covariates before and after matching! Export and do loveplot in R
local covars flood_risk_index_RP100 Etabl_share_EAIP income_category urban litorale_sea

local K = wordcount("`covars'")

matrix T_raw = J(`K', 1,.)
matrix C_raw = T_raw
matrix D_raw = T_raw
matrix P_raw = T_raw

matrix T_att = T_raw
matrix C_att = T_raw
matrix D_att = T_raw
matrix P_att = T_raw

local r = 1
foreach v of local covars{
	quietly summ `v' if flood_dummy_extreme == 1, meanonly
	matrix T_raw[`r', 1] = r(mean)
	
	quietly summ `v' if flood_dummy_extreme == 0, meanonly
	matrix C_raw[`r', 1] = r(mean)
	
	matrix D_raw[`r', 1] = T_raw[`r', 1] - C_raw[`r', 1]
	
	quietly ttest `v', by(flood_dummy_extreme) unequal
	matrix P_raw[`r', 1] = r(p)
	
	/////////
	
	quietly summ `v' if flood_dummy_extreme == 1 [aw=w_att], meanonly
	matrix T_att[`r', 1] = r(mean)
	
	quietly summ `v' if flood_dummy_extreme == 0 [aw=w_att], meanonly
	matrix C_att[`r', 1] = r(mean)
	
	matrix D_att[`r', 1] = T_att[`r', 1] - C_att[`r', 1]
	
	quietly regress `v' flood_dummy_extreme [aw=w_att], nocons
	test _b[flood_dummy_extreme] = 0
	matrix P_att[`r', 1] = r(p)
	
	local ++r
	
}

matlist T_raw
matlist C_raw
matlist D_raw
matlist P_raw

matlist T_att
matlist C_att
matlist D_att
matlist P_att


********************************************************************************
* 4 - Absorbing treatment
********************************************************************************

/*============================================================
 a) LP-DID baseline - Flood dummy extreme
============================================================*/
local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET  share_CDD  share_CDI
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM)
	
	graph save "${output_figures}/Absorbing/lpdid/`var'_extreme.gph", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace
}

/*============================================================
 b) LP-DID baseline - Flood dummy
============================================================*/
local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET  share_CDD  share_CDI
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM)
	
	graph save "${output_figures}/Absorbing/lpdid/`var'_.gph", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace
}

/*============================================================
 c) LP-DID baseline - Flood dummy extreme AND only nevertreated as control
============================================================*/
local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET  share_CDD  share_CDI
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	nevertreated ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM)
	
	graph save "${output_figures}/Absorbing/lpdid/`var'_nevertreat.gph", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace
}


/*============================================================
 d) LP-DID baseline - Flood dummy extreme + ONLY sample of never-treated-before-2010 as treatment
============================================================*/
global outTXT2 "${output_tables}/Final_res_firms_tables/lpdid_results2.txt"
capture log close
log using "${outTXT2}", text replace


local outcomes HOURS ETP EFF_MOY_ET EFF_3112 HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 share_CDD  share_CDI ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET ///

local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	preserve
	keep if flooded_2000_2009 == 0
	//keep if mono_etab == 1
	//keep if litorale_sea == 1
	//keep if redi_r310_SIRET > 0
	
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM) // try ZEMPT
	graph export "${output_figures}/Absorbing/lpdid/Final_res_firms/`var'.png", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace

	restore
}

// outli_`var'_never_flooded_before_2010.gph are regressions where i keep outliers for financial data



/*==============================
		d) By Sector
==============================*/
local outcomes HOURS ETP EFF_MOY_ET EFF_3112 HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 share_CDD  share_CDI ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET ///
			      
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

local outcomes EFF_MOY_ET EFF_3112 AVGWAGE_3112

levelsof Final_Sector, local(sectors)  
foreach sect of local sectors{
	di "Sector `sect'"
	
	preserve
		keep if flooded_2000_2009 == 0
		keep if Final_Sector == "`sect'"
		
		// keep if mono_etab == 1
		// keep if litorale_sea == 1
		// keep if redi_r310_SIRET > 0
		
	foreach var of local outcomes {
	
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM) ///
	weights(w_att)
	
	graph export "${output_figures}/Absorbing/lpdid/Final_res_firms/`var'_`sect'.png", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace
	
	}
	restore
}

/*==============================
		d) + weights (now included in all specs)
==============================*/

local outcomes HOURS ETP EFF_MOY_ET EFF_3112 HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 share_CDD  share_CDI ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET ///
			      
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	preserve
	keep if flooded_2000_2009 == 0
	// keep if mono_etab == 1
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM) ///
	weights(w_att)
	
	graph export "${output_figures}/Absorbing/lpdid/Final_res_firms/`var'_weights.png", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace

	restore
}


/*==============================
		only mono etab
==============================*/

local outcomes HOURS ETP EFF_MOY_ET EFF_3112 HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 share_CDD  share_CDI ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET ///
			      
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	preserve
	keep if flooded_2000_2009 == 0
	keep if mono_etab == 1
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM) ///
	weights(w_att)
	
	graph export "${output_figures}/Absorbing/lpdid/Final_res_firms/`var'_monoetab.png", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace

	restore
}

/*==============================
		only litorale
==============================*/

local outcomes HOURS ETP EFF_MOY_ET EFF_3112 HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 share_CDD  share_CDI ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET ///
			      
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	preserve
	keep if flooded_2000_2009 == 0
	keep if litorale == 1
	// keep if mono_etab == 1
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM) ///
	weights(w_att)
	
	graph export "${output_figures}/Absorbing/lpdid/Final_res_firms/`var'_litorale.png", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace

	restore
}

/*==============================
		 litorale + mono establishment
==============================*/

local outcomes HOURS ETP EFF_MOY_ET EFF_3112 HWAGE HWAGE_3112 AVGWAGE AVGWAGE_3112 share_CDD  share_CDI ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET ///
			      
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	preserve
	keep if flooded_2000_2009 == 0
	keep if litorale == 1
	keep if mono_etab == 1
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy_extreme) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	cluster(INSEE_COM) ///
	weights(w_att)
	
	graph export "${output_figures}/Absorbing/lpdid/Final_res_firms/`var'_monoetab_litorale.png", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace

	restore
}










/*============================================================
 Sun Abraham
============================================================*/

/*============================================================
 DiD Imputation
============================================================*/
did_imputation AVGWAGE SIRET ANNEE Ei, horizons(0/5) pretrend(2) ///
	controls(`controls') ///
	fe(SIRET SIRET#ANNEE flood_risk_group#ANNEE) ///
	cluster(INSEE_COM) ///
	autosample
	
event_plot, default_look graph_opt(xtitle("Periods since event") ytitle("Average causal effect")) ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-5(1)5)

	
/*============================================================
 did_multiplegt - de Chaisemartin and D'Haultfoeuille (2020)
============================================================*/


foreach risk_level of local risk{
	preserve
		keep if flood_risk_group == `risk_level'
		di "Stratum `risk_level'"
	
	foreach var of local outcomes {
		
		did_multiplegt_dyn `var' Ei ANNEE D, effects(5) placebo(5) controls(floods_last_10y floods_last_5y floods_last_3y years_since_last_flood) 
	
		event_plot e(estimates)#e(variances), default_look ///
			graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
			title("Outcome: `var'; de Chaisemartin and D'Haultfoeuille (2024)")  xlabel(-5(1)10)) ///
			stub_lag(Effect_#)  stub_lead(Placebo_#) together
	
	graph save "${output_figures}/Absorbing/Chais2024/`var'_`risk_level'.gph", replace
	
}
	restore

}

// HERE NEWEST VERSION TO TEST
// egen risk_time_section = group(flood_risk_group ANNEE Section), label
egen risk_SIRET = group(flood_risk_group SIRET), label(risk_SIRET, replace)



egen sector_time = group(Section ANNEE), label(sector_time, replace)
bysort SIRET: gen risk_group_invariant = round(flood_risk_group[1])
format risk_group_invariant %9.0f

local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET  share_CDD  share_CDI
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

local outcomes ETP
foreach var of local outcomes {
	
	did_multiplegt_dyn `var' risk_SIRET ANNEE D, effects(5) placebo(5) /// replaced Ei with risk_SIRET!!!!!!!
	controls(`controls') ///
	cluster(SIRET) ///
	weight(w_att) ///
	by(risk_group_invariant)
	
	event_plot e(estimates)#e(variances), default_look ///
	graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
			title("Outcome: `var'; de Chaisemartin and D'Haultfoeuille (2024)")  xlabel(-5(1)10)) ///
			stub_lag(Effect_#)  stub_lead(Placebo_#) together
	
	graph save "${output_figures}/Absorbing/Chais2024/`var'_1.gph", replace
	
}


bysort SIRET: egen flood_risk_group_sd = sd(risk_group_invariant)
tab flood_risk_group_sd

/*==============================
 Naive Estimation TWFE
==============================*/

// shift K, so that no negative values
summarize K
gen shifted_K = K - r(min)
summ shifted_K if K == -1
local true_neg1 = r(mean)


local true_neg1 = r(mean)
reghdfe EFF_3112 ib`true_neg1'.shifted_K##i.flood_risk_group, absorb(SIRET ANNEE) cluster(SIRET)


// plot
g coef = .
g se = .

levelsof shifted_K, l(times)
foreach t in `times' {
	replace coef = _b[`t'.shifted_K] if shifted_K == `t'
	replace se = _se[`t'.shifted_K] if shifted_K == `t'
}

g ci_top = coef + 1.96*se
g ci_bottom = coef - 1.96*se


keep K coef se ci_*
duplicates drop
sort K

summ ci_top
local top_range = r(max)
summ ci_bottom
local bottom_range = r(min)

twoway (sc coef K, connect(line)) ///
	(rcap ci_top ci_bottom K) ///
	(function y = 0, range(K)) ///
	(function y = 0, range(`bottom_range' `top_range')), ///
	xtitle("test") caption("test")




preserve
coefplot, ///
	keep(shifted_K0-shifted_K18) vertical

	
/*==============================
 Estimation with Stacked
==============================*/
keep SIRETS with at least 1 flood in the whole period. In a given year, if flooded then T. The controls will be SIRETS with same flood intensity (dosage of flooding) across time that however was not flooded that year. This assumes no dynamic effect over time.



********************************************************************************
* 5 - Non-Absorbing treatment
********************************************************************************

/*==============================
 LP-DiD Estimation (non-absorbing!) - Specification 1 
==============================*/

//Specification includes:
// sector-year-risk category FE

local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET  share_CDD  share_CDI
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

foreach var of local outcomes {
	
	lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy) pre(5) post(5) ///
	controls(`controls') ///
	absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
	nonabs(3, notyet firsttreat) ///
	cluster(INSEE_COM)
	
	graph save "${output_figures}/LP-DiD/Specification 1/`var'_.gph", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace
}


// Robustness I:
// Replace "flood_dummy" with "flood_dummy_extreme"
// use nevertreated only
// add lags of dependnet variable as controls
// build "dosage" of flooding, and include flood history strata
// add "nocomp" 
// add weights(att_weights) (computed below)

// Robustness II:
// same but "keep if similar_risk == 1", further restriction on similar risk



/*

The graphs in the folder:
- `var'_ had just "notyet" specified, i.e., nonabs(3, notyet)
- `var'_firsttreat have both "notyet" and "firsttreat", ie., nonabs(3, notyet firsttreat)

*/

/*==============================
  LP-DiD Estimation (non-absorbing!) - Specification 1 - By Sector
==============================*/
local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET  share_CDD  share_CDI
local controls "floods_last_10y floods_last_5y floods_last_3y years_since_last_flood"

levelsof Final_Sector, local(sectors)

foreach sect of local sectors{
	di "Sector `sect'"
	
	preserve
		keep if Final_Sector == "`sect'"
		
	
	foreach var of local outcomes {
	
		lpdid `var', time(ANNEE) unit(SIRET) treat(flood_dummy) pre(5) post(5) ///
		controls(`controls') ///
		absorb(i.flood_risk_group#i.Section##c.ANNEE) ///
		nonabs(3, notyet) ///
		cluster(INSEE_COM)
		
		graph save "${output_figures}/LP-DiD/Specification 1/Sectors/`var'_`sect'.gph", replace
		//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace
		}
	
	restore
}



********************************************************************************
* Commune level analysis
********************************************************************************


********************************************************************************
* Combine graphs
********************************************************************************  
local outcomes EFF_3112 AVGWAGE_3112

foreach var in `outcomes' {	
graph combine ///
	"${output_figures}/Absorbing/lpdid/`var'_never_flooded_before_2010.gph" ///
	"${output_figures}/Absorbing/lpdid/Sectors/`var'_Agriculture_never_flooded_before_2010.gph" ///
	"${output_figures}/Absorbing/lpdid/Sectors/`var'_Commerces_never_flooded_before_2010.gph" ///
	"${output_figures}/Absorbing/lpdid/Sectors/`var'_Construction_never_flooded_before_2010.gph" ///
	"${output_figures}/Absorbing/lpdid/Sectors/`var'_Industries_never_flooded_before_2010.gph" ///
	"${output_figures}/Absorbing/lpdid/Sectors/`var'_Services_never_flooded_before_2010.gph", ///
		rows(2) cols(3)

	graph save "${output_figures}/Absorbing/lpdid/All/`var'_all.gph", replace
}	  
	  
	  
	  
	  
	  
	  
	  
	  
	  
********************************************************************************
********************************************************************************  
/*
DRAFT BIN


levelsof ANNEE, local(years)
set trace on
foreach risk_level in 1 2 3 4 5 {
	foreach year in `years'{
		kmatch ps flood_dummy floods_last_5y ///
		if ANNEE == `year' & flood_risk_group == `risk_level', ///
		idgenerate(matched_r`risk_level'_`year')	
	}
}


logit flood_dummy floods_last_5y floods_last_3y  

kmatch ps flood_dummy floods_last_5y


cem flood_risk_group, treatment(ever_trated_since2000)

keep if cem_matched == 1

///
// generate D (treatment indicator (if Control OR not yet treated == .))
//gen D = (K >= 0) if Ei < .

// generate group variable "gvar" (for csdid) (same as Ei, but 0 for never treated)
//gen gvar = cond(missing(Ei), 0, Ei)

/*==============================
 2.3 - Matching (OLD)
==============================*/

// "years_since_last_flood" perfectly predicts "flood_dummy"

// drop ipw_temp _KM_nc _KM_nm _KM_mw _KM_ps _KM_strata
// drop ipw_strata _km_nc _km_nm _km_mw _km_ps _km_strata ate_weights att_weights
// drop _matched
gen ipw_strata = .
gen _km_nc = .
gen _km_nm  = .
gen _km_mw  = .
gen _km_ps = .
gen _km_strata  = .
gen att_weights = .

// set trace on

levelsof flood_risk_group, local(risk_level)

foreach risk of local risk_level {
	
	di "Stratum `risk'"
	
	kmatch ps flood_dummy floods_last_10y floods_last_5y floods_last_3y ///
		if flood_risk_group == `risk', ///
		generate(ipw_temp) wgenerate(_watt) att
	
	replace ipw_strata = ipw_temp   if flood_risk_group == `risk'
	replace _km_nc     = _KM_nc     if flood_risk_group == `risk'
	replace _km_nm     = _KM_nm     if flood_risk_group == `risk'
	replace _km_mw     = _KM_mw     if flood_risk_group == `risk'
	replace _km_ps     = _KM_ps     if flood_risk_group == `risk'
	replace _km_strata = _KM_strata if flood_risk_group == `risk'
	
	replace att_weights = _watt if flood_risk_group == `risk'
	
	drop ipw_temp _KM_* _watt
}

tabstat _km_ps, by(flood_risk_group) stat(mean min max count)
/*
flood_risk_group |      Mean       Min       Max         N
-----------------+----------------------------------------
               1 |  .0727373  .0677725  .2093333   2488999
               2 |  .1054766  .0940199  .3208117   1823655
               3 |  .1328977  .1202195  .2971594   2139729
               4 |  .1407842  .1275493  .2974839   2151243
               5 |  .1100974  .1060873  .1688501   2149697
-----------------+----------------------------------------
           Total |  .1113421  .0677725  .3208117  1.08e+07
----------------------------------------------------------
*/

/*==============================
 Collapse it at commune level
==============================*/
sort SIRET ANNEE INSEE_COM

tab Final_Sector, gen(Final_Sector_)

// collapse
preserve
set trace on
collapse ///
   (sum) 	   EFF_3112        = EFF_3112 				    ///
			   HWAGE           = HWAGE 						///
			   ETP             = ETP 				   		///
			   r004_SIRET      = r004_SIRET					///
			   redi_r216_SIRET = redi_r216_SIRET     		///
			   redi_r310_SIRET = redi_r310_SIRET			///
			   b330_SIRET      = b330_SIRET 				///
			   b319_SIRET      = b319_SIRET 				///
	(mean)     AVGWAGE_3112           = AVGWAGE_3112 		   ///
			   share_CDI              =  share_CDI 		       ///
			   share_CDD              =  share_CDD 			   ///
			   share_othercontract    =  share_othercontract   ///
			   share_Agric            = Final_Sector_1 		   ///
			   share_Comm             = Final_Sector_2 		   ///
			   share_Construc         = Final_Sector_3 		   ///
			   share_Industr          = Final_Sector_4 		   ///
			   share_Servic           = Final_Sector_5 		   ///
	(first)    flood_risk_index_RP100 = flood_risk_index_RP100 ///
			   flood_dummy            = flood_dummy 		   ///
			   flood_intensity        = flood_intensity        ///
	, by(INSEE_COM ANNEE) 
	  
save "$communes_dt_2005_2019", replace
restore

*/