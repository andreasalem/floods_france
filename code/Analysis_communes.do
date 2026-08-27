********************************************************************************
* Set-Up
********************************************************************************

/*

0: Set globals
1: Load data at commune level

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
global communes_dt_2000_2019   "${clean_data}/subsamples/communes_2000_2019.dta"



********************************************************************************
* 1 - Data import (2005-2019 subsample)
********************************************************************************

use "$communes_dt_2000_2019", replace

replace Etabl_nr_EAIP = trim(Etabl_nr_EAIP) // clean variables
destring Etabl_nr_EAIP, replace force

// encode duration_category
gen flood_intensity = .
replace flood_intensity = 1 if duration_category == "less than 1 day"
replace flood_intensity = 2 if duration_category == "1 to 22 days"
replace flood_intensity = 3 if duration_category == "more than 22 days"
drop duration_category

// Assign value labels
label define flood_intensity 1 "less than 1 day" 2 "1 to 22 days" 3 "more than 22 days", replace

//// -  Coarsen flood risk index
xtile flood_risk_group = flood_risk_index_RP100, nq(5)

//// -  Flood_dummy excluding low_intensity events
gen flood_dummy_extreme = (flood_dummy == 1 & flood_intensity > 1)
label variable flood_dummy_extreme 	"floods with intensity > 1"

// encode variables
encode INSEE_COM, gen(INSEE_COM_destr)
drop   INSEE_COM
rename INSEE_COM_destr INSEE_COM

// gen Ei
bysort INSEE_COM (ANNEE): egen Ei = min(cond(flood_dummy_extreme == 1, ANNEE, .))
label variable Ei 	"year of first extreme flood (intensity > 1)"

//// -  K "time to treatment" (time relative to Ei)
gen K = ANNEE - Ei
replace K = 0 if missing(Ei)
label variable K 	"time to treatment Ei"

//// - generate D (treatment indicator (=. if Control or not yet treated))
gen D = (K >= 0) if Ei < .
label variable D "indicates Ei switching on and staying on"


********************************************************************************
* Matching
********************************************************************************

logit flood_dummy_extreme percent_flooded_low percent_flooded_moderate percent_flooded_high percent_flooded_very_high Etabl_nr_EAIP Etabl_share_EAIP floods_1982_1999 TRI
// pop_CE_EAIP pop_SM_EAIP batim_CE_EAIP batim_SM_EAIP

// Estimate PS
predict pscore, pr
summ pscore if flood_dummy_extreme == 0 | flood_dummy_extreme == 1 // there should be overlap!

// compute IPW (ATT weights)
gen w_att = flood_dummy_extreme + (1-flood_dummy_extreme)*pscore/(1-pscore)



********************************************************************************
* Specf 1
********************************************************************************


local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 ///
			   redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET
local controls "floods_last_10y floods_last_5y floods_last_3y" // add years_since_last_flood

foreach var of local outcomes {
	
	preserve
	keep if floods_1982_1999 == 0
	
	lpdid `var', time(ANNEE) unit(INSEE_COM) treat(flood_dummy_extreme) pre(5) post(5) nocomp ///
	controls(`controls') ///
	absorb(i.flood_risk_group##c.ANNEE) ///
	weights(w_att)
	
	graph save "${output_figures}/Communes/lpdid/`var'_specf1_nocomp.gph", replace
	//estimates store "${output_figures}/LP-DiD/risk_pooled/`var'", replace

	restore
}


********************************************************************************
* Specf 2 -
********************************************************************************
egen risk_group = group(flood_risk_group INSEE_COM), label(risk_SIRET, replace)

local outcomes ETP EFF_3112 HWAGE AVGWAGE_3112 redi_r310_SIRET b319_SIRET redi_r216_SIRET b330_SIRET r004_SIRET
local controls "floods_last_10y floods_last_5y floods_last_3y"

foreach var of local outcomes {
	
	did_multiplegt_dyn `var' risk_group ANNEE D, effects(5) placebo(5) /// replaced Ei with risk_SIRET!!!!!!!
	controls(`controls') ///
	weight(w_att)
	// by(risk_group_invariant)

	
	graph save "${output_figures}/Communes/Chais2024/`var'_1.gph", replace
	
}

event_plot e(estimates)#e(variances), default_look ///
	graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") ///
			title("Outcome: `var'; de Chaisemartin and D'Haultfoeuille (2024)")  xlabel(-5(1)10)) ///
			stub_lag(Effect_#)  stub_lead(Placebo_#) together



