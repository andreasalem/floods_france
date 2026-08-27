/*
ssc install did_imputation
ssc install did_multiplegt
ssc install eventstudyinteract
ssc install csdid

ssc install ftools
ssc install reghdfe

i = SIRET
t = ANNEE
Ei = first_treat (year of first flood)
K = t - Ei

*/



use "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/FICUS_FARE_DADS_GASPAR_2000_to_2020.dta", clear

describe
tab flood_dummy // must be 0 or 1, no NAs
des ANNEE // convert to numeric if needed

// generate "year of first flood" (if never flooded == .)
bysort SIRET (ANNEE): egen Ei = min(cond(flood_dummy == 1, ANNEE, .))

// generate K (time relative to Ei)
gen K = ANNEE - Ei

// generate D (treatment indicator (=. if Control or not yet treated))
gen D = (K >= 0) if Ei < .

// generate group variable "gvar" (for csdid) (same as Ei, but 0 for never treated)
gen gvar = cond(missing(Ei), 0, Ei)

// generate event-time dummies (for Sun & Abraham)

/// latest treated unit serve as control
sum K if D == 1
gen lastcohort = Ei == r(max)

/// post treatment dummies
forvalues l = 0/5 {
	gen L`l'event = (K == `l')
}

/// pre treatment dummies
forvalues l = 1/14 {
	gen F`l'event = (K == -`l')
}

/// drop base period
drop F1event


// test
preserve
set seed 1234

bysort SIRET: gen rand = runiform() if _n == 1
bysort SIRET: replace rand = rand[1]

egen tag_siret = tag(SIRET)
sum rand if tag_siret, detail
local threshold = r(p10)

keep if rand <= `threshold'
drop rand tag_siret

save "C:/Users/Public/Documents/Fontaine/Floods_Shock/1 - Data processing/Clean/data_subsample.dta", replace
restore

// Estimation with did_imputation of Borusyak et al. (2021)
did_imputation AVGWAGE SIRET ANNEE Ei, allhorizons pretrend(5)
event_plot, default_look graph_opt(xtitle("Periods since event") ytitle("Average causal effect")) ///
	title("Borusyak et al. (2021) imputation estimator") xlabel(-5(1)5)

estimates store bjs


// Estimation with did_multiplegt of de Chaisemartin and D'Haultfoeuille (2020)
did_multiplegt AVGWAGE Ei ANNEE D, robust_dynamic dynamic(5) placebo(5) breps(100) cluster(SIRET)
event_plot e(estimates)#e(variances), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect")) /// title("de Chaisemartin and D'Haultfoeuille (2020)")  xlabel(-5(1)5) stub_lag(Effect_#) stub_lead(Placebo_#) together
	
matrix dcdh_b = e(estimates)
matrix dcdh_v = e(variances)
	
event_plot dcdh_b#dcdh_v,
graph export ""
	







