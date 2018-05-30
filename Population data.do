/* UK CCL SYNTH PAPER
Population data
Alice Lepissier
Version: Stata 14.2 IC
Date: 14/01/18 */

**** Required command
* ssc install mmerge

clear all
set more off

/* //////////////////////////////////////
CHANGE YOUR WORKING DIRECTORY HERE
////////////////////////////////////// */
cd "C:\Users\Alice\Box Sync\LepissierMildenberger\ScandiSynth" // Laptop
*cd "C:\boxsync\alepissier\LepissierMildenberger\ScandiSynth" // Bren Desktop
global data "Raw data"
global results "Results"
global log "Logs"
local date = c(current_date)
capture log close
log using "$log\SYNTH_data`date'.log", replace


/**** INDEX
1. Import historical population data
2. Master list for country codes merge
3. Keep SYNTH countries only
*/



**** 1. Import historical population data
import excel using "$data\Population from SkyShares.xlsx"
drop B-GJ
drop HA-HH
ren A country
renvars GK-GZ / y1990 y1991 y1992 y1993 y1994 y1995 y1996 y1997 y1998 y1999 y2000 y2001 y2002 y2003 y2004 y2005
drop in 1
saveold "$results\population.dta", replace


**** 2. Master list for country codes merge
clear
import excel using "$data\codes_masterlist.xlsx", sheet("Codes_Master") firstrow
destring UN_rcode, replace
destring UN_srcode, replace
destring UN_tag1c, replace
destring UN_tag2c, replace
sort country
saveold "$data\codes_masterlist.dta", replace


**** 3. Keep SYNTH countries only
use "$results\population.dta", clear
mmerge country using "$data\codes_masterlist.dta", unmatched(master) ukeep(ISO3166)
rename ISO3166 ISO
order country ISO
drop if ISO == ""
drop country
drop _merge

reshape long y, i(ISO) j(year)
ren y population

sort ISO year
export delimited using "$results\population.csv", replace
