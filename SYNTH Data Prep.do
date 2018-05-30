/* MILDENBERGER-STOKES RESEARCH METHODS LAB
Data Scoping for SYNTH paper
Alice Lepissier
Version: Stata 14.2 IC
Date: 01/05/17 */

**** Required commands
* ssc install wbopendata
* ssc install mmerge

clear all
set more off

/* //////////////////////////////////////
CHANGE YOUR WORKING DIRECTORY HERE
////////////////////////////////////// */
*cd "C:\Users\Alice\Box Sync\LepissierMildenberger\Synth" // Laptop
cd "C:\boxsync\alepissier\LepissierMildenberger\Synth" // Bren Desktop
global data "Raw data"
global results "Results"
global log "Logs"
local date = c(current_date)
capture log close
log using "$log\SYNTH_data`date'.log", replace
local countries "AUS;AUT;BEL;CAN;CHL;CZE;DNK;EST;FIN;FRA;DEU;GRC;HUN;ISL;IRL;ISR;ITA;JPN;KOR;LVA;LUX;MEX;NLD;NZL;NOR;POL;PRT;SVK;SVN;ESP;SWE;CHE;TUR;GBR;USA" // All of the OECD countries


/**** INDEX
//////////////////////////////////////
DATA IMPORT AND PREPARATION
//////////////////////////////////////
1. Master list for country codes merge
2. Loop WDI data import
3. Notes
*/



/* /////////////////////////////////////////////////////////////////////////////
DATA IMPORT AND PREPARATION
///////////////////////////////////////////////////////////////////////////// */

**** 1. Master list for country codes merge
import excel using "$data\codes_masterlist.xlsx", sheet("Codes_Master") firstrow
destring UN_rcode, replace
destring UN_srcode, replace
destring UN_tag1c, replace
destring UN_tag2c, replace
sort country
saveold "$data\codes_masterlist.dta", replace



**** 2. Loop WDI data import
clear
wbopendata, language(en - English) country(`countries') topics() indicator(EN.ATM.CO2E.PC) long
drop iso2code region regioncode
ren en_atm_co2e_pc CO2_emissions_PC
label var CO2_emissions_PC "CO2 emissions per capita"
*twoway (line CO2_emissions_PC year if countrycode == "SWE" | countrycode == "NOR" | countrycode == "FIN" | countrycode == "DNK"), ylabel(, format(%9.0gc)) by(, title(CO2 emissions per capita in Scandinavia) note("")) by(countryname, imargin(medium))
*graph export "$results\CO2 Emissions per capita in Scandinavia.png", as(png) replace
*twoway (line CO2_emissions_PC year), ylabel(, format(%9.0gc)) by(, title(CO2 emissions per capita) note("")) by(countryname, imargin(medium))
*graph export "$results\CO2 Emissions per capita.png", as(png) replace
saveold "$results\WDI.dta", replace


global indic "NY.GDP.PCAP.KD EG.IMP.CONS.ZS EG.FEC.RNEW.ZS EG.USE.COMM.FO.ZS GC.TAX.TOTL.GD.ZS EG.GDP.PUSE.KO.PP.KD NY.GDP.TOTL.RT.ZS SE.XPD.TOTL.GD.ZS NY.GDP.MKTP.KD.ZG EG.ELC.RNEW.ZS EG.USE.PCAP.KG.OE TX.VAL.FUEL.ZS.UN NE.EXP.GNFS.ZS NE.IMP.GNFS.ZS EN.ATM.CO2E.KT"
foreach i in $indic {
local WB = lower(`"`i'"')
local varname = subinstr(`"`WB'"', "." , "_" , .)

			clear all
			wbopendata, language(en - English) country(`countries') topics() indicator(`i') long
			drop iso2code region regioncode

			drop if missing(`varname')
			mmerge countryname countrycode year using "$results\WDI.dta"
			drop _merge
			saveold "$results\WDI.dta", replace
		}

egen countryid = group(countrycode)
order countryid countrycode countryname year CO2_emissions_PC
sort countryid year

saveold "$results\WDI.dta", replace


/**** 3. Notes
EN.ATM.CO2E.PC
CO2 emissions (metric tons per capita)
Carbon dioxide emissions are those stemming from the burning of fossil fuels and the manufacture of cement. They include carbon dioxide produced during consumption of solid, liquid, and gas fuels and gas flaring.
Source: Carbon Dioxide Information Analysis Center, Environmental Sciences Division, Oak Ridge National Laboratory, Tennessee, United States.

NY.GDP.PCAP.KD
GDP per capita (constant 2010 US$)
GDP per capita is gross domestic product divided by midyear population. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources. Data are in constant 2010 U.S. dollars.
Source: World Bank national accounts data, and OECD National Accounts data files.

EG.IMP.CONS.ZS
Energy imports, net (% of energy use)
Net energy imports are estimated as energy use less production, both measured in oil equivalents. A negative value indicates that the country is a net exporter. Energy use refers to use of primary energy before transformation to other end-use fuels, which is equal to indigenous production plus imports and stock changes, minus exports and fuels supplied to ships and aircraft engaged in international transport.
Source: IEA Statistics © OECD/IEA 2014 (iea.org/stats/index.asp), subject to iea.org/t&c/termsandconditions.

EG.FEC.RNEW.ZS
Renewable energy consumption (% of total final energy consumption)
World Bank, Sustainable Energy for All (SE4ALL) database from the SE4ALL Global Tracking Framework led jointly by the World Bank, International Energy Agency, and the Energy Sector Management Assistance Program.
Source: World Bank, Sustainable Energy for All ( SE4ALL ) database from the SE4ALL Global Tracking Framework led jointly by the World Bank, International Energy Agency, and the Energy Sector Management Assistance Program.

EG.USE.COMM.FO.ZS
Fossil fuel energy consumption (% of total)
Fossil fuel comprises coal, oil, petroleum, and natural gas products.
Source: IEA Statistics © OECD/IEA 2014 (iea.org/stats/index.asp), subject to iea.org/t&c/termsandconditions.

GC.TAX.TOTL.GD.ZS
Tax revenue (% of GDP)
Tax revenue refers to compulsory transfers to the central government for public purposes. Certain compulsory transfers such as fines, penalties, and most social security contributions are excluded. Refunds and corrections of erroneously collected tax revenue are treated as negative revenue.
Source: International Monetary Fund, Government Finance Statistics Yearbook and data files, and World Bank and OECD GDP estimates.

EG.GDP.PUSE.KO.PP.KD
GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
GDP per unit of energy use is the PPP GDP per kilogram of oil equivalent of energy use. PPP GDP is gross domestic product converted to 2011 constant international dollars using purchasing power parity rates. An international dollar has the same purchasing power over GDP as a U.S. dollar has in the United States.
Source: IEA Statistics © OECD/IEA 2014 (iea.org/stats/index.asp), subject to iea.org/t&c/termsandconditions.

NY.GDP.TOTL.RT.ZS
Total natural resources rents (% of GDP)
Total natural resources rents are the sum of oil rents, natural gas rents, coal rents (hard and soft), mineral rents, and forest rents.
Source: Estimates based on sources and methods described in "The Changing Wealth of Nations: Measuring Sustainable Development in the New Millennium" (World Bank, 2011).

SE.XPD.TOTL.GD.ZS
Government expenditure on education, total (% of GDP)
General government expenditure on education (current, capital, and transfers) is expressed as a percentage of GDP. It includes expenditure funded by transfers from international sources to government. General government usually refers to local, regional and central governments.
Source: United Nations Educational, Scientific, and Cultural Organization ( UNESCO ) Institute for Statistics.

NY.GDP.MKTP.KD.ZG
GDP growth (annual %)
Annual percentage growth rate of GDP at market prices based on constant local currency. Aggregates are based on constant 2010 U.S. dollars. GDP is the sum of gross value added by all resident producers in the economy plus any product taxes and minus any subsidies not included in the value of the products. It is calculated without making deductions for depreciation of fabricated assets or for depletion and degradation of natural resources.
Source: World Bank national accounts data, and OECD National Accounts data files.

EG.ELC.RNEW.ZS
Renewable electricity output (% of total electricity output)
World Bank, Sustainable Energy for All (SE4ALL) database from the SE4ALL Global Tracking Framework led jointly by the World Bank, International Energy Agency, and the Energy Sector Management Assistance Program.
Source: World Bank, Sustainable Energy for All ( SE4ALL ) database from the SE4ALL Global Tracking Framework led jointly by the World Bank, International Energy Agency, and the Energy Sector Management Assistance Program.

EG.USE.PCAP.KG.OE
Energy use (kg of oil equivalent per capita)
Energy use refers to use of primary energy before transformation to other end-use fuels, which is equal to indigenous production plus imports and stock changes, minus exports and fuels supplied to ships and aircraft engaged in international transport.
Source: IEA Statistics © OECD/IEA 2014 (iea.org/stats/index.asp), subject to iea.org/t&c/termsandconditions.

TX.VAL.FUEL.ZS.UN
Fuel exports (% of merchandise exports)
Fuels comprise the commodities in SITC section 3 (mineral fuels, lubricants and related materials).
Source: World Bank staff estimates through the WITS platform from the Comtrade database maintained by the United Nations Statistics Division.

NE.EXP.GNFS.ZS
Exports of goods and services (% of GDP)
Exports of goods and services represent the value of all goods and other market services provided to the rest of the world. They include the value of merchandise, freight, insurance, transport, travel, royalties, license fees, and other services, such as communication, construction, financial, information, business, personal, and government services. They exclude compensation of employees and investment income (formerly called factor services) and transfer payments.
Source: World Bank national accounts data, and OECD National Accounts data files.

NE.IMP.GNFS.ZS
Imports of goods and services (% of GDP)
Imports of goods and services represent the value of all goods and other market services received from the rest of the world. They include the value of merchandise, freight, insurance, transport, travel, royalties, license fees, and other services, such as communication, construction, financial, information, business, personal, and government services. They exclude compensation of employees and investment income (formerly called factor services) and transfer payments.
Source: World Bank national accounts data, and OECD National Accounts data files.

EN.ATM.CO2E.KT
CO2 emissions (kt)
Carbon dioxide emissions are those stemming from the burning of fossil fuels and the manufacture of cement. They include carbon dioxide produced during consumption of solid, liquid, and gas fuels and gas flaring.


TO CHECK OUT
- Trade imports and exports
- Beta convergence
- Look at HIC codes basket of what SWE and NOR export
- CDIAC sectoral data
- Create counterfactual UK for climate levy - 2000 - should factor in anticipatory dates
- UK Levy had emissions forecast. Plus counterfactual for UK didn't introduce treatment
- Dutch carbon tax and Danish carbon tax as DV. 
