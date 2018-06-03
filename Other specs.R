# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# Specification 1   
# .. Optimize over 1995-2001, CO2 per capita
# Specification 2
# .. Optimize over 1990-2001, CO2 per capita, no covariates
# Specification 3
# .. Optimize over 1995-2001, 1990 baseline
# Specification 4
# .. Optimize over 1990-2001, 1990 baseline, no covariates
# Specification 5
# .. Optimize over 1995-2001, difference in log levels



## ## ## ## ## ## ## ## ## ##
# PREAMBLE               ####
## ## ## ## ## ## ## ## ## ##

setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/Synth/Results") # Alice laptop
#setwd("~/Box Sync/LepissierMildenberger/Synth/Results") # Matto
#setwd("C:/boxsync/alepissier/LepissierMildenberger/Synth/Results") # Alice work
library(devtools)
library(dplyr)
library(foreign) # Deprecated with newest R updated
library(gghighlight)
library(ggplot2)
library(gridExtra)
library(readstata13) # Using
library(tidyr)
library(plyr)
library(stargazer)
library(Synth)



## ## ## ## ## ## ## ## ## ##
# FUNCTIONS              ####
## ## ## ## ## ## ## ## ## ##

whodat <- function(id) {
  country <- data[which(data$countryid == id), 2][1]
  return(country)
}

whichnum <- function(ISO) {
  code <- data[which(data$countrycode == ISO), 1][1]
  return(code)
}

whoder <- function() {
  unique(data$countrycode)
}



## ## ## ## ## ## ## ## ## ##
# DATA                   ####
## ## ## ## ## ## ## ## ## ##

data <- read.dta13("WDI.dta")
whoder()

data$scaledCO2 <- NA

countrygroup <- group_by(data, countryid)
base1990bycountry <- summarize(countrygroup,baseline = en_atm_co2e_kt[which(year==1990)])
base1990bycountry$countryid <- unique(countrygroup$countryid)

data <- join(data, base1990bycountry)
data$rescaled1990 <- data$en_atm_co2e_kt/data$baseline

data$logdiff <- log(data$en_atm_co2e_kt/data$baseline)

nmiss <- ddply(data, "countryid", summarize,
               gdp.missing = sum(is.na(ny_gdp_pcap_kd)),
               co2.missing = sum(is.na(rescaled1990)),
               energyimports.missing = sum(is.na(eg_imp_cons_zs)),
               renewablecons.missing = sum(is.na (eg_fec_rnew_zs)),
               FFconsumption.missing = sum(is.na(eg_use_comm_fo_zs)),
               taxrevenuegdp.missing = sum(is.na(gc_tax_totl_gd_zs)),
               gdpperenergyu.missing = sum(is.na(eg_gdp_puse_ko_pp_kd)),
               naturresrents.missing = sum(is.na(ny_gdp_totl_rt_zs)),
               gvtexpendeduc.missing = sum(is.na(se_xpd_totl_gd_zs)),
               gdpgrowthannu.missing = sum(is.na(ny_gdp_mktp_kd_zg)),
               renewableelec.missing = sum(is.na(eg_elc_rnew_zs)),
               energyuseinkg.missing = sum(is.na(eg_use_pcap_kg_oe)),
               fuelexportspc.missing = sum(is.na(tx_val_fuel_zs_un)))

missing <- subset(nmiss, (nmiss$gdp.missing > 20)
                  & (nmiss$energyimports.missing > 12)
                  & (nmiss$renewablecons.missing > 20)
                  & (nmiss$FFconsumption.missing > 12)
                  & (nmiss$taxrevenuegdp.missing > 20)
                  & (nmiss$gdpperenergyu.missing > 12)
                  & (nmiss$naturresrents.missing > 20)
                  & (nmiss$gvtexpendeduc.missing > 20)
                  & (nmiss$gdpgrowthannu.missing > 20)
                  & (nmiss$renewableelec.missing > 20)
                  & (nmiss$energyuseinkg.missing > 20)
                  & (nmiss$fuelexportspc.missing > 20)
                  | (nmiss$co2.missing > 4))

missing <- t(missing[1])
missing
for (i in 1:length(missing)){
  print(whodat(missing[i]))
}

data <- subset(data, !(countryid %in% missing))
whoder()

scandis <- c("DNK", "NLD", "NOR", "SWE")
data <- subset(data, !(countrycode %in% scandis))
whoder()

# dispropor.donors <- c("FRA", "ISL")
# data <- subset(data, !(countrycode %in% dispropor.donors))
# whoder()

# not.sure <- c("CHL", "TUR", "JPN", "KOR", "MEX", "TUR")
# data <- subset(data, !(countrycode %in% not.sure))
# whoder()

# no.weights <- c("AUT", "CAN", "CHL", "ESP", "FIN", "FRA", "GRC", "ISL", "ISR", "ITA", "PRT", "TUR", "USA")
# data <- subset(data, !(countrycode %in% no.weights))

# whodat(3)
# whodat(23)
# data <- subset(data, !(countryid %in% c(3, 23)))

whichnum("MEX")
whichnum("LUX") # Pre-treatment trend is bonkers. If removed, optimization over longer period buggers.
#data <- subset(data, !(countryid %in% c(23)))



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 1        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1995-2001, CO2 per capita ####
# Less covariates
# Counterfactual looks better

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1995:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = c("ny_gdp_pcap_kd", #GDP per capita (constant 2010 US$)
                          "eg_imp_cons_zs", #Energy imports, net (% of energy use)
                          "eg_fec_rnew_zs", #Renewable energy consumption (% of total final energy consumption)
                          "eg_use_comm_fo_zs", #Fossil fuel energy consumption (% of total)
                          #"gc_tax_totl_gd_zs", #Tax revenue (% of GDP)
                          #"eg_gdp_puse_ko_pp_kd", #GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          #"ny_gdp_totl_rt_zs", #Total natural resources rents (% of GDP)
                          #"se_xpd_totl_gd_zs", #Government expenditure on education, total (% of GDP)
                          #"ny_gdp_mktp_kd_zg", #GDP growth (annual %)
                          #"eg_elc_rnew_zs", #Renewable electricity output (% of total electricity output)
                          "eg_use_pcap_kg_oe", #Energy use (kg of oil equivalent per capita)
                          "tx_val_fuel_zs_un" #Fuel exports (% of merchandise exports)
                          #"ne_exp_gnfs_zs", #Exports of goods and services (% of GDP)
                          #"ne_imp_gnfs_zs" #Imports of goods and services (% of GDP)
           ),
           predictors.op = "mean" ,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("CO2_emissions_PC" , choose.time.predictors , "mean")),
           dependent = "CO2_emissions_PC",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1995:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Specification 1.txt")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 2        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, CO2 per capita, no covariates ####

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("CO2_emissions_PC", 1991:1992, "mean"),
             list("CO2_emissions_PC", 1993:1994, "mean"),
             list("CO2_emissions_PC", 1995:1996, "mean"),
             list("CO2_emissions_PC", 1997:1998, "mean"),
             list("CO2_emissions_PC", 1999:2000, "mean")),
           dependent = "CO2_emissions_PC",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1995:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Specification 2.txt")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 3        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1995-2001, 1990 baseline ####
# Less covariates
# Counterfactual looks better

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1995:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = c("ny_gdp_pcap_kd", #GDP per capita (constant 2010 US$)
                          "eg_imp_cons_zs", #Energy imports, net (% of energy use)
                          "eg_fec_rnew_zs", #Renewable energy consumption (% of total final energy consumption)
                          "eg_use_comm_fo_zs", #Fossil fuel energy consumption (% of total)
                          #"gc_tax_totl_gd_zs", #Tax revenue (% of GDP)
                          #"eg_gdp_puse_ko_pp_kd", #GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          #"ny_gdp_totl_rt_zs", #Total natural resources rents (% of GDP)
                          #"se_xpd_totl_gd_zs", #Government expenditure on education, total (% of GDP)
                          #"ny_gdp_mktp_kd_zg", #GDP growth (annual %)
                          #"eg_elc_rnew_zs", #Renewable electricity output (% of total electricity output)
                          "eg_use_pcap_kg_oe", #Energy use (kg of oil equivalent per capita)
                          "tx_val_fuel_zs_un" #Fuel exports (% of merchandise exports)
                          #"ne_exp_gnfs_zs", #Exports of goods and services (% of GDP)
                          #"ne_imp_gnfs_zs" #Imports of goods and services (% of GDP)
           ),
           predictors.op = "mean" ,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990" , choose.time.predictors , "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1995:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Specification 3.txt")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 4        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, 1990 baseline, no covariates ####

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean"),
             list("rescaled1990", 1997:1998, "mean"),
             list("rescaled1990", 1999:2000, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1995:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Specification 4.txt")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 5        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1995-2001, difference in log levels ####

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1995:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = c("ny_gdp_pcap_kd", #GDP per capita (constant 2010 US$)
                          "eg_imp_cons_zs", #Energy imports, net (% of energy use)
                          "eg_fec_rnew_zs", #Renewable energy consumption (% of total final energy consumption)
                          "eg_use_comm_fo_zs", #Fossil fuel energy consumption (% of total)
                          #"gc_tax_totl_gd_zs", #Tax revenue (% of GDP)
                          #"eg_gdp_puse_ko_pp_kd", #GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          #"ny_gdp_totl_rt_zs", #Total natural resources rents (% of GDP)
                          #"se_xpd_totl_gd_zs", #Government expenditure on education, total (% of GDP)
                          #"ny_gdp_mktp_kd_zg", #GDP growth (annual %)
                          #"eg_elc_rnew_zs", #Renewable electricity output (% of total electricity output)
                          "eg_use_pcap_kg_oe", #Energy use (kg of oil equivalent per capita)
                          "tx_val_fuel_zs_un" #Fuel exports (% of merchandise exports)
                          #"ne_exp_gnfs_zs", #Exports of goods and services (% of GDP)
                          #"ne_imp_gnfs_zs" #Imports of goods and services (% of GDP)
           ),
           predictors.op = "mean" ,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("logdiff" , choose.time.predictors , "mean")),
           dependent = "logdiff",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1995:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Specification 5.txt")