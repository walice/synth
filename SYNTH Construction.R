# ScanSynth
# 8 May 2017
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# Specification 1:
# .. Optimize over 1995-2001, less covariates
# Synth 1
# Generate Results 1
# Specification 2:
# .. Optimize over 1990-2001, all covariates
# Synth 2
# Placebo test for specification 1
# Trajectory balancing



## ## ## ## ## ## ## ## ## ##
# PREAMBLE               ####
## ## ## ## ## ## ## ## ## ##

setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/Synth/Results") # Alice laptop
#setwd("~/Box Sync/LepissierMildenberger/Synth/Results") # Matto
#setwd("C:/boxsync/alepissier/LepissierMildenberger/Synth/Results") # Alice work
library(Synth)
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)
library(stargazer)



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

data <- read.dta("WDI.dta")
whoder()

ggplot(data, aes(x = year, y = CO2_emissions_PC, col = countryname)) +
  geom_line()

ggplot(data[which(data$countrycode == "GBR"), ], aes(x = year, y = CO2_emissions_PC, col = countryname)) +
  geom_line() + geom_vline(xintercept = 2001, lty = 2) + 
  geom_smooth(se=F)

ggplot(data[which(data$countrycode == "GBR" & data$year>1994 & data$year<2006), ],
       aes(x = year, y = CO2_emissions_PC, col = countryname)) +
  geom_line() + geom_vline(xintercept = 2001, lty = 2) + 
  geom_smooth(se=F)

miss <- ddply(data, "countryid", summarize,
              gdp.missing = sum(is.na(ny_gdp_pcap_kd)),
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

missing <- subset(miss, (miss$gdp.missing != 0) 
                  | ((miss$energyimports.missing > 12)
                     & (miss$renewablecons.missing > 20)
                     & (miss$FFconsumption.missing > 12)
                     & (miss$taxrevenuegdp.missing > 20)
                     & (miss$gdpperenergyu.missing > 12)
                     & (miss$naturresrents.missing > 20)
                     & (miss$gvtexpendeduc.missing > 20)
                     & (miss$gdpgrowthannu.missing > 20)
                     & (miss$renewableelec.missing > 20)
                     & (miss$energyuseinkg.missing > 20)
                     & (miss$fuelexportspc.missing > 20))
                  )
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

# whichnum("MEX")
# whichnum("LUX") # Pre-treatment trend is bonkers. If removed, optimization over longer period buggers.
# data <- subset(data, !(countryid %in% c(23)))



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 1        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1995-2001 ####
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
             list("CO2_emissions_PC" , 1995:2001 , "mean")),
           dependent = "CO2_emissions_PC",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1995:2001,
           time.plot = 1995:2005)


# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1



## ## ## ## ## ## ## ## ## ##
# SYNTH 1                ####
## ## ## ## ## ## ## ## ## ##

# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# .. Calculating annual discrepancy in emissions per capita ####
# between the UK and its synthetic counterpart
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)

# .. Pre-treatment predictor values ####
# for the UK, for the synthetic control unit, and for all units in the sample
synth.tables$tab.pred

# .. Weights for the predictor variables ####
synth.tables$tab.v

# .. Weights for countries in the donor pool ####
synth.tables$tab.w

# Plot emissions per capita pre- and post-intervention in the
# and in the synthetic control
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          Ylim = c(7,12),
          Main = "Observed and Synthetic Counterfactual Emissions",
          Legend = c("United Kingdom","Synthetic UK"),
          Legend.position = "bottomright")
abline(v = 2001, lty = 2)
#lines(seq(1995,2005,1), result$Y1plot, col = "red")
#lines(seq(1995,2005,1), synth, col = "green")


# Plot gaps in outcomes between the UK and the synthetic control
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          Ylim = c(-1.5,1.5),
          Main = "Gap between Treated and Synthetic Control")
abline(v = 2001, lty = 2)



## ## ## ## ## ## ## ## ## ##
# GENERATE RESULTS 1     ####
## ## ## ## ## ## ## ## ## ##

# .. Outcomes ####
names(dataprep.out)

dataprep.out$Y0plot
# Outcome variable in donor pool

dataprep.out$Y1plot
# Outcome variable in treated unit

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

years <- seq(1995,2005,1)
# Pre- and post-intervention periods

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control

# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Emissions paths in treated and synth.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = c(1995,2005), ylim = c(7,12), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, synth, col = "royalblue1", lty = 2, lwd = 2)
abline(v = 2001, lty = 2)
legend(2001.73, 7.98, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(2000, 8, 2000.9, 8, length = 0.1, code = 2)
text(1998.9, 8.04, "CCL enacted", cex = 0.8)
dev.off()

# .. Convert emissions per capita to tonnes and mega tonnes ####
result <- data.frame(year = years, gaps = gaps)
colnames(result) <- c("year", "gapPerCap")

sum(result[7:11,2])
# Sum of gaps in CO2 per capita emissions between the UK and its synthetic counterpart,
# for 2001-2005. This is a work-around, I will implement a numerical integration method
# to get the area between the curves.

population <- read.csv("population.csv", header = T)
population <- population[which(population$ISO == "GBR"),]
population$population <- population$population*10^3

result <- merge(result, population, by.x = "year", by.y = "year")
result$gaptCO2 <- result$gapPerCap * result$population
result$gapMtCO2 <- result$gaptCO2 / 10^6
(postCCL.avoidedPerCap <- sum(result$gapPerCap[7:11]))
(postCCL.avoidedMtCO2 <- sum(result$gapMtCO2[7:11]))
result <- result[-3]

# .. Recreating built-in Synth graph for gaps ####
pdf("../Figures/Gaps in emissions per capita.pdf", 
    height = 4.5, width = 6)
par(mar = c(5.1, 4.1, 4.1, 3.1))
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, gaps, 
     type = "l", col = "purple", lwd = 2,
     xlim = c(1995,2005), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(2000, -1, 2000.9, -1, length = 0.1, code = 2)
text(1998.9, -1, "CCL enacted", cex = 0.8)
par(new = TRUE, mar = c(5.1, 4.1, 4.1, 3.1))
plot(years, result$gapMtCO2, type = "n",
     axes = F, xlab = "", ylab = "",
     xlim = c(1995,2005),
     ylim = c(-90,90))
intervals <- seq(from = -90, to = 90, by = 30)
axis(side = 4, at = intervals, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
mtext(side = 4, expression(paste("MegaTons of ","CO"[2])), line = 2)
dev.off()

# .. Simple unformatted graphs ####
plot(result$year, result$gaptCO2, 
     type = "l", lwd = 2,
     ylim = c(-90*10^6, 90*10^6),
     xlab = "Year",
     ylab = "Tonnes of CO2",
     main = "Gaps between Synthetic UK and UK")
abline(h = 0, lty = 2)
abline(v = 2001, lty = 2)

plot(result$year, result$gapMtCO2, 
     type = "l", lwd = 2,
     ylim = c(-90, 90),
     xlab = "Year",
     ylab = "MegaTonnes of CO2",
     main = "Gaps between Synthetic UK and UK")
abline(h = 0, lty = 2)
abline(v = 2001, lty = 2)



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 2        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001 ####
# All covariates
# Counterfactual doesn't look as good as option 1

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = c("ny_gdp_pcap_kd", #GDP per capita (constant 2010 US$)
                          "eg_imp_cons_zs", #Energy imports, net (% of energy use)
                          "eg_fec_rnew_zs", #Renewable energy consumption (% of total final energy consumption)
                          "eg_use_comm_fo_zs", #Fossil fuel energy consumption (% of total)
                          "gc_tax_totl_gd_zs", #Tax revenue (% of GDP)
                          "eg_gdp_puse_ko_pp_kd", #GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          "ny_gdp_totl_rt_zs", #Total natural resources rents (% of GDP)
                          "se_xpd_totl_gd_zs", #Government expenditure on education, total (% of GDP)
                          "ny_gdp_mktp_kd_zg", #GDP growth (annual %)
                          "eg_elc_rnew_zs", #Renewable electricity output (% of total electricity output)
                          "eg_use_pcap_kg_oe", #Energy use (kg of oil equivalent per capita)
                          "tx_val_fuel_zs_un", #Fuel exports (% of merchandise exports)
                          "ne_exp_gnfs_zs", #Exports of goods and services (% of GDP)
                          "ne_imp_gnfs_zs" #Imports of goods and services (% of GDP)
           ),
           predictors.op = "mean" ,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("CO2_emissions_PC" , 1995:2001 , "mean")),
           dependent = "CO2_emissions_PC",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:2001,
           time.plot = 1990:2005)


# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1



## ## ## ## ## ## ## ## ## ##
# SYNTH 2                ####
## ## ## ## ## ## ## ## ## ##

# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# .. Calculating annual discrepancy in emissions per capita #### 
# between the UK and its synthetic counterpart
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)

# .. Pre-treatment predictor values ####
# for the UK, for the synthetic control unit, and for all units in the sample
synth.tables$tab.pred

# .. Weights for the predictor variables ####
synth.tables$tab.v

# .. Weights for countries in the donor pool ####
synth.tables$tab.w

# Plot emissions per capita pre- and post-intervention in the
# and in the synthetic control
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          #Ylim = c(0,12),
          Main = "Synthetic Counterfactual",
          Legend = c("United Kingdom","Synthetic UK"),
          Legend.position = "bottomright")
abline(v = 2001)

# Plot gaps in outcomes between the UK and the synthetic control
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          #Ylim = c(-1.5,1.5),
          Main = "Gap between Treated and Synthetic Control")
abline(v = 2001)



## ## ## ## ## ## ## ## ## ##
# PLACEBO TEST 1         ####
## ## ## ## ## ## ## ## ## ##

# Placebo test for specification 1
# Mexico is the placebo (only passed carbon tax in 2013)

placebo.unit <- data[which(data$countrycode == "MEX"), 1][1]
placebocontrol.units <- t(unique(subset(data, !(countrycode %in% c("MEX", "GBR")))[1]))

for (i in 1:length(placebocontrol.units)){
  print(whodat(placebocontrol.units[i]))
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
             list("CO2_emissions_PC" , 1995:2001 , "mean")),
           dependent = "CO2_emissions_PC",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = placebo.unit,
           controls.identifier = c(placebocontrol.units),
           time.optimize.ssr = 1995:2001,
           time.plot = 1995:2005)

# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# .. Calculating annual discrepancy in emissions per capita ####
# between the placebo and its synthetic counterpart
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# Plot emissions per capita pre- and post-intervention in the placebo
# and in the synthetic control
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          #Ylim = c(0,12),
          Main = "Placebo Test - Synthetic Counterfactual",
          Legend = c("Mexico","Synthetic Mexico"),
          Legend.position = "bottomright")
abline(v = 2001)

# .. Plot gaps in outcomes between Mexico and the synthetic control ####
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          Ylim = c(-1.5,1.5),
          Main = "Gap between Placebo and Synthetic Control")
abline(v = 2001)



## ## ## ## ## ## ## ## ## ##
# GENERATE PLACEBO 1     ####
## ## ## ## ## ## ## ## ## ##

# .. Outcomes ####
names(dataprep.out)

dataprep.out$Y0plot
# Outcome variable in donor pool

dataprep.out$Y1plot
# Outcome variable in treated unit

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

years <- seq(1995,2005,1)
# Pre- and post-intervention periods

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control

# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Emissions paths in placebo and synth.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "#11904E", lwd = 2,
     xlim = c(1995,2005), 
     ylim = c(2.5, 5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, synth, col = "#014421", lty = 2, lwd = 2)
abline(v = 2001, lty = 2)
legend(2001.6, 2.99, c("Mexico", "Synthetic Mexico"),
       lty = c(1,2), lwd = c(2,2), col = c("#11904E", "#014421"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(2000, 3, 2000.9, 3, length = 0.1, code = 2)
text(1998.9, 3.02, "CCL enacted", cex = 0.8)
dev.off()

# .. Convert emissions per capita to tonnes and mega tonnes ####
result <- data.frame(year = years, gaps = gaps)
colnames(result) <- c("year", "gapPerCap")

sum(result[7:11,2])
# Sum of gaps in CO2 per capita emissions between the UK and its synthetic counterpart,
# for 2001-2005. This is a work-around, I will implement a numerical integration method
# to get the area between the curves.

population <- read.csv("population.csv", header = T)
population <- population[which(population$ISO == "MEX"),]
population$population <- population$population*10^3

result <- merge(result, population, by.x = "year", by.y = "year")
result$gaptCO2 <- result$gapPerCap * result$population
result$gapMtCO2 <- result$gaptCO2 / 10^6
(postCCL.avoidedPerCap <- sum(result$gapPerCap[7:11]))
(postCCL.avoidedMtCO2 <- sum(result$gapMtCO2[7:11]))
result <- result[-3]

# .. Recreating built-in Synth graph for gaps ####
pdf("../Figures/Gaps in emissions per capita_placebo.pdf", 
    height = 4.5, width = 6)
par(mar = c(5.1, 4.1, 4.1, 3.1))
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, gaps, 
     type = "l", col = "purple", lwd = 2,
     xlim = c(1995,2005), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(2000, -1, 2000.9, -1, length = 0.1, code = 2)
text(1998.9, -1, "CCL enacted", cex = 0.8)
par(new = TRUE, mar = c(5.1, 4.1, 4.1, 3.1))
plot(years, result$gapMtCO2, type = "n",
     axes = F, xlab = "", ylab = "",
     xlim = c(1995,2005),
     ylim = c(-150,150))
axis(side = 4, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
mtext(side = 4, expression(paste("MegaTons of ","CO"[2])), line = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# TRAJECTORY BALANCING   ####
## ## ## ## ## ## ## ## ## ##

devtools::install_github("chadhazlett/kbal")

library(KBAL)

data$GBR_treat <- 0
data$GBR_treat[which(data$countrycode=="GBR")] <- 1

dataCO2 <- data[,c(1,4:20)]
dataCO2.pretreat <- dataCO2[which(dataCO2$year < 2001),]
dataCO2.pretreat <- dataCO2.pretreat[which(dataCO2.pretreat$year > 1994),]

colnames(dataCO2.pretreat)
dataCO2.kbal <- dataCO2.pretreat[,which(names(dataCO2.pretreat)%in%c("countryid","year","GBR_treat", "CO2_emissions_PC"))]

c.ids <- levels(as.factor(dataCO2.kbal$countryid))

expand.data <- as.data.frame(c.ids)
expand.data$GBR_treat <- 0

## The UK is Country ID # 14

expand.data$GBR_treat[which(expand.data$c.ids==14)] <- 1

for (j in 1995:2000){
  eval(parse(text=paste0("expand.data$CO2.pc.",j," <- NA")))
}

for(i in 1:length(c.ids)){
  for(j in 1995:2000){
    eval(parse(text=paste0("expand.data$CO2.pc.",j,"[i] <- dataCO2.kbal$CO2_emissions_PC[dataCO2.kbal$year==j][i]")))
  }
}


write.csv(expand.data, file="matto_data.Rda")

treatment.vector <- expand.data$GBR_treat
data.matrix <- expand.data[,-which(names(expand.data)%in%c("GBR_treat", "c.ids"))]
data.matrix <- as.matrix(data.matrix)
kbal.out <- kbal(method="el",X=data.matrix, D=treatment.vector, sigma=ncol(data.matrix))


#kbal.out=kbal(method="el",X=Ypre, D=D, sigma=ncol(Ypre))

#Where Ypre is a matrix whose rows are observations and columns are Y, all for pre-treatment era.   
#Sigma is up to you. If it's a really hard problem you may need a higher Sigma.
#But a good starting point is to set it to the number of pre-treatment time periods (i.e. ncol(Ypre), which I've put above), but you can try a bunch. 
#What we typically do is choose the sigma that maximizes:#  kbal.out$L1_orig/kbal.out$L1_kbal

#kbal.w0=kbal.out$w[D==0]
#kbal.w0=kbal.w0/sum(kbal.w0)

kbal.w0=kbal.out$w[expand.data$GBR_treat==0]
kbal.w0=kbal.w0/sum(kbal.w0)

uk.actual <- expand.data[expand.data$GBR_treat==1,]

synth.uk.temp <- expand.data[expand.data$GBR_treat==0,-which(names(expand.data)%in%c("c.ids"))] * kbal.w0
uk.synthetic <- colSums(synth.uk.temp)

uk.actual
uk.synthetic

