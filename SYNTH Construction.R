# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# Specification Working
# .. Optimize over 1980-2001, 1990 baseline, no covariates
# Synth
# .. Running Synth 
# .. Pre-treatment predictor values 
# .. Weights for the predictor variables 
# .. Weights for countries in the donor pool 
# .. Export results
# Generate Results    
# .. Outcomes 
# .. Convert emissions per capita to tonnes and mega tonnes 
# .. Recreating built-in Synth graph for paths 
# .. Recreating built-in Synth graph for gaps 
# Placebo for Single Country 
# .. Running Synth 
# .. Outcomes 
# .. Recreating built-in Synth graph for paths 
# .. Recreating built-in Synth graph for gaps 
# Placebo Loops
# .. Running Synth 
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Placebo figure
# .. MSPE analysis
# Leave-One-Out Check    
# .. Running Synth 
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Leave-one-out figure 
# Re-Assign Treatment Year
# .. Running Synth
# .. Treatment in 2001
# .. Treatment in 1999
# .. Treatment in 1997
# .. Treatment in 1995
# .. Treatment in 1993
# .. Treatment in 1991
# .. Treatment in 1989
# .. Treatment in 1987
# .. Treatment in 1985
# Trajectory Balancing



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

UK <- subset(data, (countrycode %in% c("GBR")))

pdf("../Figures/CO2 emissions in sample.pdf", 
    height = 4.5, width = 6)
ggplot(data[!data$countrycode == "GBR",], aes(x = year, y = CO2_emissions_PC, col = countryname)) + 
  geom_line() +
  xlab("Year") + ylab(expression(paste("CO"[2], " emissions per capita"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data[data$countrycode == "GBR",], 
            aes(x = year, y = CO2_emissions_PC, col = "United Kingdom"), size = 1.5) +
  theme(legend.position="bottom", legend.title = element_blank())
dev.off()

pdf("../Figures/CO2 emissions (1990) in sample.pdf", 
    height = 4.5, width = 6)
ggplot(data[!data$countrycode == "GBR",], aes(x = year, y = rescaled1990, col = countryname)) + 
  geom_line() +
  xlab("Year") + ylab(expression(paste("CO"[2], " emissions against 1990 baseline"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data[data$countrycode == "GBR",], 
            aes(x = year, y = rescaled1990, col = "United Kingdom"), size = 1.5) +
  theme(legend.position="bottom", legend.title = element_blank())
dev.off()

pdf("../Figures/CO2 emissions (1990 log diff) in sample.pdf", 
    height = 4.5, width = 6)
ggplot(data[!data$countrycode == "GBR",], aes(x = year, y = logdiff, col = countryname)) + 
  geom_line() +
  xlab("Year") + ylab(expression(paste("CO"[2], " emissions against 1990 baseline (log difference)"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data[data$countrycode == "GBR",], 
            aes(x = year, y = logdiff, col = "United Kingdom"), size = 1.5) +
  theme(legend.position="bottom", legend.title = element_blank())
dev.off()

# Zooming in
ggplot(data[which(!data$countrycode == "GBR" & data$CO2_emissions_PC >= 5 & data$CO2_emissions_PC <= 15),], 
       aes(x = year, y = CO2_emissions_PC, col = countryname)) + 
  geom_line() +
  xlab("Year") + ylab(expression(paste("CO"[2], " emissions per capita"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data[which(data$countrycode == "GBR" & data$CO2_emissions_PC >= 5 & data$CO2_emissions_PC <= 15),], 
            aes(x = year, y = CO2_emissions_PC, col = "United Kingdom"), size = 1.5) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_y_continuous(labels = function(x) round(as.numeric(x), 0)) 



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION WORKING  ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1980-2001, 1990 baseline, no covariates ####

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1980:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
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
           time.plot = 1980:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Specification.txt")



## ## ## ## ## ## ## ## ## ##
# SYNTH                  ####
## ## ## ## ## ## ## ## ## ##

# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

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


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results Specification.txt")


# Plot emissions per capita pre- and post-intervention in the
# and in the synthetic control
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions relative to 1990",
          Xlab = "Year",
          #Ylim = c(0.9, 1.1),
          #Ylim = range(dataprep.out$Y1plot, dataprep.out$Y0plot %*% synth.out$solution.w),
          Main = "Observed and Synthetic Counterfactual Emissions",
          Legend = c("United Kingdom","Synthetic UK"),
          Legend.position = "bottomright")
abline(v = 2001, lty = 2)

# Plot gaps in outcomes between the UK and the synthetic control
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions relative to 1990",
          Xlab = "Year",
          #Ylim = range(gaps),
          Main = "Gap between Treated and Synthetic Control")
abline(v = 2001, lty = 2)



## ## ## ## ## ## ## ## ## ##
# GENERATE RESULTS       ####
## ## ## ## ## ## ## ## ## ##

# .. Outcomes ####
names(dataprep.out)

dataprep.out$Y0plot
# Outcome variable in donor pool

dataprep.out$Y1plot
# Outcome variable in treated unit

years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# # .. Convert emissions per capita to tonnes and mega tonnes ####
# result <- data.frame(year = years, gaps = gaps)
# colnames(result) <- c("year", "gapPerCap")
# 
# sum(result[7:11,2])
# # Sum of gaps in CO2 per capita emissions between the UK and its synthetic counterpart,
# # for 2001-2005. This is a work-around, I will implement a numerical integration method
# # to get the area between the curves.
# 
# population <- read.csv("population.csv", header = T)
# population <- population[which(population$ISO == "GBR"),]
# population$population <- population$population*10^3
# 
# result <- merge(result, population, by.x = "year", by.y = "year")
# result$gaptCO2 <- result$gapPerCap * result$population
# result$gapMtCO2 <- result$gaptCO2 / 10^6
# (postCCL.avoidedPerCap <- sum(result$gapPerCap[7:11]))
# (postCCL.avoidedMtCO2 <- sum(result$gapMtCO2[7:11]))
# result <- result[-3]


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
     xlim = c(1980,2005), ylim = c(0.9,1.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, synth, col = "royalblue1", lty = 2, lwd = 2)
abline(v = 2001, lty = 2)
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Recreating built-in Synth graph for gaps ####
pdf("../Figures/Gaps in emissions in treated.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, gaps, 
     type = "l", col = "purple", lwd = 2,
     xlim = c(1980,2005), 
     ylim = c(-0.1,0.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1996.5, -0.0695, "CCL enacted", cex = 0.8)
dev.off()


# Back of the enveloppe comparison with Cambridge Econometrics result
# 2002 emissions in reference case scenario are 154 MtC
154*44/12 * 10^6 / population$population[population$year == 2002]
# Synth value is 9.61 in 2002



## ## ## ## ## ## ## ## ## ## ## ##
# PLACEBO FOR SINGLE COUNTRY   ####
## ## ## ## ## ## ## ## ## ## ## ##

# Placebo test for a single country
# Mexico is the placebo (only passed carbon tax in 2013)

placebo.unit <- data[which(data$countrycode == "MEX"), 1][1]
placebocontrol.units <- t(unique(subset(data, !(countrycode %in% c("MEX", "GBR")))[1]))
for (i in 1:length(placebocontrol.units)){
  print(whodat(placebocontrol.units[i]))
}

choose.time.predictors <- 1980:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean"),
             list("rescaled1990", 1997:1998, "mean"),
             list("rescaled1990", 1999:2000, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = placebo.unit,
           controls.identifier = c(placebocontrol.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1980:2005)


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Plot emissions per capita pre- and post-intervention in the placebo
# and in the synthetic control
path.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions relative to 1990",
          Xlab = "Year",
          #Ylim = c(0.6,1.6),
          #Ylim = range(dataprep.out$Y1plot, dataprep.out$Y0plot %*% synth.out$solution.w),
          Main = "Placebo Test - Synthetic Counterfactual",
          Legend = c("Mexico","Synthetic Mexico"),
          Legend.position = "bottomright")
abline(v = 2001)

# Plot gaps in outcomes between Mexico and the synthetic control
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions relative to 1990",
          Xlab = "Year",
          #Ylim = c(-1.5,1.5),
          Main = "Gap between Placebo and Synthetic Control")
abline(v = 2001)


# .. Outcomes ####
names(dataprep.out)

dataprep.out$Y0plot
# Outcome variable in donor pool

dataprep.out$Y1plot
# Outcome variable in treated unit

years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

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
     type = "l", col = "#014421", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.6,1.6), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, synth, col = "#11904E", lty = 2, lwd = 2)
abline(v = 2001, lty = 2)
legend(1980, 0.8, c("Mexico", "Synthetic Mexico"),
       lty = c(1,2), lwd = c(2,2), col = c("#014421", "#11904E"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.75, 2000.9, 0.75, length = 0.1, code = 2)
text(1996.5, 0.754, "CCL enacted", cex = 0.8)
dev.off()


# .. Recreating built-in Synth graph for gaps ####
pdf("../Figures/Gaps in emissions in placebo.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, gaps, 
     type = "l", col = "purple", lwd = 2,
     xlim = c(1980,2005), 
     ylim = c(-0.1,0.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1996.5, -0.0695, "CCL enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ##
# PLACEBO LOOPS          ####
## ## ## ## ## ## ## ## ## ##

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

# for (i in 1:length(placebos)){
#   print(i)
#   print(placebos[i])
#   placebocontrol.units <- c(countries)[-i]
#   print(placebocontrol.units)
# }

placebo.names <- NA
for (i in 1:length(countries)){
placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1980:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean"),
             list("rescaled1990", 1997:1998, "mean"),
             list("rescaled1990", 1999:2000, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = placebos[i],
           controls.identifier = placebos[-i],
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1980:2005)


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")


# .. Calculating annual gaps between the treated and its synthetic counterpart ####
store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}


# .. Placebo figure ####
placebo.results <- store
rownames(placebo.results) <- c(choose.time.predictors, seq(2002, 2005, 1))
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
years <- c(choose.time.predictors, seq(2002, 2005, 1))
gap.end.pre <- which(rownames(placebo.results)=="2001")

# Mean Square Prediction Error Pre-Treatment
mse <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.mse <- as.numeric(mse["GBR"])

# Exclude countries with 5 times higher MSPE than UK
placebo.results[, mse > 5*UK.mse]
# Exclude BEL, CHL, ESP, FIN, FRA, HUN, IRL, ISL, ISR, KOR, LUX, NZL, POL, PRT, TUR
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Gaps in emissions_placebo_all.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "purple", lwd = 2,
     xlim = c(1980,2005), 
     ylim = c(-0.1,0.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1996.5, -0.0695, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "purple", lwd = 2)
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="CHE")],
      type = "l", col = "deeppink")
text(2003.5, -0.09, "CHE", cex = 0.8, col = "deeppink")
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="AUT")],
      type = "l", col = "firebrick")
text(2003.5, 0.09, "AUT", cex = 0.8, col = "firebrick")
dev.off()

# Exclude countries with 4 times higher MSPE than UK
mse <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
placebo.results[, mse > 4*UK.mse]
# Exclude AUT, CHE, GRC, MEX
placebo.results <- placebo.results[, mse < 4*UK.mse]

# Plot, stricter MSPE cut-off
pdf("../Figures/Gaps in emissions_placebo_all_MSPE4.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "purple", lwd = 2,
     xlim = c(1980,2005), 
     ylim = c(-0.1,0.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1996.5, -0.0695, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "purple", lwd = 2)
dev.off()

# How many control states remain?
colnames(placebo.results)
# 6 control states: AUS, CAN, GBR, ITA, JPN, USA


# .. MSPE analysis ####
placebo.results <- store
rownames(placebo.results) <- c(choose.time.predictors, seq(2002, 2005, 1))

# Mean Square Prediction Error Pre-Treatment
pre.mse <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.mse <- as.numeric(pre.mse["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.end.post <- which(rownames(placebo.results)=="2002")
post.mse <- apply(placebo.results[gap.end.post:gap.end, ]^2, 2, mean)
UK.post.mse <- as.numeric(post.mse["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.mse <- post.mse/pre.mse
sort(ratio.mse)
# For the UK, the post-treatment gap is 20 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 5/25 = 0.2

# Plot
pdf("../Figures/MSPE Ratio.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "purple", "grey")
a <- barplot(sort(ratio.mse),
        xaxt = "n",
        yaxt = "n",
        col = cols,
        main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
        cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "purple", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# LEAVE-ONE-OUT CHECK    ####
## ## ## ## ## ## ## ## ## ##

countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

# for (i in 1:length(countries)){
#    print(countries[i])
#    leaveoneout <- c(countries)[-i]
#    print(leaveoneout)
# }

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

# Leave-one-out controls without AUT and CHE
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR", "AUT", "CHE")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1980:2005), length(countries))
store <- matrix(NA, length(1980:2005), length(countries)-2)
# For when AUT and CHE are dropped
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

# for (i in 1:length(leaveoneout.controls)+1){
#   print(i)
# }

for (i in 1:nloops){
controls.identifier = leaveoneout.controls[-i]
print(controls.identifier)
}

nloops <- length(leaveoneout.controls)+1
for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("rescaled1990", 1981:1982, "mean"),
               list("rescaled1990", 1983:1984, "mean"),
               list("rescaled1990", 1985:1986, "mean"),
               list("rescaled1990", 1987:1988, "mean"),
               list("rescaled1990", 1989:1990, "mean"),
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
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1980:2005)


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")


# .. Calculating annual gaps between the UK and its synthetic counterpart ####
store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- c(choose.time.predictors, seq(2002, 2005, 1))
leaveoneout.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(leaveoneout.results)
years <- c(choose.time.predictors, seq(2002, 2005, 1))
gap.end.pre <- which(rownames(placebo.results)=="2001")

# Plot
pdf("../Figures/Gaps in emissions_leave one out_no AUT or CHE.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "purple", lwd = 2,
     xlim = c(1980,2005), 
     ylim = c(-0.1,0.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Leave-one-out robustness check", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1996.5, -0.0695, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "purple", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.6, -0.017, "LUX", cex = 0.8, col = "darkorange")
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_MEX")],
      type = "l", col = "#014421")
 text(2004.6, -0.099, "MEX", cex = 0.8, col = "#014421")
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_POL")],
     type = "l", col = "hotpink")
text(2001.9, -0.098, "POL", cex = 0.8, col = "hotpink")
dev.off()



## ## ## ## ## ## ## ## ## ## ##
# RE-ASSIGN TREATMENT YEAR  ####
## ## ## ## ## ## ## ## ## ## ##

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

placebo.years <- c(seq(1985, 2001, 2))

store <- matrix(NA, length(1980:2005), length(placebo.years))
colnames(store) <- paste("Tx_", c(seq(1985, 2001, 2)), sep = "")
store


# .. Treatment in 2001 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
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
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,9] <- dataprep.out$Y0plot %*% synth.out$solution.w


# .. Treatment in 1999 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1999,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean"),
             list("rescaled1990", 1997:1998, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1999,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,8] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1999.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1999", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,8], col = "coral", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1999, lty = 2, col = "coral")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1997 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1997,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1997,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,7] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1997.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1997", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,7], col = "cornflowerblue", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1997, lty = 2, col = "cornflowerblue")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1995 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1995,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1995,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,6] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1995.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1995", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,6], col = "darkgoldenrod1", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1995, lty = 2, col = "darkgoldenrod1")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1993 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1993,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean"),
             list("rescaled1990", 1991:1992, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1993,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,5] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1993.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1993", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,5], col = "darkorchid", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1993, lty = 2, col = "darkorchid")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1991 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1991,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean"),
             list("rescaled1990", 1989:1990, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1991,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,4] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1991.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1991", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,4], col = "mediumseagreen", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1991, lty = 2, col = "mediumseagreen")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1989 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1989,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean"),
             list("rescaled1990", 1987:1988, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1989,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,3] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1989.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1989", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,3], col = "darkturquoise", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1989, lty = 2, col = "darkturquoise")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1987 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1987,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean"),
             list("rescaled1990", 1985:1986, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1987,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,2] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1987.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1987", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,2], col = "darkviolet", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1987, lty = 2, col = "darkviolet")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Treatment in 1985 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           time.predictors.prior = 1980:1985,
           special.predictors = list(
             list("rescaled1990", 1981:1982, "mean"),
             list("rescaled1990", 1983:1984, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1980:1985,
           time.plot = 1980:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store[,1] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1985.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = c(1980,2005), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1985", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, placeboyear.results[,9], col = "#BE3144", lty = 2, lwd = 2)
lines(years, placeboyear.results[,1], col = "deeppink", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1985, lty = 2, col = "deeppink")
legend(1980, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1996.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()



# #### All below is archived for now.
# 
# # ## ## ## ## ## ## ## ## ## ##
# # # TRAJECTORY BALANCING   ####
# # ## ## ## ## ## ## ## ## ## ##
# # 
# # devtools::install_github("chadhazlett/kbal")
# # 
# # library(KBAL)
# # 
# # data$GBR_treat <- 0
# # data$GBR_treat[which(data$countrycode=="GBR")] <- 1
# # 
# # dataCO2 <- data[,c(1,4:20)]
# # dataCO2.pretreat <- dataCO2[which(dataCO2$year < 2001),]
# # dataCO2.pretreat <- dataCO2.pretreat[which(dataCO2.pretreat$year > 1994),]
# # 
# # colnames(dataCO2.pretreat)
# # dataCO2.kbal <- dataCO2.pretreat[,which(names(dataCO2.pretreat)%in%c("countryid","year","GBR_treat", "CO2_emissions_PC"))]
# # 
# # c.ids <- levels(as.factor(dataCO2.kbal$countryid))
# # 
# # expand.data <- as.data.frame(c.ids)
# # expand.data$GBR_treat <- 0
# # 
# # ## The UK is Country ID # 14
# # 
# # expand.data$GBR_treat[which(expand.data$c.ids==14)] <- 1
# # 
# # for (j in 1995:2000){
# #   eval(parse(text=paste0("expand.data$CO2.pc.",j," <- NA")))
# # }
# # 
# # for(i in 1:length(c.ids)){
# #   for(j in 1995:2000){
# #     eval(parse(text=paste0("expand.data$CO2.pc.",j,"[i] <- dataCO2.kbal$CO2_emissions_PC[dataCO2.kbal$year==j][i]")))
# #   }
# # }
# # 
# # write.csv(expand.data, file="matto_data.Rda")
# # 
# # treatment.vector <- expand.data$GBR_treat
# # data.matrix <- expand.data[,-which(names(expand.data)%in%c("GBR_treat", "c.ids"))]
# # data.matrix <- as.matrix(data.matrix)
# # kbal.out <- kbal(method="el",X=data.matrix, D=treatment.vector, sigma=ncol(data.matrix))
# # 
# # #kbal.out=kbal(method="el",X=Ypre, D=D, sigma=ncol(Ypre))
# # 
# # #Where Ypre is a matrix whose rows are observations and columns are Y, all for pre-treatment era.   
# # #Sigma is up to you. If it's a really hard problem you may need a higher Sigma.
# # #But a good starting point is to set it to the number of pre-treatment time periods (i.e. ncol(Ypre), which I've put above), but you can try a bunch. 
# # #What we typically do is choose the sigma that maximizes:#  kbal.out$L1_orig/kbal.out$L1_kbal
# # 
# # #kbal.w0=kbal.out$w[D==0]
# # #kbal.w0=kbal.w0/sum(kbal.w0)
# # 
# # kbal.w0=kbal.out$w[expand.data$GBR_treat==0]
# # kbal.w0=kbal.w0/sum(kbal.w0)
# # 
# # uk.actual <- expand.data[expand.data$GBR_treat==1,]
# # 
# # synth.uk.temp <- expand.data[expand.data$GBR_treat==0,-which(names(expand.data)%in%c("c.ids"))] * kbal.w0
# # uk.synthetic <- colSums(synth.uk.temp)
# # 
# # uk.actual
# # uk.synthetic