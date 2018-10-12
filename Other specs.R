# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# Specification 1
# .. Optimize over 1980-2001, 1990 baseline, no covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 2
# .. Optimize over 1990-2001, 1990 baseline, with covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 3
# .. Optimize over 1990-2001, CO2 per capita, no covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 4
# .. Optimize over 1990-2001, CO2 per capita, with covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 5   
# .. Optimize over 1995-2001, CO2 per capita, no covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 6   
# .. Optimize over 1995-2001, CO2 per capita, with covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 7
# .. Optimize over 1990-2001, difference in log levels, with covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Recreating built-in Synth graph for paths
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure



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
capture.output(synth.spec, file = "Supplementary Information/Specification 1.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 1.txt")
donor.weights <- as.data.frame(synth.tables$tab.w)[,-3]


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 1.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1), 
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


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

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
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")
  
  # Calculating annual gaps between the treated and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 1.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
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
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 1.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1980:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
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
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1980:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 1.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
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
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2003.5, -0.009, "No LUX", cex = 0.8, col = "darkorange")
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 2        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, 1990 baseline, with covariates ####

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
           predictors.op = "mean",
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", choose.time.predictors, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1990:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Supplementary Information/Specification 2.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 2.txt")
donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 2.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1), 
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
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCL enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

placebo.names <- NA
for (i in 1:length(countries)){
  placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("rescaled1990", choose.time.predictors, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the treated and its synthetic counterpart
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
# Exclude AUT, BEL, CHE, CHL, ESP, FIN, FRA, HUN, IRL, ISL, ISR, KOR, LUX, MEX, NZL, POL, PRT, TUR
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 2.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
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
arrows(1999.5, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1998, -0.0695, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
# For the UK, the post-treatment gap is 15 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 5/25 = 0.2

# Plot
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 2.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("rescaled1990", choose.time.predictors, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")
  
  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 2.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
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
arrows(1999.5, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1998, -0.0695, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.1, -0.017, "No LUX", cex = 0.8, col = "darkorange")
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 3        ####
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
           time.plot = 1990:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Supplementary Information/Specification 3.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 3.txt")
donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 3.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(7, 12), 
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
legend(1990, 7.98, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8, 2000.9, 8, length = 0.1, code = 2)
text(1997.5, 8.04, "CCL enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

placebo.names <- NA
for (i in 1:length(countries)){
  placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
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
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the treated and its synthetic counterpart
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
# Exclude AUS, AUT, BEL, CAN, CHL, ESP, FIN, FRA, HUN, IRL, ISL, ISR, KOR, LUX, MEX, POL, PRT, TUR, USA
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 3.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.5, -0.5, 2000.9, -0.5, length = 0.1, code = 2)
text(1998, -0.48, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
# For the UK, the post-treatment gap is 73 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/25 = 0.04

# Plot
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 3.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
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
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")
  
  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 3.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Leave-one-out robustness check", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.5, -1.048, 2000.9, -1.048, length = 0.1, code = 2)
text(1998, -1.045, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.1, -0.15, "No LUX", cex = 0.8, col = "darkorange")
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 4        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, CO2 per capita, with covariates ####

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
           predictors.op = "mean",
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("CO2_emissions_PC", choose.time.predictors, "mean")),
           dependent = "CO2_emissions_PC",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1990:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Supplementary Information/Specification 4.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 4.txt")
donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 4.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(7, 12), 
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
legend(1990, 7.98, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8, 2000.9, 8, length = 0.1, code = 2)
text(1997.5, 8.04, "CCL enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

placebo.names <- NA
for (i in 1:length(countries)){
  placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("CO2_emissions_PC", choose.time.predictors, "mean")),
             dependent = "CO2_emissions_PC",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the treated and its synthetic counterpart
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
# Exclude FIN, HUN, IRL, KOR, LUX, POL, USA
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 4.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.5, -1, 2000.9, -1, length = 0.1, code = 2)
text(1998, -0.98, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
# For the UK, the post-treatment gap is 0.68 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 20/25 = 0.8

# Plot
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 4.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("CO2_emissions_PC", choose.time.predictors, "mean")),
             dependent = "CO2_emissions_PC",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 4.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Leave-one-out robustness check", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.5, -1.048, 2000.9, -1.048, length = 0.1, code = 2)
text(1998, -1.045, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 5        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1995-2001, CO2 per capita, no covariates ####

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
control.units <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
control.units
for (i in 1:length(control.units)){
  print(whodat(control.units[i]))
}

choose.time.predictors <- 1995:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
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
capture.output(synth.spec, file = "Supplementary Information/Specification 5.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 5.txt")
donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 5.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(7, 12), 
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
legend(1995, 7.98, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999.9, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1998.9, 8.54, "CCL enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

placebo.names <- NA
for (i in 1:length(countries)){
  placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1995:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("CO2_emissions_PC", 1995:1996, "mean"),
               list("CO2_emissions_PC", 1997:1998, "mean"),
               list("CO2_emissions_PC", 1999:2000, "mean")),
             dependent = "CO2_emissions_PC",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1995:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the treated and its synthetic counterpart
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
# Exclude FIN, IRL, KOR, LUX, POL, TUR, USA
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 5.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.9, -0.5, 2000.9, -0.5, length = 0.1, code = 2)
text(1998.9, -0.48, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
# For the UK, the post-treatment gap is 41 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/25 = 0.08

# Plot
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 5.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1995:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("CO2_emissions_PC", 1995:1996, "mean"),
               list("CO2_emissions_PC", 1997:1998, "mean"),
               list("CO2_emissions_PC", 1999:2000, "mean")),
             dependent = "CO2_emissions_PC",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1995:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 5.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Leave-one-out robustness check", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.9, -1.048, 2000.9, -1.048, length = 0.1, code = 2)
text(1998.9, -1.045, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.4, -0.14, "No LUX", cex = 0.8, col = "darkorange")
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 6        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1995-2001, CO2 per capita, with covariates ####

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
           predictors.op = "mean",
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("CO2_emissions_PC", choose.time.predictors, "mean")),
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
capture.output(synth.spec, file = "Supplementary Information/Specification 6.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 6.txt")
donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 6.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(7, 12), 
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
legend(1995, 7.98, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999.9, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1998.9, 8.54, "CCL enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

placebo.names <- NA
for (i in 1:length(countries)){
  placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1995:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("CO2_emissions_PC", choose.time.predictors, "mean")),
             dependent = "CO2_emissions_PC",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1995:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the treated and its synthetic counterpart
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
# Exclude AUS, CHL, ESP, FIN, IRL, KOR, LUX, NZL, POL, PRT, TUR, USA
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 6.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.9, -0.5, 2000.9, -0.5, length = 0.1, code = 2)
text(1998.9, -0.48, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
# For the UK, the post-treatment gap is 96 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/25 = 0.08

# Plot
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 6.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1995:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("CO2_emissions_PC", choose.time.predictors, "mean")),
             dependent = "CO2_emissions_PC",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1995:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 6.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.5,1.5), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Leave-one-out robustness check", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.9, -1.048, 2000.9, -1.048, length = 0.1, code = 2)
text(1998.9, -1.045, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_MEX")],
      type = "l", col = "hotpink")
text(2004.4, -0.12, "No MEX", cex = 0.8, col = "hotpink")
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.4, -0.56, "No LUX", cex = 0.8, col = "darkorange")
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_AUS")],
      type = "l", col = "firebrick1")
text(2004.4, -0.98, "No AUS", cex = 0.8, col = "firebrick1")
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 7        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, difference in log levels, with covariates ####

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
           predictors.op = "mean",
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("logdiff", choose.time.predictors, "mean")),
           dependent = "logdiff",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1990:2005)

# Predictor variables for the UK
dataprep.out$X1

# Pre-intervention outcomes in the UK
dataprep.out$Z1

# Store specification details
synth.spec <- list(treated = dataprep.out$tag[["treatment.identifier"]],
                   donor.pool = dataprep.out$tag[["controls.identifier"]],
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Supplementary Information/Specification 7.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Supplementary Information/Results Specification 7.txt")
donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])
donor.weights <- donor.weights[, -c(4,6,8,10,12,14)]
donor.weights <- donor.weights %>%
  select(unit.names, everything())
colnames(donor.weights) <- c("Donor countries", "Specification 1", "Specification 2",
                             "Specification 3", "Specification 4", "Specification 5",
                             "Specification 6", "Specification 7")
stargazer(donor.weights, summary = F, rownames = F)


# .. Generate results ####
years <- c(choose.time.predictors, seq(2002, 2005, 1))
# Pre- and post-intervention periods

synth <- dataprep.out$Y0plot %*% synth.out$solution.w
# Outcome variable in synthetic unit

gaps <- dataprep.out$Y1plot - synth
# Gaps between outcomes in treated and synthetic control


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Supplementary Information/Emissions paths in treated and synth_Spec 7.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), ylim = c(-0.06, 0.06), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990 (log difference)")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, synth, col = "royalblue1", lty = 2, lwd = 2)
abline(v = 2001, lty = 2)
legend(1990, 0.06, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.05, 2000.9, 0.05, length = 0.1, code = 2)
text(1997.5, 0.051, "CCL enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####

countries <- t(unique(data[1]))
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

placebos <- c(countries)

placebo.names <- NA
for (i in 1:length(countries)){
  placebo.names[i] <- whodat(countries[i])
}
placebo.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- placebo.names
store

for (i in 1:length(placebos)){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("logdiff", choose.time.predictors, "mean")),
             dependent = "logdiff",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")

  # Calculating annual gaps between the treated and its synthetic counterpart
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
# Exclude CHL, ESP, FIN, FRA, HUN, IRL, ISL, ISR, KOR, LUX, NZL, POL, PRT, TUR
placebo.results <- placebo.results[, mse < 5*UK.mse]

# Plot
pdf("../Figures/Supplementary Information/Gaps in emissions_placebo_all_Spec 7.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.2,0.2), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990 (log difference)")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo countries", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.5, -0.15, 2000.9, -0.15, length = 0.1, code = 2)
text(1998, -0.149, "CCL enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


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
# For the UK, the post-treatment gap is 13 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 5/25 = 0.2

# Plot
pdf("../Figures/Supplementary Information/MSPE Ratio_Spec 7.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.mse),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.mse))
lab.cols <- ifelse(names(sort(ratio.mse)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.7,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
countries
for (i in 1:length(countries)){
  print(whodat(countries[i]))
}

treated.unit <- data[which(data$countrycode == "GBR"), 1][1]
leaveoneout.controls <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(1990:2005), length(countries))
colnames(store) <- paste("No_", leaveoneout.names, sep = "")
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
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
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("logdiff", choose.time.predictors, "mean")),
             dependent = "logdiff",
             unit.variable = "countryid",
             unit.names.variable = "countryname",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")
  
  
  # Calculating annual gaps between the UK and its synthetic counterpart
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
pdf("../Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 7.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.2,0.2), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990 (log difference)")),
     main = "Gap between Treated and Synthetic Control",
     frame.plot = FALSE, axes = F)
mtext("Leave-one-out robustness check", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
abline(v = 2001, lty = 2)
abline(h = 0, lty = 1, col = "darkgrey")
arrows(1999.5, -0.15, 2000.9, -0.15, length = 0.1, code = 2)
text(1998, -0.149, "CCL enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.1, -0.011, "No LUX", cex = 0.8, col = "darkorange")
dev.off()



## ## ## ## ## ## ## ## ## ##
# TABLES FOR SI          ####
## ## ## ## ## ## ## ## ## ##