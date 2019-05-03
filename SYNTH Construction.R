# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# Specification in Paper
# .. Optimize over 1990-2001, 1990 baseline, no covariates
# Synth
# .. Running Synth 
# .. Pre-treatment predictor values 
# .. Weights for the predictor variables 
# .. Weights for countries in the donor pool 
# .. Export results
# Generate Results    
# .. Outcomes 
# .. Recreating built-in Synth graph for paths 
# .. Recreating built-in Synth graph for gaps 
# Placebo for Single Country 
# .. Running Synth 
# .. Outcomes 
# .. Recreating built-in Synth graph for paths 
# .. Recreating built-in Synth graph for gaps 
# Placebo Loops
# .. Running Synth
# .. Outcomes
# .. Store emissions path in treated unit and its synthetic counterpart
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Emissions paths figures
# .. Gaps figure
# .. MSPE analysis
# Leave-One-Out Check    
# .. Running Synth 
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Leave-one-out figure 
# Re-Assign Treatment Year
# .. Treatment in 2001
# .. Treatment in 2000
# .. Treatment in 1999
# .. Treatment in 1998
# .. Treatment in 1997
# .. Treatment in 1996
# .. Treatment in 1995
# .. Treatment in 1994
# .. Treatment in 1993
# .. Treatment in 1992
# .. Treatment in 1991
# Trajectory Balancing
# .. Reshape data
# .. Mean balancing with intercept shift
# .. Weights for countries in the donor pool



## ## ## ## ## ## ## ## ## ##
# PREAMBLE               ####
## ## ## ## ## ## ## ## ## ##

#setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/Synth/Results") # Alice laptop
#setwd("~/Box Sync/LepissierMildenberger/Synth/Results") # Matto
setwd("C:/boxsync/alepissier/LepissierMildenberger/Synth/Results") # Alice work
library(devtools)
library(gghighlight)
library(gridExtra)
library(plyr)
library(readstata13)
library(stargazer)
library(Synth)
library(tidyverse)
#devtools::install_github('xuyiqing/tjbal')
library(tjbal)



## ## ## ## ## ## ## ## ## ##
# FUNCTIONS              ####
## ## ## ## ## ## ## ## ## ##

whodat <- function(id) {
  country <- data[which(data$countryid == id), 2][1]
  return(country)
}

whatname <- function(id) {
  country <- data[which(data$countryid == id), 3][1]
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

# Rescale emissions to 1990 
data <- left_join(data, data %>%
               group_by(countryid) %>%
               filter(year == 1990) %>%
               select(countryid, baseline = en_atm_co2e_kt) %>%
               ungroup(),
             by = "countryid") %>%
  mutate(rescaled1990 = en_atm_co2e_kt/baseline)

# Emissions as difference in log levels
data$logdiff <- log(data$en_atm_co2e_kt/data$baseline)

# Missing data
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
# data <- subset(data, !(countryid %in% c(23)))

UK <- subset(data, (countrycode %in% c("GBR")))

# Emissions per capita in OECD
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

# Emissions relative to 1990 in effective sample
pdf("../Figures/CO2 emissions in effective sample.pdf", 
    height = 4.5, width = 6)
ggplot(data %>% filter(countrycode == "JPN" |
                         countrycode == "FRA" |
                         countrycode == "LUX" |
                         countrycode == "HUN" |
                         countrycode == "POL"), 
       aes(x = year, y = rescaled1990, col = countryname)) + 
  geom_line() +
  xlab("Year") + ylab(expression(paste("CO"[2], " emissions against 1990 baseline"))) +
  ggtitle("Emissions trends in the United Kingdom and effective sample") +
  geom_line(data = data[data$countrycode == "GBR",], 
            aes(x = year, y = rescaled1990, col = "United Kingdom"), size = 1.5) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  xlim(1990, 2010)
dev.off()

# Emissions relative to 1990 in OECD
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

# Emissions relative to 1990 (log difference) in OECD
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

# Zooming in, emissions per capita in OECD
ggplot(data[which(!data$countrycode == "GBR" & data$CO2_emissions_PC >= 5 & data$CO2_emissions_PC <= 15),], 
       aes(x = year, y = CO2_emissions_PC, col = countryname)) + 
  geom_line() +
  xlab("Year") + ylab(expression(paste("CO"[2], " emissions per capita"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data[which(data$countrycode == "GBR" & data$CO2_emissions_PC >= 5 & data$CO2_emissions_PC <= 15),], 
            aes(x = year, y = CO2_emissions_PC, col = "United Kingdom"), size = 1.5) +
  theme(legend.position="bottom", legend.title = element_blank()) +
  scale_y_continuous(labels = function(x) round(as.numeric(x), 0)) 

rm(missing, nmiss, UK, i, scandis)



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION IN PAPER ####
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


# Plot emissions trajectories in the UK and in the synthetic control
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

# Outcome variable in donor pool
Y0plot.UK <- dataprep.out$Y0plot

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Weights applied to each country in the donor pool
w.UK <- synth.out$solution.w

# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in synthetic unit
synth.UK <- Y0plot.UK %*% w.UK

# Gaps between outcomes in treated and synthetic control
gaps.UK <- Y1plot.UK - synth.UK
colnames(gaps.UK) <- "GBR"


# .. Recreating built-in Synth graph for paths ####
pdf("../Figures/Emissions paths in treated and synth.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
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
lines(years, synth.UK, col = "royalblue1", lty = 2, lwd = 2)
abline(v = 2001, lty = 2)
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Recreating built-in Synth graph for gaps ####
pdf("../Figures/Gaps in emissions in treated.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, gaps.UK, 
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
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
arrows(1999.5, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1998, -0.0695, "CCP enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ##
# PLACEBO LOOPS          ####
## ## ## ## ## ## ## ## ## ##

countries <- t(unique(subset(data, !(countrycode %in% c("GBR")))[1]))
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

store.gaps <- matrix(NA, length(1990:2005), length(countries))
colnames(store.gaps) <- placebo.names
store.gaps

store.obs <- matrix(NA, length(1990:2005), length(countries))
colnames(store.obs) <- placebo.names

store.synth <- matrix(NA, length(1990:2005), length(countries))
colnames(store.synth) <- placebo.names


for (i in 1:length(placebos)){
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
           treatment.identifier = placebos[i],
           controls.identifier = placebos[-i],
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1990:2005)


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")


# .. Store emissions path in treated unit and its synthetic counterpart ####
store.obs[,i] <- dataprep.out$Y1plot
store.synth[,i] <- dataprep.out$Y0plot %*% synth.out$solution.w


# .. Calculating annual gaps between the treated and its synthetic counterpart ####
store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}


# .. Emissions paths figures ####
store.obs <- cbind(store.obs, Y1plot.UK)
store.synth <- cbind(store.synth, synth.UK)
store.gaps <- cbind(store.gaps, gaps.UK)

countries <- c(countries, "14")
c.labels <- NA
for (i in 1:length(countries)){
  c.labels[i] <- whatname(countries[i])
}

for (c in 1:length(countries)){
  pdf(paste0("../Figures/Emissions paths in placebo and synth_", c.labels[c], ".pdf"), 
    height = 4.5, width = 6)
  plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
  u <- par("usr") # The coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
  grid (NULL, NULL, lty = 1, col = "seashell")
  par(new = TRUE, mgp = c(2, 1, 0))
  plot(years, store.obs[,c], 
       type = "l", col = "#014421", lwd = 2,
       xlim = range(years), ylim = range(store.obs[,c], store.synth[,c]), 
       las = 1, cex.axis = 0.8, tck = -0.05,
       xlab = "Year",
       ylab = expression(paste("CO"[2], " emissions relative to 1990")),
       main = "Observed and Synthetic Counterfactual Emissions",
       frame.plot = FALSE, axes = F)
  axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
       tck = -0.01, mgp = c(0, 0.2, 0))
  axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1, 
       tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
  lines(years, store.synth[,c], col = "#11904E", lty = 2, lwd = 2)
  abline(v = 2001, lty = 2)
  mtext(c.labels[c],
        cex = 0.8, side = 3, padj = -0.8, adj = 0)
  arrows(1999, 0.75, 2000.9, 0.75, length = 0.1, code = 2)
  text(1997.5, 0.754, "CCP enacted", cex = 0.8)
  dev.off()
}


# .. Gaps figure ####
placebo.results <- store.gaps
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
# Exclude AUT, BEL, CHE, CHL, ESP, FIN, FRA, GRC, HUN, IRL, ISL, ISR, JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR
placebo.results_5 <- placebo.results[, mse < 5*UK.mse]
placebo.results_10 <- placebo.results[, mse < 10*UK.mse]

# Plot
pdf("../Figures/Gaps in emissions_placebo_all.pdf", 
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
text(1998, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results[gap.start:gap.end, which(colnames(placebo.results)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()

# Plot excluding placebos with MSPE > 5
pdf("../Figures/Gaps in emissions_placebo_MSPE5.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results_5[gap.start:gap.end, which(colnames(placebo.results_5)=="GBR")],
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
text(1998, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results_5)){
  lines(years, placebo.results_5[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results_5[gap.start:gap.end, which(colnames(placebo.results_5)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()

# Plot excluding placebos with MSPE > 10
pdf("../Figures/Gaps in emissions_placebo_MSPE10.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results_10[gap.start:gap.end, which(colnames(placebo.results_10)=="GBR")],
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
text(1998, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results_10)){
  lines(years, placebo.results_10[gap.start:gap.end, i], col = "gray") 
}
lines(years, placebo.results_10[gap.start:gap.end, which(colnames(placebo.results_10)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()

# How many control states remain?
colnames(placebo.results_5)
# 6 control states: AUS, CAN, ITA, USA

# USA is the country outside of the distribution.


# .. MSPE analysis ####
placebo.results <- store.gaps
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
# For the UK, the post-treatment gap is 100 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/25 = 0.04

# Plot
pdf("../Figures/MSPE Ratio.pdf", 
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

leaveoneout.names <- NA
for (i in 1:length(leaveoneout.controls)){
  leaveoneout.names[i] <- whodat(leaveoneout.controls[i])
}
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(1990:2005), length(countries))
colnames(store.gaps) <- paste("No_", leaveoneout.names, sep = "")
store.gaps

# for (i in 1:length(leaveoneout.controls)+1){
#   print(i)
# }

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
             time.plot = 1990:2005)


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                     method = "BFGS")


# .. Calculating annual gaps between the UK and its synthetic counterpart ####
store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}


# .. Leave-one-out figure ####
leaveoneout.results <- store.gaps
rownames(leaveoneout.results) <- c(choose.time.predictors, seq(2002, 2005, 1))
leaveoneout.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(leaveoneout.results)
years <- c(choose.time.predictors, seq(2002, 2005, 1))
gap.end.pre <- which(rownames(placebo.results)=="2001")

# Plot
pdf("../Figures/Gaps in emissions_leave one out.pdf", 
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
text(1998, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[gap.start:gap.end, i], col = "thistle") 
}
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[gap.start:gap.end, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.1, -0.023, "No LUX", cex = 0.8, col = "darkorange")
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

placebo.years <- c(seq(1991, 2001))

store.gaps <- matrix(NA, length(1990:2005), length(placebo.years))
colnames(store.gaps) <- paste("Tx_", c(seq(1991, 2001)), sep = "")
store.gaps


# .. Treatment in 2001 ####
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
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,11] <- dataprep.out$Y0plot %*% synth.out$solution.w


# .. Treatment in 2000 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:2000,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean"),
             list("rescaled1990", 1997:1998, "mean"),
             list("rescaled1990", 1998:1999, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:2000,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,10] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 2000.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 2000", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,10], col = "coral", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 2000, lty = 2, col = "coral")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1999 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1999,
           special.predictors = list(
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
           time.optimize.ssr = 1990:1999,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,9] <- dataprep.out$Y0plot %*% synth.out$solution.w

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
     xlim = range(years), ylim = c(0.9,1.1),
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
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,9], col = "cornflowerblue", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1999, lty = 2, col = "cornflowerblue")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1998 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1998,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean"),
             list("rescaled1990", 1996:1997, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1998,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,8] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1998.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1998", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,8], col = "chocolate", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1998, lty = 2, col = "chocolate")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1997 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1997,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1995:1996, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1997,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,7] <- dataprep.out$Y0plot %*% synth.out$solution.w

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
     xlim = range(years), ylim = c(0.9,1.1),
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
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,7], col = "darkorchid", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1997, lty = 2, col = "darkorchid")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1996 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1996,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean"),
             list("rescaled1990", 1994:1995, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1996,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,6] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1996.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1996", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,6], col = "darkgoldenrod1", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1996, lty = 2, col = "darkgoldenrod1")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1995 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1995,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1993:1994, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1995,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,5] <- dataprep.out$Y0plot %*% synth.out$solution.w

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
     xlim = range(years), ylim = c(0.9,1.1),
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
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,5], col = "mediumseagreen", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1995, lty = 2, col = "mediumseagreen")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1994 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1994,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean"),
             list("rescaled1990", 1992:1993, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1994,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,4] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1994.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1994", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,4], col = "deeppink", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1994, lty = 2, col = "deeppink")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1993 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1993,
           special.predictors = list(
             list("rescaled1990", 1991:1992, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1993,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,3] <- dataprep.out$Y0plot %*% synth.out$solution.w

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
     xlim = range(years), ylim = c(0.9,1.1),
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
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,3], col = "darkviolet", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1993, lty = 2, col = "darkviolet")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1992 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1992,
           special.predictors = list(
             list("rescaled1990", 1990:1991, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1992,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,2] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("../Figures/Emissions paths in treated and synth_placebo year 1992.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), ylim = c(0.9,1.1),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 1990")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 1992", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,2], col = "darkturquoise", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1992, lty = 2, col = "darkturquoise")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1991 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1991,
           special.predictors = list(
             list("rescaled1990", 1990:1991, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "countryname",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1991,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

store.gaps[,1] <- dataprep.out$Y0plot %*% synth.out$solution.w

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
     xlim = range(years), ylim = c(0.9,1.1),
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
lines(years, store.gaps[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.gaps[,1], col = "palevioletred4", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1991, lty = 2, col = "palevioletred4")
legend(1990, 0.9445, c("United Kingdom", "Synthetic UK"),
       lty = c(1,2), lwd = c(2,2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 0.93, 2000.9, 0.93, length = 0.1, code = 2)
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ## ##
# TRAJECTORY BALANCING      ####
## ## ## ## ## ## ## ## ## ## ##

# .. Reshape data ####
data <- data %>%
  mutate(treat = ifelse((countrycode == "GBR" & year > 2001), 1, 0))

data <- data %>%
  filter(!is.na(rescaled1990)) %>%
  filter(year >= 1990 & year <= 2005)


# .. Mean balancing with intercept shift ####
mbal.out <- tjbal(data = data, Y = "rescaled1990", D = "treat", 
                  X.avg.time = list(c(1990:2000)),
                  index = c("countrycode","year"), kernel = FALSE, demean = TRUE)
print(mbal.out)

# Set of time periods
mbal.out$Ttot
mbal.out$Tpre
mbal.out$Tpst
mbal.out$T0

# Number of units
mbal.out$N
mbal.out$Ntr
mbal.out$Nco

# Other summary stats
mbal.out$matchvar
mbal.out$Y.bar
mbal.out$att
mbal.out$bal.table
mbal.out$sameT0

# Plot emissions trajectories in the UK and in the synthetic control
pdf("../Figures/Emissions paths in treated and synth_Mbal.pdf", 
    height = 4.5, width = 6)
plot(mbal.out, type = "ct",
     ylim = c(0.9, 1.1))
dev.off()

# Plot gaps in outcomes between the UK and the synthetic control
plot(mbal.out, type = "gap",
     ylim = c(-0.1, 0.1))

# Plot covariance balance
pdf("../Figures/Covariate balance_Mbal.pdf", 
    height = 4.5, width = 6)
plot(mbal.out, type = "balance")
dev.off()

# Plot weights
plot(mbal.out, type = "weights")


# .. Weights for countries in the donor pool ####
mbal.out$names.co
weights.mbal <- mbal.out$weights.co
capture.output(round(weights.mbal, 3), file = "Results Specification_Mbal.txt")

# Outcome in the treated unit
Y.tr.mbal <- mbal.out$Y.tr

# Outcome in the control units
Y.co.mbal <- mbal.out$Y.co

# Pre- and post-intervention periods
years <- mbal.out$Ttot

# ID vectors for the control units
mbal.out$names.co
mbal.out$id.co
Y.co.mbal$countrycode <- mbal.out$names.co

# .. Double check ID units correspond to correct countries ####
test <- gather(Y.co.mbal, year, rescaled1990, rescaled19901990:rescaled19902005) %>%
  mutate(year = str_sub(year, start = -4)) %>%
  mutate(year = as.integer(year))
test <- left_join(test, data %>% select(countrycode, year, rescaled1990),
                  by = c("countrycode", "year"))
sum(test$rescaled1990.x == test$rescaled1990.y) == nrow(test)
# TRUE
data %>% filter(countrycode != "GBR") %>% nrow == nrow(test)
# TRUE
rm(test)

# Outcome variable in synthetic unit
Y.co.mbal$countrycode <- NULL
Y.co.mbal <- t(as.matrix(Y.co.mbal))
Y0plot.UK == Y.co.mbal
synth.mbal <- Y.co.mbal %*% weights.mbal

# Gaps between outcomes in treated and synthetic control
Y.tr.mbal <- t(as.matrix(Y.tr.mbal))
gaps.mbal <- Y.tr.mbal - synth.mbal


# .. Recreating built-in Synth graph for gaps ####
pdf("../Figures/Gaps in emissions in treated_Mbal.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, gaps.mbal, 
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
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
arrows(1999.5, -0.07, 2000.9, -0.07, length = 0.1, code = 2)
text(1998, -0.0695, "CCP enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ##
# PLACEBO LOOPS          ####
## ## ## ## ## ## ## ## ## ##

data.placebo <- data %>%
  filter(countrycode != "GBR")

placebo.countries <- data.placebo %>%
  distinct(countrycode) %>%
  as.matrix %>% t

store.gaps <- matrix(NA, length(1990:2005), length(placebo.countries))
colnames(store.gaps) <- placebo.countries
store.gaps

nloops <- length(placebo.countries)

for (i in 1:nloops){
  data.placebo <- data.placebo %>%
    mutate(treat = ifelse((countrycode == placebo.countries[i] & year > 2001), 1, 0))
  
  mbal.out <- tjbal(data = data.placebo, Y = "rescaled1990", D = "treat", 
                    X.avg.time = list(c(1990:2000)),
                    index = c("countrycode","year"), kernel = FALSE, demean = TRUE)
  
  # Weights for countries in the donor pool
  weights.mbal <- mbal.out$weights.co
  
  # Outcome in the treated unit
  Y.tr.mbal <- mbal.out$Y.tr
  
  # Outcome in the control units
  Y.co.mbal <- mbal.out$Y.co
  
  # Outcome variable in synthetic unit
  Y.co.mbal <- t(as.matrix(Y.co.mbal))
  synth.mbal <- Y.co.mbal %*% weights.mbal
  
  # Gaps between outcomes in treated and synthetic control
  Y.tr.mbal <- t(as.matrix(Y.tr.mbal))
  store.gaps[,i] <- Y.tr.mbal - synth.mbal
}

store.gaps <- cbind(store.gaps, gaps.mbal)
colnames(store.gaps)[ncol(store.gaps)] <- "GBR"
rownames(store.gaps) <- years

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(store.gaps)
gap.end.pre <- which(rownames(store.gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
mse <- apply(store.gaps[gap.start:gap.end.pre, ]^2, 2, mean)
UK.mse <- as.numeric(mse["GBR"])

# Exclude countries with 5 times higher MSPE than UK
store.gaps[, mse > 5*UK.mse]
# Exclude AUT, BEL, CHE, CHL, ESP, FIN, FRA, GRC, HUN, IRL, ISL, ISR, JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR
placebo.results_5 <- store.gaps[, mse < 5*UK.mse]
placebo.results_10 <- store.gaps[, mse < 10*UK.mse]

plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, store.gaps[, which(colnames(store.gaps) == "GBR")],
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
text(1998, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results_10)){
  lines(years, placebo.results_10[, i], col = "gray") 
}
lines(years, store.gaps[, which(colnames(store.gaps)=="GBR")],
      type = "l", col = "darkorchid", lwd = 2)




## ## ## ## ## ## ## ## ## ##
# LEAVE-ONE-OUT CHECK    ####
## ## ## ## ## ## ## ## ## ##

leaveoneout.controls <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countrycode) %>%
  as.matrix %>% t

store.gaps <- matrix(NA, length(1990:2005), length(leaveoneout.controls))
colnames(store.gaps) <- paste("No_", leaveoneout.controls, sep = "")
store.gaps

nloops <- length(leaveoneout.controls)

for (i in 1:nloops){
  data.LOO <- data %>%
    filter(countrycode != leaveoneout.controls[i])
  
  mbal.out <- tjbal(data = data.LOO, Y = "rescaled1990", D = "treat", 
                    X.avg.time = list(c(1990:2000)),
                    index = c("countrycode","year"), kernel = FALSE, demean = TRUE)
  
  # Weights for countries in the donor pool
  weights.mbal <- mbal.out$weights.co
  
  # Outcome in the treated unit
  Y.tr.mbal <- mbal.out$Y.tr
  
  # Outcome in the control units
  Y.co.mbal <- mbal.out$Y.co
  
  # Outcome variable in synthetic unit
  Y.co.mbal <- t(as.matrix(Y.co.mbal))
  synth.mbal <- Y.co.mbal %*% weights.mbal
  
  # Gaps between outcomes in treated and synthetic control
  Y.tr.mbal <- t(as.matrix(Y.tr.mbal))
  store.gaps[,i] <- Y.tr.mbal - synth.mbal
}

store.gaps <- cbind(store.gaps, gaps.mbal)
colnames(store.gaps)[ncol(store.gaps)] <- "No_Dropped"

# .. Leave-one-out figure ####
leaveoneout.results <- store.gaps
rownames(leaveoneout.results) <- mbal.out$Ttot
leaveoneout.results

# Plot
pdf("../Figures/Gaps in emissions_leave one out_Mbal.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results)=="No_Dropped")],
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
text(1998, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results)=="No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results)=="No_LUX")],
      type = "l", col = "darkorange")
text(2004.1, 0.075, "No LUX", cex = 0.8, col = "darkorange")
dev.off()


# #### All below is archived for now.
# 
# # ## ## ## ## ## ## ## ## ## ##
# # # TRAJECTORY BALANCING   ###
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