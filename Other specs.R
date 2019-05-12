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
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 2
# .. Optimize over 1990-2001, 1990 baseline, no covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 3
# .. Optimize over 1990-2001, 1990 baseline, with covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 4
# .. Optimize over 1990-2001, CO2 per capita, no covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 5
# .. Optimize over 1990-2001, CO2 per capita, with covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 6
# .. Optimize over 1990-2001, 2000 baseline, no covariates
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo loops
# .. Placebo figure
# .. MSPE analysis
# .. Leave-one-out check
# .. Leave-one-out figure



## ## ## ## ## ## ## ## ## ##
# PREAMBLE               ####
## ## ## ## ## ## ## ## ## ##

setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/Synth/") # Alice laptop
#setwd("C:/boxsync/alepissier/LepissierMildenberger/Synth/") # Alice work
#setwd("~/Box Sync/LepissierMildenberger/Synth/") # Matto
library(devtools)
#library(gghighlight)
library(ggplot2)
#library(gridExtra)
library(kableExtra)
library(Matching)
library(plyr)
library(reshape2)
library(stargazer)
library(Synth)
library(tidyverse)
set.seed(1509)



## ## ## ## ## ## ## ## ## ##
# FUNCTIONS              ####
## ## ## ## ## ## ## ## ## ##

whodat <- function(id) {
  country <- NA
  for (i in 1:length(id)){
    country[i] <- unique(data[which(data$countryid == id[i]), "countrycode"])
  }
  return(country)
}

whodis <- function(ISO) {
  country <- NA
  for (i in 1:length(ISO)){
    country[i] <- unique(data[which(data$countrycode == ISO[i]), "country"])
  }
  return(country)
}

whatname <- function(id) {
  country <- NA
  for (i in 1:length(id)){
    country[i] <- unique(data[which(data$countryid == id[i]), "country"])
  }
  return(country)
}

whichnum <- function(ISO) {
  num <- data %>%
    filter(countrycode == ISO) %>%
    distinct(countryid) %>%
    pull
  return(num)
}

whoder <- function() {
  unique(data$country)
}



## ## ## ## ## ## ## ## ## ##
# DATA                   ####
## ## ## ## ## ## ## ## ## ##

load("Data/data_clean.Rdata")
whoder()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 1        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1980-2001, 1990 baseline, no covariates ####
treated.unit <- data %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)

control.units <- control.units[which(control.units != 1)]
# HIC and OECD: Missing data

choose.time.predictors <- 1980:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled1990", 1980, "mean"),
             list("rescaled1990", 1981, "mean"),
             list("rescaled1990", 1982, "mean"),
             list("rescaled1990", 1983, "mean"),
             list("rescaled1990", 1984, "mean"),
             list("rescaled1990", 1985, "mean"),
             list("rescaled1990", 1986, "mean"),
             list("rescaled1990", 1987, "mean"),
             list("rescaled1990", 1988, "mean"),
             list("rescaled1990", 1989, "mean"),
             #list("rescaled1990", 1990, "mean"),
             list("rescaled1990", 1991, "mean"),
             list("rescaled1990", 1992, "mean"),
             list("rescaled1990", 1993, "mean"),
             list("rescaled1990", 1994, "mean"),
             list("rescaled1990", 1995, "mean"),
             list("rescaled1990", 1996, "mean"),
             list("rescaled1990", 1997, "mean"),
             list("rescaled1990", 1998, "mean"),
             list("rescaled1990", 1999, "mean"),
             list("rescaled1990", 2000, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "country",
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
synth.spec <- list(treated = whatname(dataprep.out$tag[["treatment.identifier"]]),
                   donor.pool = whatname(dataprep.out$tag[["controls.identifier"]]),
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 1.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 1.txt")
#donor.weights <- as.data.frame(synth.tables$tab.w)[,-3]


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Gaps between outcomes in treated and synthetic control
gaps <- dataprep.out$Y1plot - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# HIC and OECD: 0.0001521915
# OECD: 0.0001933036

# Extract pre-treatment outcome values in treated unit and synthetic counterpart
preT.UK <- Y1plot.UK[gap.start:gap.end.pre]
preT.synth <- synth[gap.start:gap.end.pre]

# Two-sample t test
t.test(preT.UK, preT.synth)
p.val.t <- t.test(preT.UK, preT.synth)$p.value

# Kolmogorov Smirnov test
ks.test(preT.UK, preT.synth)
p.val.KS <- ks.test(preT.UK, preT.synth)$p.value

# QQ statistics
qqstats <- unlist(qqstats(preT.UK, preT.synth))

# Export balance tests
results <- kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "rst")
capture.output(results, file = "Results/Supplementary Information/Balance Specification 1.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Supplementary Information/Emissions paths in treated and synth_Spec 1.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), 
     ylim = c(0.9, 1.1), 
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
text(1996.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####
placebos <- control.units

placebos <- placebos[which(placebos != 10)]
# HIC and OECD: SVD fails

store <- matrix(NA, length(years), length(placebos))
colnames(store) <- whodat(placebos)
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("rescaled1990", 1980, "mean"),
               list("rescaled1990", 1981, "mean"),
               list("rescaled1990", 1982, "mean"),
               list("rescaled1990", 1983, "mean"),
               list("rescaled1990", 1984, "mean"),
               list("rescaled1990", 1985, "mean"),
               list("rescaled1990", 1986, "mean"),
               list("rescaled1990", 1987, "mean"),
               list("rescaled1990", 1988, "mean"),
               list("rescaled1990", 1989, "mean"),
               #list("rescaled1990", 1990, "mean"),
               list("rescaled1990", 1991, "mean"),
               list("rescaled1990", 1992, "mean"),
               list("rescaled1990", 1993, "mean"),
               list("rescaled1990", 1994, "mean"),
               list("rescaled1990", 1995, "mean"),
               list("rescaled1990", 1996, "mean"),
               list("rescaled1990", 1997, "mean"),
               list("rescaled1990", 1998, "mean"),
               list("rescaled1990", 1999, "mean"),
               list("rescaled1990", 2000, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "country",
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

save(store, file = "Results/Supplementary Information/Placebo loops/store_Spec 1.Rdata")


# .. Placebo figure ####
placebo.results <- cbind(store, gaps)
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 4 times higher MSPE than UK
colnames(placebo.results[, MSE > 4*UK.MSE])
# HIC and OECD: Exclude ARE, ATG, BEL, BHR, BMU, BRB, BRN, CHL, FIN, FRA, FRO, GIB
# GRL, HKG, HUN, ISL, KNA, KOR, LUX, MAC, MLT, NCL, OMN, PAN
# PLW, POL, PRT, PYF, QAT, SAU, SGP, SYC, TTO, TUR, URY, VGB
# OECD: Exclude BEL, CHE, CHL, ESP, FIN, FRA, GRC, HUN, IRL, ISL, ISR, KOR
# LUX, MEX, NZL, POL, PRT, TUR
placebo.results <- placebo.results[, MSE < 4*UK.MSE]

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_placebo_MSPE4_Spec 1.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.3, 0.2), 
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
text(1996.5, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


# .. MSPE analysis ####
placebo.results <- cbind(store, gaps)

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.start.post <- which(rownames(placebo.results) == "2002")
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 31 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 13/53 = 0.25
# OECD: For the UK, the post-treatment gap is 27 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 3/25 = 0.12

# Plot
pdf("Figures/Supplementary Information/MSPE Ratio_Spec 1.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.MSE),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.MSE))
lab.cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.6,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.controls <- leaveoneout.controls[which(leaveoneout.controls != 10)]
# HIC and OECD: SVD fails

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store) <- paste0("No_", leaveoneout.names)
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
               list("rescaled1990", 1980, "mean"),
               list("rescaled1990", 1981, "mean"),
               list("rescaled1990", 1982, "mean"),
               list("rescaled1990", 1983, "mean"),
               list("rescaled1990", 1984, "mean"),
               list("rescaled1990", 1985, "mean"),
               list("rescaled1990", 1986, "mean"),
               list("rescaled1990", 1987, "mean"),
               list("rescaled1990", 1988, "mean"),
               list("rescaled1990", 1989, "mean"),
               #list("rescaled1990", 1990, "mean"),
               list("rescaled1990", 1991, "mean"),
               list("rescaled1990", 1992, "mean"),
               list("rescaled1990", 1993, "mean"),
               list("rescaled1990", 1994, "mean"),
               list("rescaled1990", 1995, "mean"),
               list("rescaled1990", 1996, "mean"),
               list("rescaled1990", 1997, "mean"),
               list("rescaled1990", 1998, "mean"),
               list("rescaled1990", 1999, "mean"),
               list("rescaled1990", 2000, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "country",
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

save(store, file = "Results/Supplementary Information/Leave-one-out/store_Spec 1.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 1.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.3, 0.2), 
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
text(1996.5, -0.0695, "CCP enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 2        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, 1990 baseline, no covariates ####
treated.unit <- data %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             #list("rescaled1990", 1990, "mean"),
             list("rescaled1990", 1991, "mean"),
             list("rescaled1990", 1992, "mean"),
             list("rescaled1990", 1993, "mean"),
             list("rescaled1990", 1994, "mean"),
             list("rescaled1990", 1995, "mean"),
             list("rescaled1990", 1996, "mean"),
             list("rescaled1990", 1997, "mean"),
             list("rescaled1990", 1998, "mean"),
             list("rescaled1990", 1999, "mean"),
             list("rescaled1990", 2000, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "country",
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
synth.spec <- list(treated = whatname(dataprep.out$tag[["treatment.identifier"]]),
                   donor.pool = whatname(dataprep.out$tag[["controls.identifier"]]),
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 2.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 2.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
pdf("Figures/Supplementary Information/Donor weights_Spec 2.pdf", 
    height = 4.5, width = 6)
par(mar = c(2, 6.1, 2, 2.1), las = 2)
a <- barplot(donor.weights$Donor.weight,
             xaxt = "n",
             main = "Donor weights",
             names.arg = donor.weights$Donor.country,
             cex.main = 0.9,
             horiz = T,
             space = 1,
             cex.names = 0.45,
             offset = -0.005,
             xlim = c(0, 0.25))
axis(side = 1, at = c(0, 0.3), labels = c("", ""), lwd.ticks = 0)
axis(side = 1, at = seq(0, 0.25, by = 0.05), 
     cex.axis = 0.5, tck = -0.01, 
     mgp = c(3, 0, 0), las = 1)
dev.off()


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Gaps between outcomes in treated and synthetic control
gaps <- dataprep.out$Y1plot - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# HIC and OECD: 0.000007564503
# OECD: 0.00003381089

# Extract pre-treatment outcome values in treated unit and synthetic counterpart
preT.UK <- Y1plot.UK[gap.start:gap.end.pre]
preT.synth <- synth[gap.start:gap.end.pre]

# Two-sample t test
t.test(preT.UK, preT.synth)
p.val.t <- t.test(preT.UK, preT.synth)$p.value

# Kolmogorov Smirnov test
ks.test(preT.UK, preT.synth)
p.val.KS <- ks.test(preT.UK, preT.synth)$p.value

# QQ statistics
qqstats <- unlist(qqstats(preT.UK, preT.synth))

# Export balance tests
results <- kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "rst")
capture.output(results, file = "Results/Supplementary Information/Balance Specification 2.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Supplementary Information/Emissions paths in treated and synth_Spec 2.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), 
     ylim = c(0.9, 1.1), 
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
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####
placebos <- control.units

placebos <- placebos[which(placebos != 10)] # AUT
placebos <- placebos[which(placebos != 148)] # URY
# HIC and OECD: SVD fails

store <- matrix(NA, length(years), length(placebos))
colnames(store) <- whodat(placebos)
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               #list("rescaled1990", 1990, "mean"),
               list("rescaled1990", 1991, "mean"),
               list("rescaled1990", 1992, "mean"),
               list("rescaled1990", 1993, "mean"),
               list("rescaled1990", 1994, "mean"),
               list("rescaled1990", 1995, "mean"),
               list("rescaled1990", 1996, "mean"),
               list("rescaled1990", 1997, "mean"),
               list("rescaled1990", 1998, "mean"),
               list("rescaled1990", 1999, "mean"),
               list("rescaled1990", 2000, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)
  
  # Calculating annual gaps between the treated and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Placebo loops/store_Spec 2.Rdata")


# .. Placebo figure ####
placebo.results <- cbind(store, gaps)
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 4 times higher MSPE than UK
colnames(placebo.results[, MSE > 4*UK.MSE])
# HIC and OECD: Exclude ABW, ARE, ARG, BEL, BHR, BHS, BMU, BRB, BRN, CHE, CHL, CYM
# CYP, DEU, ESP, FIN, FRA, FRO, GRL, HKG, HUN, IRL, ISL, ISR
# JPN, KNA, KOR, LUX, MAC, MLT, NCL, OMN, PAN, POL, PRT, PYF
# QAT, SAU, SGP, SYC, TTO, TUR, USA, VGB
# OECD: Exclude AUT, BEL, CHE, CHL, DEU, ESP, FIN, FRA, HUN, IRL, ISL, ISR
# JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR
placebo.results <- placebo.results[, MSE < 4*UK.MSE]

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_placebo_MSPE4_Spec 2.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.15, 0.15), 
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
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


# .. MSPE analysis ####
placebo.results <- cbind(store, gaps)

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.start.post <- which(rownames(placebo.results) == "2002")
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 633 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 5/53 = 0.09433962
# OECD: For the UK, the post-treatment gap is 105 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/26 = 0.03846154

# Plot
pdf("Figures/Supplementary Information/MSPE Ratio_Spec 2.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.MSE),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.MSE))
lab.cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.6,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store) <- paste0("No_", leaveoneout.names)
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
               #list("rescaled1990", 1990, "mean"),
               list("rescaled1990", 1991, "mean"),
               list("rescaled1990", 1992, "mean"),
               list("rescaled1990", 1993, "mean"),
               list("rescaled1990", 1994, "mean"),
               list("rescaled1990", 1995, "mean"),
               list("rescaled1990", 1996, "mean"),
               list("rescaled1990", 1997, "mean"),
               list("rescaled1990", 1998, "mean"),
               list("rescaled1990", 1999, "mean"),
               list("rescaled1990", 2000, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)
  
  # Calculating annual gaps between the UK and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Leave-one-out/store_Spec 2.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 2.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.15, 0.15), 
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
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 3        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, 1990 baseline, with covariates ####
treated.unit <- data %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)

missvars <- data %>% 
  summarize_all(funs(sum(is.na(.)))) %>%
  melt %>%
  arrange(desc(value))
head(missvars)

notmissing <- data %>%
  select(-c(EG.GDP.PUSE.KO.PP.KD, SE.XPD.TOTL.GD.ZS, 
            GC.TAX.TOTL.GD.ZS, EG.USE.COMM.FO.ZS,
            EG.IMP.CONS.ZS, EG.FEC.RNEW.ZS)) %>%
  filter(countrycode != "GBR" & year >= 1990 & year <= 2001) %>%
  filter(complete.cases(.)) %>%
  group_by(countryid) %>%
  summarize(nyears = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyears == 12) %>%
  select(countryid)

control.units <- notmissing %>% pull

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = c("NY.GDP.PCAP.KD", # GDP per capita (constant 2010 US$)
                          "NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                          #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                          #"EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                          #"EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                          #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                          #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          "NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                          #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                          "EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                          "EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                          "TX.VAL.FUEL.ZS.UN", # Fuel exports (% of merchandise exports)
                          "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                          "NE.IMP.GNFS.ZS" # Imports of goods and services (% of GDP)
           ),
           predictors.op = "mean",
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             #list("rescaled1990", 1990, "mean"),
             list("rescaled1990", 1991, "mean"),
             list("rescaled1990", 1992, "mean"),
             list("rescaled1990", 1993, "mean"),
             list("rescaled1990", 1994, "mean"),
             list("rescaled1990", 1995, "mean"),
             list("rescaled1990", 1996, "mean"),
             list("rescaled1990", 1997, "mean"),
             list("rescaled1990", 1998, "mean"),
             list("rescaled1990", 1999, "mean"),
             list("rescaled1990", 2000, "mean")),
           dependent = "rescaled1990",
           unit.variable = "countryid",
           unit.names.variable = "country",
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
synth.spec <- list(treated = whatname(dataprep.out$tag[["treatment.identifier"]]),
                   donor.pool = whatname(dataprep.out$tag[["controls.identifier"]]),
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 3.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 3.txt")
#donor.weights <- cbind(donor.weights, as.data.frame(synth.tables$tab.w)[,-3])


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Gaps between outcomes in treated and synthetic control
gaps <- dataprep.out$Y1plot - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# HIC and OECD: 0.0005410174
# OECD: 0.0005354843

# Extract pre-treatment outcome values in treated unit and synthetic counterpart
preT.UK <- Y1plot.UK[gap.start:gap.end.pre]
preT.synth <- synth[gap.start:gap.end.pre]

# Two-sample t test
t.test(preT.UK, preT.synth)
p.val.t <- t.test(preT.UK, preT.synth)$p.value

# Kolmogorov Smirnov test
ks.test(preT.UK, preT.synth)
p.val.KS <- ks.test(preT.UK, preT.synth)$p.value

# QQ statistics
qqstats <- unlist(qqstats(preT.UK, preT.synth))

# Export balance tests
results <- kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "rst")
capture.output(results, file = "Results/Supplementary Information/Balance Specification 3.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Supplementary Information/Emissions paths in treated and synth_Spec 3.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), 
     ylim = c(0.9, 1.1), 
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
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####
placebos <- control.units

store <- matrix(NA, length(years), length(placebos))
colnames(store) <- whodat(placebos)
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = c("NY.GDP.PCAP.KD", # GDP per capita (constant 2010 US$)
                            "NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                            #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                            #"EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                            #"EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                            #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                            #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                            "NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                            #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                            "EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                            "EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                            "TX.VAL.FUEL.ZS.UN", # Fuel exports (% of merchandise exports)
                            "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                            "NE.IMP.GNFS.ZS" # Imports of goods and services (% of GDP)
             ),
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               #list("rescaled1990", 1990, "mean"),
               list("rescaled1990", 1991, "mean"),
               list("rescaled1990", 1992, "mean"),
               list("rescaled1990", 1993, "mean"),
               list("rescaled1990", 1994, "mean"),
               list("rescaled1990", 1995, "mean"),
               list("rescaled1990", 1996, "mean"),
               list("rescaled1990", 1997, "mean"),
               list("rescaled1990", 1998, "mean"),
               list("rescaled1990", 1999, "mean"),
               list("rescaled1990", 2000, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "country",
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

save(store, file = "Results/Supplementary Information/Placebo loops/store_Spec 3.Rdata")


# .. Placebo figure ####
placebo.results <- cbind(store, gaps)
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 4 times higher MSPE than UK
colnames(placebo.results[, MSE > 4*UK.MSE])
# Exclude CHL, HKG, KOR, OMN, PAN, SGP, URY
placebo.results <- placebo.results[, MSE < 4*UK.MSE]

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_placebo_MSPE4_Spec 3.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.15, 0.15), 
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
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


# .. MSPE analysis ####
placebo.results <- cbind(store, gaps)

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.start.post <- which(rownames(placebo.results) == "2002")
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 2 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 22/28 = 0.8
# OECD: For the UK, the post-treatment gap is 1.6 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 16/21 = 0.8

# Plot
pdf("Figures/Supplementary Information/MSPE Ratio_Spec 3.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.MSE),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.MSE))
lab.cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.6,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store) <- paste0("No_", leaveoneout.names)
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = c("NY.GDP.PCAP.KD", # GDP per capita (constant 2010 US$)
                            "NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                            #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                            #"EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                            #"EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                            #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                            #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                            "NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                            #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                            "EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                            "EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                            "TX.VAL.FUEL.ZS.UN", # Fuel exports (% of merchandise exports)
                            "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                            "NE.IMP.GNFS.ZS" # Imports of goods and services (% of GDP)
             ),
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               #list("rescaled1990", 1990, "mean"),
               list("rescaled1990", 1991, "mean"),
               list("rescaled1990", 1992, "mean"),
               list("rescaled1990", 1993, "mean"),
               list("rescaled1990", 1994, "mean"),
               list("rescaled1990", 1995, "mean"),
               list("rescaled1990", 1996, "mean"),
               list("rescaled1990", 1997, "mean"),
               list("rescaled1990", 1998, "mean"),
               list("rescaled1990", 1999, "mean"),
               list("rescaled1990", 2000, "mean")),
             dependent = "rescaled1990",
             unit.variable = "countryid",
             unit.names.variable = "country",
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

save(store, file = "Results/Supplementary Information/Leave-one-out/store_Spec 3.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 3.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.15, 0.15), 
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
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 4        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, CO2 per capita, no covariates ####
treated.unit <- data %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean"),
             list("EN.ATM.CO2E.PC", 1995, "mean"),
             list("EN.ATM.CO2E.PC", 1996, "mean"),
             list("EN.ATM.CO2E.PC", 1997, "mean"),
             list("EN.ATM.CO2E.PC", 1998, "mean"),
             list("EN.ATM.CO2E.PC", 1999, "mean"),
             list("EN.ATM.CO2E.PC", 2000, "mean"),
             list("EN.ATM.CO2E.PC", 2001, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
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
synth.spec <- list(treated = whatname(dataprep.out$tag[["treatment.identifier"]]),
                   donor.pool = whatname(dataprep.out$tag[["controls.identifier"]]),
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 4.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 4.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
pdf("Figures/Supplementary Information/Donor weights_Spec 4.pdf", 
    height = 4.5, width = 6)
par(mar = c(2, 6.1, 2, 2.1), las = 2)
a <- barplot(donor.weights$Donor.weight,
             xaxt = "n",
             main = "Donor weights",
             names.arg = donor.weights$Donor.country,
             cex.main = 0.9,
             horiz = T,
             space = 1,
             cex.names = 0.45,
             offset = -0.005,
             xlim = c(0, 0.25))
axis(side = 1, at = c(0, 0.3), labels = c("", ""), lwd.ticks = 0)
axis(side = 1, at = seq(0, 0.25, by = 0.05), 
     cex.axis = 0.5, tck = -0.01, 
     mgp = c(3, 0, 0), las = 1)
dev.off()


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Gaps between outcomes in treated and synthetic control
gaps <- dataprep.out$Y1plot - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# HIC and OECD: 0.0001418258
# OECD: 0.002072709

# Extract pre-treatment outcome values in treated unit and synthetic counterpart
preT.UK <- Y1plot.UK[gap.start:gap.end.pre]
preT.synth <- synth[gap.start:gap.end.pre]

# Two-sample t test
t.test(preT.UK, preT.synth)
p.val.t <- t.test(preT.UK, preT.synth)$p.value

# Kolmogorov Smirnov test
ks.test(preT.UK, preT.synth)
p.val.KS <- ks.test(preT.UK, preT.synth)$p.value

# QQ statistics
qqstats <- unlist(qqstats(preT.UK, preT.synth))

# Export balance tests
results <- kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "rst")
capture.output(results, file = "Results/Supplementary Information/Balance Specification 4.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Supplementary Information/Emissions paths in treated and synth_Spec 4.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), 
     ylim = c(7, 12), 
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
text(1997.5, 8.04, "CCP enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####
placebos <- control.units

placebos <- placebos[which(placebos != 27)] # CHL
# HIC and OECD: SVD fails

store <- matrix(NA, length(years), length(placebos))
colnames(store) <- whodat(placebos)
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("EN.ATM.CO2E.PC", 1990, "mean"),
               list("EN.ATM.CO2E.PC", 1991, "mean"),
               list("EN.ATM.CO2E.PC", 1992, "mean"),
               list("EN.ATM.CO2E.PC", 1993, "mean"),
               list("EN.ATM.CO2E.PC", 1994, "mean"),
               list("EN.ATM.CO2E.PC", 1995, "mean"),
               list("EN.ATM.CO2E.PC", 1996, "mean"),
               list("EN.ATM.CO2E.PC", 1997, "mean"),
               list("EN.ATM.CO2E.PC", 1998, "mean"),
               list("EN.ATM.CO2E.PC", 1999, "mean"),
               list("EN.ATM.CO2E.PC", 2000, "mean"),
               list("EN.ATM.CO2E.PC", 2001, "mean")),
             dependent = "EN.ATM.CO2E.PC",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)

  # Calculating annual gaps between the treated and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Placebo loops/store_Spec 4.Rdata")


# .. Placebo figure ####
placebo.results <- cbind(store, gaps)
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 4 times higher MSPE than UK
colnames(placebo.results[, MSE > 4*UK.MSE])
# HIC and OECD: Exclude ABW, ARE, ARG, AUS, AUT, BEL, BHR, BHS, BMU, BRB, BRN, CAN
# CHE, CYM, CYP, DEU, ESP, FIN, FRA, FRO, GRL, HKG, HUN, IRL
# ISL, ISR, ITA, JPN, KNA, KOR, LUX, MAC, MLT, NCL, NZL, OMN
# PAN, PLW, POL, PRT, PYF, QAT, SAU, SGP, SYC, TTO, TUR, URY
# USA, VGB
# OECD: Exclude AUS, AUT, BEL, CAN, CHE, CHL, DEU, ESP, FIN, FRA, HUN, IRL
# ISL, ISR, JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR, USA
placebo.results <- placebo.results[, MSE < 4*UK.MSE]

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_placebo_MSPE4_Spec 4.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1, 1), 
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
text(1998, -0.48, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


# .. MSPE analysis ####
placebo.results <- cbind(store, gaps)

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.start.post <- which(rownames(placebo.results) == "2002")
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 1130 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/54 = 0.03703704
# OECD: For the UK, the post-treatment gap is 106 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/26 = 0.03846154

# Plot
pdf("Figures/Supplementary Information/MSPE Ratio_Spec 4.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.MSE),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.MSE))
lab.cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.6,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store) <- paste0("No_", leaveoneout.names)
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
               list("EN.ATM.CO2E.PC", 1990, "mean"),
               list("EN.ATM.CO2E.PC", 1991, "mean"),
               list("EN.ATM.CO2E.PC", 1992, "mean"),
               list("EN.ATM.CO2E.PC", 1993, "mean"),
               list("EN.ATM.CO2E.PC", 1994, "mean"),
               list("EN.ATM.CO2E.PC", 1995, "mean"),
               list("EN.ATM.CO2E.PC", 1996, "mean"),
               list("EN.ATM.CO2E.PC", 1997, "mean"),
               list("EN.ATM.CO2E.PC", 1998, "mean"),
               list("EN.ATM.CO2E.PC", 1999, "mean"),
               list("EN.ATM.CO2E.PC", 2000, "mean"),
               list("EN.ATM.CO2E.PC", 2001, "mean")),
             dependent = "EN.ATM.CO2E.PC",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)
  
  # Calculating annual gaps between the UK and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Leave-one-out/store_Spec 4.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 4.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1, 1), 
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
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 5        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, CO2 per capita, with covariates ####
treated.unit <- data %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)

notmissing <- data %>%
  select(-c(EG.GDP.PUSE.KO.PP.KD, SE.XPD.TOTL.GD.ZS,
            GC.TAX.TOTL.GD.ZS, EG.USE.COMM.FO.ZS,
            EG.IMP.CONS.ZS, EG.FEC.RNEW.ZS)) %>%
  filter(countrycode != "GBR" & year >= 1990 & year <= 2001) %>%
  filter(complete.cases(NY.GDP.PCAP.KD)) %>%
  group_by(countryid) %>%
  summarize(nyears = n_distinct(year)) %>%
  ungroup() %>%
  filter(nyears == 12) %>%
  select(countryid)

control.units <- notmissing %>% pull

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = c("NY.GDP.PCAP.KD" # GDP per capita (constant 2010 US$)
                          #"NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                          #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                          #"EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                          #"EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                          #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                          #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          #"NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                          #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                          #"EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                          #"EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                          #"TX.VAL.FUEL.ZS.UN" # Fuel exports (% of merchandise exports)
                          #"NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                          #"NE.IMP.GNFS.ZS" # Imports of goods and services (% of GDP)
           ),
           predictors.op = "mean",
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean"),
             list("EN.ATM.CO2E.PC", 1995, "mean"),
             list("EN.ATM.CO2E.PC", 1996, "mean"),
             list("EN.ATM.CO2E.PC", 1997, "mean"),
             list("EN.ATM.CO2E.PC", 1998, "mean"),
             list("EN.ATM.CO2E.PC", 1999, "mean"),
             list("EN.ATM.CO2E.PC", 2000, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
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
synth.spec <- list(treated = whatname(dataprep.out$tag[["treatment.identifier"]]),
                   donor.pool = whatname(dataprep.out$tag[["controls.identifier"]]),
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 5.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 5.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
pdf("Figures/Supplementary Information/Donor weights_Spec 5.pdf", 
    height = 4.5, width = 6)
par(mar = c(2, 6.1, 2, 2.1), las = 2)
a <- barplot(donor.weights$Donor.weight,
             xaxt = "n",
             main = "Donor weights",
             names.arg = donor.weights$Donor.country,
             cex.main = 0.9,
             horiz = T,
             space = 1,
             cex.names = 0.45,
             offset = -0.005,
             xlim = c(0, 0.25))
axis(side = 1, at = c(0, 0.3), labels = c("", ""), lwd.ticks = 0)
axis(side = 1, at = seq(0, 0.25, by = 0.05), 
     cex.axis = 0.5, tck = -0.01, 
     mgp = c(3, 0, 0), las = 1)
dev.off()


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Gaps between outcomes in treated and synthetic control
gaps <- dataprep.out$Y1plot - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# HIC and OECD: 0.0004844647
# OECD: 0.002363792

# Extract pre-treatment outcome values in treated unit and synthetic counterpart
preT.UK <- Y1plot.UK[gap.start:gap.end.pre]
preT.synth <- synth[gap.start:gap.end.pre]

# Two-sample t test
t.test(preT.UK, preT.synth)
p.val.t <- t.test(preT.UK, preT.synth)$p.value

# Kolmogorov Smirnov test
ks.test(preT.UK, preT.synth)
p.val.KS <- ks.test(preT.UK, preT.synth)$p.value

# QQ statistics
qqstats <- unlist(qqstats(preT.UK, preT.synth))

# Export balance tests
results <- kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "rst")
capture.output(results, file = "Results/Supplementary Information/Balance Specification 5.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Supplementary Information/Emissions paths in treated and synth_Spec 5.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11), 
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
text(1997.5, 8.04, "CCP enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####
placebos <- control.units

store <- matrix(NA, length(years), length(placebos))
colnames(store) <- whodat(placebos)
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = c("NY.GDP.PCAP.KD" # GDP per capita (constant 2010 US$)
                            #"NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                            #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                            #"EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                            #"EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                            #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                            #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                            #"NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                            #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                            #"EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                            #"EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                            #"TX.VAL.FUEL.ZS.UN" # Fuel exports (% of merchandise exports)
                            #"NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                            #"NE.IMP.GNFS.ZS" # Imports of goods and services (% of GDP)
             ),
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("EN.ATM.CO2E.PC", 1990, "mean"),
               list("EN.ATM.CO2E.PC", 1991, "mean"),
               list("EN.ATM.CO2E.PC", 1992, "mean"),
               list("EN.ATM.CO2E.PC", 1993, "mean"),
               list("EN.ATM.CO2E.PC", 1994, "mean"),
               list("EN.ATM.CO2E.PC", 1995, "mean"),
               list("EN.ATM.CO2E.PC", 1996, "mean"),
               list("EN.ATM.CO2E.PC", 1997, "mean"),
               list("EN.ATM.CO2E.PC", 1998, "mean"),
               list("EN.ATM.CO2E.PC", 1999, "mean"),
               list("EN.ATM.CO2E.PC", 2000, "mean")),
             dependent = "EN.ATM.CO2E.PC",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)

  # Calculating annual gaps between the treated and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Placebo loops/store_Spec 5.Rdata")


# .. Placebo figure ####
placebo.results <- cbind(store, gaps)
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 4 times higher MSPE than UK
colnames(placebo.results[, MSE > 4*UK.MSE])
# HIC and OECD: Exclude ABW, ARE, ARG, AUS, AUT, BEL, BHR, BHS, BMU, BRB, BRN, CAN
# CHE, CHL, CYP, DEU, ESP, FIN, FRA, GRL, HKG, IRL, ISR, JPN
# KNA, KOR, LUX, MAC, MLT, OMN, PAN, POL, PRT, SAU, SGP, SYC
# TTO, TUR, URY, USA
# OECD: Exclude AUS, AUT, BEL, CAN, CHE, CHL, DEU, ESP, FIN, FRA, IRL, ISR
# JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR, USA
placebo.results <- placebo.results[, MSE < 4*UK.MSE]

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_placebo_MSPE4_Spec 5.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.6, 1.3), 
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
text(1998, -0.98, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


# .. MSPE analysis ####
placebo.results <- cbind(store, gaps)

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.start.post <- which(rownames(placebo.results) == "2002")
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 434 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/46 = 0.04347826
# OECD: For the UK, the post-treatment gap is 111 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/24 = 0.04166667

# Plot
pdf("Figures/Supplementary Information/MSPE Ratio_Spec 5.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.MSE),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.MSE))
lab.cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.6,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store) <- paste0("No_", leaveoneout.names)
store

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  controls.identifier = leaveoneout.controls[-i]
  print(controls.identifier)
}

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = c("NY.GDP.PCAP.KD" # GDP per capita (constant 2010 US$)
                            #"NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                            #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                            #"EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                            #"EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                            #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                            #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                            #"NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                            #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                            #"EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                            #"EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                            #"TX.VAL.FUEL.ZS.UN" # Fuel exports (% of merchandise exports)
                            #"NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                            #"NE.IMP.GNFS.ZS" # Imports of goods and services (% of GDP)
             ),
             predictors.op = "mean",
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("EN.ATM.CO2E.PC", 1990, "mean"),
               list("EN.ATM.CO2E.PC", 1991, "mean"),
               list("EN.ATM.CO2E.PC", 1992, "mean"),
               list("EN.ATM.CO2E.PC", 1993, "mean"),
               list("EN.ATM.CO2E.PC", 1994, "mean"),
               list("EN.ATM.CO2E.PC", 1995, "mean"),
               list("EN.ATM.CO2E.PC", 1996, "mean"),
               list("EN.ATM.CO2E.PC", 1997, "mean"),
               list("EN.ATM.CO2E.PC", 1998, "mean"),
               list("EN.ATM.CO2E.PC", 1999, "mean"),
               list("EN.ATM.CO2E.PC", 2000, "mean")),
             dependent = "EN.ATM.CO2E.PC",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)

  # Calculating annual gaps between the UK and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Leave-one-out/store_Spec 5.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 5.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-1.6, 1.3),
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
text(1998, -1.045, "CCP enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 6        ####
## ## ## ## ## ## ## ## ## ##

# .. Optimize over 1990-2001, 2000 baseline, no covariates ####
treated.unit <- data %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- data %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)

choose.time.predictors <- 1990:2001

dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("rescaled2000", 1990, "mean"),
             list("rescaled2000", 1991, "mean"),
             list("rescaled2000", 1992, "mean"),
             list("rescaled2000", 1993, "mean"),
             list("rescaled2000", 1994, "mean"),
             list("rescaled2000", 1995, "mean"),
             list("rescaled2000", 1996, "mean"),
             list("rescaled2000", 1997, "mean"),
             list("rescaled2000", 1998, "mean"),
             list("rescaled2000", 1999, "mean")),
             #list("rescaled2000", 2000, "mean")),
           dependent = "rescaled2000",
           unit.variable = "countryid",
           unit.names.variable = "country",
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
synth.spec <- list(treated = whatname(dataprep.out$tag[["treatment.identifier"]]),
                   donor.pool = whatname(dataprep.out$tag[["controls.identifier"]]),
                   predictors = rownames(dataprep.out$X1),
                   time.optimize = dataprep.out$tag[["time.optimize.ssr"]])
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 6.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)


# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 6.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
pdf("Figures/Supplementary Information/Donor weights_Spec 6.pdf", 
    height = 4.5, width = 6)
par(mar = c(2, 6.1, 2, 2.1), las = 2)
a <- barplot(donor.weights$Donor.weight,
             xaxt = "n",
             main = "Donor weights",
             names.arg = donor.weights$Donor.country,
             cex.main = 0.9,
             horiz = T,
             space = 1,
             cex.names = 0.45,
             offset = -0.005,
             xlim = c(0, 0.25))
axis(side = 1, at = c(0, 0.3), labels = c("", ""), lwd.ticks = 0)
axis(side = 1, at = seq(0, 0.25, by = 0.05), 
     cex.axis = 0.5, tck = -0.01, 
     mgp = c(3, 0, 0), las = 1)
dev.off()


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w

# Gaps between outcomes in treated and synthetic control
gaps <- dataprep.out$Y1plot - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# HIC and OECD: 0.000006687303
# OECD: 0.00003773403

# Extract pre-treatment outcome values in treated unit and synthetic counterpart
preT.UK <- Y1plot.UK[gap.start:gap.end.pre]
preT.synth <- synth[gap.start:gap.end.pre]

# Two-sample t test
t.test(preT.UK, preT.synth)
p.val.t <- t.test(preT.UK, preT.synth)$p.value

# Kolmogorov Smirnov test
ks.test(preT.UK, preT.synth)
p.val.KS <- ks.test(preT.UK, preT.synth)$p.value

# QQ statistics
qqstats <- unlist(qqstats(preT.UK, preT.synth))

# Export balance tests
results <- kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "rst")
capture.output(results, file = "Results/Supplementary Information/Balance Specification 6.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Supplementary Information/Emissions paths in treated and synth_Spec 6.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, Y1plot.UK, 
     type = "l", col = "royalblue4", lwd = 2,
     xlim = range(years), 
     ylim = c(0.9, 1.1), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 2000")),
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
text(1997.5, 0.9307, "CCP enacted", cex = 0.8)
dev.off()


# .. Placebo loops ####
placebos <- control.units

placebos <- placebos[which(placebos != 47)] # FRA
# HIC and OECD: SVD fails

store <- matrix(NA, length(years), length(placebos))
colnames(store) <- whodat(placebos)
store

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = list(
               list("rescaled2000", 1990, "mean"),
               list("rescaled2000", 1991, "mean"),
               list("rescaled2000", 1992, "mean"),
               list("rescaled2000", 1993, "mean"),
               list("rescaled2000", 1994, "mean"),
               list("rescaled2000", 1995, "mean"),
               list("rescaled2000", 1996, "mean"),
               list("rescaled2000", 1997, "mean"),
               list("rescaled2000", 1998, "mean"),
               list("rescaled2000", 1999, "mean")),
             #list("rescaled2000", 2000, "mean")),
             dependent = "rescaled2000",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = placebos[i],
             controls.identifier = placebos[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)

  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)

  # Calculating annual gaps between the treated and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Placebo loops/store_Spec 6.Rdata")


# .. Placebo figure ####
placebo.results <- cbind(store, gaps)
placebo.results

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 4 times higher MSPE than UK
colnames(placebo.results[, MSE > 4*UK.MSE])
# HIC and OECD: Exclude ABW, ARE, ARG, AUT, BEL, BHR, BHS, BMU, BRB, BRN, CHE, CHL
# CYM, CYP, DEU, ESP, FIN, FRO, GRL, HKG, HUN, IRL, ISL, ISR
# JPN, KNA, KOR, LUX, MAC, MLT, NCL, OMN, PAN, POL, PRT, PYF
# QAT, SAU, SGP, SYC, TTO, TUR, URY, USA, VGB
# OECD: Exclude AUT, BEL, CHE, CHL, DEU, ESP, FIN, FRA, HUN, IRL, ISL, ISR
# JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR
placebo.results <- placebo.results[, MSE < 4*UK.MSE]

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_placebo_MSPE4_Spec 6.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.2, 0.2), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 2000")),
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
text(1998, -0.149, "CCP enacted", cex = 0.8)
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()


# .. MSPE analysis ####
placebo.results <- cbind(store, gaps)

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
gap.start.post <- which(rownames(placebo.results) == "2002")
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Ratio of post-treatment MSPE to pre-treatment MSPE
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 767 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 7/54 = 0.1296296
# OECD: For the UK, the post-treatment gap is 110 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/26 = 0.03846154

# Plot
pdf("Figures/Supplementary Information/MSPE Ratio_Spec 6.pdf", 
    height = 4.5, width = 6)
cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "grey")
a <- barplot(sort(ratio.MSE),
             xaxt = "n",
             yaxt = "n",
             col = cols,
             main = "Ratio of post-treatment MSPE to pre-treatment MSPE",
             cex.main = 0.9)
labs <- names(sort(ratio.MSE))
lab.cols <- ifelse(names(sort(ratio.MSE)) == "GBR", "darkorchid", "black")
text(a[,1], y = -2, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.6,
     col = lab.cols)
axis(side = 2, cex.axis = 0.8, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2)
dev.off()


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store) <- paste0("No_", leaveoneout.names)
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
               list("rescaled2000", 1990, "mean"),
               list("rescaled2000", 1991, "mean"),
               list("rescaled2000", 1992, "mean"),
               list("rescaled2000", 1993, "mean"),
               list("rescaled2000", 1994, "mean"),
               list("rescaled2000", 1995, "mean"),
               list("rescaled2000", 1996, "mean"),
               list("rescaled2000", 1997, "mean"),
               list("rescaled2000", 1998, "mean"),
               list("rescaled2000", 1999, "mean")),
             #list("rescaled2000", 2000, "mean")),
             dependent = "rescaled2000",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = leaveoneout.controls[-i],
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)
  
  
  # Running Synth
  synth.out <- synth(data.prep.obj = dataprep.out)
  
  
  # Calculating annual gaps between the UK and its synthetic counterpart
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Supplementary Information/Leave-one-out/store_Spec 6.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 6.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
     type = "l", col = "darkorchid", lwd = 2,
     xlim = range(years), 
     ylim = c(-0.2, 0.2), 
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions relative to 2000")),
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
text(1998, -0.149, "CCP enacted", cex = 0.8)
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
dev.off()



## ## ## ## ## ## ## ## ## ##
# TABLES FOR SI          ####
## ## ## ## ## ## ## ## ## ##