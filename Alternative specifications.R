# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# Specification 2
# .. Optimize over 1990-2001, CO2 per capita, with covariates, OECD & high income
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo countries
# .. Placebo figure
# .. Ratio of post-treatment MSPE to pre-treatment MSPE
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 3
# .. Optimize over 1990-2001, CO2 per capita, no covariates, OECD
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo countries
# .. Placebo figure
# .. Ratio of post-treatment MSPE to pre-treatment MSPE
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 4
# .. Optimize over 1990-2001, 1990 baseline, no covariates, OECD
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo countries
# .. Placebo figure
# .. Ratio of post-treatment MSPE to pre-treatment MSPE
# .. Leave-one-out check
# .. Leave-one-out figure
# Specification 5
# .. Optimize over 1990-2001, 2000 baseline, no covariates, OECD
# .. Running Synth
# .. Export results
# .. Generate results
# .. Balance tests
# .. Emissions trajectories figure
# .. Placebo countries
# .. Placebo figure
# .. Ratio of post-treatment MSPE to pre-treatment MSPE
# .. Leave-one-out check
# .. Leave-one-out figure



## ## ## ## ## ## ## ## ## ##
# PREAMBLE               ####
## ## ## ## ## ## ## ## ## ##

setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/Synth/") # Alice laptop
#setwd("C:/boxsync/alepissier/LepissierMildenberger/Synth/") # Alice work
#setwd("~/Box Sync/LepissierMildenberger/Synth/") # Matto
library(devtools)
library(ggplot2)
library(kableExtra)
library(Matching)
library(plyr)
library(reshape2)
library(scales)
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
# SPECIFICATION 2        ####
## ## ## ## ## ## ## ## ## ##

load("Data/data_OECD_HIC.Rdata")


# .. Optimize over 1990-2001, CO2 per capita, with covariates, OECD & high income ####
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
            GC.TAX.TOTL.GD.ZS,
            EG.IMP.CONS.ZS)) %>%
  filter(countrycode != "GBR" & year >= 1990 & year <= 2001) %>%
  filter(complete.cases(NY.GDP.PCAP.KD,
                        EG.FEC.RNEW.ZS,
                        EG.USE.COMM.FO.ZS,
                        EG.USE.PCAP.KG.OE)) %>%
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
                          #"NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                          #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                          "EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                          "EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                          #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                          #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                          #"NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                          #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                          #"EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                          "EG.USE.PCAP.KG.OE" # Energy use (kg of oil equivalent per capita)
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
weights.spec2 <- donor.weights %>%
  arrange(Donor.country)

# Plot
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, 
                               round(Donor.weight, 5), 
                               round(Donor.weight, 4))),
            size = 2, nudge_x = 0, nudge_y = 0.011) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 6))
ggsave(g,
       file = "Figures/Supplementary Information/Donor weights_Spec 2.pdf",
       height = 4.5, width = 6, units = "in")


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
MSE.spec2 <- pre.MSE
# HIC and OECD: 0.002191321

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
plot <- data.frame(years = years,
                   treated = Y1plot.UK,
                   synth = synth)
colnames(plot) <- c("years", "treated", "synth")
plot <- plot %>%
  melt(id.vars = "years", value.name = "emissions")

g <- ggplot(plot,
            aes(x = years, y = emissions, col = variable, linetype = variable)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("royalblue4", "royalblue1"),
                     labels = c("United Kingdom", "Synthetic UK")) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("United Kingdom", "Synthetic UK")) +
  ylim(8, 11) +
  labs(title = "Observed and Synthetic Counterfactual Emissions",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = 8.5, yend = 8.5,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches")),
               show.legend = F) +
  geom_text(x = 1997.5,
            y = 8.5,
            label = "CCP enacted",
            col = "black") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))
ggsave(g,
       file = "Figures/Supplementary Information/Emissions paths in treated and synth_Spec 2.pdf",
       height = 4.5, width = 6, units = "in")


# .. Placebo countries ####
placebos <- control.units

placebos <- placebos[which(placebos != 143)] # TTO
# SVD fails

store.gaps <- matrix(NA, length(years), length(placebos))
colnames(store.gaps) <- whodat(placebos)
store.gaps

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = c("NY.GDP.PCAP.KD", # GDP per capita (constant 2010 US$)
                            #"NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                            #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                            "EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                            "EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                            #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                            #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                            #"NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                            #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                            #"EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                            "EG.USE.PCAP.KG.OE" # Energy use (kg of oil equivalent per capita)
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
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Placebo countries/store.gaps_Spec 2.Rdata")
load("Results/Supplementary Information/Placebo countries/store.gaps_Spec 2.Rdata")


# .. Placebo figure ####
placebo.results <- data.frame(store.gaps,
                              gaps)

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")
gap.start.post <- which(rownames(placebo.results) == "2002")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Exclude countries with 5 times higher MSPE than UK
colnames(placebo.results[, pre.MSE > 5*UK.pre.MSE])
# Exclude AUS, AUT, BEL, CAN, CHE, CHL, CYP, DEU, ESP, FRA, IRL, ISR, JPN
# KOR, LUX, NZL, PAN, POL, PRT, SAU, SGP, TUR, URY, USA

MSPE5 <- colnames(placebo.results[, pre.MSE < 5*UK.pre.MSE])

plot.gaps <- placebo.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")
plot.gaps5 <- plot.gaps %>%
  filter(Country %in% MSPE5)

# Plot
g <- ggplot(plot.gaps5) +
  geom_line(data = plot.gaps5 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps5 %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps5 %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.75, yend = -0.75,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.75,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_placebo_MSPE5_Spec 2.pdf",
       height = 4.5, width = 6, units = "in")


# .. Ratio of post-treatment MSPE to pre-treatment MSPE ####
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
ratio.MSE["GBR"]
(length(ratio.MSE) - which(sort(ratio.MSE) == ratio.MSE["GBR"]) +1 )/ length(ratio.MSE)
# HIC and OECD: For the UK, the post-treatment gap is 45 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/29 = 0.06896552

placebo.results <- cbind(store.gaps, gaps)
MSE <- data.frame(country = names(pre.MSE),
                  pre.MSE = pre.MSE,
                  post.MSE = post.MSE,
                  ratio = post.MSE/pre.MSE,
                  TE2005 = placebo.results[gap.end,]) %>%
  mutate(col = ifelse(country == "GBR", "darkorchid", "grey"))

# Plot ratio, two-sided test
g <- ggplot(MSE,
            aes(x = reorder(country, ratio), y = ratio,
                fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("darkorchid", "grey35")) +
  scale_y_continuous(labels = comma) +
  guides(fill = F) +
  labs(title = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       subtitle = "Two-sided test",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5))
ggsave(g,
       file = "Figures/Supplementary Information/MSPE ratio_Two-sided_Spec 2.pdf",
       height = 4.5, width = 6, units = "in")


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.gaps) <- paste0("No_", leaveoneout.names)
store.gaps

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = c("NY.GDP.PCAP.KD", # GDP per capita (constant 2010 US$)
                            #"NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                            #"EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                            "EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                            "EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                            #"GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                            #"EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                            #"NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                            #"SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                            #"EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                            "EG.USE.PCAP.KG.OE" # Energy use (kg of oil equivalent per capita)
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
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Leave-one-out/store.gaps_Spec 2.Rdata")
load("Results/Supplementary Information/Leave-one-out/store.gaps_Spec 2.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- data.frame(store.gaps)

plot.LOO <- leaveoneout.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")

# Plot
g <- ggplot(plot.LOO) +
  geom_line(data = plot.LOO %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("thistle", plot.LOO %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.75, yend = -0.75,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.75,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 2.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 3        ####
## ## ## ## ## ## ## ## ## ##

load("Data/data_OECD.Rdata")


# .. Optimize over 1990-2001, CO2 per capita, no covariates, OECD ####
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
capture.output(synth.spec, file = "Results/Supplementary Information/Specification 3.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results Specification 3.txt")

donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)
weights.spec3 <- donor.weights %>%
  arrange(Donor.country)

# Plot
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, 
                               round(Donor.weight, 5), 
                               round(Donor.weight, 4))),
            size = 2, nudge_x = 0, nudge_y = 0.011) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 6))
ggsave(g,
       file = "Figures/Supplementary Information/Donor weights_Spec 3.pdf",
       height = 4.5, width = 6, units = "in")


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
MSE.spec3 <- pre.MSE
# OECD: 0.002122138

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
plot <- data.frame(years = years,
                   treated = Y1plot.UK,
                   synth = synth)
colnames(plot) <- c("years", "treated", "synth")
plot <- plot %>%
  melt(id.vars = "years", value.name = "emissions")

g <- ggplot(plot,
            aes(x = years, y = emissions, col = variable, linetype = variable)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("royalblue4", "royalblue1"),
                     labels = c("United Kingdom", "Synthetic UK")) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("United Kingdom", "Synthetic UK")) +
  ylim(8, 11) +
  labs(title = "Observed and Synthetic Counterfactual Emissions",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = 8.5, yend = 8.5,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches")),
               show.legend = F) +
  geom_text(x = 1997.5,
            y = 8.5,
            label = "CCP enacted",
            col = "black") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))
ggsave(g,
       file = "Figures/Supplementary Information/Emissions paths in treated and synth_Spec 3.pdf",
       height = 4.5, width = 6, units = "in")


# .. Placebo countries ####
placebos <- control.units

store.gaps <- matrix(NA, length(years), length(placebos))
colnames(store.gaps) <- whodat(placebos)
store.gaps

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
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
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Placebo countries/store.gaps_Spec 3.Rdata")
load("Results/Supplementary Information/Placebo countries/store.gaps_Spec 3.Rdata")


# .. Placebo figure ####
placebo.results <- data.frame(store.gaps,
                              gaps)

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")
gap.start.post <- which(rownames(placebo.results) == "2002")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Exclude countries with 5 times higher MSPE than UK
colnames(placebo.results[, pre.MSE > 5*UK.pre.MSE])
# Exclude AUS, AUT, BEL, CAN, CHE, CHL, DEU, ESP, FRA, HUN, IRL, ISL, ISR
# JPN, KOR, LUX, MEX, NZL, POL, PRT, TUR, USA

MSPE5 <- colnames(placebo.results[, pre.MSE < 5*UK.pre.MSE])

plot.gaps <- placebo.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")
plot.gaps5 <- plot.gaps %>%
  filter(Country %in% MSPE5)

# Plot
g <- ggplot(plot.gaps5) +
  geom_line(data = plot.gaps5 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps5 %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps5 %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.75, yend = -0.75,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.75,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_placebo_MSPE5_Spec 3.pdf",
       height = 4.5, width = 6, units = "in")


# .. Ratio of post-treatment MSPE to pre-treatment MSPE ####
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
ratio.MSE["GBR"]
(length(ratio.MSE) - which(sort(ratio.MSE) == ratio.MSE["GBR"]) +1 )/ length(ratio.MSE)
# OECD: For the UK, the post-treatment gap is 102 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/25 = 0.04

placebo.results <- cbind(store.gaps, gaps)
MSE <- data.frame(country = names(pre.MSE),
                  pre.MSE = pre.MSE,
                  post.MSE = post.MSE,
                  ratio = post.MSE/pre.MSE,
                  TE2005 = placebo.results[gap.end,]) %>%
  mutate(col = ifelse(country == "GBR", "darkorchid", "grey"))

# Plot ratio, two-sided test
g <- ggplot(MSE,
            aes(x = reorder(country, ratio), y = ratio,
                fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("darkorchid", "grey35")) +
  scale_y_continuous(labels = comma) +
  guides(fill = F) +
  labs(title = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       subtitle = "Two-sided test",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5))
ggsave(g,
       file = "Figures/Supplementary Information/MSPE ratio_Two-sided_Spec 3.pdf",
       height = 4.5, width = 6, units = "in")


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.gaps) <- paste0("No_", leaveoneout.names)
store.gaps

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
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
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Leave-one-out/store.gaps_Spec 3.Rdata")
load("Results/Supplementary Information/Leave-one-out/store.gaps_Spec 3.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- data.frame(store.gaps)

plot.LOO <- leaveoneout.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")

# Plot
g <- ggplot(plot.LOO) +
  geom_line(data = plot.LOO %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("thistle", plot.LOO %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.75, yend = -0.75,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.75,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 3.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 4        ####
## ## ## ## ## ## ## ## ## ##

load("Data/data_OECD.Rdata")


# .. Optimize over 1990-2001, 1990 baseline, no covariates, OECD ####
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
weights.spec4 <- donor.weights %>%
  arrange(Donor.country)

# Plot
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, 
                               round(Donor.weight, 5), 
                               round(Donor.weight, 4))),
            size = 2, nudge_x = 0, nudge_y = 0.011) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 6))
ggsave(g,
       file = "Figures/Supplementary Information/Donor weights_Spec 4.pdf",
       height = 4.5, width = 6, units = "in")


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
MSE.spec4 <- pre.MSE
# OECD: 3.301514e-05

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
plot <- data.frame(years = years,
                   treated = Y1plot.UK,
                   synth = synth)
colnames(plot) <- c("years", "treated", "synth")
plot <- plot %>%
  melt(id.vars = "years", value.name = "emissions")

g <- ggplot(plot,
            aes(x = years, y = emissions, col = variable, linetype = variable)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("royalblue4", "royalblue1"),
                     labels = c("United Kingdom", "Synthetic UK")) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("United Kingdom", "Synthetic UK")) +
  ylim(0.9, 1.1) +
  labs(title = "Observed and Synthetic Counterfactual Emissions",
       x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 1990"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = 0.925, yend = 0.925,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches")),
               show.legend = F) +
  geom_text(x = 1997.5,
            y = 0.925,
            label = "CCP enacted",
            col = "black") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))
ggsave(g,
       file = "Figures/Supplementary Information/Emissions paths in treated and synth_Spec 4.pdf",
       height = 4.5, width = 6, units = "in")


# .. Placebo countries ####
placebos <- control.units

store.gaps <- matrix(NA, length(years), length(placebos))
colnames(store.gaps) <- whodat(placebos)
store.gaps

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
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
  synth.out <- synth(data.prep.obj = dataprep.out)

  # Calculating annual gaps between the treated and its synthetic counterpart
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Placebo countries/store.gaps_Spec 4.Rdata")
load("Results/Supplementary Information/Placebo countries/store.gaps_Spec 4.Rdata")


# .. Placebo figure ####
placebo.results <- data.frame(store.gaps,
                              gaps)

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")
gap.start.post <- which(rownames(placebo.results) == "2002")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Exclude countries with 5 times higher MSPE than UK
colnames(placebo.results[, pre.MSE > 5*UK.pre.MSE])
# Exclude AUT, BEL, CHE, CHL, DEU, ESP, FRA, HUN, IRL, ISL, ISR, JPN, KOR
# LUX, MEX, NZL, POL, PRT, TUR

MSPE5 <- colnames(placebo.results[, pre.MSE < 5*UK.pre.MSE])

plot.gaps <- placebo.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")
plot.gaps5 <- plot.gaps %>%
  filter(Country %in% MSPE5)

# Plot
g <- ggplot(plot.gaps5) +
  geom_line(data = plot.gaps5 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps5 %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps5 %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 1990"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.15, yend = -0.15,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.15,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_placebo_MSPE5_Spec 4.pdf",
       height = 4.5, width = 6, units = "in")


# .. Ratio of post-treatment MSPE to pre-treatment MSPE ####
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
ratio.MSE["GBR"]
(length(ratio.MSE) - which(sort(ratio.MSE) == ratio.MSE["GBR"]) +1 )/ length(ratio.MSE)
# OECD: For the UK, the post-treatment gap is 86 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/25 = 0.04

placebo.results <- cbind(store.gaps, gaps)
MSE <- data.frame(country = names(pre.MSE),
                  pre.MSE = pre.MSE,
                  post.MSE = post.MSE,
                  ratio = post.MSE/pre.MSE,
                  TE2005 = placebo.results[gap.end,]) %>%
  mutate(col = ifelse(country == "GBR", "darkorchid", "grey"))

# Plot ratio, two-sided test
g <- ggplot(MSE,
            aes(x = reorder(country, ratio), y = ratio,
                fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("darkorchid", "grey35")) +
  scale_y_continuous(labels = comma) +
  guides(fill = F) +
  labs(title = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       subtitle = "Two-sided test",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5))
ggsave(g,
       file = "Figures/Supplementary Information/MSPE ratio_Two-sided_Spec 4.pdf",
       height = 4.5, width = 6, units = "in")


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.gaps) <- paste0("No_", leaveoneout.names)
store.gaps

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
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
  synth.out <- synth(data.prep.obj = dataprep.out)

  # Calculating annual gaps between the UK and its synthetic counterpart
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Leave-one-out/store.gaps_Spec 4.Rdata")
load("Results/Supplementary Information/Leave-one-out/store.gaps_Spec 4.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- data.frame(store.gaps)

plot.LOO <- leaveoneout.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")

# Plot
g <- ggplot(plot.LOO) +
  geom_line(data = plot.LOO %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("thistle", plot.LOO %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 1990"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.15, yend = -0.15,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.15,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 4.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION 5        ####
## ## ## ## ## ## ## ## ## ##

load("Data/data_OECD.Rdata")


# .. Optimize over 1990-2001, 2000 baseline, no covariates, OECD ####
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
           predictors.op = "mean",
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
weights.spec5 <- donor.weights %>%
  arrange(Donor.country)

# Plot
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, 
                               round(Donor.weight, 5), 
                               round(Donor.weight, 4))),
            size = 2, nudge_x = 0, nudge_y = 0.011) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 6))
ggsave(g,
       file = "Figures/Supplementary Information/Donor weights_Spec 5.pdf",
       height = 4.5, width = 6, units = "in")


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
MSE.spec5 <- pre.MSE
# OECD: 3.728242e-05

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
plot <- data.frame(years = years,
                   treated = Y1plot.UK,
                   synth = synth)
colnames(plot) <- c("years", "treated", "synth")
plot <- plot %>%
  melt(id.vars = "years", value.name = "emissions")

g <- ggplot(plot,
            aes(x = years, y = emissions, col = variable, linetype = variable)) +
  geom_line(lwd = 1) +
  scale_color_manual(values = c("royalblue4", "royalblue1"),
                     labels = c("United Kingdom", "Synthetic UK")) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("United Kingdom", "Synthetic UK")) +
  ylim(0.9, 1.1) +
  labs(title = "Observed and Synthetic Counterfactual Emissions",
       x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 2000"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = 0.925, yend = 0.925,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches")),
               show.legend = F) +
  geom_text(x = 1997.5,
            y = 0.925,
            label = "CCP enacted",
            col = "black") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))
ggsave(g,
       file = "Figures/Supplementary Information/Emissions paths in treated and synth_Spec 5.pdf",
       height = 4.5, width = 6, units = "in")


# .. Placebo countries ####
placebos <- control.units

store.gaps <- matrix(NA, length(years), length(placebos))
colnames(store.gaps) <- whodat(placebos)
store.gaps

for (i in 1:length(placebos)){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = "mean",
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
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Placebo countries/store.gaps_Spec 5.Rdata")
load("Results/Supplementary Information/Placebo countries/store.gaps_Spec 5.Rdata")


# .. Placebo figure ####
placebo.results <- data.frame(store.gaps,
                              gaps)

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")
gap.start.post <- which(rownames(placebo.results) == "2002")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.pre.MSE <- as.numeric(pre.MSE["GBR"])

# Mean Square Prediction Error Post-Treatment
post.MSE <- apply(placebo.results[gap.start.post:gap.end, ]^2, 2, mean)
UK.post.MSE <- as.numeric(post.MSE["GBR"])

# Exclude countries with 5 times higher MSPE than UK
colnames(placebo.results[, pre.MSE > 5*UK.pre.MSE])
# Exclude AUT, BEL, CHE, CHL, DEU, ESP, FRA, HUN, IRL, ISL, ISR, KOR, LUX
# NZL, POL, PRT, TUR

MSPE5 <- colnames(placebo.results[, pre.MSE < 5*UK.pre.MSE])

plot.gaps <- placebo.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")
plot.gaps5 <- plot.gaps %>%
  filter(Country %in% MSPE5)

# Plot
g <- ggplot(plot.gaps5) +
  geom_line(data = plot.gaps5 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps5 %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps5 %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 2000"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.15, yend = -0.15,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.15,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_placebo_MSPE5_Spec 5.pdf",
       height = 4.5, width = 6, units = "in")


# .. Ratio of post-treatment MSPE to pre-treatment MSPE ####
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
ratio.MSE["GBR"]
(length(ratio.MSE) - which(sort(ratio.MSE) == ratio.MSE["GBR"]) +1 )/ length(ratio.MSE)
# OECD: For the UK, the post-treatment gap is 112 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/25 = 0.04

placebo.results <- cbind(store.gaps, gaps)
MSE <- data.frame(country = names(pre.MSE),
                  pre.MSE = pre.MSE,
                  post.MSE = post.MSE,
                  ratio = post.MSE/pre.MSE,
                  TE2005 = placebo.results[gap.end,]) %>%
  mutate(col = ifelse(country == "GBR", "darkorchid", "grey"))

# Plot ratio, two-sided test
g <- ggplot(MSE,
            aes(x = reorder(country, ratio), y = ratio,
                fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("darkorchid", "grey35")) +
  scale_y_continuous(labels = comma) +
  guides(fill = F) +
  labs(title = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       subtitle = "Two-sided test",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5))
ggsave(g,
       file = "Figures/Supplementary Information/MSPE ratio_Two-sided_Spec 5.pdf",
       height = 4.5, width = 6, units = "in")


# .. Leave-one-out check ####
leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.gaps) <- paste0("No_", leaveoneout.names)
store.gaps

nloops <- length(leaveoneout.controls)+1

for (i in 1:nloops){
  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = "mean",
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
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.gaps, file = "Results/Supplementary Information/Leave-one-out/store.gaps_Spec 5.Rdata")
load("Results/Supplementary Information/Leave-one-out/store.gaps_Spec 5.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- data.frame(store.gaps)

plot.LOO <- leaveoneout.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")

# Plot
g <- ggplot(plot.LOO) +
  geom_line(data = plot.LOO %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("thistle", plot.LOO %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps %>%
              filter(Country == "GBR"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-0.2, 0.2)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Re-assigning treatment to placebo countries",
       x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 2000"))) +
  geom_vline(xintercept = 2001,
             lty = 2) +
  geom_segment(x = 1999, xend = 2001,
               y = -0.15, yend = -0.15,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.15,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Supplementary Information/Gaps in emissions_leave one out_Spec 5.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ##
# TABLES FOR SI          ####
## ## ## ## ## ## ## ## ## ##

weights <- left_join(weights.spec3 %>%
                       rename(Spec3 = Donor.weight),
                     weights.spec4 %>%
                       rename(Spec4 = Donor.weight),
                     by = c("Donor.country")) %>%
  left_join(weights.spec5 %>%
              rename(Spec5 = Donor.weight),
            by = c("Donor.country"))

kable(weights, format = "latex", digits = 4)

MSE <- c("Pre-treatment MSPE", 
         MSE.spec2, 
         MSE.spec3, 
         MSE.spec4,
         MSE.spec5)
