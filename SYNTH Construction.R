# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# .. Import indicators from WDI
# .. Create outcome variables
# .. Drop missing data and specific donors
# .. Figures of summary statistics
# Specification in Paper
# .. Optimize over 1990-2001, CO2 per capita, no covariates
# Synth
# .. Running Synth 
# .. Pre-treatment predictor values 
# .. Weights for the predictor variables 
# .. Weights for countries in the donor pool 
# .. Export results
# Generate Results    
# .. Outcomes
# .. Balance tests
# .. Emissions trajectories figure
# Placebo Loops
# .. Running Synth
# .. Outcomes
# .. Store emissions path in treated unit and its synthetic counterpart
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Emissions trajectories figure
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
#library(gridExtra)
library(kableExtra)
library(Matching)
library(plyr)
library(stargazer)
library(Synth)
library(tidyverse)
#devtools::install_github('xuyiqing/tjbal')
library(tjbal)
library(WDI)
library(xlsx)
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

codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes") %>%
  mutate_all(as.character)


# .. Import emissions for Germany from CDIAC ####
# Data for Germany prior to 1991 is missing from WDI.
# WDI uses CDIAC as the raw data source, so go to CDIAC to import emissions prior to 1991.

CDIAC <- read.csv("Data/nation.1751_2014.csv") %>%
  select(Nation, Year, Emissions = Total.CO2.emissions.from.fossil.fuels.and.cement.production..thousand.metric.tons.of.C.) %>%
  filter(Nation == "FORMER GERMAN DEMOCRATIC REPUBLIC" | Nation == "FEDERAL REPUBLIC OF GERMANY")

CDIAC <- spread(CDIAC, Nation, Emissions) %>%
  mutate(EN.ATM.CO2E.KT = (`FEDERAL REPUBLIC OF GERMANY` + `FORMER GERMAN DEMOCRATIC REPUBLIC`) * 3.667,
         country = "Germany", iso2c = "DE") %>%
  select(iso2c, country, year = Year, EN.ATM.CO2E.KT) %>%
  filter(year >= 1960)


# .. Import indicators from WDI ####
countries <- codes %>%
  filter(WB_Income_Group_Code == "HIC" | 
           WB_Income_Group_Code == "UMC" |
           OECD == 1 | 
           Commonwealth == 1) %>%
  distinct(ISO3166.2) %>%
  pull

indicators <- c("EN.ATM.CO2E.KT", # CO2 emissions (kt)
                "EN.ATM.CO2E.PC", # CO2 emissions (metric tons per capita)
                "NY.GDP.PCAP.KD", # GDP per capita (constant 2010 US$)
                "NY.GDP.PCAP.KD.ZG", # GDP per capita growth (annual %)
                "NY.GDP.MKTP.KD.ZG", # GDP growth (annual %)
                "EG.IMP.CONS.ZS", # Energy imports, net (% of energy use)
                "EG.FEC.RNEW.ZS", # Renewable energy consumption (% of total final energy consumption)
                "EG.USE.COMM.FO.ZS", # Fossil fuel energy consumption (% of total)
                "GC.TAX.TOTL.GD.ZS", # Tax revenue (% of GDP)
                "EG.GDP.PUSE.KO.PP.KD", # GDP per unit of energy use (constant 2011 PPP $ per kg of oil equivalent)
                "NY.GDP.TOTL.RT.ZS", # Total natural resources rents (% of GDP)
                "SE.XPD.TOTL.GD.ZS", # Government expenditure on education, total (% of GDP)
                "EG.ELC.RNEW.ZS", # Renewable electricity output (% of total electricity output)
                "EG.USE.PCAP.KG.OE", # Energy use (kg of oil equivalent per capita)
                "TX.VAL.FUEL.ZS.UN", # Fuel exports (% of merchandise exports)
                "NE.EXP.GNFS.ZS", # Exports of goods and services (% of GDP)
                "NE.IMP.GNFS.ZS", # Imports of goods and services (% of GDP)
                "SP.POP.TOTL" # Population, total
                )

# data <- WDI(country = countries, indicator = indicators)
# save(data, file = "Data/data.Rdata")
load("Data/data.Rdata")

data[which(data$country == "Germany" & data$year <= 1990), "EN.ATM.CO2E.KT"] <- CDIAC$EN.ATM.CO2E.KT
data <- data %>%
  mutate(EN.ATM.CO2E.PC = ifelse(country == "Germany" & year <= 1990, EN.ATM.CO2E.KT * 1000 / SP.POP.TOTL, EN.ATM.CO2E.PC))

data <- left_join(data, codes %>% 
                     select(ISO3166.2, ISO3166.3, Country) %>% 
                     distinct(ISO3166.2, .keep_all = T),
                   by = c("iso2c" = "ISO3166.2")) %>%
  select(-c(iso2c, country)) %>%
  rename(countrycode = ISO3166.3,
         country = Country) %>%
  mutate(countryid = group_indices(., countrycode)) %>%
  select(countryid, countrycode, country, year, everything()) %>%
  arrange(countryid)

countries <- unique(data$countrycode)


# .. Create outcome variables ####
# Rescale emissions to 1990 
data <- left_join(data, data %>%
                    group_by(countryid) %>%
                    filter(year == 1990) %>%
                    select(countryid, baseline1990 = EN.ATM.CO2E.KT) %>%
                    ungroup(),
             by = "countryid") %>%
  mutate(rescaled1990 = EN.ATM.CO2E.KT/baseline1990)
attr(data$baseline1990, "label") <- "CO2 emissions (kt) in 1990"
attr(data$rescaled1990, "label") <- NULL

# Rescale emissions to 2000 
data <- left_join(data, data %>%
                    group_by(countryid) %>%
                    filter(year == 2000) %>%
                    select(countryid, baseline2000 = EN.ATM.CO2E.KT) %>%
                    ungroup(),
                  by = "countryid") %>%
  mutate(rescaled2000 = EN.ATM.CO2E.KT/baseline2000)
attr(data$baseline2000, "label") <- "CO2 emissions (kt) in 2000"
attr(data$rescaled1990, "label") <- NULL


# .. Drop missing data and specific donors ####
range(data$year)
data <- data %>%
  filter(year >= 1980)

nmiss <- ddply(data, "countrycode", summarize,
               co2.missing = sum(is.na(EN.ATM.CO2E.KT)),
               co2pc.missing = sum(is.na(EN.ATM.CO2E.PC)),
               gdppc.missing = sum(is.na(NY.GDP.PCAP.KD)),
               gdppcgrowth.missing = sum(is.na(NY.GDP.PCAP.KD.ZG)),
               gdpgrowth.missing = sum(is.na(NY.GDP.MKTP.KD.ZG)),
               energyimports.missing = sum(is.na(EG.IMP.CONS.ZS)),
               renewablecons.missing = sum(is.na (EG.FEC.RNEW.ZS)),
               FFconsumption.missing = sum(is.na(EG.USE.COMM.FO.ZS)),
               taxrevenuegdp.missing = sum(is.na(GC.TAX.TOTL.GD.ZS)),
               gdpperenergyu.missing = sum(is.na(EG.GDP.PUSE.KO.PP.KD)),
               naturresrents.missing = sum(is.na(NY.GDP.TOTL.RT.ZS)),
               gvtexpendeduc.missing = sum(is.na(SE.XPD.TOTL.GD.ZS)),
               renewableelec.missing = sum(is.na(EG.ELC.RNEW.ZS)),
               energyuseinkg.missing = sum(is.na(EG.USE.PCAP.KG.OE)),
               fuelexportspc.missing = sum(is.na(TX.VAL.FUEL.ZS.UN)),
               exportsgdp.missing = sum(is.na(NE.EXP.GNFS.ZS)),
               importsgdp.missing = sum(is.na(NE.IMP.GNFS.ZS)))
summary(nmiss)

missing <- subset(nmiss, 
                  (co2.missing > 10) | (
                    co2pc.missing > 10
                    & gdppc.missing > 10
                    & gdppcgrowth.missing > 10
                    & gdpgrowth.missing > 10
                    & energyimports.missing > 10
                    & renewablecons.missing > 10
                    & FFconsumption.missing > 10
                    & taxrevenuegdp.missing > 10
                    & gdpperenergyu.missing > 10
                    & naturresrents.missing > 10
                    & gvtexpendeduc.missing > 10
                    & renewableelec.missing > 10
                    & energyuseinkg.missing > 10
                    & fuelexportspc.missing > 10
                    & exportsgdp.missing > 10
                    & importsgdp.missing > 10)
                )

missing <- missing %>% 
  select(countrycode) %>%
  pull
whodis(missing)

data <- subset(data, !(countrycode %in% missing))
whoder()

missing <- data %>% 
  filter(year >= 1990 & year < 2005) %>%
  filter(is.na(EN.ATM.CO2E.KT) | is.na(EN.ATM.CO2E.PC)) %>%
  distinct(countrycode) %>%
  pull
data <- subset(data, !(countrycode %in% missing))
whoder()

scandis <- c("DNK", "NLD", "NOR", "SWE")
data <- subset(data, !(countrycode %in% scandis))
whoder()

data <- subset(data, countrycode != "GIB")
whoder()

HIC <- codes %>%
  filter(WB_Income_Group_Code == "HIC") %>%
  distinct(ISO3166.3) %>%
  pull

UMC <- codes %>%
  filter(WB_Income_Group_Code == "UMC") %>%
  distinct(ISO3166.3) %>%
  pull

OECD <- codes %>%
  filter(OECD == 1) %>%
  distinct(ISO3166.3) %>%
  pull

Commonwealth <- codes %>%
  filter(Commonwealth == 1) %>%
  distinct(ISO3166.3) %>%
  pull

data_all <- data

data <- subset(data_all, countrycode %in% OECD)
whoder()
save(data, file = "Data/data_OECD.Rdata")

data <- subset(data_all, countrycode %in% OECD | countrycode %in% HIC)
whoder()
save(data, file = "Data/data_OECD_HIC.Rdata")

# data <- subset(data, countrycode %in% OECD | countrycode %in% HIC | countrycode %in% UMC)
# data <- subset(data, countrycode %in% OECD | countrycode %in% Commonwealth)

# tiny.countries <- data %>% 
#   filter(year == 2015) %>% 
#   filter(SP.POP.TOTL <= 500000) %>% 
#   distinct(countrycode) %>%
#   pull
# data <- subset(data, !(countrycode %in% tiny.countries))


# .. Figures of summary statistics ####
# Emissions per capita in sample
ggplot(data %>% filter(countrycode != "GBR"), 
       aes(x = year, y = EN.ATM.CO2E.PC, col = country)) + 
  geom_line() +
  xlab("Year") + 
  ylab(expression(paste("CO"[2], " emissions per capita"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data %>% filter(countrycode == "GBR"), 
            aes(x = year, y = EN.ATM.CO2E.PC, col = "United Kingdom"), size = 1.5) +
  theme(legend.position = "none")

# Emissions relative to 1990 in sample
ggplot(data %>% filter(countrycode != "GBR"), 
       aes(x = year, y = rescaled1990, col = country)) + 
  geom_line() +
  xlab("Year") + 
  ylab(expression(paste("CO"[2], " emissions against 1990 baseline"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data %>% filter(countrycode == "GBR"), 
            aes(x = year, y = rescaled1990, col = "United Kingdom"), size = 1.5) +
  theme(legend.position = "none")

# Emissions relative to 1990 in effective sample
pdf("Figures/CO2 emissions (1990) in effective sample.pdf", 
    height = 4.5, width = 6)
ggplot(data %>% filter(countrycode == "NRU" |
                         countrycode == "ROU" |
                         countrycode == "BEL" |
                         countrycode == "FRO" |
                         countrycode == "LIB" |
                         countrycode == "LUX"), 
       aes(x = year, y = rescaled1990, col = country)) + 
  geom_line() +
  xlab("Year") + 
  ylab(expression(paste("CO"[2], " emissions against 1990 baseline"))) +
  ggtitle("Emissions trends in the United Kingdom and effective sample") +
  geom_line(data = data %>% filter(countrycode == "GBR"), 
            aes(x = year, y = rescaled1990, col = "United Kingdom"), size = 1.5) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  xlim(1990, 2010)
dev.off()

# Zooming in emissions per capita in sample
ggplot(data[which(!data$countrycode == "GBR" 
                  & data$EN.ATM.CO2E.PC >= 5 
                  & data$EN.ATM.CO2E.PC <= 15),], 
       aes(x = year, y = EN.ATM.CO2E.PC, col = country)) + 
  geom_line() +
  xlab("Year") + 
  ylab(expression(paste("CO"[2], " emissions per capita"))) +
  ggtitle("Emissions trends in the United Kingdom and donor pool") +
  geom_line(data = data[which(data$countrycode == "GBR" 
                              & data$EN.ATM.CO2E.PC >= 5 
                              & data$EN.ATM.CO2E.PC <= 15),], 
            aes(x = year, y = EN.ATM.CO2E.PC, col = "United Kingdom"), size = 1.5) +
  theme(legend.position = "none") +
  scale_y_continuous(labels = function(x) round(as.numeric(x), 0)) 

rm(codes, countries, indicators, missing, nmiss, scandis, HIC, UMC, OECD, Commonwealth, CDIAC, data_all)



## ## ## ## ## ## ## ## ## ##
# SPECIFICATION IN PAPER ####
## ## ## ## ## ## ## ## ## ##

load("Data/data_OECD_HIC.RData")
whoder()

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
capture.output(synth.spec, file = "Results/Specification.txt")



## ## ## ## ## ## ## ## ## ##
# SYNTH                  ####
## ## ## ## ## ## ## ## ## ##

# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)


# .. Pre-treatment predictor values ####
synth.tables$tab.pred


# .. Weights for the predictor variables ####
synth.tables$tab.v


# .. Weights for countries in the donor pool ####
synth.tables$tab.w


# .. Export results ####
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Results Specification.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
pdf("Figures/Donor weights.pdf", 
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



## ## ## ## ## ## ## ## ## ##
# GENERATE RESULTS       ####
## ## ## ## ## ## ## ## ## ##

# .. Outcomes ####
names(dataprep.out)

# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot

# Outcome variable in donor pool
Y0plot.UK <- dataprep.out$Y0plot

# Weights applied to each country in the donor pool
w.UK <- synth.out$solution.w

# Outcome variable in synthetic unit
synth <- Y0plot.UK %*% w.UK

# Gaps between outcomes in treated and synthetic control
gaps <- Y1plot.UK - synth
colnames(gaps) <- "GBR"


# .. Balance tests ####
# Define pre-treatment period in gaps
gap.start <- 1
gap.end.pre <- which(rownames(gaps) == "2001")

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- mean(gaps[gap.start:gap.end.pre, ]^2)
pre.MSE
# 0.0001418258

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
capture.output(results, file = "Results/Balance Specification.txt")


# .. Emissions trajectories figure ####
pdf("Figures/Emissions paths in treated and synth.pdf",
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
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("royalblue4", "royalblue1"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ##
# PLACEBO LOOPS          ####
## ## ## ## ## ## ## ## ## ##

placebos <- control.units

placebos <- placebos[which(placebos != 27)] # CHL
# HIC and OECD: SVD fails

store.gaps <- matrix(NA, length(years), length(placebos))
colnames(store.gaps) <- whodat(placebos)
store.gaps

store.obs <- matrix(NA, length(years), length(placebos))
colnames(store.obs) <- whodat(placebos)

store.synth <- matrix(NA, length(years), length(placebos))
colnames(store.synth) <- whodat(placebos)

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


  # .. Running Synth ####
  synth.out <- synth(data.prep.obj = dataprep.out)


  # .. Store emissions path in treated unit and its synthetic counterpart ####
  store.obs[,i] <- dataprep.out$Y1plot
  store.synth[,i] <- dataprep.out$Y0plot %*% synth.out$solution.w


  # .. Calculating annual gaps between the treated and its synthetic counterpart ####
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.obs, file = "Results/Placebo loops/store.obs.Rdata")
save(store.synth, file = "Results/Placebo loops/store.synth.Rdata")
save(store.gaps, file = "Results/Placebo loops/store.gaps.Rdata")


# .. Emissions trajectories figure for placebos ####
store.obs <- cbind(store.obs, Y1plot.UK)
store.synth <- cbind(store.synth, synth)
store.gaps <- cbind(store.gaps, gaps)

countries <- c(placebos, treated.unit)
c.labels <- whatname(countries)

for (c in 1:length(countries)){
  pdf(paste0("Figures/Emissions paths in placebo and synth_", c.labels[c], ".pdf"), 
    height = 4.5, width = 6)
  plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
  u <- par("usr") # The coordinates of the plot area
  rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
  grid (NULL, NULL, lty = 1, col = "seashell")
  par(new = TRUE, mgp = c(2, 1, 0))
  plot(years, store.obs[,c], 
       type = "l", col = "#014421", lwd = 2,
       xlim = range(years), 
       ylim = range(store.obs[,c], store.synth[,c]), 
       las = 1, cex.axis = 0.8, tck = -0.05,
       xlab = "Year",
       ylab = expression(paste("CO"[2], " emissions per capita")),
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
  dev.off()
}


# .. Gaps figure ####
placebo.results <- store.gaps

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 5 times higher MSPE than UK
colnames(placebo.results[, MSE > 5*UK.MSE])
# Exclude ABW, ARE, ARG, AUS, AUT, BEL, BHR, BHS, BMU, BRB, BRN, CAN
# CHE, CYM, CYP, DEU, ESP, FIN, FRA, FRO, GRL, HKG, HUN, IRL
# ISL, ISR, ITA, JPN, KNA, KOR, LUX, MAC, MLT, NCL, NZL, OMN
# PAN, PLW, POL, PRT, PYF, QAT, SAU, SGP, SYC, TTO, TUR, URY
# USA, VGB
placebo.results_5 <- placebo.results[, MSE < 5*UK.MSE]
placebo.results_10 <- placebo.results[, MSE < 10*UK.MSE]

# Plot
pdf("Figures/Gaps in emissions_placebo_all.pdf", 
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
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()

# Plot excluding placebos with MSPE > 5
pdf("Figures/Gaps in emissions_placebo_MSPE5.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results_5[, which(colnames(placebo.results_5) == "GBR")],
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
for (i in 1:ncol(placebo.results_5)){
  lines(years, placebo.results_5[, i], col = "gray") 
}
lines(years, placebo.results_5[, which(colnames(placebo.results_5) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()

# Plot excluding placebos with MSPE > 10
pdf("Figures/Gaps in emissions_placebo_MSPE10.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results_10[, which(colnames(placebo.results_10) == "GBR")],
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
for (i in 1:ncol(placebo.results_10)){
  lines(years, placebo.results_10[, i], col = "gray") 
}
lines(years, placebo.results_10[, which(colnames(placebo.results_10) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()

# How many control states remain?
colnames(placebo.results_5)
# 3 control states


# .. MSPE analysis ####
placebo.results <- store.gaps

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
# For the UK, the post-treatment gap is 1130 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/54 = 0.03703704

# Plot
pdf("Figures/MSPE Ratio.pdf", 
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
text(a[,1], y = -200, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.5,
     col = lab.cols)
axis(side = 2, at = axTicks(2), 
     cex.axis = 0.7, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2,
     labels = formatC(axTicks(2), format = "d", big.mark = ','))
dev.off()



## ## ## ## ## ## ## ## ## ##
# LEAVE-ONE-OUT CHECK    ####
## ## ## ## ## ## ## ## ## ##

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


  # .. Running Synth ####
  synth.out <- synth(data.prep.obj = dataprep.out)


  # .. Calculating annual gaps between the UK and its synthetic counterpart ####
  store[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store, file = "Results/Leave-one-out/store.Rdata")


# .. Leave-one-out figure ####
leaveoneout.results <- store
rownames(leaveoneout.results) <- years
leaveoneout.results

# Plot
pdf("Figures/Gaps in emissions_leave one out.pdf", 
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
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ## ##
# RE-ASSIGN TREATMENT YEAR  ####
## ## ## ## ## ## ## ## ## ## ##

whatname(control.units)
placebo.years <- c(seq(1991, 2001))

store.synth <- matrix(NA, length(1990:2005), length(placebo.years))
colnames(store.synth) <- paste("Tx_", c(seq(1991, 2001)), sep = "")
store.synth


# .. Treatment in 2001 ####
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

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,11] <- dataprep.out$Y0plot %*% synth.out$solution.w


# .. Treatment in 2000 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:2000,
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
           time.optimize.ssr = 1990:2000,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,10] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 2000.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
     las = 1, cex.axis = 0.8, tck = -0.05,
     xlab = "Year",
     ylab = expression(paste("CO"[2], " emissions per capita")),
     main = "Observed and Synthetic Counterfactual Emissions",
     frame.plot = FALSE, axes = F)
mtext("Re-assigning treatment to placebo year 2000", side = 3, line = 0.4, font = 3)
axis(side = 1, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(0, 0.2, 0))
axis(side = 2, cex.axis = 0.8, lwd = 0, lwd.ticks = 1,
     tck = -0.01, mgp = c(3, 0.5, 0), las = 2)
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,10], col = "coral", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 2000, lty = 2, col = "coral")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1999 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1999,
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
             list("EN.ATM.CO2E.PC", 1999, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1999,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,9] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1999.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,9], col = "cornflowerblue", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1999, lty = 2, col = "cornflowerblue")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1998 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1998,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean"),
             list("EN.ATM.CO2E.PC", 1995, "mean"),
             list("EN.ATM.CO2E.PC", 1996, "mean"),
             list("EN.ATM.CO2E.PC", 1997, "mean"),
             list("EN.ATM.CO2E.PC", 1998, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1998,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,8] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1998.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,8], col = "chocolate", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1998, lty = 2, col = "chocolate")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1997 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1997,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean"),
             list("EN.ATM.CO2E.PC", 1995, "mean"),
             list("EN.ATM.CO2E.PC", 1996, "mean"),
             list("EN.ATM.CO2E.PC", 1997, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1997,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,7] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1997.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,7], col = "darkorchid", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1997, lty = 2, col = "darkorchid")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1996 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1996,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean"),
             list("EN.ATM.CO2E.PC", 1995, "mean"),
             list("EN.ATM.CO2E.PC", 1996, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1996,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,6] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1996.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,6], col = "darkgoldenrod1", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1996, lty = 2, col = "darkgoldenrod1")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1995 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1995,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean"),
             list("EN.ATM.CO2E.PC", 1995, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1995,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,5] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1995.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,5], col = "mediumseagreen", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1995, lty = 2, col = "mediumseagreen")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1994 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1994,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean"),
             list("EN.ATM.CO2E.PC", 1994, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1994,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,4] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1994.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,4], col = "deeppink", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1994, lty = 2, col = "deeppink")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1993 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1993,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean"),
             list("EN.ATM.CO2E.PC", 1993, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1993,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,3] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1993.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,3], col = "darkviolet", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1993, lty = 2, col = "darkviolet")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1992 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1992,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1992,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,2] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1992.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,2], col = "darkturquoise", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1992, lty = 2, col = "darkturquoise")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()


# .. Treatment in 1991 ####
dataprep.out <-
  dataprep(foo = data,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = 1990:1992,
           special.predictors = list(
             list("EN.ATM.CO2E.PC", 1990, "mean"),
             list("EN.ATM.CO2E.PC", 1991, "mean"),
             list("EN.ATM.CO2E.PC", 1992, "mean")),
           dependent = "EN.ATM.CO2E.PC",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = 1990:1992,
           time.plot = 1990:2005)

synth.out <- synth(data.prep.obj = dataprep.out)

store.synth[,1] <- dataprep.out$Y0plot %*% synth.out$solution.w

# Plot
pdf("Figures/Emissions paths in treated and synth_placebo year 1991.pdf",
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, dataprep.out$Y1plot,
     type = "l", col = "#872341", lwd = 2,
     xlim = range(years), 
     ylim = c(8, 11),
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
lines(years, store.synth[,11], col = "#BE3144", lty = 2, lwd = 2)
lines(years, store.synth[,1], col = "palevioletred4", lty = 2)
abline(v = 2001, lty = 2)
abline(v = 1991, lty = 2, col = "palevioletred4")
legend(1990, 8.75, c("United Kingdom", "Synthetic UK"),
       lty = c(1, 2), lwd = c(2, 2), col = c("#872341", "#BE3144"),
       cex = 0.8, box.col = "seashell", bg = "seashell")
arrows(1999, 8.5, 2000.9, 8.5, length = 0.1, code = 2)
text(1997.5, 8.5, "CCP enacted", cex = 0.8)
dev.off()



## ## ## ## ## ## ## ## ## ## ##
# TRAJECTORY BALANCING      ####
## ## ## ## ## ## ## ## ## ## ##

# .. Reshape data ####
data <- data %>%
  mutate(treat = ifelse((countrycode == "GBR" & year > 2001), 1, 0))

data <- data %>%
  filter(!is.na(EN.ATM.CO2E.PC)) %>%
  filter(year >= 1990 & year <= 2005)


# .. Mean balancing with intercept shift ####
mbal.out <- tjbal(data = data, Y = "EN.ATM.CO2E.PC", D = "treat", 
                  X.avg.time = list(c(1990:2000)),
                  index = c("countrycode", "year"), 
                  kernel = FALSE, demean = TRUE)
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
plot(mbal.out, type = "ct",
     ylim = c(8, 11))

# Plot gaps in outcomes between the UK and the synthetic control
plot(mbal.out, type = "gap",
     ylim = c(-1, 1))

# Plot covariance balance
plot(mbal.out, type = "balance")

# Plot weights
plot(mbal.out, type = "weights")


# Weights for countries in the donor pool
mbal.out$names.co
weights.mbal <- mbal.out$weights.co
capture.output(round(weights.mbal, 3), file = "Results/Results Specification_Mbal.txt")

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

# Double check ID units correspond to correct countries
test <- gather(Y.co.mbal, year, EN.ATM.CO2E.PC, EN.ATM.CO2E.PC1990:EN.ATM.CO2E.PC2005) %>%
  mutate(year = str_sub(year, start = -4)) %>%
  mutate(year = as.integer(year))
test <- left_join(test, data %>% select(countrycode, year, EN.ATM.CO2E.PC),
                  by = c("countrycode", "year"))
sum(test$EN.ATM.CO2E.PC.x == test$EN.ATM.CO2E.PC.y) == nrow(test)
# TRUE
data %>% filter(countrycode != "GBR") %>% nrow == nrow(test)
# TRUE
rm(test)

# Outcome variable in synthetic unit
Y.co.mbal$countrycode <- NULL
Y.co.mbal <- t(as.matrix(Y.co.mbal))
Y0plot.UK == Y.co.mbal

synth.mbal <- mbal.out$Y.bar[, "Y.ct.bar"]

# Recreating synth and gaps from weights
Y.co.mbal %*% weights.mbal == synth.mbal
# When demeaning, synth cannot be recreated from weights

# (Y.co.mbal - Y.pre.mean.mat ) %*% weights.mbal
# Y.co.mbal - Y.pre.mean.mat == Y.dm.mbal
# round(apply(Y.dm.mbal.wide * weights.mbal, 2, sum), 10) == 
#   round(Y.dm.mbal %*% weights.mbal, 10)
# Y.dm.mbal.wide <- mbal.out$data.wide %>%
#   filter(treat == 0) %>%
#   select(contains(".dm"))
# Y.pre.mean.wide <- mbal.out$data.wide %>%
#   filter(treat == 0) %>%
#   select(num_range("EN.ATM.CO2E.PC", 1990:2001))
# Y.pre.mean <- apply(Y.pre.mean.wide, 1, mean)
# Y.pre.mean.mat <- matrix(rep(Y.pre.mean, 16),
#                          nrow = 16, byrow = T)
# Y.pre.mean.py <- apply(Y.pre.mean.wide, 2, mean)

# Gaps between outcomes in treated and synthetic control
Y.tr.mbal <- t(as.matrix(Y.tr.mbal))
Y1plot.UK == Y.tr.mbal

gaps.mbal <- mbal.out$att


# .. Placebo loops ####
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
  
  mbal.out <- tjbal(data = data.placebo, Y = "EN.ATM.CO2E.PC", D = "treat", 
                    X.avg.time = list(c(1990:2000)),
                    index = c("countrycode","year"), 
                    kernel = FALSE, demean = TRUE)
  
  # Weights for countries in the donor pool
  weights.mbal <- mbal.out$weights.co

  # Outcome variable in synthetic unit
  synth.mbal <- mbal.out$Y.bar[, "Y.ct.bar"]
  
  # Gaps between outcomes in treated and synthetic control
  store.gaps[,i] <- mbal.out$att
}

store.gaps <- cbind(store.gaps, gaps.mbal)
colnames(store.gaps)[ncol(store.gaps)] <- "GBR"
rownames(store.gaps) <- years


# .. Placebo figure ####
placebo.results <- store.gaps

# Define pre-treatment period in gaps
gap.start <- 1
gap.end <- nrow(placebo.results)
gap.end.pre <- which(rownames(placebo.results) == "2001")

# Mean Square Prediction Error Pre-Treatment
MSE <- apply(placebo.results[gap.start:gap.end.pre, ]^2, 2, mean)
UK.MSE <- as.numeric(MSE["GBR"])

# Exclude countries with 5 times higher MSPE than UK
colnames(placebo.results[, MSE > 5*UK.MSE])
# Exclude ABW, ARE, AUS, AUT, BEL, BHR, BHS, BMU, BRB, BRN, CAN, CHE, CHL
# CYM, CYP, DEU, ESP, FIN, FRA, FRO, GRL, HKG, HUN, IRL, ISL, ISR
# ITA, JPN, KOR, LUX, MLT, NCL, NZL, OMN, PLW, POL, PRT, QAT, SAU
# SGP, SYC, TTO, TUR, URY, USA, VGB
placebo.results_5 <- placebo.results[, MSE < 5*UK.MSE]
placebo.results_10 <- placebo.results[, MSE < 10*UK.MSE]

# Plot
pdf("Figures/Gaps in emissions_placebo_all_Mbal.pdf", 
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
for (i in 1:ncol(placebo.results)){
  lines(years, placebo.results[, i], col = "gray") 
}
lines(years, placebo.results[, which(colnames(placebo.results) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()

# Plot excluding placebos with MSPE > 5
pdf("Figures/Gaps in emissions_placebo_MSPE5_Mbal.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results_5[, which(colnames(placebo.results_5) == "GBR")],
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
for (i in 1:ncol(placebo.results_5)){
  lines(years, placebo.results_5[, i], col = "gray") 
}
lines(years, placebo.results_5[, which(colnames(placebo.results_5) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()

# Plot excluding placebos with MSPE > 10
pdf("Figures/Gaps in emissions_placebo_MSPE10_Mbal.pdf", 
    height = 4.5, width = 6)
plot(0, 0, type = "n", ann = FALSE, axes = FALSE)
u <- par("usr") # The coordinates of the plot area
rect(u[1], u[3], u[2], u[4], col = "grey90", border = NA)
grid (NULL, NULL, lty = 1, col = "seashell")
par(new = TRUE, mgp = c(2, 1, 0))
plot(years, placebo.results_10[, which(colnames(placebo.results_10) == "GBR")],
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
for (i in 1:ncol(placebo.results_10)){
  lines(years, placebo.results_10[, i], col = "gray") 
}
lines(years, placebo.results_10[, which(colnames(placebo.results_10) == "GBR")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()

# How many control states remain?
colnames(placebo.results_5)
# 9 control states


# .. MSPE analysis ####
placebo.results <- store.gaps

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
# For the UK, the post-treatment gap is 510 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 6/55 = 0.1090909

# Plot
pdf("Figures/MSPE Ratio_Mbal.pdf", 
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
text(a[,1], y = -200, 
     labels = labs, xpd = TRUE, srt = 60, adj = 1, cex = 0.5,
     col = lab.cols)
axis(side = 2, at = axTicks(2), 
     cex.axis = 0.7, lwd.ticks = 1, tck = -0.01, 
     mgp = c(3, 0.5, 0), las = 2,
     labels = formatC(axTicks(2), format = "g", big.mark = ','))
dev.off()


# .. Leave-one-out check ####
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
  
  mbal.out <- tjbal(data = data.LOO, Y = "EN.ATM.CO2E.PC", D = "treat", 
                    X.avg.time = list(c(1990:2000)),
                    index = c("countrycode","year"), 
                    kernel = FALSE, demean = TRUE)
  
  # Weights for countries in the donor pool
  weights.mbal <- mbal.out$weights.co
  
  # Outcome variable in synthetic unit
  synth.mbal <- mbal.out$Y.bar[, "Y.ct.bar"]
  
  # Gaps between outcomes in treated and synthetic control
  store.gaps[,i] <- mbal.out$att
}

store.gaps <- cbind(store.gaps, gaps.mbal)
colnames(store.gaps)[ncol(store.gaps)] <- "No_Dropped"


# .. Leave-one-out figure ####
leaveoneout.results <- store.gaps
rownames(leaveoneout.results) <- mbal.out$Ttot
leaveoneout.results

# Plot
pdf("Figures/Gaps in emissions_leave one out_Mbal.pdf", 
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
for (i in 1:ncol(leaveoneout.results)){
  lines(years, leaveoneout.results[, i], col = "thistle") 
}
lines(years, leaveoneout.results[, which(colnames(leaveoneout.results) == "No_Dropped")],
      type = "l", col = "darkorchid", lwd = 2)
arrows(1999.5, -0.92, 2000.9, -0.92, length = 0.1, code = 2)
text(1998, -0.9, "CCP enacted", cex = 0.8)
dev.off()