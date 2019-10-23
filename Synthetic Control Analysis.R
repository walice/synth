# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ##
# INDEX                  ####
## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# .. Import emissions for Germany from CDIAC
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
# .. Convert emissions per capita
# Placebo Countries
# .. Running Synth
# .. Outcomes
# .. Store emissions path in treated unit and its synthetic counterpart
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Emissions trajectories figure
# .. Gaps figure
# .. Ratio of post-treatment MSPE to pre-treatment MSPE
# Leave-One-Out Check    
# .. Running Synth 
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Leave-one-out figure
# Placebo Years
# .. Running Synth
# .. Store emissions paths
# .. Figure
# Carbon Leakage
# .. Calculate emissions from transport sectors
# .. Run on mobile emissions
# .. Generate results
# .. Emissions trajectories figure
# .. Convert emissions per capita
# .. Run on immobile emissions
# .. Generate results
# .. Emissions trajectories figure
# .. Convert emissions per capita
# Trajectory Balancing
# .. Reshape data
# .. Mean balancing with intercept shift
# .. Placebo countries
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
library(ggrepel)
library(kableExtra)
library(Matching)
library(paletteer)
library(plyr)
library(reshape2)
library(scales)
library(stargazer)
library(Synth)
library(tidyverse)
#devtools::install_github('xuyiqing/tjbal')
library(tjbal)
library(WDI)
library(xlsx)
set.seed(1509)

theme_set(theme_classic())
theme_update(axis.line = element_line(color = "grey"))



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
                "SP.POP.TOTL", # Population, total
                "EN.CO2.TRAN.ZS" # CO2 emissions from transport (% of total fuel combustion)
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
attr(data$rescaled2000, "label") <- NULL


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
ggplot(data %>% 
         filter(countrycode != "GBR"), 
       aes(x = year, y = EN.ATM.CO2E.PC, col = country)) + 
  geom_line() +
  geom_line(data = data 
            %>% filter(countrycode == "GBR"), 
            aes(x = year, y = EN.ATM.CO2E.PC, col = "United Kingdom"), size = 1.5) +
  labs(title = "Emissions trends in the United Kingdom and donor pool",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  theme(legend.position = "none")

# Emissions relative to 1990 in sample
ggplot(data %>% 
         filter(countrycode != "GBR"), 
       aes(x = year, y = rescaled1990, col = country)) + 
  geom_line() +
  geom_line(data = data %>% 
              filter(countrycode == "GBR"), 
            aes(x = year, y = rescaled1990, col = "United Kingdom"), size = 1.5) +
  labs(title = "Emissions trends in the United Kingdom and donor pool",
       x = "Year",
       y = expression(paste("CO"[2], " emissions against 1990 baseline"))) +
  theme(legend.position = "none")

# Emissions in effective sample
effective.sample <- c("BEL", "CHE", "BHS", "PLW", "AUT", "POL", "NCL", "FRO", "SAU", "PYF", "LUX", "URY")
g <- ggplot(data %>% 
              filter(countrycode %in% effective.sample) %>%
              filter(year >= 1990 & year <= 2010), 
            aes(x = year, y = EN.ATM.CO2E.PC, col = fct_reorder2(country, year, EN.ATM.CO2E.PC))) + 
  geom_line() +
  geom_line(data = data %>% 
              filter(countrycode == "GBR") %>%
              filter(year >= 1990 & year <= 2010), 
            aes(x = year, y = EN.ATM.CO2E.PC), 
            col = "black", linetype = 2) +
  scale_color_paletteer_d(package = "rcartocolor",
                          palette = "Bold") +
  labs(title = "Emissions trends in the United Kingdom and effective sample",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_label_repel(data = data %>% 
                     filter(countrycode %in% effective.sample) %>%
                     filter(year == 2010),
                   aes(label = country)) +
  geom_label_repel(data = data %>% 
                     filter(countrycode == "GBR") %>%
                     filter(year == 1990),
                   label = "UK", col = "black") +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/CO2 emissions in effective sample.pdf",
       height = 5, width = 6, units = "in")

rm(codes, countries, indicators, missing, nmiss, 
   scandis, HIC, UMC, OECD, Commonwealth, CDIAC, 
   data_all, effective.sample, g)



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
kable(donor.weights %>%
        arrange(desc(Donor.weight)), 
      format = "latex",
      digits = 4)
bal <- cbind(synth.tables$tab.pred, synth.tables$tab.v)
row.names(bal) <- choose.time.predictors
kable(bal,
      format = "latex")

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
ggsave(g, file = "Figures/Donor weights.pdf",
       width = 6, height = 5, units = "in")



## ## ## ## ## ## ## ## ## ##
# GENERATE RESULTS       ####
## ## ## ## ## ## ## ## ## ##

# .. Outcomes ####
names(dataprep.out)

# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot
colnames(Y1plot.UK) <- "GBR"

# Outcome variable in donor pool
Y0plot.UK <- dataprep.out$Y0plot

# Weights applied to each country in the donor pool
w.UK <- synth.out$solution.w

# Outcome variable in synthetic unit
synth <- Y0plot.UK %*% w.UK
colnames(synth) <- "GBR"

# Gaps between outcomes in treated and synthetic control
gaps <- Y1plot.UK - synth


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
kable(cbind(p.val.t, p.val.KS, t(qqstats)), format = "latex")

# Balance figure
bal <- synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred
bal <- data.frame(bal) %>%
  mutate(Year = seq(1990, 2001),
         Weighted = Treated - Synthetic,
         Unweighted = Treated - Sample.Mean) %>%
  select(Year, Weighted, Unweighted) %>%
  melt(id.vars = "Year")

g <- ggplot(bal, aes(x = value, y = Year)) +
  geom_point() +
  facet_grid(rows = vars(variable)) +
  theme_minimal() +
  theme(strip.background = element_rect(fill = "grey", color = "grey")) +
  labs(title = "Balance in weighted synthetic control and in unweighted sample",
       x = "Difference in means with pre-treatment values in the UK") +
  scale_y_continuous(breaks = seq(1990, 2001))
ggsave(g,
       file = "Figures/Balance in weighted and unweighted samples.pdf",
       height = 5, width = 6, units = "in")

# Difference in means for weighted and unweighted
t.test(synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Treated"],
       synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Synthetic"])$p.value
# 0.9954754
t.test(synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Treated"],
       synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Sample Mean"])$p.value
# 1.059105e-06


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
            col = "black",
            fontface = "bold") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15))
ggsave(g,
       file = "Figures/Emissions paths in treated and synth.pdf",
       height = 4.5, width = 6, units = "in")


# .. Convert emissions per capita ####
results <- left_join(data.frame(synth) %>%
                       select(synth.PC = GBR) %>%
                       mutate(year = years),
                     data %>%
                       filter(countrycode == "GBR") %>%
                       filter(year >= 1990 & year <= 2005) %>%
                       select(year, 
                              population = SP.POP.TOTL, 
                              UK.t = EN.ATM.CO2E.KT,
                              UK.PC = EN.ATM.CO2E.PC) %>%
                       mutate(UK.t = UK.t * 10^3),
                     by = c("year")) %>%
  mutate(synth.t = synth.PC * population,
         synth.Mt = synth.t / 10^6,
         UK.Mt = UK.t / 10^6) %>%
  left_join(data.frame(gaps) %>%
              select(gaps.PC = GBR) %>%
              mutate(year = years),
            by = c("year")) %>%
  mutate(gaps.t = gaps.PC * population,
         gaps.Mt = gaps.t / 10^6) %>%
  select(year, population,
         UK.PC, synth.PC, gaps.PC,
         UK.Mt, synth.Mt, gaps.Mt) %>%
  mutate(gaps.pct = (UK.Mt - synth.Mt) / synth.Mt)

results %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.Mt), sum)

results %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.Mt), mean)

results %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.PC), mean)

results %>% filter(year == 2005) %>% select(gaps.pct)

results %>% filter(year == 2002) %>% select(gaps.Mt)



## ## ## ## ## ## ## ## ## ##
# PLACEBO COUNTRIES      ####
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

# for (i in 1:length(placebos)){
#   dataprep.out <-
#     dataprep(foo = data,
#              predictors = NULL,
#              predictors.op = NULL,
#              time.predictors.prior = choose.time.predictors,
#              special.predictors = list(
#                list("EN.ATM.CO2E.PC", 1990, "mean"),
#                list("EN.ATM.CO2E.PC", 1991, "mean"),
#                list("EN.ATM.CO2E.PC", 1992, "mean"),
#                list("EN.ATM.CO2E.PC", 1993, "mean"),
#                list("EN.ATM.CO2E.PC", 1994, "mean"),
#                list("EN.ATM.CO2E.PC", 1995, "mean"),
#                list("EN.ATM.CO2E.PC", 1996, "mean"),
#                list("EN.ATM.CO2E.PC", 1997, "mean"),
#                list("EN.ATM.CO2E.PC", 1998, "mean"),
#                list("EN.ATM.CO2E.PC", 1999, "mean"),
#                list("EN.ATM.CO2E.PC", 2000, "mean"),
#                list("EN.ATM.CO2E.PC", 2001, "mean")),
#              dependent = "EN.ATM.CO2E.PC",
#              unit.variable = "countryid",
#              unit.names.variable = "country",
#              time.variable = "year",
#              treatment.identifier = placebos[i],
#              controls.identifier = placebos[-i],
#              time.optimize.ssr = choose.time.predictors,
#              time.plot = 1990:2005)
# 
# 
#   # .. Running Synth ####
#   synth.out <- synth(data.prep.obj = dataprep.out)
# 
# 
#   # .. Store emissions path in treated unit and its synthetic counterpart ####
#   store.obs[,i] <- dataprep.out$Y1plot
#   store.synth[,i] <- dataprep.out$Y0plot %*% synth.out$solution.w
# 
# 
#   # .. Calculating annual gaps between the treated and its synthetic counterpart ####
#   store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
# }
#
# save(store.obs, file = "Results/Placebo countries/store.obs.Rdata")
# save(store.synth, file = "Results/Placebo countries/store.synth.Rdata")
# save(store.gaps, file = "Results/Placebo countries/store.gaps.Rdata")

load("Results/Placebo countries/store.obs.Rdata")
load("Results/Placebo countries/store.synth.Rdata")
load("Results/Placebo countries/store.gaps.Rdata")


# .. Emissions trajectories figure for placebos ####
store.obs <- data.frame(Year = years, 
                        store.obs, 
                        Y1plot.UK) %>%
  melt(id.vars = c("Year"), value.name = "Observed", variable = "Country")
store.synth <- data.frame(Year = years, 
                          store.synth, 
                          synth) %>%
  melt(id.vars = c("Year"), value.name = "Synthetic", variable = "Country")

plot.emissions <- full_join(store.obs,
                            store.synth,
                            by = c("Year", "Country"))

countries <- c(placebos, treated.unit)
countries <- whodat(countries)

for (c in 1:length(countries)){
  g <- ggplot(plot.emissions %>%
                filter(Country == countries[c])) +
    geom_line(aes(x = Year, y = Observed),
              col = "#014421", lwd = 1, lty = 1) +
    geom_line(aes(x = Year, y = Synthetic),
              col = "#11904E", lwd = 1, lty = 2) +
    ylim(range(plot.emissions[plot.emissions$Country == countries[c], "Observed"], 
               plot.emissions[plot.emissions$Country == countries[c], "Synthetic"])) +
    labs(title = "Observed and Synthetic Counterfactual Emissions",
         subtitle = whodis(countries[c]),
         x = "Year",
         y = expression(paste("CO"[2], " emissions per capita"))) +
    geom_vline(xintercept = 2001,
               lty = 2) +
    geom_segment(x = 1999, xend = 2001,
                 y = 8.5, yend = 8.5,
                 col = "black",
                 arrow = arrow(ends = "last", type = "closed",
                               length = unit(0.1, "inches")),
                 show.legend = F)
  ggsave(g,
         file = paste0("Figures/Emissions paths in treated and synth_", whodis(countries[c]), ".pdf"),
         height = 4.5, width = 6, units = "in")
}


# .. Gaps figure ####
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
# Exclude ABW, ARE, ARG, AUS, AUT, BEL, BHR, BHS, BMU, BRB, BRN, CAN, CHE, CYM, CYP, DEU, ESP,
# FIN, FRA, FRO, GRL, HKG, HUN, IRL, ISL, ISR, ITA, JPN, KNA, KOR, LUX, MAC, MLT, NCL,
# NZL, OMN, PAN, PLW, POL, PRT, PYF, QAT, SAU, SGP, SYC, TTO, TUR, URY, USA, VGB

MSPE5 <- colnames(placebo.results[, pre.MSE < 5*UK.pre.MSE])
MSPE10 <- colnames(placebo.results[, pre.MSE < 10*UK.pre.MSE])
MSPE20 <- colnames(placebo.results[, pre.MSE < 20*UK.pre.MSE])

plot.gaps <- placebo.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")
plot.gaps5 <- plot.gaps %>%
  filter(Country %in% MSPE5)
plot.gaps10 <- plot.gaps %>%
  filter(Country %in% MSPE10)
plot.gaps20 <- plot.gaps %>%
  filter(Country %in% MSPE20)

# Plot all
g <- ggplot(plot.gaps) +
  geom_line(data = plot.gaps %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps %>%
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
               y = -0.9, yend = -0.9,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.9,
            label = "CCP enacted",
            col = "black",
            fontface = "bold")
ggsave(g,
       file = "Figures/Gaps in emissions_placebo_all.pdf",
       height = 4.5, width = 6, units = "in")

# Plot excluding placebos with MSPE > 5
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
               y = -0.9, yend = -0.9,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.9,
            label = "CCP enacted",
            col = "black",
            fontface = "bold")
ggsave(g,
       file = "Figures/Gaps in emissions_placebo_MSPE5.pdf",
       height = 4.5, width = 6, units = "in")

# Plot excluding placebos with MSPE > 10
g <- ggplot(plot.gaps10) +
  geom_line(data = plot.gaps10 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps10 %>% 
                                    filter(Country != "GBR") %>% 
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps10 %>%
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
               y = -0.9, yend = -0.9,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.9,
            label = "CCP enacted",
            col = "black",
            fontface = "bold")
ggsave(g,
       file = "Figures/Gaps in emissions_placebo_MSPE10.pdf",
       height = 4.5, width = 6, units = "in")

# Plot excluding placebos with MSPE > 20
g <- ggplot(plot.gaps20) +
  geom_line(data = plot.gaps20 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps20 %>% 
                                    filter(Country != "GBR") %>% 
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps20 %>%
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
               y = -0.9, yend = -0.9,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.9,
            label = "CCP enacted",
            col = "black",
            fontface = "bold")
ggsave(g,
       file = "Figures/Gaps in emissions_placebo_MSPE20.pdf",
       height = 4.5, width = 6, units = "in")

# How many control states remain?
plot.gaps5 %>% distinct(Country)
# 4 control states


# .. Ratio of post-treatment MSPE to pre-treatment MSPE ####
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
# For the UK, the post-treatment gap is 1130 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/54 = 0.03703704

# Plot
placebo.results <- cbind(store.gaps, gaps)
MSE <- data.frame(country = names(pre.MSE),
                  pre.MSE = pre.MSE,
                  post.MSE = post.MSE,
                  ratio = post.MSE/pre.MSE,
                  TE2005 = placebo.results[gap.end,]) %>%
  mutate(col = ifelse(country == "GBR", "darkorchid", "grey"))

g <- ggplot(MSE,
            aes(x = reorder(country, ratio), y = log(ratio),
                fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("darkorchid", "grey35")) +
  guides(fill = F) +
  labs(title = "Log of ratio of post-treatment MSPE to pre-treatment MSPE",
       subtitle = "Two-sided test",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, size = 6, vjust = 0.5))
ggsave(g,
       file = "Figures/Log MSPE ratio_Two-sided.pdf",
       height = 5, width = 6, units = "in")

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
       file = "Figures/MSPE ratio_Two-sided.pdf",
       height = 5, width = 6, units = "in")

g <- ggplot(MSE %>%
              filter(TE2005 < 0),
            aes(x = reorder(country, ratio), y = ratio,
                fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("darkorchid", "grey35")) +
  scale_y_continuous(labels = comma) +
  guides(fill = F) +
  labs(title = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       subtitle = "One-sided test",
       x = "",
       y = "") +
  theme(axis.text.x = element_text(angle = 90, size = 8, vjust = 0.5))
ggsave(g,
       file = "Figures/MSPE ratio_One-sided.pdf",
       height = 5, width = 6, units = "in")

g <- ggplot(MSE %>%
              filter(TE2005 < 0),
            aes(x = ratio)) +
  geom_density(fill = "grey35",
               col = "grey35") +
  labs(title = "Empirical distribution of ratio",
       subtitle = "One-sided test",
       x = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       y = "Density") +
  geom_segment(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull - 30,
               xend = 1000,
               y = 0.001,
               yend = 0.003,
               col = "darkorchid",
               arrow = arrow(ends = "first", type = "closed",
                             length = unit(0.1, "inches")),
               arrow.fill = "darkorchid") +
  geom_text(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull - 30,
            y = 0.0035,
            label = "United Kingdom",
            col = "darkorchid",
            hjust = 1,
            fontface = "bold")
ggsave(g,
       file = "Figures/Empirical distribution_One-sided.pdf",
       height = 5, width = 6, units = "in")

g <- ggplot(MSE,
            aes(x = ratio)) +
  geom_density(fill = "grey35",
               col = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(title = "Empirical distribution of ratio",
       subtitle = "Two-sided test",
       x = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       y = "Density") +
  geom_segment(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull + 100,
               xend = 2500,
               y = 0.0005,
               yend = 0.0025,
               col = "darkorchid",
               arrow = arrow(ends = "first", type = "closed",
                             length = unit(0.1, "inches")),
               arrow.fill = "darkorchid") +
  geom_text(x = 2500,
            y = 0.0028,
            label = "United Kingdom",
            col = "darkorchid",
            hjust = 0.4,
            fontface = "bold")
ggsave(g,
       file = "Figures/Empirical distribution_Two-sided.pdf",
       height = 5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ##
# LEAVE-ONE-OUT CHECK    ####
## ## ## ## ## ## ## ## ## ##

leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.gaps) <- paste0("No_", leaveoneout.names)
store.gaps

nloops <- length(leaveoneout.controls)+1

# for (i in 1:nloops){
#   controls.identifier = leaveoneout.controls[-i]
#   print(controls.identifier)
# }
# 
# for (i in 1:nloops){
#   dataprep.out <-
#     dataprep(foo = data,
#              predictors = NULL,
#              predictors.op = NULL,
#              time.predictors.prior = choose.time.predictors,
#              special.predictors = list(
#                list("EN.ATM.CO2E.PC", 1990, "mean"),
#                list("EN.ATM.CO2E.PC", 1991, "mean"),
#                list("EN.ATM.CO2E.PC", 1992, "mean"),
#                list("EN.ATM.CO2E.PC", 1993, "mean"),
#                list("EN.ATM.CO2E.PC", 1994, "mean"),
#                list("EN.ATM.CO2E.PC", 1995, "mean"),
#                list("EN.ATM.CO2E.PC", 1996, "mean"),
#                list("EN.ATM.CO2E.PC", 1997, "mean"),
#                list("EN.ATM.CO2E.PC", 1998, "mean"),
#                list("EN.ATM.CO2E.PC", 1999, "mean"),
#                list("EN.ATM.CO2E.PC", 2000, "mean"),
#                list("EN.ATM.CO2E.PC", 2001, "mean")),
#              dependent = "EN.ATM.CO2E.PC",
#              unit.variable = "countryid",
#              unit.names.variable = "country",
#              time.variable = "year",
#              treatment.identifier = treated.unit,
#              controls.identifier = leaveoneout.controls[-i],
#              time.optimize.ssr = choose.time.predictors,
#              time.plot = 1990:2005)
# 
# 
#   # .. Running Synth ####
#   synth.out <- synth(data.prep.obj = dataprep.out)
# 
# 
#   # .. Calculating annual gaps between the UK and its synthetic counterpart ####
#   store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
# }
# 
# save(store.gaps, file = "Results/Leave-one-out/store.gaps.Rdata")
load("Results/Leave-one-out/store.gaps.Rdata")


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
               y = -0.9, yend = -0.9,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.9,
            label = "CCP enacted",
            col = "black",
            fontface = "bold")
ggsave(g,
       file = "Figures/Gaps in emissions_leave one out.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# PLACEBO YEARS             ####
## ## ## ## ## ## ## ## ## ## ##

whatname(control.units)
placebo.years <- c(seq(1991, 2001))

store.synth <- matrix(NA, length(1990:2005), length(placebo.years))
colnames(store.synth) <- paste("Tx_", c(seq(1991, 2001)), sep = "")
store.synth

pred2001 <- list(
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
  list("EN.ATM.CO2E.PC", 2001, "mean"))
pred2000 <- list(
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
  list("EN.ATM.CO2E.PC", 2000, "mean"))
pred1999 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"),
  list("EN.ATM.CO2E.PC", 1997, "mean"),
  list("EN.ATM.CO2E.PC", 1998, "mean"),
  list("EN.ATM.CO2E.PC", 1999, "mean"))
pred1998 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"),
  list("EN.ATM.CO2E.PC", 1997, "mean"),
  list("EN.ATM.CO2E.PC", 1998, "mean"))
pred1997 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"),
  list("EN.ATM.CO2E.PC", 1997, "mean"))
pred1996 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"))
pred1995 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"))
pred1994 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"))
pred1993 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"))
pred1992 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"))
pred1991 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"))

# for (y in 1:ncol(store.synth)){
#   placebo.year <- substr(colnames(store.synth)[y], 4, 7)
#   choose.time.predictors <- 1990:placebo.year
#   
#   dataprep.out <-
#     dataprep(foo = data,
#              predictors = NULL,
#              predictors.op = NULL,
#              time.predictors.prior = choose.time.predictors,
#              special.predictors = get(paste0("pred", placebo.year)),
#              dependent = "EN.ATM.CO2E.PC",
#              unit.variable = "countryid",
#              unit.names.variable = "country",
#              time.variable = "year",
#              treatment.identifier = treated.unit,
#              controls.identifier = c(control.units),
#              time.optimize.ssr = choose.time.predictors,
#              time.plot = 1990:2005)
#   
#   
#   # .. Running Synth ####
#   synth.out <- synth(data.prep.obj = dataprep.out)
#   
#   # .. Store emissions paths ####
#   store.synth[,y] <- dataprep.out$Y0plot %*% synth.out$solution.w
# }
#
# save(store.synth, file = "Results/Placebo years/store.synth.Rdata")
load("Results/Placebo years/store.synth.Rdata")

plot <- data.frame(years = years,
                   treated = dataprep.out$Y1plot,
                   store.synth) %>%
  rename(treated = X50)
plot <- plot %>%
  melt(id.vars = "years", value.name = "emissions")


# .. Figure ####
g <- ggplot() +
  geom_line(data = plot %>%
              filter(variable == "treated" | variable == "Tx_2001"),
            aes(x = years, y = emissions, 
                col = variable, linetype = variable),
            lwd = 1) +
  scale_color_manual(values = c("#872341", "#BE3144"),
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
            col = "black",
            fontface = "bold") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15))

for (y in 1:(ncol(store.synth)-1)){
  placebo.year <- substr(colnames(store.synth)[y], 4, 7)
  p <- g + geom_line(data = plot %>%
                       filter(variable == paste0("Tx_", placebo.year)),
                     aes(x = years, y = emissions),
                     col = "coral",
                     lty = "dotted") +
    geom_vline(xintercept = as.numeric(placebo.year),
               col = "coral",
               lty = "dotted") +
    labs(subtitle = paste0("Re???assigning treatment to placebo year ",
                           placebo.year))
  ggsave(p,
         file = paste0("Figures/Emissions paths in treated and synth_placebo year ",
                       placebo.year, ".pdf"),
         height = 4.5, width = 6, units = "in")
}



## ## ## ## ## ## ## ## ## ## ##
# CARBON LEAKAGE            ####
## ## ## ## ## ## ## ## ## ## ##

# .. Calculate emissions from transport sectors ####
data <- data %>%
  mutate(mobile = EN.ATM.CO2E.PC * EN.CO2.TRAN.ZS / 100,
         immobile = EN.ATM.CO2E.PC * (1 - EN.CO2.TRAN.ZS / 100))
missing <- data %>% 
  filter(year >= 1990 & year <= 2005) %>%
  filter(is.na(mobile) | is.na(immobile)) %>%
  distinct(countrycode) %>%
  as.matrix %>% t

leakage <- subset(data, !(countrycode %in% missing))

ggplot(leakage %>%
         filter(year >= 1990 & year <= 2015),
       aes(x = year, y = EN.CO2.TRAN.ZS, col = country)) +
  geom_line()

treated.unit <- leakage %>%
  filter(countrycode == "GBR") %>%
  distinct(countryid) %>%
  pull
control.units <- leakage %>%
  filter(countrycode != "GBR") %>%
  distinct(countryid) %>%
  pull %>% t
whatname(control.units)


# .. Run on mobile emissions ####
dataprep.out <-
  dataprep(foo = leakage,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("mobile", 1990, "mean"),
             list("mobile", 1991, "mean"),
             list("mobile", 1992, "mean"),
             list("mobile", 1993, "mean"),
             list("mobile", 1994, "mean"),
             list("mobile", 1995, "mean"),
             list("mobile", 1996, "mean"),
             list("mobile", 1997, "mean"),
             list("mobile", 1998, "mean"),
             list("mobile", 1999, "mean"),
             list("mobile", 2000, "mean"),
             list("mobile", 2001, "mean")),
           dependent = "mobile",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1990:2005)

# Running Synth
synth.out <- synth(data.prep.obj = dataprep.out)

# Export results
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results mobile emissions.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, round(Donor.weight, 5), round(Donor.weight, 4))),
            size = 2, nudge_x = 0, nudge_y = 0.01) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 6))
ggsave(g, file = "Figures/Supplementary Information/Donor weights_mobile.pdf",
       width = 6, height = 5, units = "in")


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot
colnames(Y1plot.UK) <- "GBR"

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w
colnames(synth) <- "GBR"

# Gaps between outcomes in treated and synthetic control
gaps <- Y1plot.UK - synth


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
  labs(title = "Observed and Synthetic Counterfactual Emissions",
       subtitle = "Mobile emissions",
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
            col = "black",
            fontface = "bold") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.115))
ggsave(g,
       file = "Figures/Supplementary Information/Emissions paths in treated and synth_mobile.pdf",
       height = 4.5, width = 6, units = "in")


# .. Convert emissions per capita ####
synth.t <- left_join(data.frame(synth) %>%
                       mutate(year = years),
                     leakage %>%
                       filter(countrycode == "GBR") %>%
                       filter(year >= 1990 & year <= 2005) %>%
                       select(year, SP.POP.TOTL),
                     by = c("year")) %>%
  mutate(synth.t = GBR * SP.POP.TOTL,
         synth.Mt = synth.t / 10^6)

gaps.t <- left_join(data.frame(gaps) %>%
                      mutate(year = years),
                    leakage %>%
                      filter(countrycode == "GBR") %>%
                      filter(year >= 1990 & year <= 2005) %>%
                      select(year, SP.POP.TOTL),
                    by = c("year")) %>%
  mutate(gaps.t = GBR * SP.POP.TOTL,
         gaps.Mt = gaps.t / 10^6)

gaps.t %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.Mt), sum)


# .. Run on immobile emissions ####
dataprep.out <-
  dataprep(foo = leakage,
           predictors = NULL,
           predictors.op = NULL,
           time.predictors.prior = choose.time.predictors,
           special.predictors = list(
             list("immobile", 1990, "mean"),
             list("immobile", 1991, "mean"),
             list("immobile", 1992, "mean"),
             list("immobile", 1993, "mean"),
             list("immobile", 1994, "mean"),
             list("immobile", 1995, "mean"),
             list("immobile", 1996, "mean"),
             list("immobile", 1997, "mean"),
             list("immobile", 1998, "mean"),
             list("immobile", 1999, "mean"),
             list("immobile", 2000, "mean"),
             list("immobile", 2001, "mean")),
           dependent = "immobile",
           unit.variable = "countryid",
           unit.names.variable = "country",
           time.variable = "year",
           treatment.identifier = treated.unit,
           controls.identifier = c(control.units),
           time.optimize.ssr = choose.time.predictors,
           time.plot = 1990:2005)

# Running Synth
synth.out <- synth(data.prep.obj = dataprep.out)

# Export results
results <- list(cbind(synth.tables$tab.pred, synth.tables$tab.v),
                synth.tables$tab.w)
capture.output(results, file = "Results/Supplementary Information/Results immobile emissions.txt")
donor.weights <- data.frame(synth.tab(dataprep.res = dataprep.out,
                                      synth.res = synth.out,
                                      round.digit = 10)$tab.w) %>%
  select(Donor.country = unit.names, Donor.weight = w.weights) %>%
  arrange(Donor.weight)

# Plot
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  theme_minimal() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, round(Donor.weight, 5), round(Donor.weight, 4))),
            size = 2, nudge_x = 0, nudge_y = 0.01) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 6))
ggsave(g, file = "Figures/Supplementary Information/Donor weights_immobile.pdf",
       width = 6, height = 5, units = "in")


# .. Generate results ####
# Pre- and post-intervention periods
years <- c(choose.time.predictors, seq(2002, 2005, 1))

# Outcome variable in treated unit
Y1plot.UK <- dataprep.out$Y1plot
colnames(Y1plot.UK) <- "GBR"

# Outcome variable in synthetic unit
synth <- dataprep.out$Y0plot %*% synth.out$solution.w
colnames(synth) <- "GBR"

# Gaps between outcomes in treated and synthetic control
gaps <- Y1plot.UK - synth


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
  labs(title = "Observed and Synthetic Counterfactual Emissions",
       subtitle = "Immobile emissions",
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
            col = "black",
            fontface = "bold") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.115))
ggsave(g,
       file = "Figures/Supplementary Information/Emissions paths in treated and synth_immobile.pdf",
       height = 4.5, width = 6, units = "in")


# .. Convert emissions per capita ####
synth.t <- left_join(data.frame(synth) %>%
                       mutate(year = years),
                     leakage %>%
                       filter(countrycode == "GBR") %>%
                       filter(year >= 1990 & year <= 2005) %>%
                       select(year, SP.POP.TOTL),
                     by = c("year")) %>%
  mutate(synth.t = GBR * SP.POP.TOTL,
         synth.Mt = synth.t / 10^6)

gaps.t <- left_join(data.frame(gaps) %>%
                      mutate(year = years),
                    leakage %>%
                      filter(countrycode == "GBR") %>%
                      filter(year >= 1990 & year <= 2005) %>%
                      select(year, SP.POP.TOTL),
                    by = c("year")) %>%
  mutate(gaps.t = GBR * SP.POP.TOTL,
         gaps.Mt = gaps.t / 10^6)

gaps.t %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.Mt), sum)






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


# .. Placebo countries ####
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