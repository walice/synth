# Synth
# Alice Lepissier

## ## ## ## ## ## ## ## ## ## ##
# INDEX                     ####
## ## ## ## ## ## ## ## ## ## ##
# Preamble
# Functions
# Data
# .. Import emissions for Germany from CDIAC
# .. Import population for Kuwait from UN World Population Prospects
# .. Import emissions for Liechtenstein from UNFCCC GHG Inventory
# .. Import indicators from WDI
# .. Create outcome variables
# .. Drop missing data and specific donors
# .. Figures of summary statistics
# .. Table of sample for SI
# Specification in Paper
# .. Optimize over 1990-2001, CO2 per capita, no covariates, OECD & high & upper middle income
# .. Running Synth 
# .. Export results
# .. Donor weights figure
# Generate Results    
# .. Outcomes
# .. Balance tests
# .. Emissions trajectories figure
# .. Convert emissions per capita
# Placebo Countries
# .. Running Synth
# .. Store emissions trajectories in treated unit and its synthetic counterpart
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Emissions trajectories figure
# .. Placebo countries figure
# .. Ratio of post-treatment MSPE to pre-treatment MSPE
# .. RMSPE figures
# Leave-One-Out Check
# .. Running Synth
# .. Store emissions trajectories for synthetic UK when leaving out one country
# .. Calculating annual gaps between the UK and its synthetic counterpart
# .. Leave-one-out figure
# .. More analysis when specific countries are dropped
# Placebo Years
# .. Running Synth
# .. Store emissions trajectories in synthetic control
# .. Placebo years figure



## ## ## ## ## ## ## ## ## ## ##
# PREAMBLE                  ####
## ## ## ## ## ## ## ## ## ## ##

setwd("~/Synth/") # Peregrine server
#setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/Synth/") # Alice laptop
#setwd("C:/Users/alepissier/Box Sync/LepissierMildenberger/Synth/") # Alice work
#setwd("~/Box Sync/LepissierMildenberger/Synth/") # Matto
library(ggpubr)
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
library(WDI)
library(xlsx)
set.seed(1509)

theme_set(theme_classic())
theme_update(axis.line = element_line(color = "grey"))



## ## ## ## ## ## ## ## ## ## ##
# FUNCTIONS                 ####
## ## ## ## ## ## ## ## ## ## ##

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



## ## ## ## ## ## ## ## ## ## ##
# DATA                      ####
## ## ## ## ## ## ## ## ## ## ##

codes <- read.xlsx2("Data/Codes_Masterlist.xlsx", sheetName = "Codes") %>%
  mutate_all(as.character)

policies <- read.xlsx2("Data/Countries and climate policies.xlsx", sheetName = "Countries") %>%
  mutate_all(as.character)

WB <- read.xlsx2("Data/OGHIST.xls", sheetName = "Country Analytical History",
                 startRow = 5) %>%
  mutate_all(as.character) %>%
  slice(-1:-6) %>%
  select(ISO = X.,
         Country = Bank.s.fiscal.year.,
         FY01)


# .. Import emissions for Germany from CDIAC ####
# Data on emissions for Germany prior to 1991 is missing from WDI.
# WDI uses CDIAC as the raw data source, so go to CDIAC to import emissions prior to 1991.

CDIAC <- read.csv("Data/nation.1751_2014.csv") %>%
  select(Nation, Year, Emissions = Total.CO2.emissions.from.fossil.fuels.and.cement.production..thousand.metric.tons.of.C.) %>%
  filter(Nation == "FORMER GERMAN DEMOCRATIC REPUBLIC" | Nation == "FEDERAL REPUBLIC OF GERMANY")

CDIAC <- spread(CDIAC, Nation, Emissions) %>%
  mutate(EN.ATM.CO2E.KT = (`FEDERAL REPUBLIC OF GERMANY` + `FORMER GERMAN DEMOCRATIC REPUBLIC`) * 44/12,
         country = "Germany", iso2c = "DE") %>%
  select(iso2c, country, year = Year, EN.ATM.CO2E.KT) %>%
  filter(year >= 1960)


# .. Import population for Kuwait from UN World Population Prospects ####
# Data on population for Kuwait from 1992-1994 is missing from WDI.
# WDI uses UN World Population Prospects as the raw data source, so go to UN WPP to import population from 1992-1994.

WPP <- read.xlsx2("Data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", 
                  sheetName = "ESTIMATES",
                  startRow = 17) %>%
  mutate_all(as.character)

WPP <- WPP %>%
  select(country = Region..subregion..country.or.area..,
         X1992:X1994) %>%
  filter(country == "Kuwait") %>%
  gather(year, SP.POP.TOTL, X1992:X1994) %>%
  mutate(year = as.integer(substr(year, 2, 5)),
         SP.POP.TOTL = as.numeric(SP.POP.TOTL) * 1000,
         iso2c = "KW")


# .. Import emissions for Liechtenstein from UNFCCC GHG Inventory ####
# Data on emissions for Liechtenstein prior to 2007 is missing from WDI and CDIAC.
# Go to UNFCCC GHG Inventory for raw data on emissions reported by Liechtenstein:
# available on p. 18 of https://www.llv.li/files/au/lie-2017-apr-nir.pdf,
# in the second row of the table (CO2 emissions excl. net CO2 from LULUCF)

LIE <- read.csv("Data/LIE.1990_2015.csv")


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
  mutate(EN.ATM.CO2E.PC = ifelse(country == "Germany" & year <= 1990, 
                                 EN.ATM.CO2E.KT * 1000 / SP.POP.TOTL, EN.ATM.CO2E.PC))

data[which(data$country == "Kuwait" & data$year >= 1992 & data$year <= 1994), "SP.POP.TOTL"] <- WPP$SP.POP.TOTL
data <- data %>%
  mutate(EN.ATM.CO2E.PC = ifelse(country == "Kuwait" & data$year >= 1992 & data$year <= 1994, 
                                 EN.ATM.CO2E.KT * 1000 / SP.POP.TOTL, EN.ATM.CO2E.PC))

data[which(data$country == "Liechtenstein" & data$year >= 1990 & data$year <= 2015), "EN.ATM.CO2E.KT"] <- LIE$CO2..kt.
data <- data %>%
  mutate(EN.ATM.CO2E.PC = ifelse(country == "Liechtenstein" & year >= 1990 & year <= 2015, 
                                 EN.ATM.CO2E.KT * 1000 / SP.POP.TOTL, EN.ATM.CO2E.PC))

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

# Emissions per capita demeaned from pre-treatment
data <- left_join(data, data %>%
                    filter(year >= 1990 & year < 2001) %>%
                    group_by(countryid) %>%
                    summarize(mean.preT = mean(EN.ATM.CO2E.PC, na.rm = T)) %>%
                    ungroup(),
                  by = "countryid") %>%
  mutate(EN.ATM.CO2E.PC.demean = EN.ATM.CO2E.PC - mean.preT)


# .. Drop missing data and specific donors ####
treated <- policies %>%
  filter(Treated.in.2001 == "Yes") %>%
  select(ISO.code) %>%
  filter(ISO.code != "GBR") %>%
  pull

data <- subset(data, !(countrycode %in% treated))
whoder()

range(data$year)
data <- data %>%
  filter(year >= 1980 & year <= 2005)

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
# 26 countries
# If we calculate missing countries solely on the basis of co2pc.missing > 10,
# then the same 26 countries are missing

data <- subset(data, !(countrycode %in% missing))
whoder()

missing <- data %>%
  filter(year >= 1990 & year < 2005) %>%
  filter(is.na(EN.ATM.CO2E.KT) | is.na(EN.ATM.CO2E.PC)) %>%
  distinct(countrycode) %>%
  pull
data <- subset(data, !(countrycode %in% missing))
whoder()

tinycountries <- data %>%
  filter(year == 2001) %>%
  filter(SP.POP.TOTL < 2.5*10^5) %>%
  distinct(countrycode) %>%
  pull
data <- subset(data, !(countrycode %in% tinycountries))

HIC <- codes %>%
  filter(WB_Income_Group_Code == "HIC") %>%
  distinct(ISO3166.3) %>%
  pull

HIC01 <- WB %>%
  filter(FY01 == "H") %>%
  distinct(ISO) %>%
  pull

UMC <- codes %>%
  filter(WB_Income_Group_Code == "UMC") %>%
  distinct(ISO3166.3) %>%
  pull

UMC01 <- WB %>%
  filter(FY01 == "UM") %>%
  distinct(ISO) %>%
  pull

OECD <- codes %>%
  filter(OECD == 1) %>%
  distinct(ISO3166.3) %>%
  pull

OECD01 <- policies %>%
  filter(OECD..2001. == "Yes") %>%
  distinct(ISO.code) %>%
  pull

Commonwealth <- codes %>%
  filter(Commonwealth == 1) %>%
  distinct(ISO3166.3) %>%
  pull

data_all <- data

data <- subset(data_all, countrycode %in% OECD01)
whoder()
save(data, file = "Data/data_OECD.Rdata")

data <- subset(data_all, countrycode %in% OECD01 | countrycode %in% HIC01)
whoder()
save(data, file = "Data/data_OECD_HIC.Rdata")

data <- subset(data_all, countrycode %in% OECD01 | countrycode %in% HIC01 | countrycode %in% UMC01)
whoder()
save(data, file = "Data/data_OECD_HIC_UMC.Rdata")


# .. Figures of summary statistics ####
# Figure showing the differences in levels and the need to rescale the outcome variable
a <- ggplot(data,
       aes(x = year, y = EN.ATM.CO2E.KT)) +
  geom_line(data = data %>%
              filter(countrycode != "GBR" & (year >= 1990 & year <= 2001)),
            aes(col = country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", data %>%
                                    filter(countrycode != "GBR") %>%
                                    distinct(country) %>% nrow)) +
  geom_line(data = data %>%
              filter(countrycode == "GBR" & (year >= 1990 & year <= 2001)),
            col = "black",
            lwd = 1) +
  geom_line(data = data %>%
              filter(countrycode == data[which.max(data$EN.ATM.CO2E.KT), "countrycode"] & (year >= 1990 & year <= 2001)),
            col = "hotpink",
            lwd = 1) +
  labs(x = "Year",
       y = expression(paste("CO"[2], " emissions (kilotons)"))) +
  geom_label_repel(data = data %>%
                     filter(countrycode == data[which.max(data$EN.ATM.CO2E.KT), "countrycode"]) %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "hotpink",
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "black",
                   size = 3,
                   nudge_y = 200000) +
  geom_label(aes(x = 1990, y = 6*10^6, 
                 label = paste0("Var %~~% 5.4", "%*%10", "^11")), 
             parse = TRUE,
             fontface = "italic",
             fill = "lemonchiffon",
             size = 2.5,
             hjust = 0)
data %>% filter(year >= 1990 & year <= 2001) %>% summarize(var(EN.ATM.CO2E.KT))
# 538267234910

b <- ggplot(data,
       aes(x = year, y = EN.ATM.CO2E.PC)) +
  geom_line(data = data %>%
              filter(countrycode != "GBR" & (year >= 1990 & year <= 2001)),
            aes(col = country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", data %>%
                                    filter(countrycode != "GBR") %>%
                                    distinct(country) %>% nrow)) +
  geom_line(data = data %>%
              filter(countrycode == "GBR" & (year >= 1990 & year <= 2001)),
            col = "black",
            lwd = 1) +
  geom_line(data = data %>%
              filter(countrycode == data[which.max(data$EN.ATM.CO2E.PC), "countrycode"] & (year >= 1990 & year <= 2001)),
            col = "hotpink",
            lwd = 1) +
  labs(x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_label_repel(data = data %>%
                     filter(countrycode == data[which.max(data$EN.ATM.CO2E.PC), "countrycode"]) %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "hotpink",
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "black",
                   size = 3,
                   nudge_y = 2) +
  geom_label(aes(x = 1990, y = 70, 
                 label = paste0("Var %~~% 8.6", "%*%10", "^1")), 
             parse = TRUE,
             fontface = "italic",
             fill = "lemonchiffon",
             size = 2.5,
             hjust = 0)
data %>% filter(year >= 1990 & year <= 2001) %>% summarize(var(EN.ATM.CO2E.PC))
# 86.35207

c <- ggplot(data,
            aes(x = year, y = rescaled1990)) +
  geom_line(data = data %>%
              filter(countrycode != "GBR" & (year >= 1990 & year <= 2001)),
            aes(col = country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", data %>%
                                    filter(countrycode != "GBR") %>%
                                    distinct(country) %>% nrow)) +
  geom_line(data = data %>%
              filter(countrycode == "GBR" & (year >= 1990 & year <= 2001)),
            col = "black",
            lwd = 1) +
  geom_line(data = data %>%
              filter(countrycode == data[which.max(data$rescaled1990), "countrycode"] & (year >= 1990 & year <= 2001)),
            col = "hotpink",
            lwd = 1) +
  labs(x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 1990"))) +
  geom_label_repel(data = data %>%
                     filter(countrycode == data[which.max(data$rescaled1990), "countrycode"]) %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "hotpink",
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "black",
                   size = 3,
                   nudge_y = 0.1) +
  geom_label(aes(x = 1990, y = 3.5, 
                 label = paste0("Var %~~% 1.2", "%*%10", "^-1")), 
             parse = TRUE,
             fontface = "italic",
             fill = "lemonchiffon",
             size = 2.5,
             hjust = 0)
data %>% filter(year >= 1990 & year <= 2001) %>% summarize(var(rescaled1990))
# 0.1224106

d <- ggplot(data,
            aes(x = year, y = rescaled2000)) +
  geom_line(data = data %>%
              filter(countrycode != "GBR" & (year >= 1990 & year <= 2001)),
            aes(col = country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", data %>%
                                    filter(countrycode != "GBR") %>%
                                    distinct(country) %>% nrow)) +
  geom_line(data = data %>%
              filter(countrycode == "GBR" & (year >= 1990 & year <= 2001)),
            col = "black",
            lwd = 1) +
  geom_line(data = data %>%
              filter(countrycode == data[which.max(data$rescaled2000), "countrycode"] & (year >= 1990 & year <= 2001)),
            col = "hotpink",
            lwd = 1) +
  labs(x = "Year",
       y = expression(paste("CO"[2], " emissions relative to 2000"))) +
  geom_label_repel(data = data %>%
                     filter(countrycode == data[which.max(data$rescaled2000), "countrycode"]) %>%
                     filter(year == 1990),
                   aes(label = country),
                   col = "hotpink",
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 2001),
                   aes(label = country),
                   col = "black",
                   size = 3,
                   nudge_y = 0.04) +
  geom_label(aes(x = 1990, y = 1.5, 
                 label = paste0("Var %~~% 2.6", "%*%10", "^-2")), 
             parse = TRUE,
             fontface = "italic",
             fill = "lemonchiffon",
             size = 2.5,
             hjust = 0)
data %>% filter(year >= 1990 & year <= 2001) %>% summarize(var(rescaled2000))
# 0.02599876

fig <- ggarrange(a, b, c, d, ncol = 2, nrow = 2,
                 labels = c("A", "B", "C", "D"))
g <- annotate_figure(fig,
                top = text_grob("Differences in pre-treatment levels of outcome variable", face = "bold"))
ggsave(g,
       file = "Figures/Comparing differences in levels.pdf",
       height = 7, width = 6, units = "in")

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

# Emissions in effective sample when donor pool is OECD & high & upper middle income
effective.sample <- c("POL", "LBY", "BHS", "BEL", "TTO", "URY", "LUX", "BRN")
g <- ggplot(data %>% 
              filter(countrycode %in% effective.sample) %>%
              filter(year >= 1990 & year <= 2010), 
            aes(x = year, y = EN.ATM.CO2E.PC, col = fct_reorder2(country, year, EN.ATM.CO2E.PC))) + 
  geom_line() +
  geom_line(data = data %>%
              filter(countrycode == "GBR") %>%
              filter(year >= 1990 & year <= 2010),
            aes(x = year, y = EN.ATM.CO2E.PC),
            col = "black") +
  scale_color_paletteer_d("rcartocolor::Bold") +
  labs(title = "Emissions trends in the United Kingdom and effective sample",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_label_repel(data = data %>% 
                     filter(countrycode %in% effective.sample) %>%
                     filter(year == 2005),
                   aes(label = country),
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 1990),
                   label = "UK", 
                   col = "black",
                   size = 3) +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/CO2 emissions in effective sample_OECD_HIC_UMC.pdf",
       height = 4.5, width = 6, units = "in")

# Population in effective sample when donor pool is OECD & high & upper middle income
effective.sample <- c("POL", "LBY", "BHS", "BEL", "TTO", "URY", "LUX", "BRN")
ggplot(data %>%
              filter(countrycode %in% effective.sample) %>%
              filter(year >= 1990 & year <= 2010),
            aes(x = year, y = SP.POP.TOTL, col = fct_reorder2(country, year, SP.POP.TOTL))) +
  geom_line() +
  geom_line(data = data %>%
              filter(countrycode == "GBR") %>%
              filter(year >= 1990 & year <= 2010),
            aes(x = year, y = SP.POP.TOTL),
            col = "black") +
  scale_color_paletteer_d("rcartocolor::Bold") +
  labs(title = "Population trends in the United Kingdom and effective sample",
       x = "Year",
       y = "Population") +
  geom_label_repel(data = data %>%
                     filter(countrycode %in% effective.sample) %>%
                     filter(year == 2005),
                   aes(label = country),
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 1990),
                   label = "UK",
                   col = "black",
                   size = 3) +
  theme(legend.position = "none")

# Emissions levels in effective sample when donor pool is OECD & high & upper middle income
effective.sample <- c("POL", "LBY", "BHS", "BEL", "TTO", "URY", "LUX", "BRN")
ggplot(data %>%
         filter(countrycode %in% effective.sample) %>%
         filter(year >= 1990 & year <= 2010),
       aes(x = year, y = EN.ATM.CO2E.KT, col = fct_reorder2(country, year, EN.ATM.CO2E.KT))) +
  geom_line() +
  geom_line(data = data %>%
              filter(countrycode == "GBR") %>%
              filter(year >= 1990 & year <= 2010),
            aes(x = year, y = EN.ATM.CO2E.KT),
            col = "black") +
  scale_color_paletteer_d("rcartocolor::Bold") +
  labs(title = "Emissions trends in the United Kingdom and effective sample",
       x = "Year",
       y = expression(paste("CO"[2], " emissions (kilotons)"))) +
  geom_label_repel(data = data %>%
                     filter(countrycode %in% effective.sample) %>%
                     filter(year == 2005),
                   aes(label = country),
                   size = 3) +
  geom_label_repel(data = data %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 1990),
                   label = "UK",
                   col = "black",
                   size = 3) +
  theme(legend.position = "none")

# Emissions in effective sample when donor pool is OECD & high income
effective.sample <- c("FRA", "DEU", "JPN", "BEL", "ITA")
g <- ggplot(data_all %>% 
              filter(countrycode %in% effective.sample) %>%
              filter(year >= 1990 & year <= 2010), 
            aes(x = year, y = EN.ATM.CO2E.PC, col = fct_reorder2(country, year, EN.ATM.CO2E.PC))) + 
  geom_line() +
  geom_line(data = data_all %>%
              filter(countrycode == "GBR") %>%
              filter(year >= 1990 & year <= 2010),
            aes(x = year, y = EN.ATM.CO2E.PC),
            col = "black") +
  scale_color_paletteer_d(package = "rcartocolor",
                          palette = "Bold") +
  labs(title = "Emissions trends in the United Kingdom and effective sample",
       x = "Year",
       y = expression(paste("CO"[2], " emissions per capita"))) +
  geom_label_repel(data = data_all %>% 
                     filter(countrycode %in% effective.sample) %>%
                     filter(year == 2005),
                   aes(label = country),
                   size = 3) +
  geom_label_repel(data = data_all %>%
                     filter(countrycode == "GBR") %>%
                     filter(year == 1990),
                   label = "UK", 
                   col = "black",
                   size = 3) +
  theme(legend.position = "none")
ggsave(g,
       file = "Figures/CO2 emissions in effective sample_OECD_HIC.pdf",
       height = 4.5, width = 6, units = "in")


# .. Table of sample for SI ####
donorpool <- data %>% distinct(countrycode) %>% pull
kable(policies %>% 
        filter(ISO.code %in% donorpool) %>%
        select(Country, ISO.code, World.Bank.classification..2001.,
                          OECD..2001., Annex.I.party, Carbon.pricing.policy.first.passed.in,
                          Treated.in.2001),
      format = "latex",
      col.names = c("Country", "ISO code", "World Bank classification (2001)", 
                    "OECD (2001)", "Annex I party", 
                    "Carbon pricing policy first passed in", "Treated in 2001"))

rm(codes, countries, policies, WB, treated, indicators, missing, nmiss,
   HIC, HIC01, UMC, UMC01, OECD, OECD01, Commonwealth, CDIAC, WPP, LIE, donorpool, tinycountries,
   data_all, effective.sample, g)



## ## ## ## ## ## ## ## ## ## ##
# SPECIFICATION IN PAPER    ####
## ## ## ## ## ## ## ## ## ## ##

load("Data/data_OECD_HIC_UMC.Rdata")
whoder()


# .. Optimize over 1990-2001, CO2 per capita, no covariates, OECD & high & upper middle income ####
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
capture.output(synth.spec, file = "Results/Specification.txt")


# .. Running Synth ####
synth.out <- synth(data.prep.obj = dataprep.out)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)

# Pre-treatment predictor values
synth.tables$tab.pred

# Weights for the predictor variables
synth.tables$tab.v

# Weights for countries in the donor pool
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
row.names(bal) <- seq(1990, 2000)
kable(bal,
      format = "latex")


# .. Donor weights figure ####
g <- ggplot(donor.weights,
            aes(x = reorder(Donor.country, Donor.weight), 
                y = Donor.weight)) +
  geom_col(width = 0.8) +
  coord_flip() +
  geom_text(aes(label = ifelse(Donor.weight < 1e-04, 
                               round(Donor.weight, 5), 
                               round(Donor.weight, 4))),
            size = 1.8, nudge_x = 0, nudge_y = 0.011) +
  labs(title = "Donor weights",
       x = "", y = "") +
  theme(axis.text.y = element_text(size = 5))
ggsave(g, file = "Figures/Donor weights.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# GENERATE RESULTS          ####
## ## ## ## ## ## ## ## ## ## ##

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
# 0.0001239377

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

(mean.UK <- data.frame(bal) %>%
    mutate(Year = seq(1990, 2000)) %>%
    filter(Year <= 1992) %>%
    summarize(mean.UK = mean(Treated)))
# 9.74754
(mean.sample <- data.frame(bal) %>% 
    mutate(Year = seq(1990, 2000)) %>%
    filter(Year <= 1992) %>%
    summarize(mean.sample = mean(Sample.Mean)))
# 9.131185
(mean.sample - mean.UK)/mean.UK
# -0.06323183

(mean.UK <- data.frame(bal) %>%
    mutate(Year = seq(1990, 2000)) %>%
    filter(Year > 1992) %>%
    summarize(mean.UK = mean(Treated)))
# 9.25531
(mean.sample <- data.frame(bal) %>% 
    mutate(Year = seq(1990, 2000)) %>%
    filter(Year > 1992) %>%
    summarize(mean.sample = mean(Sample.Mean)))
# 9.979805
(mean.sample - mean.UK)/mean.UK
# 0.07827883

bal <- data.frame(bal) %>%
  mutate(Year = seq(1990, 2000),
         `Synthetic control` = Synthetic - Treated,
         `Unweighted sample` = Sample.Mean - Treated) %>%
  select(Year, `Synthetic control`, `Unweighted sample`) %>%
  melt(id.vars = "Year")

g <- ggplot(bal, aes(x = value, y = Year, col = variable)) +
  geom_point() +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  labs(title = "Balance in weighted synthetic control and in unweighted sample",
       x = "Difference in means with pre-treatment values in the UK") +
  scale_y_continuous(breaks = seq(1990, 2000))
ggsave(g,
       file = "Figures/Balance in weighted and unweighted samples.pdf",
       height = 4.5, width = 6, units = "in")

# Difference in means for weighted and unweighted
t.test(synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Treated"],
       synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Synthetic"])$p.value
# 0.9996069
t.test(synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Treated"],
       synth.tab(synth.res = synth.out,
                 dataprep.res = dataprep.out,
                 round.digit = 10)$tab.pred[, "Sample Mean"])$p.value
# 0.03201298


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
# -148.331

results %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.Mt), mean)
# -37.08274

results %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.PC), mean)
# -0.6178388

results %>% filter(year == 2005) %>% select(gaps.pct)
# -0.09751784

results %>% filter(year == 2002) %>% select(gaps.Mt)
# -17.08163

results %>% 
  filter(year > 2001) %>% 
  summarize_at(vars(UK.Mt), sum)
# 2150.468

results %>%
  filter(year > 2001) %>%
  summarize_at(vars(gaps.Mt), sum) /
  results %>% 
  filter(year > 2001) %>% 
  summarize_at(vars(UK.Mt), sum) * 100
# -6.897613



## ## ## ## ## ## ## ## ## ## ##
# PLACEBO COUNTRIES         ####
## ## ## ## ## ## ## ## ## ## ##

placebos <- control.units

placebos <- placebos[which(placebos != 22)] # BRB
# SVD fails

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
               list("EN.ATM.CO2E.PC", 2000, "mean")),
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


  # .. Store emissions trajectories in treated unit and its synthetic counterpart ####
  store.obs[,i] <- dataprep.out$Y1plot
  store.synth[,i] <- dataprep.out$Y0plot %*% synth.out$solution.w


  # .. Calculating annual gaps between the treated and its synthetic counterpart ####
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.obs, file = "Results/Placebo countries/store.obs.Rdata")
save(store.synth, file = "Results/Placebo countries/store.synth.Rdata")
save(store.gaps, file = "Results/Placebo countries/store.gaps.Rdata")

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

countries <- whodat(placebos)

for (c in 1:length(countries)){
  country <- whodis(countries[c])
  g <- ggplot(plot.emissions %>%
                filter(Country == countries[c])) +
    geom_line(aes(x = Year, y = Observed),
              col = "#014421", lwd = 1, lty = 1) +
    geom_line(aes(x = Year, y = Synthetic),
              col = "#11904E", lwd = 1, lty = 2) +
    ylim(range(plot.emissions[plot.emissions$Country == countries[c], "Observed"], 
               plot.emissions[plot.emissions$Country == countries[c], "Synthetic"])) +
    labs(title = "Observed and Synthetic Counterfactual Emissions",
         subtitle = country,
         x = "Year",
         y = expression(paste("CO"[2], " emissions per capita"))) +
    geom_vline(xintercept = 2001,
               lty = 2)
  ggsave(g,
         file = paste0("Figures/Emissions paths in placebo and synth_", country, ".pdf"),
         height = 4.5, width = 6, units = "in")
}


# .. Placebo countries figure ####
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
# Exclude ARE, ARG, AUS, AUT, BEL, BHR, BHS, BRA, BRN, BWA, CAN, CHE, CHL, CYP, DEU, ESP, FRA
# GAB, HKG, HUN, IRL, ISL, ISR, ITA, JPN, KOR, KWT, LBN, LBY, LUX, MAC, MLT, MUS, MYS
# NZL, OMN, PAN, POL, PRT, QAT, SAU, SGP, TTO, TUR, URY, USA, VEN, ZAF

MSPE5 <- colnames(placebo.results[, pre.MSE < 5*UK.pre.MSE])
MSPE30 <- colnames(placebo.results[, pre.MSE < 30*UK.pre.MSE])
MSPE50 <- colnames(placebo.results[, pre.MSE < 50*UK.pre.MSE])
MSPE100 <- colnames(placebo.results[, pre.MSE < 100*UK.pre.MSE])

plot.gaps <- placebo.results %>%
  mutate(Year = years) %>%
  melt(id.vars = "Year", value.name = "Gaps", variable.name = "Country")
plot.gaps5 <- plot.gaps %>%
  filter(Country %in% MSPE5)
plot.gaps30 <- plot.gaps %>%
  filter(Country %in% MSPE30)
plot.gaps50 <- plot.gaps %>%
  filter(Country %in% MSPE50)
plot.gaps100 <- plot.gaps %>%
  filter(Country %in% MSPE100)

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
               y = -0.75, yend = -0.75,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.75,
            label = "CCP enacted",
            col = "black")
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
               y = -0.75, yend = -0.75,
               col = "black",
               arrow = arrow(ends = "last", type = "closed",
                             length = unit(0.1, "inches"))) +
  geom_text(x = 1997.5,
            y = -0.75,
            label = "CCP enacted",
            col = "black")
ggsave(g,
       file = "Figures/Gaps in emissions_placebo_MSPE5.pdf",
       height = 4.5, width = 6, units = "in")

# Plot excluding placebos with MSPE > 30
g <- ggplot(plot.gaps30) +
  geom_line(data = plot.gaps30 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps30 %>% 
                                    filter(Country != "GBR") %>% 
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps30 %>%
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
       file = "Figures/Gaps in emissions_placebo_MSPE30.pdf",
       height = 4.5, width = 6, units = "in")

# Plot excluding placebos with MSPE > 50
g <- ggplot(plot.gaps50) +
  geom_line(data = plot.gaps50 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps50 %>% 
                                    filter(Country != "GBR") %>% 
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps50 %>%
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
       file = "Figures/Gaps in emissions_placebo_MSPE50.pdf",
       height = 4.5, width = 6, units = "in")

# Plot excluding placebos with MSPE > 100
g <- ggplot(plot.gaps100) +
  geom_line(data = plot.gaps100 %>%
              filter(Country != "GBR"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("grey", plot.gaps100 %>% 
                                    filter(Country != "GBR") %>% 
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.gaps100 %>%
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
       file = "Figures/Gaps in emissions_placebo_MSPE100.pdf",
       height = 4.5, width = 6, units = "in")

# How many control states remain?
plot.gaps30 %>% distinct(Country)
# 7 control states


# .. Ratio of post-treatment MSPE to pre-treatment MSPE ####
ratio.MSE <- post.MSE/pre.MSE
sort(ratio.MSE)
ratio.MSE["GBR"]
(length(ratio.MSE) - which(sort(ratio.MSE) == ratio.MSE["GBR"]) +1 )/ length(ratio.MSE)
# For the UK, the post-treatment gap is 3687 times larger than
# the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/51 = 0.01960784

placebo.results <- cbind(store.gaps, gaps)
MSE <- data.frame(country = names(pre.MSE),
                  pre.MSE = pre.MSE,
                  post.MSE = post.MSE,
                  ratio = post.MSE/pre.MSE,
                  TE2005 = placebo.results[gap.end,]) %>%
  mutate(col = ifelse(country == "GBR", "darkorchid", "grey"))

MSE %>% filter(TE2005 <0) %>% arrange(ratio)
# For a one-sided test, the ratio in the UK is the largest.
# The changes of obtaining a ratio as high as this one would be
# 1/30 = 0.03333333

RMSPE.spec1 <- MSE %>% select(country, ratio)
save(RMSPE.spec1, file = "Results/Placebo countries/RMSPE.Rdata")


# .. RMSPE figures ####
# Plot log of ratio, two-sided test
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
       height = 4.5, width = 6, units = "in")

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
       file = "Figures/MSPE ratio_Two-sided.pdf",
       height = 4.5, width = 6, units = "in")

# Plot log of ratio, one-sided test
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
       height = 4.5, width = 6, units = "in")

# Plot empirical distribution of ratio, two-sided test
g <- ggplot(MSE,
            aes(x = ratio)) +
  geom_density(fill = "grey35",
               col = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(title = "Empirical distribution of ratio",
       subtitle = "Two-sided test",
       x = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       y = "Density") +
  geom_segment(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull,
               xend = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull - 500,
               y = 0.0009,
               yend = 0.003,
               col = "darkorchid",
               arrow = arrow(ends = "first", type = "closed",
                             length = unit(0.1, "inches")),
               arrow.fill = "darkorchid") +
  geom_text(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull - 500,
            y = 0.0035,
            label = "United Kingdom",
            col = "darkorchid",
            hjust = 0.4)
ggsave(g,
       file = "Figures/Empirical distribution_Two-sided.pdf",
       height = 4.5, width = 6, units = "in")

# Plot empirical distribution of ratio, one-sided test
g <- ggplot(MSE %>%
              filter(TE2005 < 0),
            aes(x = ratio)) +
  geom_density(fill = "grey35",
               col = "grey35") +
  scale_x_continuous(labels = comma) +
  labs(title = "Empirical distribution of ratio",
       subtitle = "One-sided test",
       x = "Ratio of post-treatment MSPE to pre-treatment MSPE",
       y = "Density") +
  geom_segment(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull,
               xend = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull - 500,
               y = 0.0013,
               yend = 0.003,
               col = "darkorchid",
               arrow = arrow(ends = "first", type = "closed",
                             length = unit(0.1, "inches")),
               arrow.fill = "darkorchid") +
  geom_text(x = MSE %>% filter(country == "GBR") %>% select(ratio) %>% pull,
            y = 0.0035,
            label = "United Kingdom",
            col = "darkorchid",
            hjust = 1)
ggsave(g,
       file = "Figures/Empirical distribution_One-sided.pdf",
       height = 4.5, width = 6, units = "in")



## ## ## ## ## ## ## ## ## ## ##
# LEAVE-ONE-OUT CHECK       ####
## ## ## ## ## ## ## ## ## ## ##

leaveoneout.controls <- control.units

leaveoneout.names <- whodat(leaveoneout.controls)
leaveoneout.names <- append(leaveoneout.names, "Dropped", length(leaveoneout.names))
leaveoneout.names

store.gaps <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.gaps) <- paste0("No_", leaveoneout.names)
store.gaps

store.synth <- matrix(NA, length(years), length(leaveoneout.names))
colnames(store.synth) <- paste0("No_", leaveoneout.names)

nloops <- length(leaveoneout.controls)+1

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
               list("EN.ATM.CO2E.PC", 2000, "mean")),
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

  
  # .. Store emissions trajectories for synthetic UK when leaving out one country ####
  store.synth[,i] <- dataprep.out$Y0plot %*% synth.out$solution.w
  

  # .. Calculating annual gaps between the UK and its synthetic counterpart ####
  store.gaps[,i] <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)
}

save(store.synth, file = "Results/Leave-one-out/store.synth.Rdata")
save(store.gaps, file = "Results/Leave-one-out/store.gaps.Rdata")

load("Results/Leave-one-out/store.synth.Rdata")
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
  geom_line(data = plot.LOO %>%
              filter(Country == "No_Dropped"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Leave-one-out robustness check",
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
       file = "Figures/Gaps in emissions_leave one out.pdf",
       height = 4.5, width = 6, units = "in")

# Plot with highlighted countries
g <- ggplot(plot.LOO) +
  geom_line(data = plot.LOO %>%
              filter(Country != "GBR") %>%
              filter(Country != "BEL") %>%
              filter(Country != "BHS")%>%
              filter(Country != "LUX"),
            aes(x = Year,
                y = Gaps,
                col = Country),
            show.legend = F) +
  scale_color_manual(values = rep("thistle", plot.LOO %>%
                                    filter(Country != "GBR") %>%
                                    distinct(Country) %>% nrow)) +
  geom_line(data = plot.LOO %>%
              filter(Country == "No_Dropped"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid",
            lwd = 1) +
  geom_line(data = plot.LOO %>%
              filter(Country == "No_LUX"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid4") +
  geom_line(data = plot.LOO %>%
              filter(Country == "No_BEL"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid4") +
  geom_line(data = plot.LOO %>%
              filter(Country == "No_BHS"),
            aes(x = Year,
                y = Gaps),
            col = "darkorchid4") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(title = "Gap between Treated and Synthetic Control",
       subtitle = "Leave-one-out robustness check",
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
       file = "Figures/Gaps in emissions_leave one out_highlight.pdf",
       height = 4.5, width = 6, units = "in")


# .. More analysis when specific countries are dropped ####
# Convert emissions per capita
results <- left_join(
  data.frame(store.gaps) %>%
    mutate(Year = years) %>%
    reshape2::melt(id.vars = "Year", value.name = "gaps.PC", variable.name = "Country"),
  data.frame(store.synth) %>%
    mutate(Year = years) %>%
    reshape2::melt(id.vars = "Year", value.name = "synth.PC", variable.name = "Country"),
  by = c("Year", "Country")) %>%
  left_join(data %>%
              filter(countrycode == "GBR") %>%
              filter(year >= 1990 & year <= 2005) %>%
              select(year, 
                     population = SP.POP.TOTL, 
                     UK.t = EN.ATM.CO2E.KT,
                     UK.PC = EN.ATM.CO2E.PC) %>%
              mutate(UK.t = UK.t * 10^3),
            by = c("Year" = "year")) %>%
  mutate(synth.t = synth.PC * population,
         synth.Mt = synth.t / 10^6,
         UK.Mt = UK.t / 10^6) %>%
  mutate(gaps.t = gaps.PC * population,
         gaps.Mt = gaps.t / 10^6) %>%
  select(Year, Country,
         UK.PC, synth.PC, gaps.PC,
         UK.Mt, synth.Mt, gaps.Mt) %>%
  mutate(gaps.pct = (UK.Mt - synth.Mt) / synth.Mt)

# Range of results
results %>% filter(Year == 2005 & Country == "No_Dropped") %>% select(gaps.pct)
# -0.09751784
results %>% filter(Year == 2005) %>% filter(gaps.pct == min(gaps.pct)) %>% 
  select(Country, gaps.pct)
# No_BEL -0.1081561
results %>% filter(Year == 2005) %>% filter(gaps.pct == max(gaps.pct)) %>% 
  select(Country, gaps.pct)
# No_BHS -0.05361657
results %>% filter(Year == 2005 & Country == "No_LUX") %>% select(gaps.pct)
# -0.08487441
results %>% filter(Year == 2005 & Country == "No_TTO") %>% select(gaps.pct)
# -0.0633569

# Mean Square Prediction Error Pre-Treatment
pre.MSE <- apply(leaveoneout.results[gap.start:gap.end.pre, ]^2, 2, mean)

# Mean Square Prediction Error Post-Treatment
post.MSE <- apply(leaveoneout.results[gap.start.post:gap.end, ]^2, 2, mean)

# Ratio of post-treatment MSPE to pre-treatment MSPE
leaveoneout.results <- store.gaps
MSE.LOO <- data.frame(country = names(pre.MSE),
                      pre.MSE = pre.MSE,
                      post.MSE = post.MSE,
                      ratio = post.MSE/pre.MSE,
                      TE2005 = leaveoneout.results[gap.end,])

ratio.NoPOL <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_POL")) %>%
  arrange(ratio)
ratio.NoPOL
(nrow(ratio.NoPOL) - which(ratio.NoPOL$country == "No_POL") +1 )/ nrow(ratio.NoPOL)
# When Poland is dropped, the post-treatment gap for the UK 
# is 359 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 4/51 = 0.07843137

ratio.NoLBY <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_LBY")) %>%
  arrange(ratio)
ratio.NoLBY
(nrow(ratio.NoLBY) - which(ratio.NoLBY$country == "No_LBY") +1 )/ nrow(ratio.NoLBY)
# When Libya is dropped, the post-treatment gap for the UK 
# is 908 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 2/51 = 0.03921569

ratio.NoBHS <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_BHS")) %>%
  arrange(ratio)
ratio.NoBHS
(nrow(ratio.NoBHS) - which(ratio.NoBHS$country == "No_BHS") +1 )/ nrow(ratio.NoBHS)
# When The Bahamas is dropped, the post-treatment gap for the UK 
# is 126 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 6/51 = 0.1176471

ratio.NoBEL <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_BEL")) %>%
  arrange(ratio)
ratio.NoBEL
(nrow(ratio.NoBEL) - which(ratio.NoBEL$country == "No_BEL") +1 )/ nrow(ratio.NoBEL)
# When Belgium is dropped, the post-treatment gap for the UK 
# is 389 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 4/51 = 0.07843137

ratio.NoTTO <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_TTO")) %>%
  arrange(ratio)
ratio.NoTTO
(nrow(ratio.NoTTO) - which(ratio.NoTTO$country == "No_TTO") +1 )/ nrow(ratio.NoTTO)
# When Trinidad and Tobago is dropped, the post-treatment gap for the UK 
# is 585 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 3/51 = 0.05882353

ratio.NoURY <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_URY")) %>%
  arrange(ratio)
ratio.NoURY
(nrow(ratio.NoURY) - which(ratio.NoURY$country == "No_URY") +1 )/ nrow(ratio.NoURY)
# When Uruguay is dropped, the post-treatment gap for the UK 
# is 4418 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/51 = 0.01960784

ratio.NoLUX <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_LUX")) %>%
  arrange(ratio)
ratio.NoLUX
(nrow(ratio.NoLUX) - which(ratio.NoLUX$country == "No_LUX") +1 )/ nrow(ratio.NoLUX)
# When Luxembourg is dropped, the post-treatment gap for the UK 
# is 1638 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/51 = 0.01960784

ratio.NoBRN <- rbind(MSE %>% filter(country != "GBR") %>% select(-col),
                     MSE.LOO %>% filter(country == "No_BRN")) %>%
  arrange(ratio)
ratio.NoBRN
(nrow(ratio.NoBRN) - which(ratio.NoBRN$country == "No_BRN") +1 )/ nrow(ratio.NoBRN)
# When Brunei is dropped, the post-treatment gap for the UK 
# is 2075 times larger than the pre-treatment gap.
# If we were to pick a country at random from this sample,
# the chances of obtaining a ratio as high as this one would be
# 1/51 = 0.01960784



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
  list("EN.ATM.CO2E.PC", 2000, "mean"))
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
  list("EN.ATM.CO2E.PC", 1999, "mean"))
pred1999 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"),
  list("EN.ATM.CO2E.PC", 1997, "mean"),
  list("EN.ATM.CO2E.PC", 1998, "mean"))
pred1998 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"),
  list("EN.ATM.CO2E.PC", 1997, "mean"))
pred1997 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"),
  list("EN.ATM.CO2E.PC", 1996, "mean"))
pred1996 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"),
  list("EN.ATM.CO2E.PC", 1995, "mean"))
pred1995 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"),
  list("EN.ATM.CO2E.PC", 1994, "mean"))
pred1994 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"),
  list("EN.ATM.CO2E.PC", 1993, "mean"))
pred1993 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"),
  list("EN.ATM.CO2E.PC", 1992, "mean"))
pred1992 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"),
  list("EN.ATM.CO2E.PC", 1991, "mean"))
pred1991 <- list(
  list("EN.ATM.CO2E.PC", 1990, "mean"))

for (y in 1:ncol(store.synth)){
  placebo.year <- substr(colnames(store.synth)[y], 4, 7)
  choose.time.predictors <- 1990:placebo.year

  dataprep.out <-
    dataprep(foo = data,
             predictors = NULL,
             predictors.op = NULL,
             time.predictors.prior = choose.time.predictors,
             special.predictors = get(paste0("pred", placebo.year)),
             dependent = "EN.ATM.CO2E.PC",
             unit.variable = "countryid",
             unit.names.variable = "country",
             time.variable = "year",
             treatment.identifier = treated.unit,
             controls.identifier = c(control.units),
             time.optimize.ssr = choose.time.predictors,
             time.plot = 1990:2005)


  # .. Running Synth ####
  synth.out <- synth(data.prep.obj = dataprep.out)

  
  # .. Store emissions trajectories in synthetic control ####
  store.synth[,y] <- dataprep.out$Y0plot %*% synth.out$solution.w
}

save(store.synth, file = "Results/Placebo years/store.synth.Rdata")
load("Results/Placebo years/store.synth.Rdata")

plot <- data.frame(years = years,
                   treated = dataprep.out$Y1plot,
                   store.synth) %>%
  rename(treated = X50)
plot <- plot %>%
  melt(id.vars = "years", value.name = "emissions")


# .. Placebo years figure ####
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
            col = "black") +
  theme(legend.title = element_blank(),
        legend.position = c(0.132, 0.15)) +
  guides(linetype = guide_legend(override.aes = list(size = 0.5)))

for (y in 1:(ncol(store.synth)-1)){
  placebo.year <- substr(colnames(store.synth)[y], 4, 7)
  p <- g + geom_line(data = plot %>%
                       filter(variable == paste0("Tx_", placebo.year)),
                     aes(x = years, y = emissions),
                     col = "coral",
                     lty = "dotted",
                     lwd = 1) +
    geom_vline(xintercept = as.numeric(placebo.year),
               col = "coral",
               lty = 2) +
    labs(subtitle = paste0("Re-assigning treatment to placebo year ",
                           placebo.year))
  ggsave(p,
         file = paste0("Figures/Emissions paths in placebo year ",
                       placebo.year, ".pdf"),
         height = 4.5, width = 6, units = "in")
}
