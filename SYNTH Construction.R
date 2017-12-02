# ScanSynth
# 8 May 2017
# Alice Lepissier

############################
# INDEX                    #
############################
# Preamble
# Functions
# Data
# Specification 1:
### Optimize over 1995-2001, less covariates
# Synth 1

############################
# PREAMBLE                 #
############################
setwd("C:/Users/Alice/Box Sync/LepissierMildenberger/ScandiSynth/Results") # Alice laptop
#setwd("~/Box Sync/LepissierMildenberger/ScandiSynth/Results") # Matto
#setwd("C:/boxsync/alepissier/LepissierMildenberger/ScandiSynth/Results") # Alice work
library(Synth)
library(foreign)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(devtools)



############################
# FUNCTIONS                #
############################

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



############################
# DATA                     #
############################

data <- read.dta("WDI.dta")
whoder()

ggplot(data, aes(x = year, y = CO2_emissions_PC, col = countryname)) +
  geom_line()

ggplot(data[which(data$countrycode == "GBR"), ], aes(x = year, y = CO2_emissions_PC, col = countryname)) +
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



############################
# SPECIFICATION 1          #
############################

# Optimize over 1995-2001
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


############################
# TRAJECTORY BALANCING     #
############################

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




############################
# SYNTH 1                  #
############################

# Running Synth
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Calculating annual discrepancy in emissions per capita between
# the UK and its synthetic counterpart
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)

# Pre-treatment predictor values for the UK, for the synthetic control unit,
# and for all units in the sample
synth.tables$tab.pred

# Weights for the predictor variables
synth.tables$tab.v

# Weights for countries in the donor pool
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


############################
# SPECIFICATION 2          #
############################

# Optimize over 1990-2001
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



############################
# SYNTH 2                  #
############################

# Running Synth
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Calculating annual discrepancy in emissions per capita between
# the UK and its synthetic counterpart
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# Housekeping
synth.tables <- synth.tab(dataprep.res = dataprep.out,
                          synth.res = synth.out)

# Pre-treatment predictor values for the UK, for the synthetic control unit,
# and for all units in the sample
synth.tables$tab.pred

# Weights for the predictor variables
synth.tables$tab.v

# Weights for countries in the donor pool
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



############################
# PLACEBO TEST 1           #
############################

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

# Running Synth
synth.out <- synth(data.prep.obj = dataprep.out,
                   method = "BFGS")

# Calculating annual discrepancy in emissions per capita between
# the placebo and its synthetic counterpart
gaps <- dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w)

# Plot emissions per capita pre- and post-intervention in the
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

# Plot gaps in outcomes between Mexico and the synthetic control
gaps.plot(synth.res = synth.out,
          dataprep.res = dataprep.out,
          Ylab = "CO2 emissions per capita",
          Xlab = "Year",
          Ylim = c(-1.5,1.5),
          Main = "Gap between Treated and Synthetic Control")
abline(v = 2001)



############################
# TRAJECTORY BALANCING     #
############################

# devtools::install_github("chadhazlett/kbal")
# 
# library(KBAL)
# 
# data$SWE_treat <- 0
# data$SWE_treat[which(data$countrycode=="SWE")] <- 1
# 
# dataCO2 <- data[,c(1,4:20)]
# dataCO2.pretreat <- dataCO2[which(dataCO2$year < 1990),]
# dataCO2.pretreat <- dataCO2.pretreat[which(dataCO2.pretreat$year > 1970),]
# 
# colnames(dataCO2.pretreat)
# dataCO2.kbal <- dataCO2.pretreat[,which(names(dataCO2.pretreat)%in%c("countryid","year","SWE_treat", "CO2_emissions_PC"))]
# 
# c.ids <- levels(as.factor(dataCO2.kbal$countryid))
# 
# expand.data <- as.data.frame(c.ids)
# expand.data$SWE_treat <- 0
# 
# ## Sweden is Country ID # 33
# 
# expand.data$SWE_treat[which(expand.data$c.ids==33)] <- 1
# 
# for (j in 1971:1989){
#   eval(parse(text=paste0("expand.data$CO2.pc.",j," <- NA")))
# }
# 
# for(i in 1:length(c.ids)){
#   for(j in 1971:1989){
#     eval(parse(text=paste0("expand.data$CO2.pc.",j,"[i] <- dataCO2.kbal$CO2_emissions_PC[dataCO2.kbal$year==j][i]")))
#   }
# }
# 
# 
# write.csv(expand.data, file="matto_data.Rda")
# 
# treatment.vector <- expand.data$SWE_treat
# data.matrix <- expand.data[,-which(names(expand.data)%in%c("SWE_treat", "c.ids"))]
# data.matrix <- as.matrix(data.matrix)
# kbal.out <- kbal(method="el",X=data.matrix, D=treatment.vector, sigma=ncol(data.matrix))
# 
# 
# #kbal.out=kbal(method="el",X=Ypre, D=D, sigma=ncol(Ypre))
# 
# #Where Ypre is a matrix whose rows are observations and columns are Y, all for pre-treatment era.   
# #Sigma is up to you. If it's a really hard problem you may need a higher Sigma.
# #But a good starting point is to set it to the number of pre-treatment time periods (i.e. ncol(Ypre), which I've put above), but you can try a bunch. 
# #What we typically do is choose the sigma that maximizes:#  kbal.out$L1_orig/kbal.out$L1_kbal
# 
# #kbal.w0=kbal.out$w[D==0]
# #kbal.w0=kbal.w0/sum(kbal.w0)
# 
# kbal.w0=kbal.out$w[expand.data$SWE_treat==0]
# kbal.w0=kbal.w0/sum(kbal.w0)
# 
# sweden.actual <- expand.data[expand.data$SWE_treat==1,]
# 
# synth.sweden.temp <- expand.data[expand.data$SWE_treat==0,-which(names(expand.data)%in%c("c.ids"))] * kbal.w0
# sweden.synthetic <- colSums(synth.sweden.temp)
# 
# sweden.actual
# sweden.synthetic
# 
# 
# #To get the weights you'd want on the controls, 
# #kbal.w0=kbal.out$w[D==0]
# #kbal.w0=kbal.w0/sum(kbal.w0)
# 
# #For simple mean balancing, you can use kbal as well, but with a linear kernel, as in:
# #kbal.lin.out=kbal(method="el",X=Ypre, D=D, linkernel = TRUE)
# #kbal.lin.w0=kbal.lin.out$w[D==0]
# #kbal.lin.w0=kbal.lin.w0/sum(kbal.lin.w0)
# 
# # now let's map Sweden against synthetic Sweden in the whole data set
# 
# 
# 
# 
