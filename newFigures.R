


## analysis of response variable: 2020

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

library(ggplot2)


x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")


## box plots forall years combined
## Cl
## SO4
## pH

## box plots for each year separately
## DOC
## Phenolics
## CMin
## LOI
# SIR
