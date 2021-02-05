


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

labs <- c("Dry", "Int." , "Wet")
names(labs) <- c("1","3", "5")

laby <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(laby) <- c("(0-5)", "(5-10)")

data <- x
data$Site <- as.factor(data$Site)
data$Date <- as.factor(data$Date)
data$Date <- factor(data$Date , levels=c("May 10th, 2018", "July 10th, 2018", "June 20th, 2019", "August 8th, 2020"))
date <- c(rep("Aug20", 60) , rep("Jun19", 60) , rep("May18", 60), rep("Jul18", 60))
data$date <- factor(date, levels = c("May18", "Jul18", "Jun19", "Aug20"))
data$Depth <- as.factor(data$Depth)
data$Treatment <- as.factor(data$Treatment)
data$response <- data$pH

ggplot(data=data, aes(x=date, y = response, fill = Treatment)) + 
  geom_boxplot() + 
  facet_grid(Depth ~ Site, labeller = labeller(Site = labs, Depth = laby)) +
  theme_bw() +
  xlab(" ") +
  ylab("pH") + 
  theme(legend.position = "none") +
  scale_fill_manual(values=c("#FFFFFF", "#db351f"), labels = c("Control", "Salt"))
