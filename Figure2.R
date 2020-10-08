

## Figure 1
#https://www.r-graph-gallery.com/96-boxplot-with-jitter.html

## boxplot with jigger

## all sites and all years combined 

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")


x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")

x$Yr_mo <- c(rep("2020",60), rep("2019", 60), rep("2018 May", 60), rep("2018 Jul", 60))
x$xx <- c(rep(4,60), rep(3, 60), rep(1, 60), rep(2, 60))

# #outlier exclusion
# x <- x[-26,]
# x <- x[-21,]

x5 <- x[which(x$Depth == "(0-5)"),]
x10 <- x[which(x$Depth == "(5-10)"),]

## version 1 -- normal scales

par(mfrow = c(4,2), oma = c(1,7,1,1), mar = c(1,0.2,1,0.2))


