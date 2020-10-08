
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




boxplot(Cl~Treatment, data = x, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 18), xaxt = 'n')
