



##pH_facet_plot



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")
x$carb <- x$BD*x$LOI/100 ## grams carbon per cubic cm soil

x$Yr_mo <- c(rep("2020",60), rep("2019", 60), rep("2018 May", 60), rep("2018 Jul", 60))
x$xx <- c(rep(4,60), rep(3, 60), rep(1, 60), rep(2, 60))

x$xxx <- as.factor(x$xx)

Site1 <- x[which(x$Site == "1"),]
Site3 <- x[which(x$Site == "3"),]
Site5 <- x[which(x$Site == "5"),]



par(mfrow = c(2,3), mar = c(5,5,3,3))




c <- x[which(x$Treatment == "Control"),]
s <- x[which(x$Treatment == "Salt"),]