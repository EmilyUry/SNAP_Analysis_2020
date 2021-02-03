

### two way anovas -- SNAP --- ALL data





x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")

### replace NAs in nutrient (NO3, NH4, PO4) data with 0.005, which is half detection limit. 

x$ICNO3[is.na(x$ICNO3)] <- 0.005
x$NH4[is.na(x$NH4)] <- 0.005
x$PO4[is.na(x$PO4)] <- 0.005








x$Site <- as.factor(x$Site)
res.aov <- aov(response ~ Date*Site*Depth*Treatment, data = x)
summary(res.aov)
TukeyHSD(res.aov)