
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")


x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")

x$carbon <- x$BD*x$LOI/100 ## grams carbon per cubic cm soil

x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "SM", "BD", "pH", "carbon", 
                          "DOC", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN", "NH4", "ICNO3", "PO4","Phenol",
                          "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Roots" ))












library(data.table)
library(plotrix)

x <- data.table(x)

output <- x[,.("mean" = mean(SM), "se" = std.error(SM)),
             by = c("Date", "Site", "Treatment", "Depth")]
