

## c 2018-2020

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")


x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")



## key vars

x$carb <- x$BD*x$LOI/100 ## grams carbon per cubic cm soil

par(mfrow = c(1,1))
boxplot(carb~Depth*Treatment*Site, data = x)



y18m <- x[which(x$Date == "May 10th, 2018"),]
y18j <- x[which(x$Date == "July 10th, 2018"),]
y19 <- x[which(x$Year == "2019"),]
y20 <- x[which(x$Year == "2020"),]

boxplot(carb~Depth*Treatment*Site, data = y20)




######## Effect size calculation

## 




