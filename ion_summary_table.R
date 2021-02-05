


## ion summary table

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")
library(data.table)
library(plotrix)

x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")
x$Mg[is.na(x$Mg)] <- 0.005
x$Ca[is.na(x$Ca)] <- 0.005
x$Mg[x$Mg < 0] <- 0.005
x$Ca[x$Ca < 0] <- 0.005

## reorder and select variables
x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC"))
y <- x
x <- x[which(x$Date == "August 8th, 2020" | x$Date == "May 10th, 2018"),]
x <- data.table(x)

x$response <- x$Cl
x$response <- x$Mg

output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Treatment")]

y <- data.table(y)
y$response <- y$Cl+ y$SO4 + y$Na + y$K + y$Mg + y$Ca

output <- y[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Treatment")]
output$mean <- round(output$mean, 5)


output <- y[,.("mean" = mean(response), "se" = std.error(response)), by = c("Date", "Site", "Treatment", "Depth")]
