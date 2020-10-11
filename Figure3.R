

## Figure 3


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

x <- read.csv("SNAP_timeseries_data.csv", head = T)
head(x)
names(x) <- c( "Date", "Year", "Site", "Treatment", "Month", "Depth", "Core", "Salinity","Cond", "BD", 
               "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
               "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
               "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")
x$Treatment <- as.factor(x$Treatment)
x$Depth <- as.factor(x$Depth)
a <- x[which(x$Site == "1"),]
b <- x[which(x$Site == "3"),]
c <- x[which(x$Site == "5"),]





par(mfrow = c(4,3),  oma = c(3,7,3,1), mar = c(0.4,0.2,0.4,0.2))
plot(a$Month, a$Cl, ylim = c(0,2900), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
mtext("Cl-", 2, 3, outer = F, las =1, cex = 1.1, adj = 1.6)
mtext("ug/g soil", 2, 3, outer = F, las =1, cex = 0.8, adj = 1, padj = 2)
mtext("Dry", 3, 1, cex = 1.2)
plot(b$Month, b$Cl, ylim = c(0,2900), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
mtext("Intermediate", 3, 1, cex = 1.2)
plot(c$Month, c$Cl, ylim = c(0,2900), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
mtext("Wet", 3, 1, cex = 1.2)

plot(a$Month, a$SM, ylim = c(0,50), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
mtext("Soil", 2, 2, outer = F, las =1, cex = 1, adj = 1.7)
mtext("moisture", 2, 2, outer = F, las =1, cex = 1, adj = 1, padj = 2)
mtext("%", 2, 3, outer = F, las =1, cex = 0.8, adj = 2, padj = 4)
plot(b$Month, b$SM, ylim = c(0,50), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
plot(c$Month, c$SM, ylim = c(0,50), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)

plot(a$Month, a$pH, ylim = c(3,7), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
mtext("pH", 2, 3, outer = F, las =1, cex = 1.1, adj = 1.6)
plot(b$Month, b$pH, ylim = c(3,7), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)
plot(c$Month, c$pH, ylim = c(3,7), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
abline(v=0, lty =2)


plot(a$Month, a$DOC, ylim = c(0,40), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
axis(1, at = c(-11, 20, 32,46), c("2015", "2018", "2019", "2020"))
abline(v=0, lty =2)
mtext("DOC", 2, 1, outer = F, las =1, cex = 1.1, adj = 1.8)
mtext("mg/L", 2, 3, outer = F, las =1, cex = 0.8, adj = 1.3, padj = 2)
plot(b$Month, b$DOC, ylim = c(0,40), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
axis(1, at = c(-11, 20, 32,46), c("2015", "2018", "2019", "2020"))
abline(v=0, lty =2)
plot(c$Month, c$DOC, ylim = c(0,40), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#143ee3bb", "#ff9b4aee")[a$Treatment])
axis(1, at = c(-11, 20, 32,46), c("2015", "2018", "2019", "2020"))
abline(v=0, lty =2)
