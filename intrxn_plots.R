

### DOC ~ cl and ph interaction plots


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
### FIGURE

d1 <- density(x$pH[which(x$Site == 1)])
plot(d1, main="pH", ylim = c(0,1.7), xlim = c(3,7.5), xlab = "pH")
polygon(d1, col="#FF000099", border="red")
d3 <- density(x$pH[which(x$Site == 3)])
polygon(d3, col="#9400D399", border="purple")
d5 <- density(x$pH[which(x$Site == 5)])
polygon(d5, col="#0000FF99", border="blue")


plot((x$Cl), x$DOC, log = 'x', pch = 16, 
     col = c("#FF000099","black", "#9400D399", "black", "#0000FF99")[x$Site], 
     )

fit <- lm(x$DOC ~ log10(x$Cl))
abline(fit, lty = 2)

fit1 <- lm(Site1$DOC ~ log10(Site1$Cl))
abline(fit1, col = "red")

fit3 <- lm(Site3$DOC ~ log10(Site3$Cl))
abline(fit3, col = "purple")

fit5 <- lm(Site5$DOC ~ log10(Site5$Cl))
abline(fit5, col = "blue")



HpH <- x[which(x$pH >= "5.1"),]
LpH <- x[which(x$pH <= "5.1"),]


plot((HpH$Cl), HpH$DOC, log = 'x', pch = 16, 
     col = "brown3")
points((LpH$Cl), LpH$DOC, pch = 16, 
       col = "black")
fit <- lm(x$DOC ~ log10(x$Cl))
abline(fit, lty = 2)

fitH <- lm(HpH$DOC ~ log10(HpH$Cl))
abline(fitH, col = "brown3")

fitL <- lm(LpH$DOC ~ log10(LpH$Cl))
abline(fitL, col = "black")




fit
fit1

hist(x$pH)



c <- x[which(x$Treatment == "Control"),]
s <- x[which(x$Treatment == "Salt"),]
col <- c("red", "black", "purple", "black", "blue") 

par(mfrow = c(2,2), mar = c(5,5,3,3))


hist(c$pH, col = "#9400D399", breaks = 10)

hist(c$pH, col = "purple")

d <- density(x$pH)
plot(d)
d1 <- density(x$pH[which(x$Site == 1)])
plot(d1, main="pH", ylim = c(0,1.7), xlim = c(3,7.5), xlab = "pH")
polygon(d1, col="#FF000099", border="red")
d3 <- density(x$pH[which(x$Site == 3)])
polygon(d3, col="#9400D399", border="purple")
d5 <- density(x$pH[which(x$Site == 5)])
polygon(d5, col="#0000FF99", border="blue")
