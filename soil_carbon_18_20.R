

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
#boxplot(carb~Depth*Treatment*Site, data = x)



y18m <- x[which(x$Date == "May 10th, 2018"),]
y18j <- x[which(x$Date == "July 10th, 2018"),]
y19 <- x[which(x$Year == "2019"),]
y20 <- x[which(x$Year == "2020"),]

#boxplot(carb~Depth*Treatment*Site, data = y20)




######## Effect size calculation

### for 2020

y <- y20
y$var <- y$DOC

## Site 1
S <- mean(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C <- mean(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Control"])
pwsd <- sqrt((S.sd^2 + C.sd^2)/2)
hc1.5 <-  (C - S)/pwsd*0.62

S <- mean(y$var[y$Site == "1" & y$Depth == "(5-10)" & y$Treatment == "Salt"])
C <- mean(y$var[y$Site == "1" & y$Depth == "(5-10)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "1" & y$Depth == "(5-10)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "1" & y$Depth == "(5-10)" & y$Treatment == "Control"])
pwsd <- sqrt((S.sd^2 + C.sd^2)/2)
hc1.10 <-  (C - S)/pwsd*0.62

## Site 3
S <- mean(y$var[y$Site == "3" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C <- mean(y$var[y$Site == "3" & y$Depth == "(0-5)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "3" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "3" & y$Depth == "(0-5)" & y$Treatment == "Control"])
pwsd <- sqrt((S.sd^2 + C.sd^2)/2)
hc3.5 <-  (C - S)/pwsd*0.62

S <- mean(y$var[y$Site == "3" & y$Depth == "(5-10)" & y$Treatment == "Salt"])
C <- mean(y$var[y$Site == "3" & y$Depth == "(5-10)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "3" & y$Depth == "(5-10)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "3" & y$Depth == "(5-10)" & y$Treatment == "Control"])
pwsd <- sqrt((S.sd^2 + C.sd^2)/2)
hc3.10 <-  (C - S)/pwsd*0.62

## Site 5
S <- mean(y$var[y$Site == "5" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C <- mean(y$var[y$Site == "5" & y$Depth == "(0-5)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "5" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "5" & y$Depth == "(0-5)" & y$Treatment == "Control"])
pwsd <- sqrt((S.sd^2 + C.sd^2)/2)
hc5.5 <-  (C - S)/pwsd*0.62

##
S <- mean(y$var[y$Site == "5" & y$Depth == "(5-10)" & y$Treatment == "Salt"])
C <- mean(y$var[y$Site == "5" & y$Depth == "(5-10)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "5" & y$Depth == "(5-10)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "5" & y$Depth == "(5-10)" & y$Treatment == "Control"])
pwsd <- sqrt((S.sd^2 + C.sd^2)/2)
hc5.10 <-  (C - S)/pwsd*0.62


ES20 <- c(hc1.5, hc1.10, hc3.5, hc3.10, hc5.5, hc5.10)
ES20

xx <- c(10,9,6,5,2,1)

par(mar = c(4,5,4,4))
plot(ES20, xx, xlim = c(-1,5), ylim = c(0,11), yaxt = 'n', ylab = " ", 
     pch = c(16,1))
abline(v=0, lty =2)
axis(2, xx, c("Site 1 (0-5)", "Site 1 (5-10)",
              "Site 3 (0-5)", "Site 3 (5-10)",
              "Site 3 (0-5)", "Site 3 (5-10)"), 
     las = 1)
arrows(-0.58, 10, 2.14, 10, angle = 90, length = 0.05, code = 3)





install.packages("effsize")
library(effsize)



#################### Effect size plots

## years are y20, y19, y18j, y18m
y <- y19
y$var <- y$Cmin_c

{
One <- cohen.d(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Salt"], 
        y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Control"],
        conf.level = 0.95,
        hedges.correction = T)
E1.5 <- One$estimate
CI <- as.numeric(One$conf.int)
E1.5.lower <- CI[1]
E1.5.upper <- CI[2]

One <- cohen.d(y$var[y$Site == "1" & y$Depth == "(5-10)" & y$Treatment == "Salt"], 
               y$var[y$Site == "1" & y$Depth == "(5-10)" & y$Treatment == "Control"],
               conf.level = 0.95,
               hedges.correction = T)
E1.10 <- One$estimate
CI <- as.numeric(One$conf.int)
E1.10.lower <- CI[1]
E1.10.upper <- CI[2]


Three <- cohen.d(y$var[y$Site == "3" & y$Depth == "(0-5)" & y$Treatment == "Salt"], 
               y$var[y$Site == "3" & y$Depth == "(0-5)" & y$Treatment == "Control"],
               conf.level = 0.95,
               hedges.correction = T)
E3.5 <- Three$estimate
CI <- as.numeric(Three$conf.int)
E3.5.lower <- CI[1]
E3.5.upper <- CI[2]

Three <- cohen.d(y$var[y$Site == "3" & y$Depth == "(5-10)" & y$Treatment == "Salt"], 
               y$var[y$Site == "3" & y$Depth == "(5-10)" & y$Treatment == "Control"],
               conf.level = 0.95,
               hedges.correction = T)
E3.10 <- Three$estimate
CI <- as.numeric(Three$conf.int)
E3.10.lower <- CI[1]
E3.10.upper <- CI[2]


Five <- cohen.d(y$var[y$Site == "5" & y$Depth == "(0-5)" & y$Treatment == "Salt"], 
               y$var[y$Site == "5" & y$Depth == "(0-5)" & y$Treatment == "Control"],
               conf.level = 0.95,
               hedges.correction = T)
E5.5 <- Five$estimate
CI <- as.numeric(Five$conf.int)
E5.5.lower <- CI[1]
E5.5.upper <- CI[2]

Five <- cohen.d(y$var[y$Site == "5" & y$Depth == "(5-10)" & y$Treatment == "Salt"], 
               y$var[y$Site == "5" & y$Depth == "(5-10)" & y$Treatment == "Control"],
               conf.level = 0.95,
               hedges.correction = T)
E5.10 <- Five$estimate
CI <- as.numeric(Five$conf.int)
E5.10.lower <- CI[1]
E5.10.upper <- CI[2]


### summary

E.size <- c(E1.5, E1.10, E3.5, E3.10, E5.5, E5.10)
lowerCI <- c(E1.5.lower, E1.10.lower, E3.5.lower, E3.10.lower, E5.5.lower, E5.10.lower)
upperCI <- c(E1.5.upper, E1.10.upper, E3.5.upper, E3.10.upper, E5.5.upper, E5.10.upper)

}

xx <- c(10,9, 6,5,2,1)
par(mar = c(4,12,4,12), xpd = F)

plot(E.size, xx, xlim = c(-6,5), ylim = c(0,11), yaxt = 'n', ylab = " ", 
     pch = c(16,1), main = "Cmin \n 2019")
abline(v=0, lty =2)
axis(2, xx, c("Site 1 (0-5)", "Site 1 (5-10)",
              "Site 3 (0-5)", "Site 3 (5-10)",
              "Site 5 (0-5)", "Site 5 (5-10)"), 
     las = 1)
arrows(lowerCI, xx, upperCI, xx, angle = 90, length = 0.05, code = 3)



y20.5 <- y20[which(y20$Depth == "(0-5)"),]
y20.10 <- y20[which(y20$Depth == "(5-10)"),]
y19.5 <- y19[which(y19$Depth == "(0-5)"),]
y19.10 <- y19[which(y19$Depth == "(5-10)"),]
y18j.5 <- y18j[which(y18j$Depth == "(0-5)"),]
y18j.10 <- y18j[which(y18j$Depth == "(5-10)"),]
y18m.5 <- y18m[which(y18m$Depth == "(0-5)"),]
y18m.10 <- y18m[which(y18m$Depth == "(5-10)"),]
par(mfrow = c(2,4), mar = c(1,1,1,1), oma = c(3, 6, 4, 0.5))
boxplot(Cl~Treatment*Depth, data = y18m.5, xaxt = 'n', ylim = c(0,3000))
boxplot(Cl~Treatment*Depth, data = y18j.5, xaxt = 'n', ylim = c(0,3000))
boxplot(Cl~Treatment*Depth, data = y19.5, xaxt = 'n', ylim = c(0,3000))
boxplot(Cl~Treatment*Depth, data = y20.5, xaxt = 'n', ylim = c(0,3000))

boxplot(Cl~Treatment*Depth, data = y18m.10, xaxt = 'n', ylim = c(0,3000))
boxplot(Cl~Treatment*Depth, data = y18j.10, xaxt = 'n', ylim = c(0,3000))
boxplot(Cl~Treatment*Depth, data = y19.10, xaxt = 'n', ylim = c(0,3000))
boxplot(Cl~Treatment*Depth, data = y20.10, xaxt = 'n', ylim = c(0,3000))


boxplot(SM~Treatment*Depth, data = y18m.5, xaxt = 'n', ylim = c(0,50))
boxplot(SM~Treatment*Depth, data = y18j.5, xaxt = 'n', ylim = c(0,50))
boxplot(SM~Treatment*Depth, data = y19.5, xaxt = 'n', ylim = c(0,50))
boxplot(SM~Treatment*Depth, data = y20.5, xaxt = 'n', ylim = c(0,50))

boxplot(SM~Treatment*Depth, data = y18m.10, xaxt = 'n', ylim = c(0,50))
boxplot(SM~Treatment*Depth, data = y18j.10, xaxt = 'n', ylim = c(0,50))
boxplot(SM~Treatment*Depth, data = y19.10, xaxt = 'n', ylim = c(0,50))
boxplot(SM~Treatment*Depth, data = y20.10, xaxt = 'n', ylim = c(0,50))


