
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

# #outlier exclusion
# x <- x[-26,]
# x <- x[-21,]

x5 <- x[which(x$Depth == "(0-5)"),]
x10 <- x[which(x$Depth == "(5-10)"),]

## version 1 -- normal scales

par(mfrow = c(3,2), oma = c(1,7,1,1), mar = c(1,0.2,1,0.2))

boxplot((Cl)~Treatment, data = x5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 3000))
mtext("Cl-", 2, 3, outer = F, las =1, cex = 1.2)
mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8, font = 2)
text(1.4, 2500, "*", cex = 3)
boxplot((Cl)~Treatment, data = x10, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 3000))
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8, font = 2)
text(1.4, 2500, "*", cex = 3)

boxplot((pH)~Treatment, data = x5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(3,8))
mtext("pH", 2, 3.3, outer = F, las =1, cex = 1.2)
boxplot((pH)~Treatment, data = x10, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(3,8))


boxplot((SO4)~Treatment, data = x5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0,600))
mtext( expression('SO'[4]^'2-'), 2, 2.5, outer = F, las =1, cex = 1.2)
text(1.4, 500, "*", cex = 3)
boxplot((SO4)~Treatment, data = x10, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0,600))
text(1.4, 500, "*", cex = 3)



### significance testing
library(effsize)
y <- x
y$var <- y$pH 
cohen.d(y$var[y$Depth == "(0-5)" & y$Treatment == "Salt"], 
        y$var[y$Depth == "(0-5)" & y$Treatment == "Control"],
        conf.level = 0.95,
        hedges.correction = T)
cohen.d(y$var[y$Depth == "(5-10)" & y$Treatment == "Salt"], 
        y$var[y$Depth == "(5-10)" & y$Treatment == "Control"],
        conf.level = 0.95,
        hedges.correction = T)

## cl <-- significant effect
## pH  <-- not signifacant
## SO4 <-- significant effect



###version 2 --- log scales

par(mfrow = c(3,2), oma = c(1,7,1,1), mar = c(1,0.2,1,0.2))

boxplot(log10(Cl)~Treatment, data = x5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 4))
mtext("log\nCl-", 2, 3, outer = F, las =1, cex = 1.2)
mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8, font = 2)
boxplot(log10(Cl)~Treatment, data = x10, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 4))
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8, font = 2)


boxplot((pH)~Treatment, data = x5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(3, 8))
mtext("pH", 2, 3.3, outer = F, las =1, cex = 1.2)
boxplot((pH)~Treatment, data = x10, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(3, 8))


boxplot(log10(SO4)~Treatment, data = x5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 4))
mtext( expression('log\nSO'[4]^'2-'), 2, 2.5, outer = F, las =1, cex = 1.2)
boxplot(log10(SO4)~Treatment, data = x10, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 4))



### version 3 -- with points jittered!

par(mfrow = c(3,2), oma = c(3,7,3,1), mar = c(0.2,0.2,0.2,0.2))

boxplot((Cl)~Treatment, data = x5, col = c("#143ee3bb", "#ff9b4a90"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 3000))
points(jitter(rep(1,60), 10), x5$Cl[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$Cl[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("Cl-", 2, 3, outer = F, las =1, cex = 1.2)
mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8)
text(1.4, 2500, "*", cex = 3)
boxplot((Cl)~Treatment, data = x10, col = c("#143ee3bb", "#ff9b4a90"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 3000))
points(jitter(rep(1,60), 10), x10$Cl[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$Cl[x10$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8)
text(1.4, 2500, "*", cex = 3)

boxplot((pH)~Treatment, data = x5, col = c("#143ee3bb", "#ff9b4a90"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(3,8))
points(jitter(rep(1,60), 10), x5$pH[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$pH[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("pH", 2, 3.3, outer = F, las =1, cex = 1.2)
boxplot((pH)~Treatment, data = x10, col = c("#143ee3bb", "#ff9b4a90"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(3,8))
points(jitter(rep(1,60), 10), x10$pH[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$pH[x10$Treatment == "Salt"], pch = 16, cex = 0.3)

boxplot((SO4)~Treatment, data = x5, col = c("#143ee3bb", "#ff9b4a90"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0,600))
points(jitter(rep(1,60), 10), x5$SO4[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$SO4[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext( expression('SO'[4]^'2-'), 2, 2.5, outer = F, las =1, cex = 1.2)
text(1.4, 500, "*", cex = 3)
axis(1, c(1,2), c("Control", "Salt"), cex.axis = 1.5)
boxplot((SO4)~Treatment, data = x10, col = c("#143ee3bb", "#ff9b4a90"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0,600))
points(jitter(rep(1,60), 10), x10$SO4[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$SO4[x10$Treatment == "Salt"], pch = 16, cex = 0.3)
text(1.4, 500, "*", cex = 3)
axis(1, c(1,2), c("Control", "Salt"), cex.axis =1.5)


