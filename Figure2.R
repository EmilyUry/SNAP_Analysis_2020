

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
x$carb <- x$BD*x$LOI/100 ## grams carbon per cubic cm soil

x$Yr_mo <- c(rep("2020",60), rep("2019", 60), rep("2018 May", 60), rep("2018 Jul", 60))
x$xx <- c(rep(4,60), rep(3, 60), rep(1, 60), rep(2, 60))

x$xxx <- as.factor(x$xx)

# #outlier exclusion
# x <- x[-26,]
# x <- x[-21,]

x5 <- x[which(x$Depth == "(0-5)"),]
x10 <- x[which(x$Depth == "(5-10)"),]

## version 1 -- normal scales

par(mfrow = c(4,2),  oma = c(3,7,3,1), mar = c(0.4,0.2,0.4,0.2))

boxplot((DOC)~Treatment*xx, data = x5, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 50),
        at = c(1,2,4,5,7,8,10,11), las = 1)
text(10.5, 42, "*", cex = 3)
legend("topleft", c("Control", "Salt"), pt.bg = c("#143ee3bb", "#ff9b4aee"),  pch = 22, pt.cex = 1.7, bty = 'n', ncol = 2)
mtext("DOC", 2, 3, outer = F, las =1, cex = 1.1)
mtext("mg/L", 2, 3, outer = F, las =1, cex = 0.8, adj = 1.1, padj = 2.2)

mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8, font = 2)

boxplot((DOC)~Treatment*xx, data = x10, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 50),
        at = c(1,2,4,5,7,8,10,11))
text(10.5, 42, "*", cex = 3)
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8, font = 2)
#text(1.4, 2500, "*", cex = 3)




boxplot((carb)~Treatment*xx, data = x5, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 0.2),
        at = c(1,2,4,5,7,8,10,11), las = 1)
mtext("C", 2, 3, outer = F, las =1, cex = 1.1, adj = 2.3)
mtext(expression("g/cm"^3), 2, 3, outer = F, las =1, cex = 0.8, adj = 1.2, padj = 1.5)
#text(1.4, 2500, "*", cex = 3)
boxplot((carb)~Treatment*xx, data = x10, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 0.20),
        at = c(1,2,4,5,7,8,10,11))
#text(1.4, 2500, "*", cex = 3)


boxplot((Cmin_c)~Treatment*xx, data = x5, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 18),
        at = c(4,5,7,8,10,11), xlim = c(0.5,11.5), las = 1)
mtext("Cmin", 2, 3, outer = F, las =1, cex = 1.1)
mtext(expression(paste(mu,"gC/hr/gc")), 2,3, las = 1, cex =0.8, adj = 0.8, padj = 2)
#text(1.4, 2500, "*", cex = 3)
boxplot((Cmin_c)~Treatment*xx, data = x10, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 18),
        at = c(4,5,7,8,10,11), xlim = c(0.5,11.5))
#text(1.4, 2500, "*", cex = 3)


boxplot((Phenol)~Treatment*xx, data = x5, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 10),
        at = c(1,2,4,5,7,8,10,11), las = 1)
text(10.5, 8, "*", cex = 3)
axis(1, c(1.5, 4.5, 7.5, 10.5), c("2018\nMay", "2018\nJul", "2019\nJun", "2020\nAug"), tick = F)
mtext("Phenol", 2, 3, outer = F, las =1, cex = 1.1, adj = 0.8)
mtext("mg/L", 2,3, las = 1, cex =0.8, adj = 1, padj = 2)
#text(1.4, 2500, "*", cex = 3)
boxplot((Phenol)~Treatment*xx, data = x10, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 10),
        at = c(1,2,4,5,7,8,10,11))
text(10.5, 8, "*", cex = 3)
#text(1.4, 2500, "*", cex = 3)

axis(1, c(1.5, 4.5, 7.5, 10.5), c("2018\nMay", "2018\nJul", "2019\nJun", "2020\nAug"), tick = F)





### signficance test



lm <- lm(DOC ~ Treatment*xxx, data = x5)
lm <- lm(DOC ~ Treatment*xxx, data = x10)

lm <- lm(carb ~ Treatment*xxx, data = x5)
lm <- lm(carb ~ Treatment*xxx, data = x10)

lm <- lm(Cmin_c ~ Treatment*xxx, data = x5)
lm <- lm(Cmin_c ~ Treatment*xxx, data = x10)

lm <- lm(Phenol ~ Treatment*xxx, data = x5)
lm <- lm(Phenol ~ Treatment*xxx, data = x10)


av <- aov(lm)

TukeyHSD(av)


## sig dif


## cmin (0-5) Salt 2020 - control 2020 p = 0.0667

# phenol (0-5) Salt 2020 - control 2020 p = 0.0171
# phenol (5-10) Salt 2020 - control 2020 p = 0.000929



### the proportion of DOC that is phenolic is decreasing over time

# boxplot((Phenol/DOC)~Treatment*xx, data = x5, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 0.5),
#         at = c(1,2,4,5,7,8,10,11), las = 1)
# axis(1, c(1.5, 4.5, 7.5, 10.5), c("2018\nMay", "2018\nJul", "2019\nJun", "2020\nAug"), tick = F)
# mtext("Phenol", 2, 3, outer = F, las =1, cex = 1.1, adj = 0.8)
# mtext("mg/L", 2,3, las = 1, cex =0.8, adj = 1, padj = 2)
# #text(1.4, 2500, "*", cex = 3)
# boxplot((Phenol/DOC)~Treatment*xx, data = x10, col = c("#143ee3bb", "#ff9b4aee"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 0.5),
#         at = c(1,2,4,5,7,8,10,11))


     