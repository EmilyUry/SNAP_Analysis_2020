

## Figure 1
#https://www.r-graph-gallery.com/96-boxplot-with-jitter.html

## boxplot with jigger

## all sites and all years combined 

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

library(FSA)

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

boxplot((DOC)~Treatment*xx, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 50),
        at = c(1,2,4,5,7,8,10,11), las = 1)
text(10.5, 42, "*", cex = 3)
legend("topleft", c("Control", "Salt"), pt.bg = c("#333333", "#E54C4C"),  pch = 22, pt.cex = 1.7, bty = 'n', ncol = 2)
mtext("DOC", 2, 3, outer = F, las =1, cex = 1.1)
mtext("mg/L", 2, 3, outer = F, las =1, cex = 0.8, adj = 1.1, padj = 2.2)

mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8, font = 2)

boxplot((DOC)~Treatment*xx, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 50),
        at = c(1,2,4,5,7,8,10,11))
text(10.5, 42, "*", cex = 3)
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8, font = 2)
#text(1.4, 2500, "*", cex = 3)




boxplot((carb)~Treatment*xx, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 0.2),
        at = c(1,2,4,5,7,8,10,11), las = 1)

mtext("C", 2, 3, outer = F, las =1, cex = 1.1, adj = 2.3)
mtext(expression("g/cm"^3), 2, 3, outer = F, las =1, cex = 0.8, adj = 1.2, padj = 1.5)
#text(1.4, 2500, "*", cex = 3)
boxplot((carb)~Treatment*xx, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 0.20),
        at = c(1,2,4,5,7,8,10,11))
#text(1.4, 2500, "*", cex = 3)


boxplot((Cmin_c)~Treatment*xx, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 18),
        at = c(4,5,7,8,10,11), xlim = c(0.5,11.5), las = 1)
mtext("Cmin", 2, 3, outer = F, las =1, cex = 1.1)
text(10.5, 12, "*", cex = 2)

mtext(expression(paste(mu,"gC/hr/gc")), 2,3, las = 1, cex =0.8, adj = 0.8, padj = 2)
#text(1.4, 2500, "*", cex = 3)
boxplot((Cmin_c)~Treatment*xx, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 18),
        at = c(4,5,7,8,10,11), xlim = c(0.5,11.5))
#text(1.4, 2500, "*", cex = 3)


boxplot((Phenol)~Treatment*xx, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 10),
        at = c(1,2,4,5,7,8,10,11), las = 1)
text(10.5, 8, "*", cex = 3)
axis(1, c(1.5, 4.5, 7.5, 10.5), c("2018\nMay", "2018\nJul", "2019\nJun", "2020\nAug"), tick = F)
mtext("Phenol", 2, 3, outer = F, las =1, cex = 1.1, adj = 0.8)
mtext("mg/L", 2,3, las = 1, cex =0.8, adj = 1, padj = 2)
#text(1.4, 2500, "*", cex = 3)
boxplot((Phenol)~Treatment*xx, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 10),
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




### FIgure 2a
par(mfrow = c(4,2),  oma = c(3,7,3,1), mar = c(0.4,0.2,0.4,0.2))

boxplot((DOC)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 50),
 las = 1)
points(jitter(rep(1,60), 10), x5$DOC[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$DOC[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
legend("topleft", c("Control", "Salt"), pt.bg = c("#333333", "#E54C4C"),  pch = 22, pt.cex = 1.7, bty = 'n', ncol = 2)
mtext("DOC", 2, 3, outer = F, las =1, cex = 1.1)
mtext("mg/L", 2, 3, outer = F, las =1, cex = 0.8, adj = 1.1, padj = 2.2)

mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8, font = 2)
boxplot((DOC)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 50))
points(jitter(rep(1,60), 10), x10$DOC[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$DOC[x10$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8, font = 2)


boxplot((carb)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 0.2), las = 1)
points(jitter(rep(1,60), 10), x5$carb[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$carb[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("C", 2, 3, outer = F, las =1, cex = 1.1, adj = 2.3)
mtext(expression("g/cm"^3), 2, 3, outer = F, las =1, cex = 0.8, adj = 1.2, padj = 1.5)
boxplot((carb)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 0.20))
points(jitter(rep(1,60), 10), x10$carb[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$carb[x10$Treatment == "Salt"], pch = 16, cex = 0.3)

boxplot((Cmin_c)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 18), las = 1)
points(jitter(rep(1,60), 10), x5$Cmin_c[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$Cmin_c[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("Cmin", 2, 3, outer = F, las =1, cex = 1.1)
#text(10.5, 12, "*", cex = 2)
mtext(expression(paste(mu,"gC/hr/gc")), 2,3, las = 1, cex =0.8, adj = 0.8, padj = 2)
#text(1.4, 2500, "*", cex = 3)
boxplot((Cmin_c)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 18))
#text(1.4, 2500, "*", cex = 3)
points(jitter(rep(1,60), 10), x10$Cmin_c[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$Cmin_c[x10$Treatment == "Salt"], pch = 16, cex = 0.3)

boxplot((Phenol)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 10),
         las = 1)
points(jitter(rep(1,60), 10), x5$Phenol[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$Phenol[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
#text(10.5, 8, "*", cex = 3)
#axis(1, c(1.5, 4.5, 7.5, 10.5), c("2018\nMay", "2018\nJul", "2019\nJun", "2020\nAug"), tick = F)
axis(1, c(1,2), c("Control", "Salt"))
mtext("Phenol", 2, 3, outer = F, las =1, cex = 1.1, adj = 0.8)
mtext("mg/L", 2,3, las = 1, cex =0.8, adj = 1, padj = 2)
#text(1.4, 2500, "*", cex = 3)
boxplot((Phenol)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 10))
points(jitter(rep(1,60), 10), x10$Phenol[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$Phenol[x10$Treatment == "Salt"], pch = 16, cex = 0.3)
text(10.5, 8, "*", cex = 3)
#text(1.4, 2500, "*", cex = 3)
axis(1, c(1,2), c("Control", "Salt"))


lm <- lm(DOC ~ Treatment, data = x5)
lm <- lm(DOC ~ Treatment, data = x10)

lm <- lm(carb ~ Treatment, data = x5)
lm <- lm(carb ~ Treatment, data = x10)

lm <- lm(Cmin_c ~ Treatment, data = x5)
lm <- lm(Cmin_c ~ Treatment, data = x10)

lm <- lm(Phenol ~ Treatment, data = x5)
lm <- lm(Phenol ~ Treatment, data = x10)


av <- aov(lm)
TukeyHSD(av)

###################################################################################################
###################################################################################################

## Figure 2c
x <- read.csv("SNAP_timeseries_data.csv", head = T)
head(x)
names(x) <- c( "Date", "Year", "Site", "Treatment", "Month", "Depth", "Core", "Salinity","Cond", "BD", 
               "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
               "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
               "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")
x$carb <- x$BD*x$LOI/100 ## grams carbon per cubic cm soil
x$Treatment <- as.factor(x$Treatment)
x$Depth <- as.factor(x$Depth)

tapply(x$SM, x$Site, quantile)

C1 <- x[which(x$Site == "1" & x$Depth == "(0-5)" & x$Treatment == "Control"),]
c1 <- x[which(x$Site == "1" & x$Depth == "(5-10)" & x$Treatment == "Control"),]
C3 <- x[which(x$Site == "3" & x$Depth == "(0-5)" & x$Treatment == "Control"),]
c3 <- x[which(x$Site == "3" & x$Depth == "(5-10)" & x$Treatment == "Control"),]
C5 <- x[which(x$Site == "5" & x$Depth == "(0-5)" & x$Treatment == "Control"),]
c5 <- x[which(x$Site == "5" & x$Depth == "(5-10)" & x$Treatment == "Control"),]


S1 <- x[which(x$Site == "1" & x$Depth == "(0-5)" & x$Treatment == "Salt"),]
s1 <- x[which(x$Site == "1" & x$Depth == "(5-10)" & x$Treatment == "Salt"),]
S3 <- x[which(x$Site == "3" & x$Depth == "(0-5)" & x$Treatment == "Salt"),]
s3 <- x[which(x$Site == "3" & x$Depth == "(5-10)" & x$Treatment == "Salt"),]
S5 <- x[which(x$Site == "5" & x$Depth == "(0-5)" & x$Treatment == "Salt"),]
s5 <- x[which(x$Site == "5" & x$Depth == "(5-10)" & x$Treatment == "Salt"),]



### DOC plot
{
        C1mean <- tapply(C1$DOC, C1$Month, mean)
        C1se <- tapply(C1$DOC, C1$Month, se)
        c1mean <- tapply(c1$DOC, C1$Month, mean)
        c1se <- tapply(c1$DOC, C1$Month, se)
        C3mean <- tapply(C3$DOC, C1$Month, mean)
        C3se <- tapply(C3$DOC, C1$Month, se)
        c3mean <- tapply(c3$DOC, C1$Month, mean)
        c3se <- tapply(c3$DOC, C1$Month, se)
        C5mean <- tapply(C5$DOC, C1$Month, mean)
        C5se <- tapply(C5$DOC, C1$Month, se)
        c5mean <- tapply(c5$DOC, C1$Month, mean)
        c5se <- tapply(c5$DOC, C1$Month, se)
        
        
        S1mean <- tapply(S1$DOC, S1$Month, mean)
        S1se <- tapply(S1$DOC, S1$Month, se)
        s1mean <- tapply(s1$DOC, s1$Month, mean)
        s1se <- tapply(s1$DOC, s1$Month, se)
        S3mean <- tapply(S3$DOC, S3$Month, mean)
        S3se <- tapply(S3$DOC, S3$Month, se)
        s3mean <- tapply(s3$DOC, s3$Month, mean)
        s3se <- tapply(s3$DOC, s3$Month, se)
        S5mean <- tapply(S5$DOC, S5$Month, mean)
        S5se <- tapply(S5$DOC, S5$Month, se)
        s5mean <- tapply(s5$DOC, s5$Month, mean)
        s5se <- tapply(s5$DOC, s5$Month, se)
        
        xx <- c(-11, 19, 21, 32, 46)
        xxs <- c(19, 21, 32, 46)
        par(mfrow = c(4,3),  oma = c(3,7,3,1), mar = c(0.2,0.2,0.2,0.2))
        plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(0,50), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
        mtext("Dry", 3, 1.2, cex = 1)
        mtext("Soil moisture: 20-26%", 3, 0.2, cex = 0.8)
        
        arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s1mean, xlim = c(-12, 47), ylim = c(0,50), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C1mean, xlim = c(-12, 47), ylim = c(0,50), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c1mean, xlim = c(-12, 47), ylim = c(0,50), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        
        
        plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(0,50), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
        mtext("Intermediate", 3, 1.2, cex = 1)
        mtext("Soil moisture: 28-32%", 3, 0.2, cex = 0.8)
        
        arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s3mean, xlim = c(-12, 47), ylim = c(0,50), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C3mean, xlim = c(-12, 47), ylim = c(0,50), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c3mean, xlim = c(-12, 47), ylim = c(0,50), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        
        
        plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(0,50), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
        mtext("Wet", 3, 1.2, cex = 1)
        mtext("Soil moisture: 26-36%", 3, 0.2, cex = 0.8)
        arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s5mean, xlim = c(-12, 47), ylim = c(0,50), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C5mean, xlim = c(-12, 47), ylim = c(0,50), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c5mean, xlim = c(-12, 47), ylim = c(0,50), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
}


### Carbon plot
{
        C1mean <- tapply(C1$carb, C1$Month, mean)
        C1se <- tapply(C1$carb, C1$Month, se)
        c1mean <- tapply(c1$carb, C1$Month, mean)
        c1se <- tapply(c1$carb, C1$Month, se)
        C3mean <- tapply(C3$carb, C1$Month, mean)
        C3se <- tapply(C3$carb, C1$Month, se)
        c3mean <- tapply(c3$carb, C1$Month, mean)
        c3se <- tapply(c3$carb, C1$Month, se)
        C5mean <- tapply(C5$carb, C1$Month, mean)
        C5se <- tapply(C5$carb, C1$Month, se)
        c5mean <- tapply(c5$carb, C1$Month, mean)
        c5se <- tapply(c5$carb, C1$Month, se)
        
        
        S1mean <- tapply(S1$carb, S1$Month, mean)
        S1se <- tapply(S1$carb, S1$Month, se)
        s1mean <- tapply(s1$carb, s1$Month, mean)
        s1se <- tapply(s1$carb, s1$Month, se)
        S3mean <- tapply(S3$carb, S3$Month, mean)
        S3se <- tapply(S3$carb, S3$Month, se)
        s3mean <- tapply(s3$carb, s3$Month, mean)
        s3se <- tapply(s3$carb, s3$Month, se)
        S5mean <- tapply(S5$carb, S5$Month, mean)
        S5se <- tapply(S5$carb, S5$Month, se)
        s5mean <- tapply(s5$carb, s5$Month, mean)
        s5se <- tapply(s5$carb, s5$Month, se)
        
        xx <- c(-11, 19, 21, 32, 46)
        xxs <- c(19, 21, 32, 46)
        #par(mfrow = c(3,3),  oma = c(3,7,3,1), mar = c(0.2,0.2,0.2,0.2))
        plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
        #mtext("Dry", 3, 1.2, cex = 1)
        #mtext("Soil moisture: 20-26%", 3, 0.2, cex = 0.8)
        
        arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s1mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C1mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c1mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        
        
        plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
        #mtext("Intermediate", 3, 1.2, cex = 1)
        #mtext("Soil moisture: 28-32%", 3, 0.2, cex = 0.8)
        
        arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s3mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C3mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c3mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        
        
        plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
        #mtext("Wet", 3, 1.2, cex = 1)
        #mtext("Soil moisture: 26-36%", 3, 0.2, cex = 0.8)
        arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s5mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C5mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c5mean, xlim = c(-12, 47), ylim = c(0,0.2), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
}

### Cmin plot
{
        C1mean <- tapply(C1$Cmin_c, C1$Month, mean)
        C1se <- tapply(C1$Cmin_c, C1$Month, se)
        c1mean <- tapply(c1$Cmin_c, C1$Month, mean)
        c1se <- tapply(c1$Cmin_c, C1$Month, se)
        C3mean <- tapply(C3$Cmin_c, C1$Month, mean)
        C3se <- tapply(C3$Cmin_c, C1$Month, se)
        c3mean <- tapply(c3$Cmin_c, C1$Month, mean)
        c3se <- tapply(c3$Cmin_c, C1$Month, se)
        C5mean <- tapply(C5$Cmin_c, C1$Month, mean)
        C5se <- tapply(C5$Cmin_c, C1$Month, se)
        c5mean <- tapply(c5$Cmin_c, C1$Month, mean)
        c5se <- tapply(c5$Cmin_c, C1$Month, se)
        
        
        S1mean <- tapply(S1$Cmin_c, S1$Month, mean)
        S1se <- tapply(S1$Cmin_c, S1$Month, se)
        s1mean <- tapply(s1$Cmin_c, s1$Month, mean)
        s1se <- tapply(s1$Cmin_c, s1$Month, se)
        S3mean <- tapply(S3$Cmin_c, S3$Month, mean)
        S3se <- tapply(S3$Cmin_c, S3$Month, se)
        s3mean <- tapply(s3$Cmin_c, s3$Month, mean)
        s3se <- tapply(s3$Cmin_c, s3$Month, se)
        S5mean <- tapply(S5$Cmin_c, S5$Month, mean)
        S5se <- tapply(S5$Cmin_c, S5$Month, se)
        s5mean <- tapply(s5$Cmin_c, s5$Month, mean)
        s5se <- tapply(s5$Cmin_c, s5$Month, se)
        
        xx <- c(-11, 19, 21, 32, 46)
        xxs <- c(19, 21, 32, 46)
        #par(mfrow = c(3,3),  oma = c(3,7,3,1), mar = c(0.2,0.2,0.2,0.2))
        plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(0,20), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
        #mtext("Dry", 3, 1.2, cex = 1)
        #mtext("Soil moisture: 20-26%", 3, 0.2, cex = 0.8)
        
        arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s1mean, xlim = c(-12, 47), ylim = c(0,20), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C1mean, xlim = c(-12, 47), ylim = c(0,20), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c1mean, xlim = c(-12, 47), ylim = c(0,20), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        
        
        plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(0,20), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
        #mtext("Intermediate", 3, 1.2, cex = 1)
        #mtext("Soil moisture: 28-32%", 3, 0.2, cex = 0.8)
        
        arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s3mean, xlim = c(-12, 47), ylim = c(0,20), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C3mean, xlim = c(-12, 47), ylim = c(0,20), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c3mean, xlim = c(-12, 47), ylim = c(0,20), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        
        
        plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(0,20), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
        #mtext("Wet", 3, 1.2, cex = 1)
        #mtext("Soil moisture: 26-36%", 3, 0.2, cex = 0.8)
        arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s5mean, xlim = c(-12, 47), ylim = c(0,20), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C5mean, xlim = c(-12, 47), ylim = c(0,20), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c5mean, xlim = c(-12, 47), ylim = c(0,20), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
}

### Phenol plot
{
        C1mean <- tapply(C1$Phenol, C1$Month, mean)
        C1se <- tapply(C1$Phenol, C1$Month, se)
        c1mean <- tapply(c1$Phenol, C1$Month, mean)
        c1se <- tapply(c1$Phenol, C1$Month, se)
        C3mean <- tapply(C3$Phenol, C1$Month, mean)
        C3se <- tapply(C3$Phenol, C1$Month, se)
        c3mean <- tapply(c3$Phenol, C1$Month, mean)
        c3se <- tapply(c3$Phenol, C1$Month, se)
        C5mean <- tapply(C5$Phenol, C1$Month, mean)
        C5se <- tapply(C5$Phenol, C1$Month, se)
        c5mean <- tapply(c5$Phenol, C1$Month, mean)
        c5se <- tapply(c5$Phenol, C1$Month, se)
        
        
        S1mean <- tapply(S1$Phenol, S1$Month, mean)
        S1se <- tapply(S1$Phenol, S1$Month, se)
        s1mean <- tapply(s1$Phenol, s1$Month, mean)
        s1se <- tapply(s1$Phenol, s1$Month, se)
        S3mean <- tapply(S3$Phenol, S3$Month, mean)
        S3se <- tapply(S3$Phenol, S3$Month, se)
        s3mean <- tapply(s3$Phenol, s3$Month, mean)
        s3se <- tapply(s3$Phenol, s3$Month, se)
        S5mean <- tapply(S5$Phenol, S5$Month, mean)
        S5se <- tapply(S5$Phenol, S5$Month, se)
        s5mean <- tapply(s5$Phenol, s5$Month, mean)
        s5se <- tapply(s5$Phenol, s5$Month, se)
        
        xx <- c(-11, 19, 21, 32, 46)
        xxs <- c(19, 21, 32, 46)
        
        plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(0,10), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
        arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s1mean, xlim = c(-12, 47), ylim = c(0,10), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C1mean, xlim = c(-12, 47), ylim = c(0,10), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c1mean, xlim = c(-12, 47), ylim = c(0,10), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        axis(1, at = c(-11,0, 20, 32,46), c("2015", "E", "2018", "2019", "2020"))
        #abline(v=0, lty =2)
        
        plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(0,10), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
        arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s3mean, xlim = c(-12, 47), ylim = c(0,10), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C3mean, xlim = c(-12, 47), ylim = c(0,10), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c3mean, xlim = c(-12, 47), ylim = c(0,10), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        axis(1, at = c(-11,0, 20, 32,46), c("2015", "E", "2018", "2019", "2020"))
        #abline(v=0, lty =2)
        
        plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(0,10), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
        legend("topleft", c("Control (0-5 cm)", "Control (5-10 cm)"), col = c("#333333", "#333333"), pch = c(16,21), bty = 'n')
        legend("topleft", c("Salt (0-5 cm)", "Salt (5-10 cm)"), col = c("#E54C4C", "#E54C4C"), pch = c(16,21), bty = 'n', inset= c(0.5,0))
        
        arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xxs, s5mean, xlim = c(-12, 47), ylim = c(0,10), pch = 21, cex = 2, col = "#E54C4C", type = "b")
        arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
        points(xx, C5mean, xlim = c(-12, 47), ylim = c(0,10), pch = 16, cex = 2, col = "#333333", type = "b")
        arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        points(xx, c5mean, xlim = c(-12, 47), ylim = c(0,10), pch = 21, cex = 2, col = "#333333", type = "b")
        arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
        axis(1, at = c(-11,0, 20, 32,46), c("2015", "E", "2018", "2019", "2020"))
        #abline(v=0, lty =2)
}
