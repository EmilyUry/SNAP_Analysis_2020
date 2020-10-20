

## Figure 1 update

## color scheme <- black #333333 or #191919 and red #CD3333 or #E54C4C


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


######################################################
#########                                 ############
#########               A                 ############
#########                                 ############
######################################################


par(mfrow = c(3,2), oma = c(3,7,3,1), mar = c(0.2,0.2,0.2,0.2))

boxplot((Cl)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0, 3000))
points(jitter(rep(1,60), 10), x5$Cl[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$Cl[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("Cl-", 2, 3, outer = F, las =1, cex = 1.2)
mtext("Depth: 0-5 cm", 3, 0.5, cex = 0.8)
text(1.4, 2500, "*", cex = 3)
boxplot((Cl)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0, 3000))
points(jitter(rep(1,60), 10), x10$Cl[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$Cl[x10$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("Depth: 5-10 cm", 3, 0.5, cex = 0.8)
text(1.4, 2500, "*", cex = 3)

boxplot((pH)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(3,8))
points(jitter(rep(1,60), 10), x5$pH[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$pH[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext("pH", 2, 3.3, outer = F, las =1, cex = 1.2)
boxplot((pH)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(3,8))
points(jitter(rep(1,60), 10), x10$pH[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$pH[x10$Treatment == "Salt"], pch = 16, cex = 0.3)

boxplot((SO4)~Treatment, data = x5, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', ylim = c(0,600))
points(jitter(rep(1,60), 10), x5$SO4[x5$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x5$SO4[x5$Treatment == "Salt"], pch = 16, cex = 0.3)
mtext( expression('SO'[4]^'2-'), 2, 2.5, outer = F, las =1, cex = 1.2)
text(1.4, 500, "*", cex = 3)
axis(1, c(1,2), c("Control", "Salt"), cex.axis = 1.5)
boxplot((SO4)~Treatment, data = x10, col = c("#333333", "#E54C4C"), xlab = " ", ylab = " ", xaxt = 'n', yaxt = 'n', ylim = c(0,600))
points(jitter(rep(1,60), 10), x10$SO4[x10$Treatment == "Control"], pch = 16, cex = 0.3)
points(jitter(rep(2,60), 5), x10$SO4[x10$Treatment == "Salt"], pch = 16, cex = 0.3)
text(1.4, 500, "*", cex = 3)
axis(1, c(1,2), c("Control", "Salt"), cex.axis =1.5)






######################################################
#########                                 ############
#########               B                 ############
#########                                 ############
######################################################

x <- read.csv("SNAP_timeseries_data.csv", head = T)
head(x)
names(x) <- c( "Date", "Year", "Site", "Treatment", "Month", "Depth", "Core", "Salinity","Cond", "BD", 
               "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
               "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
               "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")
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



### Chloride plot
{
C1mean <- tapply(C1$Cl, C1$Month, mean)
C1se <- tapply(C1$Cl, C1$Month, se)
c1mean <- tapply(c1$Cl, C1$Month, mean)
c1se <- tapply(c1$Cl, C1$Month, se)
C3mean <- tapply(C3$Cl, C1$Month, mean)
C3se <- tapply(C3$Cl, C1$Month, se)
c3mean <- tapply(c3$Cl, C1$Month, mean)
c3se <- tapply(c3$Cl, C1$Month, se)
C5mean <- tapply(C5$Cl, C1$Month, mean)
C5se <- tapply(C5$Cl, C1$Month, se)
c5mean <- tapply(c5$Cl, C1$Month, mean)
c5se <- tapply(c5$Cl, C1$Month, se)


S1mean <- tapply(S1$Cl, S1$Month, mean)
S1se <- tapply(S1$Cl, S1$Month, se)
s1mean <- tapply(s1$Cl, s1$Month, mean)
s1se <- tapply(s1$Cl, s1$Month, se)
S3mean <- tapply(S3$Cl, S3$Month, mean)
S3se <- tapply(S3$Cl, S3$Month, se)
s3mean <- tapply(s3$Cl, s3$Month, mean)
s3se <- tapply(s3$Cl, s3$Month, se)
S5mean <- tapply(S5$Cl, S5$Month, mean)
S5se <- tapply(S5$Cl, S5$Month, se)
s5mean <- tapply(s5$Cl, s5$Month, mean)
s5se <- tapply(s5$Cl, s5$Month, se)

xx <- c(-11, 19, 21, 32, 46)
xxs <- c(19, 21, 32, 46)
par(mfrow = c(3,3),  oma = c(3,7,3,1), mar = c(0.2,0.2,0.2,0.2))
plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
mtext("Dry", 3, 1.2, cex = 1)
mtext("Soil moisture: 20-26%", 3, 0.2, cex = 0.8)

arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xxs, s1mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 21, cex = 2, col = "#E54C4C", type = "b")
arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xx, C1mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 16, cex = 2, col = "#333333", type = "b")
arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
points(xx, c1mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 21, cex = 2, col = "#333333", type = "b")
arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )


plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
mtext("Intermediate", 3, 1.2, cex = 1)
mtext("Soil moisture: 28-32%", 3, 0.2, cex = 0.8)

arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xxs, s3mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 21, cex = 2, col = "#E54C4C", type = "b")
arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xx, C3mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 16, cex = 2, col = "#333333", type = "b")
arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
points(xx, c3mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 21, cex = 2, col = "#333333", type = "b")
arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )


plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
mtext("Wet", 3, 1.2, cex = 1)
mtext("Soil moisture: 26-36%", 3, 0.2, cex = 0.8)
arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xxs, s5mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 21, cex = 2, col = "#E54C4C", type = "b")
arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xx, C5mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 16, cex = 2, col = "#333333", type = "b")
arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
points(xx, c5mean, xlim = c(-12, 47), ylim = c(0,3000), pch = 21, cex = 2, col = "#333333", type = "b")
arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
}

###pH plot
{
C1mean <- tapply(C1$pH, C1$Month, mean)
C1se <- tapply(C1$pH, C1$Month, se)
c1mean <- tapply(c1$pH, C1$Month, mean)
c1se <- tapply(c1$pH, C1$Month, se)
C3mean <- tapply(C3$pH, C1$Month, mean)
C3se <- tapply(C3$pH, C1$Month, se)
c3mean <- tapply(c3$pH, C1$Month, mean)
c3se <- tapply(c3$pH, C1$Month, se)
C5mean <- tapply(C5$pH, C1$Month, mean)
C5se <- tapply(C5$pH, C1$Month, se)
c5mean <- tapply(c5$pH, C1$Month, mean)
c5se <- tapply(c5$pH, C1$Month, se)


S1mean <- tapply(S1$pH, S1$Month, mean)
S1se <- tapply(S1$pH, S1$Month, se)
s1mean <- tapply(s1$pH, s1$Month, mean)
s1se <- tapply(s1$pH, s1$Month, se)
S3mean <- tapply(S3$pH, S3$Month, mean)
S3se <- tapply(S3$pH, S3$Month, se)
s3mean <- tapply(s3$pH, s3$Month, mean)
s3se <- tapply(s3$pH, s3$Month, se)
S5mean <- tapply(S5$pH, S5$Month, mean)
S5se <- tapply(S5$pH, S5$Month, se)
s5mean <- tapply(s5$pH, s5$Month, mean)
s5se <- tapply(s5$pH, s5$Month, se)

xx <- c(-11, 19, 21, 32, 46)
xxs <- c(19, 21, 32, 46)

plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(3,7), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xxs, s1mean, xlim = c(-12, 47), ylim = c(3,7), pch = 21, cex = 2, col = "#E54C4C", type = "b")
arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xx, C1mean, xlim = c(-12, 47), ylim = c(3,7), pch = 16, cex = 2, col = "#333333", type = "b")
arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
points(xx, c1mean, xlim = c(-12, 47), ylim = c(3,7), pch = 21, cex = 2, col = "#333333", type = "b")
arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )


plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(3,7), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xxs, s3mean, xlim = c(-12, 47), ylim = c(3,7), pch = 21, cex = 2, col = "#E54C4C", type = "b")
arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xx, C3mean, xlim = c(-12, 47), ylim = c(3,7), pch = 16, cex = 2, col = "#333333", type = "b")
arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
points(xx, c3mean, xlim = c(-12, 47), ylim = c(3,7), pch = 21, cex = 2, col = "#333333", type = "b")
arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )


plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(3,7), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xxs, s5mean, xlim = c(-12, 47), ylim = c(3,7), pch = 21, cex = 2, col = "#E54C4C", type = "b")
arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
points(xx, C5mean, xlim = c(-12, 47), ylim = c(3,7), pch = 16, cex = 2, col = "#333333", type = "b")
arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
points(xx, c5mean, xlim = c(-12, 47), ylim = c(3,7), pch = 21, cex = 2, col = "#333333", type = "b")
arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
}


### SO4 plot
{
  C1mean <- tapply(C1$SO4, C1$Month, mean)
  C1se <- tapply(C1$SO4, C1$Month, se)
  c1mean <- tapply(c1$SO4, C1$Month, mean)
  c1se <- tapply(c1$SO4, C1$Month, se)
  C3mean <- tapply(C3$SO4, C1$Month, mean)
  C3se <- tapply(C3$SO4, C1$Month, se)
  c3mean <- tapply(c3$SO4, C1$Month, mean)
  c3se <- tapply(c3$SO4, C1$Month, se)
  C5mean <- tapply(C5$SO4, C1$Month, mean)
  C5se <- tapply(C5$SO4, C1$Month, se)
  c5mean <- tapply(c5$SO4, C1$Month, mean)
  c5se <- tapply(c5$SO4, C1$Month, se)
  
  
  S1mean <- tapply(S1$SO4, S1$Month, mean)
  S1se <- tapply(S1$SO4, S1$Month, se)
  s1mean <- tapply(s1$SO4, s1$Month, mean)
  s1se <- tapply(s1$SO4, s1$Month, se)
  S3mean <- tapply(S3$SO4, S3$Month, mean)
  S3se <- tapply(S3$SO4, S3$Month, se)
  s3mean <- tapply(s3$SO4, s3$Month, mean)
  s3se <- tapply(s3$SO4, s3$Month, se)
  S5mean <- tapply(S5$SO4, S5$Month, mean)
  S5se <- tapply(S5$SO4, S5$Month, se)
  s5mean <- tapply(s5$SO4, s5$Month, mean)
  s5se <- tapply(s5$SO4, s5$Month, se)
  
  xx <- c(-11, 19, 21, 32, 46)
  xxs <- c(19, 21, 32, 46)
  
  plot(xxs, S1mean, xlim = c(-12, 47), ylim = c(0,600), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n')
  arrows(xxs, S1mean-1.96*S1se, xxs, S1mean+1.96*S1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
  points(xxs, s1mean, xlim = c(-12, 47), ylim = c(0,600), pch = 21, cex = 2, col = "#E54C4C", type = "b")
  arrows(xxs, s1mean-1.96*s1se, xxs, s1mean+1.96*s1se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
  points(xx, C1mean, xlim = c(-12, 47), ylim = c(0,600), pch = 16, cex = 2, col = "#333333", type = "b")
  arrows(xx, C1mean-1.96*C1se, xx, C1mean+1.96*C1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
  points(xx, c1mean, xlim = c(-12, 47), ylim = c(0,600), pch = 21, cex = 2, col = "#333333", type = "b")
  arrows(xx, c1mean-1.96*c1se, xx, c1mean+1.96*c1se, angle = 90, length = 0.05, code = 3, col = "#333333" )
  axis(1, at = c(-11,0, 20, 32,46), c("2015", "E", "2018", "2019", "2020"))
  #abline(v=0, lty =2)
  
  plot(xxs, S3mean, xlim = c(-12, 47), ylim = c(0,600), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = 'n')
  arrows(xxs, S3mean-1.96*S3se, xxs, S3mean+1.96*S3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
  points(xxs, s3mean, xlim = c(-12, 47), ylim = c(0,600), pch = 21, cex = 2, col = "#E54C4C", type = "b")
  arrows(xxs, s3mean-1.96*s3se, xxs, s3mean+1.96*s3se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
  points(xx, C3mean, xlim = c(-12, 47), ylim = c(0,600), pch = 16, cex = 2, col = "#333333", type = "b")
  arrows(xx, C3mean-1.96*C3se, xx, C3mean+1.96*C3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
  points(xx, c3mean, xlim = c(-12, 47), ylim = c(0,600), pch = 21, cex = 2, col = "#333333", type = "b")
  arrows(xx, c3mean-1.96*c3se, xx, c3mean+1.96*c3se, angle = 90, length = 0.05, code = 3, col = "#333333" )
  axis(1, at = c(-11,0, 20, 32,46), c("2015", "E", "2018", "2019", "2020"))
  #abline(v=0, lty =2)
  
  plot(xxs, S5mean, xlim = c(-12, 47), ylim = c(0,600), pch = 16, cex = 2, col = "#E54C4C", type = "b", xaxt = 'n', yaxt = "n")
  legend("topleft", c("Control (0-5 cm)", "Control (5-10 cm)"), col = c("#333333", "#333333"), pch = c(16,21), bty = 'n')
  legend("topleft", c("Salt (0-5 cm)", "Salt (5-10 cm)"), col = c("#E54C4C", "#E54C4C"), pch = c(16,21), bty = 'n', inset= c(0.5,0))
  
  arrows(xxs, S5mean-1.96*S5se, xxs, S5mean+1.96*S5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
  points(xxs, s5mean, xlim = c(-12, 47), ylim = c(0,600), pch = 21, cex = 2, col = "#E54C4C", type = "b")
  arrows(xxs, s5mean-1.96*s5se, xxs, s5mean+1.96*s5se, angle = 90, length = 0.05, code = 3, col = "#E54C4C" )
  points(xx, C5mean, xlim = c(-12, 47), ylim = c(0,600), pch = 16, cex = 2, col = "#333333", type = "b")
  arrows(xx, C5mean-1.96*C5se, xx, C5mean+1.96*C5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
  points(xx, c5mean, xlim = c(-12, 47), ylim = c(0,600), pch = 21, cex = 2, col = "#333333", type = "b")
  arrows(xx, c5mean-1.96*c5se, xx, c5mean+1.96*c5se, angle = 90, length = 0.05, code = 3, col = "#333333" )
  axis(1, at = c(-11,0, 20, 32,46), c("2015", "E", "2018", "2019", "2020"))
  #abline(v=0, lty =2)
}










## Cl
a1 <- mean(y$var[y$Depth == "(0-5)" & y$Treatment == "Salt"])
a1 <- mean(y$var[y$Depth == "(0-5)" & y$Treatment == "Control"])
C <- mean(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Control"])
S.sd <- sd(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Salt"])
C.sd <- sd(y$var[y$Site == "1" & y$Depth == "(0-5)" & y$Treatment == "Control"])



### CI formula: mean(x)-1.96*se(x), mean(x)+1.96*se(x)
S1<- mean



par(mfrow = c(3,3),  oma = c(3,7,3,1), mar = c(0.4,0.4,0.4,0.4))
plot(a$Month, a$Cl, ylim = c(0,2900), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
abline(v=0, lty =2)
mtext("Cl-", 2, 3, outer = F, las =1, cex = 1.1, adj = 1.6)
mtext("ug/g soil", 2, 3, outer = F, las =1, cex = 0.8, adj = 1, padj = 2)
mtext("Dry", 3, 1, cex = 1)
plot(b$Month, b$Cl, ylim = c(0,2900), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
abline(v=0, lty =2)
mtext("Intermediate", 3, 1, cex = 1)
plot(c$Month, c$Cl, ylim = c(0,2900), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
abline(v=0, lty =2)
mtext("Wet", 3, 1, cex = 1)



plot(a$Month, a$pH, ylim = c(3,7), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
abline(v=0, lty =2)
mtext("pH", 2, 3, outer = F, las =1, cex = 1.1, adj = 1.6)
plot(b$Month, b$pH, ylim = c(3,7), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
abline(v=0, lty =2)
plot(c$Month, c$pH, ylim = c(3,7), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
abline(v=0, lty =2)


plot(a$Month, a$DOC, ylim = c(0,40), xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
axis(1, at = c(-11, 20, 32,46), c("2015", "2018", "2019", "2020"))
abline(v=0, lty =2)
mtext("DOC", 2, 1, outer = F, las =1, cex = 1.1, adj = 1.8)
mtext("mg/L", 2, 3, outer = F, las =1, cex = 0.8, adj = 1.3, padj = 2)
plot(b$Month, b$DOC, ylim = c(0,40), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
axis(1, at = c(-11, 20, 32,46), c("2015", "2018", "2019", "2020"))
abline(v=0, lty =2)
plot(c$Month, c$DOC, ylim = c(0,40), yaxt = 'n', xaxt = 'n', pch = c(19,21)[a$Depth], col = c("#333333", "#E54C4C")[a$Treatment])
axis(1, at = c(-11, 20, 32,46), c("2015", "2018", "2019", "2020"))
abline(v=0, lty =2)

