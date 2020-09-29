

#' ---
#' title: " 2019-2020 SNAP Data Analysis"
#' author: "Emily Ury"
#' date: "Sept 11, 2020"
#' output: github_document
#' ---
#'
#'
#'
#'
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

x <- read.csv("2020_SNAP_master.csv", head = T)
head(x)
names(x) <- c("Date","Site", "Treatment", "Depth", "Core", "Salinity", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")


lX<-log(x[,c(14:19, 22, 23, 32)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$Treatment <- as.factor(x$Treatment)
x$Depth <- as.factor(x$Depth)



y <- read.csv("2019_SNAP_master.csv", head = T)
head(y)
names(y) <- c("Date","Site", "Treatment", "Core", "Depth", "Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4")


ly<-log(x[,c(14:19, 22, 23, 32)])
colnames(ly)<-paste("log",colnames(ly),sep="")
y<-cbind(y,ly); rm(ly)

# y$Treatment <- as.factor(y$Treatment)
# y$Depth <- as.factor(y$Depth)

d20 <- x; rm(x)
d19 <- y; rm(y)

d19 <- d19[which(d19$Treatment== "Salt" | d19$Treatment == "Control"),]



d20.0 <- d20[which(d20$Depth== "(0-5)"),]
d20.5 <- d20[which(d20$Depth != "(0-5)"),]

d19.0 <- d19[which(d19$Depth== "(0-5)"),]
d19.5 <- d19[which(d19$Depth != "(0-5)"),]


### FIGURE 1 (LOI -- all plots together)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(3, 6, 4, 0.5))

boxplot(LOI~Treatment, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 18), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
mtext("2019", side = 3, line = 0.5, font = 2)
boxplot(LOI~Treatment, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 18), yaxt ='n', xaxt='n')
mtext("2020", side = 3, line = 0.5, font = 2)
boxplot(LOI~Treatment, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 18))
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
boxplot(LOI~Treatment, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 18), yaxt = 'n')
mtext("Carbon (% loss on ignition)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)



### FIGURE 2 (DOC -- all plots together)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(3, 6, 4, 0.5))

boxplot(DOC~Treatment, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
mtext("2019", side = 3, line = 0.5, font = 2)
boxplot(DOC~Treatment, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), yaxt ='n', xaxt='n')
mtext("2020", side = 3, line = 0.5, font = 2)
boxplot(DOC~Treatment, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40))
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
boxplot(LOI~Treatment, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), yaxt = 'n')
mtext("DOC (mg/L)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)



### FIGURE 3 (CMIN (per gram carbon)-- all plots together)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(3, 6, 4, 0.5))

boxplot(Cmin_c~Treatment, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
mtext("2019", side = 3, line = 0.5, font = 2)
boxplot(Cmin_c~Treatment, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), yaxt ='n', xaxt='n')
mtext("2020", side = 3, line = 0.5, font = 2)
boxplot(Cmin_c~Treatment, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8))
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
boxplot(Cmin_c~Treatment, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), yaxt = 'n')
mtext("C-min (ugC-Co2/hr/gramC)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)



### FIGURE 3alternate (CMIN (per gram dry soil)-- all plots together)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(3, 6, 4, 0.5))

boxplot(Cmin_s~Treatment, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 1.2), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
mtext("2019", side = 3, line = 0.5, font = 2)
boxplot(Cmin_s~Treatment, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 1.2), yaxt ='n', xaxt='n')
mtext("2020", side = 3, line = 0.5, font = 2)
boxplot(Cmin_s~Treatment, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 1.2))
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
boxplot(Cmin_s~Treatment, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 1.2), yaxt = 'n')
mtext("C-min (ugC-Co2/hr/gds)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)







### FIGURE 4 (LOI -- by Site)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.2), oma = c(3, 6, 4, 0.5))
labs <- c("Control", "Salt", "Control", "Salt", "Control", "Salt")

boxplot(LOI~Treatment*Site, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 20), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
abline(v=2.5); abline(v=4.5)
text(1.5, 19, "Dry", font = 4); text(3.5, 19, "Intermed", font = 4); text(5.5, 19, "Wet", font = 4)
mtext("2019", side = 3, line = 0.5, font = 2)

boxplot(LOI~Treatment*Site, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 20), yaxt ='n', xaxt='n')
abline(v=2.5); abline(v=4.5)
text(1.5, 19, "Dry", font = 4); text(3.5, 19, "Intermed", font = 4); text(5.5, 19, "Wet", font = 4)
mtext("2020", side = 3, line = 0.5, font = 2)

boxplot(LOI~Treatment*Site, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 20), xaxt = 'n')
axis(side = 1, at = 1:6, labels = labs)
abline(v=2.5); abline(v=4.5)
text(1.5, 19, "Dry", font = 4); text(3.5, 19, "Intermed", font = 4); text(5.5, 19, "Wet", font = 4)
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)

boxplot(LOI~Treatment*Site, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 20), yaxt = 'n', xaxt = 'n')
axis(side = 1, at = 1:6, labels = labs)
abline(v=2.5); abline(v=4.5)
text(1.5, 19, "Dry", font = 4); text(3.5, 19, "Intermed", font = 4); text(5.5, 19, "Wet", font = 4)
mtext("Carbon (% loss on ignition)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)



### FIGURE 5 (DOC -- by Site)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.4), oma = c(3, 6, 4, 0.5))
labs <- c("Control", "Salt", "Control", "Salt", "Control", "Salt")

boxplot(DOC~Treatment*Site, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 40, "Dry", font = 4, col = "gray50"); text(3.5, 40, "Intermed", font = 4, col = "gray50"); text(5.5, 40, "Wet", font = 4, col = "gray50")
mtext("2019", side = 3, line = 0.5, font = 2)

boxplot(DOC~Treatment*Site, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), yaxt ='n', xaxt='n')
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 40, "Dry", font = 4, col = "gray50"); text(3.5, 40, "Intermed", font = 4, col = "gray50"); text(5.5, 40, "Wet", font = 4, col = "gray50")
mtext("2020", side = 3, line = 0.5, font = 2)

boxplot(DOC~Treatment*Site, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), xaxt = 'n')
axis(side = 1, at = 1:6, labels = labs)
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 40, "Dry", font = 4, col = "gray50"); text(3.5, 40, "Intermed", font = 4, col = "gray50"); text(5.5, 40, "Wet", font = 4, col = "gray50")
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)

boxplot(DOC~Treatment*Site, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 40), yaxt = 'n', xaxt = 'n')
axis(side = 1, at = 1:6, labels = labs)
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 40, "Dry", font = 4, col = "gray50"); text(3.5, 40, "Intermed", font = 4, col = "gray50"); text(5.5, 40, "Wet", font = 4, col = "gray50")
mtext("DOC (mg/L)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)


### FIGURE 6 (CMIN (per gram carbon)-- all plots together)

par(mfrow = c(2,2), mar = c(0.2,0.2,0.2,0.4), oma = c(3, 6, 4, 0.5))
labs <- c("Control", "Salt", "Control", "Salt", "Control", "Salt")

boxplot(Cmin_c~Treatment*Site, data = d19.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), xaxt = 'n')
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 8, "Dry", font = 4, col = "gray50"); text(3.5, 8, "Intermed", font = 4, col = "gray50"); text(5.5, 8, "Wet", font = 4, col = "gray50")
mtext("2019", side = 3, line = 0.5, font = 2)

boxplot(Cmin_c~Treatment*Site, data = d20.0, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), yaxt ='n', xaxt='n')
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 8, "Dry", font = 4, col = "gray50"); text(3.5, 8, "Intermed", font = 4, col = "gray50"); text(5.5, 8, "Wet", font = 4, col = "gray50")
mtext("2020", side = 3, line = 0.5, font = 2)

boxplot(Cmin_c~Treatment*Site, data = d19.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), xaxt = 'n')
axis(side = 1, at = 1:6, labels = labs)
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 8, "Dry", font = 4, col = "gray50"); text(3.5, 8, "Intermed", font = 4, col = "gray50"); text(5.5, 8, "Wet", font = 4, col = "gray50")
mtext("Depth \n 0-5 cm", side = 2, line = 2.5, las = 1, font = 2)

boxplot(Cmin_c~Treatment*Site, data = d20.5, col = c("dodgerblue", "tomato"), xlab = " ", ylab = " ", ylim = c(0, 8), yaxt = 'n', xaxt = 'n')
axis(side = 1, at = 1:6, labels = labs)
abline(v=2.5, col = "gray60"); abline(v=4.5, col = "gray60")
text(1.5, 8, "Dry", font = 4, col = "gray50"); text(3.5, 8, "Intermed", font = 4, col = "gray50"); text(5.5, 8, "Wet", font = 4, col = "gray50")
mtext("C-min (ugC-Co2/hr/gds)", outer = TRUE, side = 3, line = 2, font = 2, cex = 1.3)









