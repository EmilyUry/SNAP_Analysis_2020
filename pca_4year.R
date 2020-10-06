
### Pca 4 year compilation

### PCA 4 year



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

x <- read.csv("pca_in.csv", head = T)
head(x)
names(x) <- c( "Year", "Month", "Color", "Time", "Date","Site", "Treatment", "Depth", "Core", "SM", "LOI", 
               "pH", "DOC",  
               "Cl", "SO4", "Na", "K", "Mg", "Ca", "TN", "NH4", "ICNO3", "PO4", "ICPO4",
               "NO3", "NOx", "NO3_other", "PO4_other")

#major outlier
x <- x[-110,]
info <- x[,c(1,3, 6,7,9) ]
head(info)

var3 <- x[,c(12,13,14)]
var3 <- na.omit(var3)
info3 <- info[-30,]
info3 <- info3[-25,]

var12 <- x[,c(1,3, 6,7,9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25)]
var12 <- na.omit(var12)
info12 <- var12[1:5]
var12 <- var12[, 6:17]


head(var3)
head(var12)




col.s <- c("#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026")   #reds
col.c <- c("#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695")  ##blues



par(mfrow = c(1,3))


pca <-prcomp(var3, center = TRUE, scale = TRUE)
print(pca)
plot(pca)


plot(pca,type="line",cex.lab=1.5, cex.main=1.5)
abline(h=1,lty=3, col="red") ## keep only the first three principle components (var > 1)


## explore some correlations
cor(var3$Cl, pca$x[,1])
plot(log10(var3$Cl), pca$x[,1], xlab = "Chloride", ylab = "PC1", frame = F)

summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(info3, pca.scores[,1:3])
df$Treatment <- as.factor(df$Treatment)
df$Color <- as.factor(df$Color)

#' Plot the pca

plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")


#' Facet out the PCA, a plot for each experimental site

df1 <- df[which(df$Site == "1"),]
df3 <- df[which(df$Site == "3"),]
df5 <- df[which(df$Site == "5"),]
{
par(mfrow = c(1,3))
plot(df1$PC1, df1$PC2, pch = 16, cex = 0.9, 
     xlab = "PC1", ylab = "PC2", xlim = c(-4,4), ylim = c(-3,5), 
     main = "Site 1 (Dry)")
arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")

plot(df3$PC1, df3$PC2, pch = 16, cex = 0.9, 
     xlab = "PC1", ylab = "PC2", xlim = c(-4,4), ylim = c(-3,5), 
     main = "Site 3 (Itermediate)")
arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")

plot(df5$PC1, df5$PC2, pch = 16, cex = 0.9, 
     xlab = "PC1", ylab = "PC2", xlim = c(-4,4), ylim = c(-3,5), 
     main = "Site 5 (Wet)")
arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")
}



#' Symbol is the treatment block 


par(mfrow = c(1,3))
par(mar = c(4,1,5,.4), oma = c(1,4,1,1), xpd = T)
plot(df1$PC1, df1$PC2, cex = 2.2, 
     xlab = "PC1", ylab = "PC2", xlim = c(-3,3), ylim = c(-2,6.7), 
     main = " ", 
     pch = c(21,21)[df1$Treatment], 
     col = "black",
     bg = ifelse(df1$Treatment == "Salt", col.s[df1$Color], col.c[df1$Color]))
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1.2, col = "red")
mtext("PC2", 2, line = 1, outer = T, cex = 0.7)
mtext("Site 1 (dry)", side = 3, line =-2, cex = 1.2, font = 2)
legend("topleft", c("2015", "2018 (May)", "2018 (July)", "2019", "2020"), pch = 21, pt.bg = col.c, 
       inset = c(0.1, 0.11), bty = 'n', pt.cex = 1.5, title = "Control plot" )
legend("topleft", c("2015", "2018 (May)", "2018 (July)", "2019", "2020"), pch = 21, pt.bg = col.s, 
       inset = c(0.4, 0.11), bty = 'n', pt.cex = 1.5, title = "Salt plot" )

plot(df3$PC1, df3$PC2, cex = 2.2, yaxt = 'n',
     xlab = "PC1", ylab = "PC2", xlim = c(-3,3), ylim = c(-2,6.7), 
     main = "", 
     pch = c(21,21)[df3$Treatment], 
     col = "black",
     bg = ifelse(df3$Treatment == "Salt", col.s[df3$Color], col.c[df3$Color]))
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1.2, col = "red")
mtext("Site 3 (itermediate)", side = 3, line =-2, cex = 1.2, font = 2)


plot(df5$PC1, df5$PC2, cex = 2.2, yaxt = 'n',
     xlab = "PC1", ylab = "PC2", xlim = c(-3,3), ylim = c(-2,6.7), 
     main = "", 
     pch = c(21,21)[df5$Treatment], 
     col = "black",
     bg = ifelse(df5$Treatment == "Salt", col.s[df5$Color], col.c[df5$Color]))
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1.2, col = "red")
mtext("Site 5 (wet)", side = 3, line =-2, cex = 1.2, font = 2)


#########################

#      12 var           #
 
#########################



pca <-prcomp(var12, center = TRUE, scale = TRUE)
print(pca)
plot(pca)


plot(pca,type="line",cex.lab=1.5, cex.main=1.5)
abline(h=1,lty=3, col="red") ## keep only the first three principle components (var > 1)


## explore some correlations
cor(var12$Cl, pca$x[,1])
plot((var12$Cl), pca$x[,1], xlab = "Chloride", ylab = "PC1", frame = F)

summary(pca) #100% of PCA variance, but not the actual NMS axis variance
pca.scores<-pca$x
pca.loading <- pca$rotation
df <- cbind(info12, pca.scores[,1:3])
df$Treatment <- as.factor(df$Treatment)
df$Color <- as.factor(df$Color)

#' Plot the pca

plot(df$PC1, df$PC2, pch = 16, cex = 0.9, main = "PCA all soil cores", 
     xlab = "PC1", ylab = "PC2")
arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")


#' Facet out the PCA, a plot for each experimental site

df1 <- df[which(df$Site == "1"),]
df3 <- df[which(df$Site == "3"),]
df5 <- df[which(df$Site == "5"),]
{
  par(mfrow = c(1,3))
  plot(df1$PC1, df1$PC2, pch = 16, cex = 0.9, 
       xlab = "PC1", ylab = "PC2", xlim = c(-4,14), ylim = c(-3,5), 
       main = "Site 1 (Dry)")
  arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
  text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")
  
  plot(df3$PC1, df3$PC2, pch = 16, cex = 0.9, 
       xlab = "PC1", ylab = "PC2", xlim = c(-4,14), ylim = c(-3,5), 
       main = "Site 3 (Itermediate)")
  arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
  text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")
  
  plot(df5$PC1, df5$PC2, pch = 16, cex = 0.9, 
       xlab = "PC1", ylab = "PC2", xlim = c(-4,14), ylim = c(-3,5), 
       main = "Site 5 (Wet)")
  arrows(0,0, pca.loading[,1]*4, pca.loading[,2]*4, length = 0.1, lwd = 1.5, col = "red")
  text(pca.loading[,1]*4.5, pca.loading[,2]*4.3, row.names(pca.loading), cex = 1.2, col = "red")
}



#' Symbol is the treatment block 


par(mfrow = c(1,3))
par(mar = c(4,1,5,.4), oma = c(1,4,1,1), xpd = T)
plot(df1$PC1, df1$PC2, cex = 2.2, 
     xlab = "PC1", ylab = "PC2", xlim = c(-2,15), ylim = c(-3,3), 
     main = " ", 
     pch = c(21,21)[df1$Treatment], 
     col = "black",
     bg = ifelse(df1$Treatment == "Salt", col.s[df1$Color], col.c[df1$Color]))
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "gray70")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1.2, col = "gray70")
mtext("PC2", 2, line = 1, outer = T, cex = 0.7)
mtext("Site 1 (dry)", side = 3, line =2, cex = 1.2, font = 2)
legend("bottomright", c("2015", "2018 (May)", "2018 (July)", "2019", "2020"), pch = 21, pt.bg = col.c, 
       inset = c(0.01, 0.03), bty = 'n', pt.cex = 1.5, title = "Control plot" )
legend("bottomright", c("2015", "2018 (May)", "2018 (July)", "2019", "2020"), pch = 21, pt.bg = col.s, 
       inset = c(0.25, 0.03), bty = 'n', pt.cex = 1.5, title = "Salt plot" )

plot(df3$PC1, df3$PC2, cex = 2.2, yaxt = 'n',
     xlab = "PC1", ylab = "PC2", xlim = c(-2,15), ylim = c(-3,3), 
     main = "", 
     pch = c(21,21)[df3$Treatment], 
     col = "black",
     bg = ifelse(df3$Treatment == "Salt", col.s[df3$Color], col.c[df3$Color]))
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "gray70")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1.2, col = "gray70")
mtext("Site 3 (itermediate)", side = 3, line =2, cex = 1.2, font = 2)


plot(df5$PC1, df5$PC2, cex = 2.2, yaxt = 'n',
     xlab = "PC1", ylab = "PC2", xlim = c(-2,15), ylim = c(-3,3), 
     main = "", 
     pch = c(21,21)[df5$Treatment], 
     col = "black",
     bg = ifelse(df5$Treatment == "Salt", col.s[df5$Color], col.c[df5$Color]))
arrows(0,0, pca.loading[,1]*3, pca.loading[,2]*3, length = 0.1, lwd = 1.5, col = "gray70")
text(pca.loading[,1]*3.5, pca.loading[,2]*3.3, row.names(pca.loading), cex = 1.2, col = "gray70")
mtext("Site 5 (wet)", side = 3, line =2, cex = 1.2, font = 2)




