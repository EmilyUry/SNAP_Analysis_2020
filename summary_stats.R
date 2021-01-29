
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")


x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")

x$carb <- x$BD*x$LOI/100 ## grams carbon per cubic cm soil
lX<-log(x[,c(16:21, 24, 33, 34)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)
x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")
x$Treatment <- as.factor(x$Treatment)
x$Site <- as.factor(x$Site)
x$Depth <- as.factor(x$Depth)


### CMIN
library(plyr)
z <- ddply(x, .(Date, Site, Treatment, Depth), summarise, 
           cmin = mean(SM, na.rm = T),
           se.cmin = sqrt(var(SM, na.rm = T))/length(SM))
names(z) <- c("Date", "Site", "Treatment", "depth", "mean", "se")
z$var <- rep("SM", 48)
z

y <- ddply(x, .(Date, Site, Treatment, Depth), summarise, 
           cmin = mean(BD, na.rm = T),
           se.cmin = sqrt(var(BD, na.rm = T))/length(BD))
names(y) <- c("Date", "Site", "Treatment", "depth", "mean", "se")
y$var <- rep("BD", 48)
y

dz <- data.frame(z)
dy <- data.frame(y)
df <- rbind(dz, dy)
df$mean <- round(df$mean, 2)
df$se <- round(df$se, 2)
write.csv(df, "stat_sum.csv")
z




for( i in c(9:34)){
Var <- df[,i]

Var1 <- ddply(x, .(Date, Site, Treatment, Depth), summarise, 
           cmin = mean(Var, na.rm = T),
           se.cmin = sqrt(var(Var, na.rm = T))/length(Var))
names(y) <- c("Date", "Site", "Treatment", "depth", "mean", "se")
y$var <- rep("BD", 48)
y}



