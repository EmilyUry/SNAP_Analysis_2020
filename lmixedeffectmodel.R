

### linear mixed effects models


library(lme4)



x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")


lX<-log(x[,c(16:21, 24, 33, 34)])
colnames(lX)<-paste("log",colnames(lX),sep="")
x<-cbind(x,lX); rm(lX)

x$ID<-paste(x$Site,x$Treatment,x$Core,sep="")


x$Treatment <- as.factor(x$Treatment)
x$Site <- as.factor(x$Site)
x$Depth <- as.factor(x$Depth)

## A = 2020 (0-5), a = 2020 (5-10)
## B = 2019 (0-5), a = 2019 (5-10)
## C = July 2018 (0-5), a = July 2018 (5-10)
## D = May 2018 (0-5), a = May 2018 (5-10)

A <- x[which(x$Year == 2020 & x$Depth == "(0-5)"),]
a <- x[which(x$Year == 2020 & x$Depth == "(5-10)"),]

B <- x[which(x$Year == 2019 & x$Depth == "(0-5)"),]
b <- x[which(x$Year == 2019 & x$Depth == "(5-10)"),]

C <- x[which(x$Date == "July 10th, 2018"& x$Depth == "(0-5)"),]
c <- x[which(x$Date == "July 10th, 2018" & x$Depth == "(5-10)"),]

D <- x[which(x$Date == "May 10th, 2018" & x$Depth == "(0-5)"),]
d <- x[which(x$Date == "May 10th, 2018" & x$Depth == "(5-10)"),]


##### DOC

model1 <- lmer(DOC~logCl + logSO4 + pH + SM + (1|Site), data = A, REML = F)
model2 <- lmer(DOC~ pH + SM + (1|Site), data = A, REML = F)
model2 <- lmer(DOC~ logCl + SM + (1|Site), data = A, REML = F)

model3 <- lmer(DOC~ logCl + pH + SM + (1|Site), data = A, REML = F)

model2 <- lmer(DOC~logCl + logSO4 + pH + (1|Site), data = A, REML = F)

model2 <- lmer(DOC~logCl + pH + SM + (1|Site), data = A, REML = F)
model2 <- lmer(DOC~  logSO4 + pH + SM + (1|Site), data = A, REML = F)

null1 <- lmer(DOC~1 + (1|Site), data = A, REML = F)
anova(null1, model2)

anova(model2, model1)
summary(model1)
summary(model2)

anova(model2, model3)


