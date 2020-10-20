

## linear mixed effects model to tease appart the Cl pH thing



library(lme4)
install.packages("lmerTest")
library(lmerTest)
library(MuMIn)



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

Z <- x[which(x$Depth == "(0-5)"),]
z <- x[which(x$Depth == "(5-10)"),]


model1 <- lmer(DOC~logCl + pH + SM + (1|Site), data = A, REML = F)
model1 <- lmer(DOC~logCl + pH + logCl*pH + (1|Site), data = A, REML = F)


options(na.action = "na.fail")
dredge(model1, beta = "sd", rank = "AICc")



M.A <- lmer(DOC~logCl + pH + logCl*pH + SM + (1|Site), data = A, REML = F)
summary(M.A)
M.a <- lmer(DOC~logCl + pH + logCl*pH + (1|Site), data = a, REML = F)
summary(M.a)
M.B <- lmer(DOC~logCl + pH + logCl*pH +  (1|Site), data = B, REML = F)
summary(M.B)
M.b <- lmer(DOC~logCl + pH + logCl*pH +  (1|Site), data = b, REML = F)
summary(M.b)
M.C <- lmer(DOC~logCl + pH + logCl*pH +  (1|Site), data = C, REML = F)
summary(M.C)
M.c <- lmer(DOC~logCl + pH + logCl*pH +  (1|Site), data = c, REML = F)
summary(M.c)
M.D <- lmer(DOC~logCl + pH + logCl*pH +  (1|Site), data = D, REML = F)
summary(M.D)
M.d <- lmer(DOC~logCl + pH + logCl*pH + (1|Site), data = d, REML = F)
summary(M.d)





M.A <- lmer(DOC~Treatment +  (1|Site), data = a, REML = T)
summary(M.A)



MZ <- lmer(DOC~logCl + pH + logCl*pH + (1 + Date | Site), data = Z, REML = F)
Mz <- lmer(DOC~logCl + pH + logCl*pH + (1 + Date | Site), data = z, REML = F)
summary(MZ)
summary(Mz)


### note in the above that the variance term of the random slope Date is very high