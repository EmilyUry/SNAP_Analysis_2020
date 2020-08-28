
#' ---
#' title: "SNAP 2020 data vis"
#' author: "Emily Ury"
#' date: "August 25, 2020"
#' output: github_document
#' ---
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


### optical responses

## phenolics and SUVA 254

plot(x$DOC, x$Phenol)
plot(x$DOC, x$Suva254)

col3 <- c("#d6604d", "black", "#9970AB","black", "#4393C3") ## medium


# plot(x$DOC, x$Suva254, pch = c(21,22)[x$Treatment], col = col3[x$Site], 
#      bg = ifelse(x$Depth == "(5-10)", "white", col3[x$Site]))


plot(x$DOC, x$Suva254, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
# legend("bottomright", c("Salt", "Control"), pch = c(24,24), col = c("#d6604d", "#d6604d"), pt.bg = c("#d6604d", "white")  ) 
# legend("topleft", c("0-5 cm", "5-10 cm"), pch = c(24,25), col = c("#d6604d", "#d6604d"), pt.bg = c("#d6604d", "#d6604d")  ) 

plot(x$DOC, x$Phenol, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))

par(mfrow = c(1,1))
col.bp <- c("white", "#d6604d", "white", "#9970AB","white", "#4393C3")
col.bp2 <- c("#b2182b", "#b2182b", "#762A83", "#762A83","#2166AC", "#2166AC")
col.bp3<- c("white",  "white", "#d6604d","#d6604d","white","white", "#9970AB", "#9970AB",
            "white", "white","#4393C3",  "#4393C3")
col.bp4 <- c("#b2182b", "#b2182b","#b2182b", "#b2182b", "#762A83", "#762A83","#762A83", "#762A83",
             "#2166AC", "#2166AC", "#2166AC", "#2166AC")


boxplot(DOC~Treatment*Site, data = x, col = col.bp, border = col.bp2)

boxplot(DOC~Depth*Treatment*Site, data = x)



boxplot(Suva254~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(Phenol~Treatment*Site, data = x, col = col.bp, border = col.bp2)



## soil carbon


boxplot(LOI~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(LOI~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


## CMIN


boxplot(Cmin_s~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(Cmin_c~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(Cmin_c~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)

plot(log10(x$Salinity), x$Cmin_c, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))


## SIR

boxplot(SIR_c~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(SIR_s~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(SIR_s~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


plot(log10(x$Salinity), x$SIR_s, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))



#### Other properties
## pH
plot(log10(x$Salinity), x$pH, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(pH~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(pH~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)



x19 <- read.csv("2019_SNAP_master.csv", head = TRUE)

x19 <- x19[which(x19$Treatment == "Salt" | x19$Treatment == "Control"),]
x19$Treatment <- as.factor(x19$Treatment)

boxplot(pH~Treatment*Site, data = x19, col = col.bp, border = col.bp2)


## Roots
plot(log10(x$Salinity), x$Roots, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(Roots~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(Roots~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)



## Bulk Density
plot(log10(x$Salinity), x$BD, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(BD~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(BD~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


## soil moisture

plot(log10(x$Salinity), x$SM, pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(SM~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(SM~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)



### nutrients

## NH4
plot(log10(x$Salinity), log10(x$NH4), pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(NH4~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(NH4~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)

## NH4
plot(log10(x$Salinity), log10(x$PO4), pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(PO4~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(PO4~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


## ICNO3
plot(log10(x$Salinity), log10(x$ICNO3), pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(ICNO3~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(ICNO3~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


## TN
plot(log10(x$Salinity), log10(x$TDN), pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(TDN~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(TDN~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)





## TIC
plot(log10(x$Salinity), log10(x$TIC), pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(TIC~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(TIC~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


### TCC
plot(log10(x$Salinity), log10(x$TCC), pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))
boxplot(TCC~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(TCC~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)


plot(x$TIC, x$TCC,  pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))

plot(log10(x$TIC), log10(x$TCC),  pch = c(24,25)[x$Depth], col = col3[x$Site], 
     bg = ifelse(x$Treatment == "Control", "white", col3[x$Site]))



boxplot(Salinity~Treatment*Site, data = x, col = col.bp, border = col.bp2)
boxplot(Salinity~Depth*Treatment*Site, data = x, col = col.bp3, border = col.bp4)

