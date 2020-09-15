

#' ---
#' title: " NCDA soils data -- preliminary look"
#' author: "Emily Ury"
#' date: "Sept 11, 2020"
#' output: github_document
#' ---
#'


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")


y <- read.csv("NCDA_soils_data.csv", head = T)
names(y) <- c("Site", "Plot", "Depth", "ID", "humic", "wv", "pH2", "BaseSat", "AC",  "CEC", "Na2", "P", "K2", "Ca2", "Mg2",
              "S", "Mn", "Cu", "Zn", "Ec")
head(y)

col.bp2 <- c("#b2182b", "#b2182b","#b2182b", "#b2182b", "#762A83", "#762A83","#762A83", "#762A83",
             "#2166AC", "#2166AC", "#2166AC", "#2166AC")
col.bp3<- c("white",  "white", "#d6604d","#d6604d","white","white", "#9970AB", "#9970AB",
            "white", "white","#4393C3",  "#4393C3")
col.bp4 <- c("#b2182b", "#d6604d","#b2182b", "#d6604d", "#762A83", "#9970AB","#762A83",  "#9970AB",
             "#2166AC", "#4393C3", "#2166AC",  "#4393C3")


par(mar = c(6,6,6,6), xpd = T)
labs <- c("Control", "Salt", "Control", "Salt","Control", "Salt")


boxplot(humic ~ Depth*Plot*Site, data = y, col = col.bp4, xaxt = "n", main = "Humics (%)")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
               "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')


boxplot(BaseSat ~ Depth*Plot*Site, data = y, col = col.bp4, xaxt = "n", main = "Base Saturation (%)")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')


boxplot(AC ~ Depth*Plot*Site, data = y, col = col.bp4, xaxt = "n", main = "Exchangable Acidity (meq/100cm3) ")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')

boxplot(CEC ~ Depth*Plot*Site, data = y, col = col.bp4, xaxt = "n", main = "Cation Exchange Capacity (meq/10cm3)")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')

boxplot(pH2 ~ Depth*Plot*Site, data = y, col = col.bp4, xaxt = "n", main = "pH")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')

boxplot(pH ~ Depth*Treatment*Site, data = x, col = col.bp4, xaxt = "n", main = "pH")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')

boxplot(DOC ~ Depth*Treatment*Site, data = x, col = col.bp4, xaxt = "n", main = "DOC")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')

boxplot(BD ~ Depth*Treatment*Site, data = x, col = col.bp4, xaxt = "n", main = "BD")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')
boxplot(LOI*BD ~ Depth*Treatment*Site, data = x, col = col.bp4, xaxt = "n", main = "LOI*BD")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')



boxplot(Ca2 ~ Depth*Plot*Site, data = y, col = col.bp4, xaxt = "n", main = "calcium")
axis(1, c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5), labs, font = 2)
legend("topleft", inset= c(0.25,-0.12), c("Dry (0-5)", "Dry (5-10)", "Int (0-5)", "Int (5-10)", "Wet (0-5)", "Wet (5-10)"), 
       text.col = c("#b2182b", "#d6604d", "#762A83", "#9970AB",
                    "#2166AC", "#4393C3"), text.font = 2, ncol = 3, bty = 'n')


### compare the NCAR data to our 2020 annual sampling (Salt and Control, 0-5 depth only)




x <- read.csv("2020_SNAP_master.csv", head = T)
head(x)
names(x) <- c("Date","Site", "Treatment", "Depth", "Core", "Salinity", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")


y <- read.csv("NCDA_soils_data.csv", head = T)
head(y)
names(y) <- c("Site", "Plot", "Depth", "ID", "humic", "wv", "pH2", "BaseSat", "AC",  "CEC", "Na2", "P", "K2", "Ca2", "Mg2",
              "S", "Mn", "Cu", "Zn", "Ec")


data <- cbind(x,y)
head(data)


c2 <- c("#b2182b", "black", "#762A83", "black", "#2166AC")
c4 <- c("#d6604d", "black", "#9970AB", "black", "#4393C3")
data$Treatment <- as.factor(data$Treatment)
data$Depth <- as.factor(data$Depth)


plot(data$pH, data$pH2, cex = 1, pch = c(24,25)[data$Depth],  col = c2[data$Site], 
     bg = ifelse(data$Treatment == "Salt", c2[data$Site], "white"))
legend("topleft", inset = c(0.0,-0.15), c("Salt (0-5)", "Control (5-10)"), pt.bg = c("black", "white"),
       pch = c(24,25), bty = "n")

plot(data$Phenol, data$Cmin_s, cex = 1, pch = c(24,25)[data$Depth],  col = c2[data$Site], 
     bg = ifelse(data$Treatment == "Salt", c2[data$Site], "white"))
legend("topleft", inset = c(0.0,-0.15), c("Salt (0-5)", "Control (5-10)"), pt.bg = c("black", "white"),
       pch = c(24,25), bty = "n")





plot(log(data$Salinity), data$Ec, cex = 1, pch = c(24,25)[data$Depth],  col = c2[data$Site], 
     bg = ifelse(data$Treatment == "Salt", c2[data$Site], "white"))




plot(data$Ca, data$Ca2, cex = 1, pch = c(24,25)[data$Depth],  col = c2[data$Site], 
     bg = ifelse(data$Treatment == "Salt", c2[data$Site], "white"))

plot(data$Phenol, data$humic, cex = 1, pch = c(24,25)[data$Depth],  col = c2[data$Site], 
     bg = ifelse(data$Treatment == "Salt", c2[data$Site], "white"))


plot(data$TCC, data$CEC)
plot(data$pH2, data$AC)
plot(data$CEC, data$pH2)
plot(data$BaseSat, data$pH2)

plot(data$BaseSat, data$AC)

