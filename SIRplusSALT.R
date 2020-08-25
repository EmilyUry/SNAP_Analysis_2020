#' ---
#' title: "SIR + salt talk figure"
#' author: "Emily Ury"
#' date: "August 24, 2020"
#' output: github_document
#' ---
#'



setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

x <-  read.csv("SIR_plus_salt_expt.csv")
head(x)


# 
# Response <- x$µg_C_CO2_hr_gC
# Response <- x$µg_C_CO2_hr_gds
# 
# x$Treatment <-replace(x$Treatment, c(1:30), "a_zero")
# x$Treatment <-replace(x$Treatment, c(31:60), "b_two")
# x$Treatment <- replace(x$Treatment, c(61:90), "c_six")
# 
# 
# par(mar = c(7,5,2,2))
# boxplot(Response ~ Treatment*Plot*Site, data = x, las = 2, 
#         col = c("#f4a582","#f4a582","#f4a582", "#d6604d", "#d6604d","#d6604d",
#                 "#C2A5CF", "#C2A5CF", "#C2A5CF", "#9970AB","#9970AB","#9970AB",
#                 "#92C5DE","#92C5DE","#92C5DE",  "#4393C3",  "#4393C3",  "#4393C3" ))




x$Respiration <- x$µg.C.CO2.hr.gds

x$Treatment <-replace(x$Treatment, c(1:30), 0)
x$Treatment <-replace(x$Treatment, c(31:60), 2)
x$Treatment <- replace(x$Treatment, c(61:90), 6)




### summarize the data
library(dplyr)


x %>%
  group_by(Site, Plot, Treatment) %>%
  summarize(n())

summary <- x %>%
  group_by(Site, Plot, Treatment) %>%
  summarize(mean_resp = mean(Respiration), sd_resp = sd(Respiration))


Salt <- summary[which(summary$Plot == "Salt"),]
Control <- summary[which(summary$Plot == "Control"),]


col <- c("#d6604d", "#000000", "#9970AB", "#000000",  "#4393c3")
treatment <- c(0, 2, 6, 0, 2, 6, 0, 2, 6)



### figure here

{
  par(mfrow = c(1,2), mar = c(5,5,2,1))
  plot(Control$Treatment, Control$mean_resp, 
       xlab = "Salinity Treatment (ppt)",
       ylab = "Respiration (ug C-CO2/g dry soil/hour)", 
       main = "Control Plot", 
       pch = 15,
       col = col[Control$Site],
       cex = 1.5, 
       ylim = c(0, 5))
  
  
  arrows(treatment, (Control$mean_resp+Control$sd_resp/2.3), treatment,
         (Control$mean_resp-Control$sd_resp/2.3), length = 0.05, angle = 90, code = 3, 
         col = "gray60")
  
  treatment <- c(0,2,6)
  C1 <- Control[which(Control$Site == 1),]
  abline(lm(C1$mean_resp ~ treatment), lwd = 2, col = "red")
  
  C3 <- Control[which(Control$Site == 3),]
  abline(lm(C3$mean_resp ~ treatment), lwd = 2, col = "purple")
  
  C5 <- Control[which(Control$Site == 5),]
  abline(lm(C5$mean_resp ~ treatment), lwd = 2, col = "blue")
  
  
  
  plot(Salt$Treatment, Salt$mean_resp, 
       xlab = "Salinity Treatment (ppt)",
       ylab = " ", 
       main = "Salt Plot", 
       pch = 15,
       col = col[Control$Site],
       cex = 1.5, 
       ylim = c(0, 5))
  
  arrows(treatment, (Salt$mean_resp+Salt$sd_resp/2.3), treatment,
         (Salt$mean_resp-Salt$sd_resp/2.3), length = 0.05, angle = 90, code = 3, 
         col = "gray60")
  
  S1 <- Salt[which(Salt$Site == 1),]
  abline(lm(S1$mean_resp ~ treatment), lwd = 2, col = "red")
  
  S3 <- Salt[which(Salt$Site == 3),]
  abline(lm(S3$mean_resp ~ treatment), lwd = 2, col = "purple")
  
  S5 <- Salt[which(Salt$Site == 5),]
  abline(lm(S5$mean_resp ~ treatment), lwd = 2, col = "blue")
}
