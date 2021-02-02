
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")


x <- read.csv("SNAP_3year_harm.csv", head = T)
head(x)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")


x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "pH", "LOI", 
                          "DOC", "Phenol", "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Roots" ))


library(data.table)
library(plotrix)
library(ggplot2)

x <- data.table(x)


x$response <- x$pH
  
  
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
             by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("pH", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)


###### figure

output$Site <- as.factor(output$Site)
output$Date <- as.factor(output$Date)

date <- c(rep("Aug 2020", 12), rep("Jun 2019", 12), rep("May 2018", 12), rep("Jul 2018", 12))
date <- factor(date, levels = c("May 2018", "Jul 2018", "Jun 2019", "Aug 2020") )
output$date <- date


### version 1 -- points and error bars 

ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ .) + 
  scale_color_manual(values=c("#db351f", "#948b8a", "#000000")) +
  theme_bw() +
  xlab(" ") +
  ylab("pH") +
  geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
                              
                              
## version 2 - no frills

ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
  geom_line() +
  facet_grid(Depth ~ .) + 
  scale_color_manual(values=c("#db351f", "#948b8a", "#000000")) +
  theme_bw() +
  xlab(" ") +
  ylab("pH") 


## version 3 - x axis adjusted to scale (ish)

daten <- c(rep(3.3, 12), rep(2.1, 12), rep(1, 12), rep(1.3, 12))
output$daten <- daten

ggplot(output, aes(x = daten, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
  geom_line() +
  geom_point() +
  facet_grid(Depth ~ .) + 
  scale_color_manual(values=c("#db351f", "#948b8a", "#000000")) +
  theme_bw() +
  xlab(" ") +
  ylab("pH") + 
  scale_x_continuous(breaks = c(1,1.3, 2.1, 3.3), limits = c(0.9,3.4), labels=c("May\n2018", "July\n2018", "June\n2019","August\n2020"))
  



###### ANOVA


res.aov <- aov(response ~ Date*Site*Depth*Treatment, data = x)
summary(res.aov)
TukeyHSD(res.aov)

