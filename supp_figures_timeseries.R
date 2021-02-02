
setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

library(data.table)
library(plotrix)
library(ggplot2)


x <- read.csv("SNAP_3year_harm.csv", head = T)
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "ICNO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "NO3", "PO4", "Suva254")

# 
# x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "pH", "LOI", 
#                           "DOC", "Phenol", "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Roots" ))

x <- data.table(x)


date <- c(rep("Aug 2020", 12), rep("Jun 2019", 12), rep("May 2018", 12), rep("Jul 2018", 12))
date <- factor(date, levels = c("May 2018", "Jul 2018", "Jun 2019", "Aug 2020") )


############ Figures -- timeseries

## pH 
{
x$response <- x$pH
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
             by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("pH", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.pH <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab("pH") +
  geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}
                              
### LOI
{
x$response <- x$LOI
output <- x[,.("mean" = mean(response), "se" = std.error(response)),
            by = c("Date", "Site", "Treatment", "Depth")]
output$variable <- rep("LOI", 48)
output$mean <- round(output$mean, 2)
output$se <- round(output$se, 2)
output$Site <- as.factor(output$Site)
output$Treatment <- as.factor(output$Treatment)
output$date <- date
s.LOI <- output

### FIGURE  points and error bars 
labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
names(labs) <- c("(0-5)", "(5-10)")
ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
  geom_point() +
  geom_line() +
  facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
  scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
  theme_bw() +
  xlab(" ") +
  ylab("LOI (%)") +
  geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### DOC
{
  x$response <- x$DOC
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("DOC", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.DOC <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('DOC (mg · L'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Phenolics
{
  x$response <- x$Phenol
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("phenolics", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.phenol <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Phenolics (mg · L'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

## phenolics/DOC
{
  x$response <- x$Phenol/x$DOC
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("phenolics/doc", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.phenolics.doc <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Phenolics (mg · mg DOC'^-1, ')'))) +   
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Cmin_s
{
  x$response <- x$Cmin_s
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("Cmin_s", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.Cmin_s <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('C'[mineralization], '(', mu, 'g C-CO'[2], ' gds'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Cmin_c
{
  x$response <- x$Cmin_c
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("Cmin_c", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.Cmin_c <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('C'[mineralization], '(', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### SIR_s
{
  x$response <- x$SIR_s
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("SIR_s", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.SIR_s <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('SIR (', mu, 'g C-CO'[2], ' gds'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### SIR_c
{
  x$response <- x$SIR_c
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("SIR_c", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.SIR_c <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('SIR (', mu, 'g C-CO'[2], ' g C'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Roots
{
  x$response <- x$Roots
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("roots", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  s.roots <- output
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Roots (g / 100 cm '^3, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

######################################
#### summary 

df <- rbind(s.pH, s.LOI, s.DOC, s.phenol, s.phenolics.doc, s.Cmin_c, s.Cmin_s, s.SIR_c, s.SIR_s, s.roots)
write.csv(df, "stat_sum.csv")



#######################predictors
### BD
{
  x$response <- x$BD
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("BD", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date

  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Bulk density (g / cm'^3, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### SM
{
  x$response <- x$SM
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("SM", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date

  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab("SM (%)") +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### TDN
{
  x$response <- x$TDN
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("TDN", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('TDN (mg · L'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### NH4
{
  x$response <- x$NH4
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("NH4", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('NH' [4], ' (ug N · gds'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### ICNO3
{
  x$response <- x$ICNO3
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("NO3", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('NO' [3], ' (ug N · gds'^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### PO4
{
  x$response <- x$PO4
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("PO4", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('PO' [4], ' (ug P · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}


### Cl
{
  x$response <- x$Cl
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("Cl", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Cl (ug · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### SO4
{
  x$response <- x$SO4
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("SO4", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('SO'[4], ' (ug · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Na
{
  x$response <- x$Na
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("Na", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Na (ug · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### K
{
  x$response <- x$K
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("K", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('K (ug · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Mg
{
  x$response <- x$Mg
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("Mg", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Mg (ug · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}

### Ca
{
  x$response <- x$Ca
  output <- x[,.("mean" = mean(response), "se" = std.error(response)),
              by = c("Date", "Site", "Treatment", "Depth")]
  output$variable <- rep("Ca", 48)
  output$mean <- round(output$mean, 2)
  output$se <- round(output$se, 2)
  output$Site <- as.factor(output$Site)
  output$Treatment <- as.factor(output$Treatment)
  output$date <- date
  
  ### FIGURE  points and error bars 
  labs <- c("Depth: 0-5 cm", "Depth: 5-10 cm")
  names(labs) <- c("(0-5)", "(5-10)")
  ggplot(output, aes(x = date, y = mean, group = interaction(Treatment, Site), color = Site, linetype = Treatment)) +
    geom_point() +
    geom_line() +
    facet_grid(Depth ~ . ,labeller = labeller(Depth = labs)) + 
    scale_color_manual(values=c("#db351f", "#948b8a", "#000000"), labels = c("Dry", "Int.", "Wet")) +
    theme_bw() +
    xlab(" ") +
    ylab(expression(paste('Ca (ug · gds' ^-1, ')'))) +  
    geom_errorbar(data = output, aes(ymin = mean - se, ymax = mean + se, x = date, width = 0)) 
}



