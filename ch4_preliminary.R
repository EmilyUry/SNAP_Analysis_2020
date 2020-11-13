

## Chapter 4


setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/chapter 4")


data <- read.csv("chapter4_mater.csv", head = T)
head(data)

data$Site <- as.factor(data$Site)
data$pH_treat <- as.factor(data$pH_treat)


data <- data[which(data$Sal_treat == 0),]

cols <- c("purple", "dodgerblue") 
pchs <- c(15, 16)
cols <- c("gray80", "gray50", "gray10")

boxplot(C_end ~ Site, data = data, col = c("purple", "dodgerblue"))
plot(DOC_mg_L_init~pH_init, data = data, pch = pchs[data$Site], col = cols[data$pH_treat] )
plot(DOC_mg_L_end~pH_end, data = data, pch = pchs[data$Site], col = cols[data$pH_treat] )

legend("bottomleft", c("Site 3", "Site 5"), pch = c(15,16))
plot(DOC_mg_L_end~pH_end, data = data, col = cols[data$Site])

abline(h = 11.2, col = "purple")
abline(h = 8.1, col = "dodgerblue")




data <- data[which(data$Sal_treat == 0 & data$pH_treat == "5.5"),]


