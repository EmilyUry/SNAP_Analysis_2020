
##### note, a few months are missing (feb-may 2019) and I totally ignored that...so

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")

data <- read.csv("Site5_Welldata.csv", head = TRUE)
head(data)
names(data) <- c("date_c", "sal_c", "sal_c2", " ", " ", " ", "date_S", "sal_S", "sal_S2")
head(data)
control <- data[1:50,1:3]
salt <- data[1:50,7:9]

salt$day <- strsplit(salt$date_S, " ")
salt$d <- salt$day[1]


salt$date_S <- as.Date(salt$date_S)
plot(c(1:50), salt$sal_S)
plot(data$sal_S)


data <- data[-3874,]
data <- data[-3874,]

plot(data$sal_S, type = 'l', col = "red", xlim = c(-29000, 97000), 
     ylim = c(0, 18000), xaxt = 'n',
     xlab = ' ', ylab = "Conductivity uS/cm")
points(data$sal_c2, type = 'l')
legend("bottomleft", c("Salt Addition","Salt","Control"), pch = c(25, 20, 20), 
       pt.cex = c(1.3,0.01,0.01), pt.bg = c("black"),
       col = c("white", "red", "black"), lty = 1)

axis(1, c(-28000, 30000, 90000), c("October 2016", "February 2018", "June 2020"))       


### salt additions

SA <- c(-28000,-25216,-1930,700,6600,13200,16400,33330,39400,45370,52600,
        57000,61840,64000,67820,70480,74310,76654,92934,
        94566,96774)
ys <- rep(17000, 21)
points(SA, ys, pch = 25, bg = "black")
        


#### log scale

plot(data$sal_S, type = 'l', col = "red", xlim = c(-29000, 97000), 
     #ylim = c(0, 18000), 
     xaxt = 'n',
     xlab = ' ', ylab = "Conductivity uS/cm",
     log ="y")
points(data$sal_c, type = 'l', log = "y")
legend("bottomleft", c("Salt Addition","Salt","Control"), pch = c(25, 20, 20), 
       pt.cex = c(1.3,0.01,0.01), pt.bg = c("black"),
       col = c("white", "red", "black"), lty = 1)

axis(1, c(-28000, 30000, 90000), c("October 2016", "February 2018", "June 2020"))   

        
        