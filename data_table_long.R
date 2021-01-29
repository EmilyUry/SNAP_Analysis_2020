

#' Code for reshaping data from wide to long
#' Preparation of data for sharing in a repository

setwd("C:/Users/uryem/Dropbox (Duke Bio_Ea)/My data/SNAP_2020")
x <- read.csv("SNAP_3year_harm.csv", head = T)

## rename variables
names(x) <- c("Date", "Year", "Site", "Treatment", "Depth", "Core", "Salinity","Cond", "BD", 
              "SM", "LOI", "pH", "Roots", "DOC", "TDN", 
              "Cl", "SO4", "Na", "K", "Mg", "Ca", "TIC", "TCC", "NH4", "NO3", "ICPO4",
              "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Br", "Phenol", "xNO3", "PO4", "Suva254")

## reorder and select variables
x <- subset(x, select = c("Date", "Site", "Treatment", "Depth", "Core", "SM", "BD", "pH", "LOI", 
                          "DOC", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN", "NH4", "NO3", "PO4","Phenol",
                          "Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Roots" ))


## create a key for column names and units
units <- c("%", "g/cm3", "pH", "%", "mg/L", "ug/gds", "ug/gds", "ug/gds", "ug/gds", "ug/gds", "ug/gds", 
           "mg/L", "ug/gds", "ug/gds", "ug/gds","mg/L",
            "ugC-CO2/hour/gds", "ugC-CO2/hour/goc", "ugC-CO2/hour/gds", "ugC-CO2/hour/goc", "g/100cm3" )
variable <- c("SM", "BD", "pH", "LOI", 
"DOC", "Cl", "SO4", "Na", "K", "Mg", "Ca", "TDN", "NH4", "ICNO3", "PO4","Phenol",
"Cmin_s", "Cmin_c", "SIR_s", "SIR_c", "Roots" )
key <- data.frame(variable, units)



library(dplyr)

### transform the data from wide to long
output <- pivot_longer(x, cols = SM:Roots, names_to = "variable") %>%
arrange(variable) %>%              ## arrange in order of variable
left_join(key, by = 'variable')    ## add units

### write out csv file to directory
write.csv(output, "SNAP_soil_core_data_long.csv")






