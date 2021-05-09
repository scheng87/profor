##Preparing RDS file for map filtering app
library(dplyr)
library(tidyr)
library(stringr)
load("PROFOR_Evidence_Map_7_5_2018.RData")

#Isolate country data and add region column
country_data <- select(data.study,aid,Study_country)
country_data <- distinct(country_data)
reg <- read.csv("allcountries.csv",header=TRUE)
#create blank data matrix
rows <- c(1:nrow(country_data))
reg_data <- matrix(nrow=nrow(country_data),ncol=2)
rownames(reg_data) <- rows
colnames(reg_data) <- c("region","subregion")

#Assign regions
for (i in rows){
  country <- country_data$Study_country[i]
  sub <- filter(reg,Country == country)
  if (nrow(sub) != 0){
    reg_data[i,"region"] <- as.character(sub$Region)
    reg_data[i,"subregion"] <- as.character(sub$Subregion)
  } else {
    reg_data[i,"region"] <- as.character("NA")
    reg_data[i,"subregion"] <- as.character("NA")
  }
}

reg_data <- as.data.frame(reg_data)
country_data_final <- bind_cols(country_data,reg_data)

#Read in biomes
biome_data <- distinct(data.biomes)

#Attach final data frames
map_data_final <- left_join(data.biblio,data.interv,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- select(map_data_final,-Assess_date,-Assessor_2)
map_data_final <- left_join(map_data_final,biome_data,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,country_data_final,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,data.outcome,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,data.study,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- left_join(map_data_final,data.pathways,by="aid")
map_data_final <- distinct(map_data_final)
map_data_final <- select(map_data_final,-Study_country.y)
map_data_final <- distinct(map_data_final)

saveRDS(map_data_final,file="map_data_final_7_5_18.rds")
