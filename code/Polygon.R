library(readxl)
library(tidyverse)
setwd("~/DataAnalysis")

Polygon <- read_xlsx("./data/Polygon_Adventdalen.xlsx")

str(Polygon)

names(Polygon) <- c("Location", "Polygon", "Position" ,"Soil_Moisture","ALT" , "BT", "OLT")


Polygon$code <- paste(substr(Polygon$Location, 1,7), Polygon$Polygon)

Polygon$area <- substr(Polygon$Location, 1,7)

ggplot(Polygon, aes(x=Polygon, y=Soil_Moisture, color=Position))+
  geom_point()+
  facet_grid(rows=vars(area))+
  theme_bw()


ggplot(Polygon, aes(x=code, y=as.numeric(Soil_Moisture), color=Position))+
  geom_point()+
  theme_bw()
