# global.R


#Load required library
library(leaflet)
library(rgdal)
library(shinyjs)

# Get dataset (PWS service areas) from shapefile
serviceAreas <- readOGR(dsn="C:/Users/JSpector/Documents/Source Water Time Machine/Service_Areas/service_areas.shp",layer="service_areas")

# Get source point data from CSV
sourcePoints <- read.csv("C:/Users/JSpector/Documents/Source Water Time Machine/20190619 DDW Source Points.csv")
sourcePoints <- na.omit(sourcePoints)
saveRDS(sourcePoints, "C:/Users/JSpector/Documents/Source Water Time Machine/data.rds")


# Create a vector of counties
counties <- unique(as.vector(serviceAreas$d_prin_cnt))
counties <- sort(counties)