# global.R

#Load required libraries
library(leaflet)
library(shinyjs)
library(sf)
library(rmapshaper)
library(dplyr)
library(ggplot2)
library(scales)

setwd("C:/Users/JSpector/Documents/")

# Get dataset (public water system service areas) from shapefile
serviceAreas <- read_sf(dsn="~/Source Water Time Machine/Service_Areas_2019_07_09/service_areas.shp",layer="service_areas")

# Get source point data from CSV
sourcePoints <- read.csv("~/Source Water Time Machine/20190619 DDW Source Points.csv")

# remove any source point data that has NA in rows
sourcePoints <- na.omit(sourcePoints)

# determine common PWSIDs between service areas and source points
com_id <- intersect(serviceAreas$pwsid, sourcePoints$PWS.ID)

# take a subset of service area data for common PWSIDs
serviceAreas <- subset(serviceAreas, serviceAreas$pwsid %in% com_id)

# simplify shapefile so loads faster
serviceAreas <- ms_simplify(serviceAreas, keep=0.07, method="dp")

# Create a vector of county names
counties <- unique(as.vector(serviceAreas$d_prin_cnt))
counties <- sort(counties)

# Bring in source point key values
facility_keys <- read.csv("~/Source Water Time Machine/Water_Source_Facilities.csv")
availability_keys <- read.csv("~/Source Water Time Machine/Availability.csv")

sourcePoints <- select(sourcePoints, -c(Activity.Status, PWS.Type, Activity.Status2))

# bring in production data
production_data <- read.csv("~/Source Water Time Machine/EAR 2013-2016 PRODUCTION FINAL 06-22-2018.csv")
production_data$Date <- as.Date(production_data$Date)
# convert desired columns into usable forms
production_data <- production_data %>%
  mutate_at(c("WATER.PRODUCED.FROM.GROUNDWATER", "WATER.PRODUCED.FROM.SURFACE.WATER","FINSIHIED.WATER.PURCHASED.OR.RECEIVED.FROM.ANOTHER.PUBLIC.WATER.SYSTEM"), as.numeric)%>%
  mutate_at(c("PWSID"), as.character)
# remove white space before and after PWSID
production_data$PWSID <- trimws(production_data$PWSID, "both")
# convert proportions
production_data$sw_prop <- round(production_data$WATER.PRODUCED.FROM.SURFACE.WATER/(production_data$WATER.PRODUCED.FROM.GROUNDWATER+production_data$WATER.PRODUCED.FROM.SURFACE.WATER+production_data$FINSIHIED.WATER.PURCHASED.OR.RECEIVED.FROM.ANOTHER.PUBLIC.WATER.SYSTEM), digits=2)
production_data$dw_prop <- round(production_data$FINSIHIED.WATER.PURCHASED.OR.RECEIVED.FROM.ANOTHER.PUBLIC.WATER.SYSTEM/(production_data$WATER.PRODUCED.FROM.GROUNDWATER+production_data$WATER.PRODUCED.FROM.SURFACE.WATER+production_data$FINSIHIED.WATER.PURCHASED.OR.RECEIVED.FROM.ANOTHER.PUBLIC.WATER.SYSTEM), digits=2)
production_data$gw_prop <- round(production_data$WATER.PRODUCED.FROM.GROUNDWATER/(production_data$WATER.PRODUCED.FROM.GROUNDWATER+production_data$WATER.PRODUCED.FROM.SURFACE.WATER+production_data$FINSIHIED.WATER.PURCHASED.OR.RECEIVED.FROM.ANOTHER.PUBLIC.WATER.SYSTEM), digits=2)

# only work with production data relevant to PWSIDs that have source points
com_id_production <- intersect(serviceAreas$pwsid, production_data$PWSID)
production_data <- subset(production_data, production_data$PWSID %in% com_id_production)

# this function converts date to first day of month for slider input
monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}