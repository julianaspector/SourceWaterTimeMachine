library(shiny)
library(leaflet)
library(dplyr)
library(rgdal)

# bring in data from source water sampling points and public water service areas
df <- readRDS("C:/Users/JSpector/Documents/Source Water Time Machine/data.rds")
service_areas <- readOGR("https://trackingcalifornia.org/geoserver/ows?service=wfs&version=1.1.0&request=GetFeature&typeName=water:service_areas&srsName=epsg:4326&outputFormat=json")

# break counties in regions (based on 2020 census)
superior_ca_counties <- c("BUTTE", "COLUSA", "EL DORADO", "GLENN", "LASSEN", "MODOC", "NEVADA", "PLACER","PLUMAS", "SACRAMENTO","SHASTA", "SIERRA", "SISKIYOU","SUTTER", "TEHAMA", "YOLO", "YUBA")
north_coast_counties <- c("DEL NORTE", "HUMBOLDT", "LAKE", "MENDOCINO", "NAPA", "SONOMA", "TRINITY")
sf_bay_area_counties <- c("ALAMEDA", "CONTRA COSTA", "MARIN", "SAN FRANCISCO", "SAN MATEO", "SANTA CLARA", "SOLANO")
nor_sj_valley_counties <- c("ALPINE", "AMADOR", "CALAVERAS", "MADERA", "MARIPOSA", "MERCED", "MONO", "SAN JOAQUIN", "STANISLAUS", "TUOLUMNE")
central_coast_counties <- c("MONTEREY", "SAN BENITO", "SAN LUIS OBISPO", "SANTA BARBARA", "SANTA CRUZ", "VENTURA")
so_sj_valley_counties <- c("FRESNO", "INYO", "KERN", "KINGS", "TULARE")
inland_empire_counties <- c("RIVERSIDE", "SAN BERNARDINO")
los_angeles_county <- c("LOS ANGELES")
orange_county <- c("ORANGE")
imperial_sd_counties <- c("IMPERIAL", "SAN DIEGO")

# only include service areas on map that correspond to source points
pwsid <- unique(df$PWS.ID)
pwsid <- sort(pwsid)
sb <- subset(service_areas, pwsid %in% df$PWS.ID)

regions <- list(superior_ca_counties, north_coast_counties, sf_bay_area_counties, nor_sj_valley_counties, central_coast_counties, so_sj_valley_counties, inland_empire_counties, los_angeles_county, orange_county, imperial_sd_counties)

fnsubset<-function(regions, areas){
  areas<-subset(sb, sb$d_prin_cnty_svd_nm==regions)
}
regional_areas <- lapply(regions, fnsubset)

superior_ca <- subset(service_areas, superior_ca_counties %in% sb$d_prin_cnty_svd_nm)
