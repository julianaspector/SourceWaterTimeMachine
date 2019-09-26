#Load required libraries
library(leaflet)
library(shinyjs)
library(sf)
library(rmapshaper)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)
library(spatstat)
library(forcats)

setwd("C:/Users/JSpector/Documents/")

# Get dataset (public water system service areas) from shapefile
serviceAreas <- read_sf(dsn="~/Source Water Time Machine/Service_Areas_2019_09_25/service_areas.shp",layer="service_areas")

# Get source point data from CSV
sourcePoints <- read.csv("~/Source Water Time Machine/20190619 DDW Source Points.csv")
sourcePoints$join_ID <- paste0(sourcePoints$PWS.ID,"-",sourcePoints$State.Asgn.ID)
sourcePoints$join_ID <- gsub('[CA]', '', sourcePoints$join_ID)

# Get additional source point status and type location from EDF library CSV
add_sourcePoints <- read.csv("~/Source Water Time Machine/sitelocations_20190915.csv")

# join source point information
add_sourcePoints$SYSTEM_NO <- paste0("CA0", add_sourcePoints$SYSTEM_NO)
add_sourcePoints <- add_sourcePoints %>% rename("join_ID"="PRI_STA_C")
sourcePoints <- full_join(sourcePoints, add_sourcePoints, by="join_ID")
sourcePoints <- sourcePoints %>% drop_na("Latitude")


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
water_type_keys <- read.csv("~/Source Water Time Machine/Water_Facility_Types.csv")
availability_keys <- read.csv("~/Source Water Time Machine/Availability.csv")


abandoned <- sourcePoints %>%
  filter(STATUS =='AB')
abandoned$Level <- "Abandoned"

destroyed <- sourcePoints %>% 
  filter(STATUS == 'DS')
destroyed$Level <- 'Destroyed'

inactive <- sourcePoints %>%
  filter(STATUS == 'IR' | STATUS == 'IT' | STATUS == 'IU')
inactive$Level <- 'Inactive'

standby <- sourcePoints %>%
  filter(STATUS == 'SR' | STATUS == 'ST'| STATUS == 'SU')
standby$Level <- 'Standby'

active <- sourcePoints %>%
  filter(STATUS == 'AR' | STATUS == 'AT' | STATUS == 'AU')
active$Level <- 'Active'

agriculture <- sourcePoints %>%
  filter(STATUS == 'AG')
agriculture$Level <- 'Agriculture/Irrigation'

distribution <- sourcePoints %>%
  filter(STATUS == 'DT' | STATUS == 'DR')
distribution$Level <- 'Distribution'

combined <- sourcePoints %>%
  filter(STATUS == 'CT' | STATUS == 'CU' | STATUS == 'CR' | STATUS == 'CM')
combined$Level <- 'Combined'

purchased <- sourcePoints %>%
  filter(STATUS == 'PT')
purchased$Level <- 'Purchased'

unknown <- sourcePoints %>%
  filter(is.na(STATUS))
unknown$Level <- 'Unknown'


sourcePoints <- rbind(abandoned, destroyed, inactive, standby, active, agriculture, distribution, combined, purchased, unknown)

sourcePoints <- select(sourcePoints, -c(PWS.Type,
                                        Activity.Status,
                                        Activity.Status2,
                                        State.Asgn.ID,
                                        FRDS_NO,
                                        COUNTY,
                                        DISTRICT,
                                        USER_ID,
                                        STATION_TY,
                                        COMMENT_1,
                                        SOURCE_NAM,
                                        PWS.Name,
                                        join_ID,
                                        WATER_TYPE))
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
# Make leaflet map, with basemap
map<-leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  htmlwidgets::onRender("function(el, x) {
                        L.control.zoom({ position: 'topright' }).addTo(this)
                        }") %>%
  # make basemap load faster
  addProviderTiles("Esri.WorldTopoMap", options=providerTileOptions(
    updatewhenZooming=FALSE,
    updatesWhenIdle=TRUE
  ))

ui<-fluidPage(
  useShinyjs(),
  titlePanel("California Public Water Systems Source Locations and Information"),
  sidebarPanel("The State Water Board's Division of Drinking Water (DDW) regulates approximately 7,500 public water systems (PWS) in California. A PWS is defined as a system that provides drinking water for human consumption to 15 or more connections or regularly serves 25 or more people daily for at least 60 days out of the year. The DDW allocates permits for PWS and collects an annual report from each system.",
               br(),
               br(),
               tags$b("How to use this app:"), 
               br(),
               "1) Select a county of interest to zoom map into relevant area.",
               br(),
               "2) Click on public water system area to see Public Water System ID.",
               br(),
               "3) Select a Public Water System ID of interest.",
               br(),
               "4) Click on pins to see name of water source(s), water source availability, and water source facility type. The same information is available in tabular format in the Table tab.",
               br(),
               "5) The Graphs tab shows how the water source composition (relative fractions of groundwater, surface water, and delivered water) changed between 2013-2016.",
               br(),
               br(),
               fluidRow(
                 column(6, offset=1, tableOutput('tbl_types')),
                 column(6, offset=1, tableOutput('tbl_availability'))
               ),
               br(),
               tags$b("Data Sources:"),
               br(),
               br(),
               "Tracking California, Public Health Institute. Water Boundary Tool. Accessed 9/25/2019 from https://www.trackingcalifornia.water",
               br(),
               br(),
               "Division of Drinking Water Source Points, State Water Resources Control Board. California Integrated Water Quality System Project. Accessed 7/2/2019 from https://github.com/CAWaterBoardDataCenter/PWStoSources",
               br(),
               br(),
               "Electronic Annual Report, Safe Drinking Water Information System. Accessed 7/2/2019 from https://data.ca.gov/dataset/drinking-water-public-water-system-annually-reported-water-production-and-delivery-information"
  ),
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Map", 
                         # column widths must be between 1-12
                         fluidRow(
                           column(width=6,
                                  helpText("Please select a county."),
                                  selectInput("counties", choices=counties, label="Counties"))
                         ),
                         fluidRow(
                           column(width=12,
                                  helpText("Please select a Public Water System ID."),
                                  uiOutput("secondSelection"))
                         ),
                         fluidRow(
                           column(width=12,
                                  actionButton("resetPWSID", "Reset PWSID"))
                         ),
                         # add a break beneath button
                         br(),
                         fluidRow(
                           column(width=12,
                                  leafletOutput("map"))
                         )
                ),
                tabPanel("Table", tableOutput("table_select")),
                tabPanel("Graphs", 
                         fluidRow(
                           column(12, sliderInput("date", "Date:", min=as.Date("2013-01-31"), max=as.Date("2016-12-31"),value=as.Date("2014-12-01"), timeFormat="%b %Y"),
                                  textOutput("SliderText"))
                         ),
                         fluidRow(
                           column(12, plotOutput("pie_chart"))
                         )
                )
    )
  )
)



# add data to map
server<- function(input, output, session){
    # this allows the user to select a PWSID within the county of their choice
    output$secondSelection <- renderUI({
      PWSIDs <- subset(serviceAreas, serviceAreas$d_prin_cnt==input$counties)
      selectInput("pwsid", choices = sort(PWSIDs$pwsid), label="PWSID")
    })
    # show source water availability key
    output$tbl_availability <- renderTable({head(availability_keys, n = 7)}, bordered=TRUE)
    # show facilties key
    output$tbl_types <- renderTable({
      head(water_type_keys, n = 2)}, bordered=TRUE)
    # this creates a table of records for select PWSID
    output$table_select <- renderTable({
      subset(sourcePoints, PWS.ID==input$pwsid)})
    # allows user to select a date along slider between 2013-2016
    sliderMonth <- reactiveValues()
    observe({
      full.date <- as.POSIXct(input$date, tz="GMT")
      sliderMonth$Month <- as.character(monthStart(full.date))
    })
    output$SliderText <- renderText({sliderMonth$Month})
    # this produces a pie chart with proportions of water produced from 
    # SW, GW, or delivered from another PWS
    output$pie_chart <- renderPlot({
      sb <- subset(production_data, PWSID==input$pwsid & Date==sliderMonth$Month)
      # this provides a helpful message if production data is not available for selected date
      validate(
        need(!is.empty(sb$Date), 'No data available for this date.')
      )
      fields <- c("Groundwater", "Surface Water", "Delivered Water")
      pie_data <- data.frame(
        field=factor(fields[c(1,2,3)], levels=fields),
        proportions=(c(sb$gw_prop, sb$sw_prop, sb$dw_prop)))
      ggplot(pie_data, aes(x = "", y=proportions, fill = factor(field))) + 
        theme(axis.text = element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank())+
        geom_bar(width = 1, stat="identity")+
        geom_col(width=1)+
        coord_polar(theta = "y", direction=-1)+
        # remove proportions = 0% and label percentages to ones place accuracy
        geom_text(data=subset(pie_data, proportions != 0), aes(label=percent(proportions, accuracy=1)), 
                  size=5,
                  color="white",
                  position = position_stack(vjust = 0.5))+labs(fill="Water Type")+
        ggtitle("Produced and Delivered Water Fractions (2013-2016)")
      
      
    })
    # show map
    output$map <- renderLeaflet({
      # user chooses county of interest
      areas <- subset(serviceAreas, d_prin_cnt == input$counties)
      # user chooses water system of interest
      points <- subset(sourcePoints, PWS.ID == input$pwsid)
      areas_interest <- subset(serviceAreas, pwsid==input$pwsid)
      # add public water system service area of interest to map
      map  <- addPolygons(map, data=areas, popup=areas$pwsid)
      # add source water points to map
      pal <- colorFactor(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
                         domain=c("Abandoned", "Active", "Agriculture/Irrigation",
                                  "Combined", "Destroyed", "Distribution",
                                  "Inactive", "Purchased",
                                  "Standby", "Unknown"))
      # make selected pws area red
      map <- addPolygons(map, data=areas_interest, color="red")
      
      pal <- colorFactor(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"),
                         domain=c("Abandoned", "Active", "Agriculture/Irrigation",
                                  "Combined", "Destroyed", "Distribution",
                                  "Inactive", "Purchased",
                                  "Standby", "Unknown"))
      map  <- addCircles(map,data=points, lng=~Longitude, lat=~Latitude, color=~pal(Level), popup=paste("Name:",points$WSF.Name,"<br>", 
                                                                                                        "Source Availability:", 
                                                                                                        points$Availability, "<br>", 
                                                                                                        "Water Source Facility Type:", 
                                                                                                        points$WSF.Type, "<br>",
                                                                                                        "Water Source Status:",
                                                                                                        points$STATUS))
      map
    }
    )
    # reset PWSID if user hits button
    output<-observeEvent(input$resetPWSID,{
      reset("secondSelection")
    })
  }
# Run the application 
shinyApp(ui = ui, server = server)