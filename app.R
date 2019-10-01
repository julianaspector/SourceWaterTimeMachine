#Load required libraries
library(leaflet)
library(shinyjs)
library(sf)
library(rmapshaper)
library(dplyr)
library(ggplot2)
library(scales)
library(spatstat)
library(tidyr)


# Get public water system service areas from shapefile
serviceAreas <- read_sf(dsn="Service_Areas_2019_09_25/service_areas.shp",layer="service_areas")

# Get source point data
sourcePoints <- read.csv("20190619 DDW Source Points.csv")
sourcePoints$join_ID <- paste0(sourcePoints$PWS.ID,"-",sourcePoints$State.Asgn.ID)
sourcePoints$join_ID <- gsub('[CA]', '', sourcePoints$join_ID)
sourcePoints %>% mutate_at("join_ID", as.character())

# Get additional source point status and type location from EDF library
add_sourcePoints <- read.csv("sitelocations_20190915.csv")

# join source point information
add_sourcePoints$SYSTEM_NO <- paste0("CA0", add_sourcePoints$SYSTEM_NO)
add_sourcePoints <- add_sourcePoints %>% rename("join_ID"="PRI_STA_C") %>% mutate_at("join_ID", as.character())
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
water_type_keys <- read.csv("Water_Facility_Types.csv")
availability_keys <- read.csv("Availability.csv")
status_keys <- read.csv("status_table.csv")

# create levels based on source water operation statuses
abandoned <- sourcePoints %>%
  filter(STATUS =='AB')
abandoned$Level <- "Abandoned"
abandoned$Color <- "red"

destroyed <- sourcePoints %>% 
  filter(STATUS == 'DS')
destroyed$Level <- 'Destroyed'
destroyed$Color <- "darkred"

inactive <- sourcePoints %>%
  filter(STATUS == 'IR' | STATUS == 'IT' | STATUS == 'IU')
inactive$Level <- 'Inactive'
inactive$Color <- "orange"

standby <- sourcePoints %>%
  filter(STATUS == 'SR' | STATUS == 'ST'| STATUS == 'SU')
standby$Level <- 'Standby'
standby$Color <- "lightgreen"

active <- sourcePoints %>%
  filter(STATUS == 'AR' | STATUS == 'AT' | STATUS == 'AU')
active$Level <- 'Active'
active$Color <- "green"

agriculture <- sourcePoints %>%
  filter(STATUS == 'AG')
agriculture$Level <- 'Agriculture/Irrigation'
agriculture$Color <- "lightblue"

distribution <- sourcePoints %>%
  filter(STATUS == 'DT' | STATUS == 'DR')
distribution$Level <- 'Distribution'
distribution$Color <- "darkblue"

combined <- sourcePoints %>%
  filter(STATUS == 'CT' | STATUS == 'CU' | STATUS == 'CR' | STATUS == 'CM')
combined$Level <- 'Combined'
combined$Color <- "purple"

purchased <- sourcePoints %>%
  filter(STATUS == 'PT')
purchased$Level <- 'Purchased'
purchased$Color <- "darkpurple"

unknown <- sourcePoints %>%
  filter(is.na(STATUS))
unknown$Level <- 'Unknown'
unknown$Color <- "pink"


sourcePoints <- rbind(abandoned, destroyed, inactive, standby, active, agriculture, distribution, combined, purchased, unknown)

# hide columns
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
production_data <- read.csv("2013-2016 Production Final.csv")
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
  addProviderTiles("Stamen.Toner", options=providerTileOptions(
    updatewhenZooming=FALSE,
    updatesWhenIdle=TRUE
  ))

# legend html generator:
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "ion") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 35px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 8px; margin-top: 45px; 'class= 'ion ion-",Icon[["icon"]]," ion-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: 12px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

IconSet <- awesomeIconList(
  "Abandoned"   = makeAwesomeIcon(icon= 'whatever', markerColor = 'red', iconColor = 'black', library = "ion"),
  "Destroyed" = makeAwesomeIcon(icon= 'whatever', markerColor = 'darkred', iconColor = 'black', library = "ion"),
  "Inactive" = makeAwesomeIcon(icon= 'whatever', markerColor = 'orange', iconColor = 'black', library = "ion"),
  "Standby" = makeAwesomeIcon(icon= 'whatever', markerColor = 'lightgreen', iconColor = 'black', library = "ion"),
  "Active" = makeAwesomeIcon(icon= 'whatever', markerColor = 'green', iconColor = 'black', library = "ion"),
  "Agriculture/Irrigation" = makeAwesomeIcon(icon= 'whatever', markerColor = 'lightblue', iconColor = 'black', library = "ion"),
  "Distribution" = makeAwesomeIcon(icon= 'whatever', markerColor = 'darkblue', iconColor = 'black', library = "ion"),
  "Combined" = makeAwesomeIcon(icon= 'whatever', markerColor = 'purple', iconColor = 'black', library = "ion"),
  "Purchased" = makeAwesomeIcon(icon= 'whatever', markerColor = 'darkpurple', iconColor = 'black', library = "ion"),
  "Unknown" = makeAwesomeIcon(icon= 'whatever', markerColor = 'pink', iconColor = 'black', library = "ion")
)



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
               fluidRow(
                 column(12, tableOutput('tbl_status'))
               ),
               br(),
               tags$b("Data Sources:"),
               br(),
               br(),
               "Division of Drinking Water Source Points, State Water Resources Control Board. Safe Drinking Water Information System. Accessed 7/2/2019 from https://github.com/CAWaterBoardDataCenter/PWStoSources",
               br(),
               br(),
               "Drinking Water Source identification number (PS Code), name, and type (SITELOC.DBF), State Water Resources Control Board. Electronic Data Transfer Library. Accessed 9/25/2019 from https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/EDTlibrary.html",
               br(),
               br(),
               "Electronic Annual Report, Safe Drinking Water Information System. Accessed 7/2/2019 from https://data.ca.gov/dataset/drinking-water-public-water-system-annually-reported-water-production-and-delivery-information",
               br(),
               br(),
               "Tracking California, Public Health Institute. Water Boundary Tool. Accessed 9/25/2019 from https://www.trackingcalifornia.water"
               
  ),
  mainPanel(
    tabsetPanel(type="tabs",
                tabPanel("Map", 
                         # column widths must be between 1-12
                         fluidRow(
                           column(width=12,
                                  helpText("Please select a county."),
                                  selectInput("counties", choices=counties, label="Counties"), align="center")
                         ),
                         fluidRow(
                           column(width=12,
                                  helpText("Please select a Public Water System ID."),
                                  uiOutput("secondSelection"), align="center")
                         ),
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
      selectInput("pwsid", selected="PWSID", choices = sort(PWSIDs$pwsid), label="PWSID")
    })
    # show source water availability key
    output$tbl_availability <- renderTable({head(availability_keys, n = 7)}, bordered=TRUE)
    # show facilties key
    output$tbl_types <- renderTable({
      head(water_type_keys, n = 2)}, bordered=TRUE)
    # show status key
    output$tbl_status <- renderTable({
      head(status_keys, n=10)}, bordered=TRUE)
    
    # this creates a table of records for select PWSID
    output$table_select <- renderTable({
      tb <- select(sourcePoints, -c("Color", "Latitude", "Longitude", "SYSTEM_NO"))
      subset(tb, PWS.ID==input$pwsid)})
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
    areas <- reactive({
      subset(serviceAreas, d_prin_cnt == input$counties)
    })
    points <- reactive({
      subset(sourcePoints, PWS.ID == input$pwsid) 
    })
    areas_interest <- reactive({
      subset(serviceAreas, pwsid==input$pwsid)
    })
    icons <- reactive({
      awesomeIcons(icon="whatever",
                  iconColor="black",
                  library="ion",
                  markerColor=points()$Color)
    })
    
    
    output$map <- renderLeaflet({
      leaflet() %>% addProviderTiles(providers$Stamen.Toner) %>% addPolygons(data=areas(), popup=areas()$pwsid, color="#67a9cf")%>%
        addPolygons(data=areas_interest(), color="#ef8a62") %>%
        addLegend("bottomleft", colors=c("#67a9cf", "#ef8a62"), labels=c("Public Water Systems in County", "Selected Public Water System"))
      })

      
  observe({     
     map <- leafletProxy("map") %>%
       clearMarkers() %>%
       addAwesomeMarkers(data=points(),lng=~Longitude, lat=~Latitude, icon=icons(), popup=~paste("Name:", as.character(WSF.Name),"<br>", 
                                                                                                        "Source Availability:", 
                                                                                                        as.character(Availability), "<br>", 
                                                                                                        "Water Source Facility Type:", 
                                                                                                        as.character(WSF.Type), "<br>",
                                                                                                        "Water Source Status:",
                                                                                                        as.character(STATUS))) %>%
       addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomright")
     

    })
}
# Run the application 
shinyApp(ui = ui, server = server)