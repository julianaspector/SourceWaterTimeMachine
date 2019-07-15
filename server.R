# server.R

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

# add data to map
shinyServer(
  function(input, output, session){
    # this allows the user to select a PWSID within the county of their choice
    output$secondSelection <- renderUI({
      PWSIDs <- subset(serviceAreas, serviceAreas$d_prin_cnt==input$counties)
      selectInput("pwsid", choices = sort(PWSIDs$pwsid), label="PWSID")
    })
    # show source water availability key
    output$tbl_availability <- renderTable({head(availability_keys, n = 7)}, bordered=TRUE)
    # show facilties key
    output$tbl_facilities <- renderTable({
      head(facility_keys, n = 2)}, bordered=TRUE)
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
      # removes error message and replaces it with a helpful message
      #validate(
        #need(input$pwsid != "", "Please select a PWSID in Maps tab.")
        #)
      sb <- subset(production_data, PWSID==input$pwsid & Date==sliderMonth$Month)
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
      map  <- addMarkers(map,data=points, lng=~Longitude, lat=~Latitude, popup=paste("Name:",points$WSF.Name,"<br>", "Source Availability:", points$Availability, "<br>", "Water Source Facility Type:", points$WSF.Type))
      # make selected pws area red
      map <- addPolygons(map, data=areas_interest, color="red")
      map
    }
    )
  # reset PWSID if user hits button
  output<-observeEvent(input$resetPWSID,{
    reset("secondSelection")
  })
  }
)

