# server.R

# Insantiate leaflet map
map <- leaflet()
map <- addTiles(map)

shinyServer(
  function(input, output) {

  output$map <- renderLeaflet({
    areas <- subset(serviceAreas, d_prin_cnt == input$counties)
    points <- subset(sourcePoints, PWS.ID == input$pwsid)
    map  <- addPolygons(map, data=areas, popup=areas$pwsid)
    map  <- addMarkers(map,data=points, lng=~Longitude, lat=~Latitude, popup=paste("Name:",points$WSF.Name,"<br>", "Source Availability:", points$Availability, "<br>", "Water Source Facility Type:", points$WSF.Type))
    map
    }
  )

  output<-observeEvent(input$resetPWSID, {
    reset("pwsid")
    })
  }
)
  
