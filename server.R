# server.R

# Insantiate leaflet map
map <- leaflet()%>% addTiles()


shinyServer(
  function(input, output){
    
    output$map <- renderLeaflet({
      polygon <- subset(regional_areas)
      map  <- addPolygons(data=regional_areas[[input$region]], popup = ~pwsid)
      map  
    })

  }
)

#sourceWaterlocations <- subset(df, PWS.ID == input$pwsid)

#addMarkers(map, sourceWaterlocations$Longitude, sourceWaterlocations$Latitude, popup=paste("Name",sourceWaterlocations$WSF.Name,"<br>","Type",sourceWaterlocations$WSF.Type))%>%