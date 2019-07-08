ui<-fluidPage(
  titlePanel("Source Water Locations"),
  mainPanel(
  leafletOutput('map', height=1000)),
  sidebarPanel(
  fluidRow(
  column(12,
  helpText("Select Region of interest"),
  selectInput("region", "Region:", c("Region 1: Superior CA"= 1,
                                       "Region 2: North Coast"= 2,
                                        "Region 3 : San Francisco Bay Area"= 3,
                                        "Region 4 : Northern San Joaquin Valley"= 4,
                                        "Region 5: Central Coast" = 5,
                                        "Region 6: Southern San Joaquin Valley"= 6,
                                        "Region 7: Inland Empire" = 7,
                                        "Region 8: Los Angeles County"= 8,
                                        "Region 9: Orange County"= 9,
                                        "Region 10: San Diego-Imperial"= 10))
  )
  )
)
)


#column(6,
       #helpText("Enter PWSID of interest."),
       #textInput("pwsid", "PWSID", "")
#),