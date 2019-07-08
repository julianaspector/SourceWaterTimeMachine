# ui.R

ui<-fluidPage(
  titlePanel("Source Water Locations"),
  fluidRow(
    column(6,
           helpText("Please select a county."),
           selectInput("counties", choices=counties, label="Counties"),
    fluidRow(
      column(6,
      helpText("Please copy and paste a PWSID."),
      textInput("pwsid", "PWSID", "PWSID")
    )),
  leafletOutput("map")
  )
))
