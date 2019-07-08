# ui.R

ui<-fluidPage(
  useShinyjs(),
  titlePanel("Source Water Locations"),
  fluidRow(
    column(4,
           helpText("Please select a county."),
           selectInput("counties", choices=counties, label="Counties")),
    fluidRow(
        column(4,
        helpText("Please copy and paste a PWSID."),
        textInput("pwsid", "PWSID"))),
    fluidRow(
        column(4,
        actionButton("resetPWSID", "Reset PWSID"))
    ),
  leafletOutput("map")
    )
)
