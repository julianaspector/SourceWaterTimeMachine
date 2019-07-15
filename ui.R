# ui.R

ui<-fluidPage(
  useShinyjs(),
  titlePanel("California Public Systems Source Water Locations and Information"),
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
                         # make keys and map appear in same row
                         fluidRow(
                           column(10, leafletOutput("map")),
                           column(1, offset=1, tableOutput('tbl_facilities')),
                           column(1, offset=1, tableOutput('tbl_availability'))
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