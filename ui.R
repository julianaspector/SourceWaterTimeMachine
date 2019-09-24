# ui.R

ui<-fluidPage(
  useShinyjs(),
  titlePanel("California Public Water Systems Source Locations and Information"),
  sidebarPanel("The State Water Board's Division of Drinking Water (DDW) regulates approximately 7,500 public water systems (PWS) in California. A PWS is defined as a system that provides drinking water for human consumption to 15 or more connections or regularly serves 25 or more people daily for at least 60 days out of the year. The DDW allocates permits for PWS, and collects an annual report from each system.",
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
                 column(6, offset=1, tableOutput('tbl_facilities')),
                 column(6, offset=1, tableOutput('tbl_availability'))
               ),
               br(),
               tags$b("Data Sources:"),
               br(),
               br(),
               "Tracking California, Public Health Institute. Water Boundary Tool. Accessed 7/9/2019 from https://www.trackingcalifornia.water",
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